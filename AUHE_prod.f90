
! ---------------------------------------------------------------------
! Programa de asignacion de unidades termo e hidro con aspectos de    *
! seguridad para el Mercado en un Dia en Adelanto (AU-CHT), y con las *
! siguientes caraterísticas:                                          *
!                                                                     *
! a)	Manejo de ofertas de venta de energía al mercado              *
! b)	Manejo de pronostico de demanda de energia electrica          *
! c)	Manejo de ofertas de servicios auxiliares (reservas de        *
!       generación y demandas controlables)                           *
! d)	Consideración de demandas de capacidad para la reserva        *
! e)	Consideración de diferentes configuraciones topológicas de la *
!       red a nivel horario                                           *
! f)	Cálculo de precios nodales considerando pérdidas en la red    *
! g)	Inclusion del modelado de embalses y red hidraulica           *
!                                                                     *
!                                                                     *
! Instituto Nacional de Electricidad y Energias Limpias               *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Desarrolladores: Juan Alvarez Lopez                                 *
!                  Armando de la Torre Sanchez                        *
!                  Jose Luis Ceciliano Meza                           *
!                                                                     *
! Junio de 2019                                                       *
! ---------------------------------------------------------------------
Program AUCHT

use ParAUHE
use ProblemaAUHE
use ParGloRed, only: NumGruRamSis, RamActiva

use symtypes
!use cplex_ifaces, only: CPXsetintparam, CPXfopen, CPXsetlogfile   !Windows (comentarizar para Linux)
use cplex_cons, only:   CPX_ON, CPX_PARAM_SCRIND, CPX_OFF, CPX_PARAM_CLONELOG

implicit none

integer                 CPXsetintparam, CPXfopen, CPXsetlogfile   !Linux (comentarizar para Windows)

integer ibanbit, ierror, ite, status, sistema, EstaoIslaCopy ( maxsis )
integer(8) logfile

CHARACTER fecha_Ej*19

CHARACTER(20):: NOM_PRG

real   time_ini, time_fin, time_cpu
character*16 aux


CALL OBTEN_PID
call CPU_TIME ( time_ini )

! Se abre archivo de Bitacora
OPEN ( UNIT = 444, FILE = RUT_RES//'Bitacora.res', IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3024 )

!Se abre archivo de bitacora de lecturas y se obtiene ruta de lectura
call Abre_Bit_Lec

!Se lee horizonte
call data_horizo

! Tipo de ejecucion
SELECT CASE ( TipoEjecu )
!   MDA
    CASE ( 0 )
        NOM_PRG='MDA'
!   AUGC
    CASE ( 1 )
        NOM_PRG='AUGC'
!   EXPOST
    CASE ( 2 )
        NOM_PRG='EXPOST'
!   AUHE
    CASE ( 3 )
        NOM_PRG='AUHE'
END SELECT

CALL ABRBIT (NOM_PRG)


! Se abre archivo de estado de ejecucion
OPEN ( UNIT = 555, FILE = RUT_RES//'BANDERA_'//trim(NomEjecu)//'.res', IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 250 )
!Algoritmo en proceso
rewind(555)
write(555,*) '2'

! Abre ambiente de CPLEX
Call openCplex

logfile = CPXfopen ("./dirres/Optimizador.log", "w");
status = CPXsetlogfile (enb, logfile);
ibanbit = 1
ierror = 0
RamActiva = 0

status = CPXsetintparam(enb, CPX_PARAM_CLONELOG, -1)

Call FechaEjecucion (fecha_Ej)
BMensaje = fecha_Ej//' '//NomEjecu//'001 VERSION 1.0.1'
call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
Call FechaEjecucion (fecha_Ej)
bmensaje = fecha_Ej//' '//NomEjecu//'001 INICIA ASIGNACION DE UNIDADES '//NomEjecu
Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )

! escitura al archivo Version_ control del ejcutable
OPEN ( UNIT = 999, FILE = RUT_RES//'Version_'//trim(NomEjecu)//'.res', IOSTAT = IERROR, &
       STATUS='UNKNOWN', RECORDSIZE = 250                )
     write( 999, 100)

!goto 1234

! Se abre archivo csv de banderas que indican la calidad de la solucion obtenida
UniSemaf = 155
OPEN ( UNIT = UniSemaf, FILE = trim(rut_dat_1)//'SEMAFOROS'//trim(TipoLec)//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )

!Se leen datos de subsistemas
call LeeSubsistemas
EstaoIslaCopy = EstadoIsla

! Se lee informacion de la red eléctrica
call LecDatRedElectrica

! para todos los subsistemas
do sistema = 1, numsis
    
    EstadoIsla = 0

!   si es sistema esta activo
    if ( EstaoIslaCopy ( sistema ) .eq. 1 ) then

!       se resolvera el sistema
        EstadoIsla ( sistema ) = 1
      
        ibanbit = 1
        ierror = 0
        ite = 0
        
!       se llama a la subrutina de lecturas
        call Lecturas_AUHE

!       En AUGC no hay estimacion de perdidas
!        if ( TipoEjecu .eq. 1 ) then
!            SiPerdidas = 0
!        endif

!       escribe a pantalla mensajes de CPLEX
        status = CPXsetintparam (enb, CPX_PARAM_SCRIND, CPX_ON)

!       preparacion de informacion de nodos por subsistema
        call PreDatosSistema ( sistema )

!       si no existe transmision en el subsistema
        if ( NumGruRamSis ( sistema ) .eq. 0 ) then
            SiConjuntoActivo = 0
            SiTransmision = 0
        endif
        
!       se detrminan dimensiones conforme a los parametros de sintonizacion
        call DefineDimensionesMILP
        
!       se abren los archivos de debugger
        call AbreArchivos ( sistema )

!       Se forma y resuleve el problema de asignacion de unidades (AUHE)
        if ( SiConjuntoActivo .eq. 1 ) then
!           Se resuelve el problema con conjunto activo
            call FormaResuleveSCA ( sistema, ite )
        else
!           Se resuelve el problema no con conjunto activo
            call FormaResuleveNCA ( sistema )
        endif

!       Calcula e imprime marginales por nodo y por region
        call CalculaImprimeLMP_PMR ( sistema )        

!       Escribe a archivos CSV los resultados de generacion y estado de las unidades
        call EscribeCSVGen ( sistema )

!       Escribe a archivos CSV los resultados de consumo de combustible
        call EscConsGas ( sistema )
!        call EscConsGas_new ( sistema )

!       Archivo de salida de disponibilidad y asignabilidad para otros algoritmos
        SELECT CASE ( TipoEjecu )
!           MDA
            CASE ( 0 )
                call Asig_Out_MDA
!           AUGC
            CASE ( 1 )
                call Asig_Out_AUGC
            CASE ( 2 )
                call Asig_Out_AUGC
        END SELECT
        
    endif
    
enddo


! Cierra ambiente de CPLEX
call closeCplex

1234 continue
     
call CPU_TIME ( time_fin )

time_cpu = time_fin - time_ini

Call FechaEjecucion (fecha_Ej)
bmensaje = fecha_Ej
Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
Call FechaEjecucion (fecha_Ej)
bmensaje = fecha_Ej//' '//NomEjecu//'002 TERMINACION NORMAL DE ASIGNACION DE UNIDADES'
Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
write ( aux, 5101 ) time_cpu
BMensaje = fecha_Ej//'         '
Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
Call FechaEjecucion (fecha_Ej)
BMensaje = fecha_Ej//' '//NomEjecu//'002 TIEMPO DE CPU (seg) '//aux
Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )

write(*,*) '0'
write(998,*) '0'

!Terminacion normal del algoritmo
rewind(555)
write(555,*) '0'

!goto 4321

! Se ecribe resultado de semaforos
write ( UniSemaf, * ) SemBandera ( 1 ),',',' Corte de energia'
write ( UniSemaf, * ) SemBandera ( 2 ),',',' Excedente de energia'
write ( UniSemaf, * ) SemBandera ( 3 ),',',' Escasez de reservas'
write ( UniSemaf, * ) SemBandera ( 4 ),',',' Infactibilidad en transmision'
write ( UniSemaf, * ) SemBandera ( 5 ),',',' Violacion de limites de energia termo'
write ( UniSemaf, * ) SemBandera ( 6 ),',',' Violacion de limites de energia hidro'
write ( UniSemaf, * ) SemBandera ( 7 ),',',' Problema de optimizacion infactible'
!write ( UniSemaf, * ) SemBandera ( 8 ),',',' Violacion de limites de combustible'

4321 continue

5100 FORMAT (I3)
5101 FORMAT (F8.2)


100  FORMAT (1X,'_',//, &
     1X,'+=========================================================+',/, &
     1X,'|      Bitacora de cambios del ejecutable del AUHE        |',/, &
     1X,'+=========================================================+',/, &
     1X,'|                                                         |',/, &
     1X,'|  Fecha                       Causa                      |',/, &
     1X,'|                                                         |',/, &
     1X,'|  2016/01/31     Version inicial                         |',/, &
     1X,'|  2017/05/19     Inclusion de los nuevos conceptos en    |',/, &
     1X,'|                 MDA, AUGC y EXPOST, al AUHE. Se obtiene |',/, &
     1X,'|                 un solo algoritmo que representa a los  |',/, &
     1X,'|                 cuatro algoritmos.                      |',/, &
     1X,'|  2017/07/17     Incremento de dimension en numero de    |',/, &
     1X,'|                 unidades no programables, de 350 a 400. |',/, &
     1X,'|  2017/08/02     Inclusion del archivo BANDERA_*.RES     |',/, &
     1X,'|  2017/09/20     Inclusion validacion de minimo numero   |',/, &
     1X,'|                 de unidades para dar reserva de         |',/, &
     1X,'|                 regulacion por zona.                    |',/, &
     1X,'|                 Escalamiento de las perdidas por area.  |',/, &
     1X,'|  2017/10/03     Escritura de flujo en todas las ramas   |',/, &
     1X,'|                 de la red electrica, perdidas y angulos |',/, &
     1X,'|                 de voltaje nodal.                       |',/, &
     1X,'|  2017/12/01     Correccion en la impresion del costo    |',/, &
     1X,'|                 de sincronizacion durante el proceso    |',/, &
     1X,'|                 de arranque, archivo RESCSTOPARR.       |',/, &
     1X,'|  2018/01/08     Incremento de la dimension del numero   |',/, &
     1X,'|                 maximo de unidades renovables (de 50    |',/, &
     1X,'|                 a 60).                                  |',/, &
     1X,'|  2018/01/09     Modificacion del esquema para acotar    |',/, &
     1X,'|                 y penalizar la variable de perdidas,    |',/, &
     1X,'|                 para el sistema BCA.                    |',/, &
     1X,'|  2018/01/29     Calculo de rampa de arranque con base   |',/, &
     1X,'|                 a la potencia minima en todo el         |',/, &
     1X,'|                 horizonte de planeacion.                |',/, &
     1X,'|  2018/01/30     Incremento de dimension en el numero    |',/, &
     1X,'|                 de unidades no programables, de 400 a   |',/, &
     1X,'|                 450 unidades.                           |',/, &
     1X,'|  2018/02/22     Cambio en la consideracion del minimo   |',/, &
     1X,'|                 de unidades que aportan regulacion por  |',/, &
     1X,'|                 zona (ahora es por intervalo).          |',/, &
     1X,'|  2018/04/26     Se reportan cortes y excedentes por     |',/, &
     1X,'|                 nodo. Ademas, se escribe el resultado   |',/, &
     1X,'|                 de consumo de combustible por unidad    |',/, &
     1X,'|                 e intervalo.                            |',/, &
     1X,'|  2018/05/08     Se realizan ajustes a la solucion del   |',/, &
     1X,'|                 segundo problema para la determinacion  |',/, &
     1X,'|                 de precios.                             |',/, &
     1X,'|  2018/05/09     Se realiza correccion en restriccion de |',/, &
     1X,'|                 tiempos minimos de operacion de unidades|',/, &
     1X,'|                 de rango continuo.                      |',/, &
     1X,'|  2018/05/16     Escritura del costo marginal (precio    |',/, &
     1X,'|                 sombra) asociado a las restricciones de |',/, &
     1X,'|                 limitacion de energia en unidades termo.|',/, &
     1X,'|  2018/06/13     Escritura del nombre del grupo de       |',/, &
     1X,'|                 unidades termo con restriccion de limite|',/, &
     1X,'|                 de energia (archivo GPOUTERRES).        |',/, &
     1X,'|  2018/06/20     Se realiza correccion en restriccion de |',/, &
     1X,'|                 costos variables de arranque de unidades|',/, &
     1X,'|                 de rango continuo.                      |',/, &
     1X,'|  2018/07/02     Se realiza correccion en la disponibili-|',/, &
     1X,'|                 dad de compensadores sincronos con band-|',/, &
     1X,'|                 era igual a 1.                          |',/, &
     1X,'|  2018/07/11     Se cambian las restricciones de limites |',/, &
     1X,'|                 de energia en grupos de unidades termo, |',/, &
     1X,'|                 de horizonte a por dia.                 |',/, &
     1X,'|  2018/08/09     Se realiza ajuste en el segundo problema|',/, &
     1X,'|                 respecto a la variable perdidas (cota   |',/, &
     1X,'|                 superior del primer problema).          |',/, &
     1X,'|                 Se realiza ajuste en el segundo problema|',/, &
     1X,'|                 respecto a la variable limite de        |',/, &
     1X,'|                 energia en embalses, y grupos de unida- |',/, &
     1X,'|                 des termo.                              |',/, &
     1X,'|                 Se agrega proteccion para que en caso de|',/, &
     1X,'|                 resultar infactible el segundo problema,|',/, &
     1X,'|                 el programa no aborte y se reporte la   |',/, &
     1X,'|                 solucion obtenida del primer problema.  |',/, &
     1X,'|  2018/08/17     Se realiza ajuste en el segundo proble- |',/, &
     1X,'|                 ma para el calculo de la variable dual  |',/, &
     1X,'|                 de la restriccion de perdidas.          |',/, &
     1X,'|  2018/08/22     Se realiza nuevo ajuste  restriccion de |',/, &
     1X,'|                 tiempos minimos de operacion de unidades|',/, &
     1X,'|                 de rango continuo.                      |',/, &
     1X,'|                 No se acota la variable de perdidas.    |',/, &
     1X,'|  2018/09/26     Se agrega la opcion para resolver el    |',/, &
     1X,'|                 segundo problema de ajuste de precios.  |',/, &
     1X,'|  2018/11/15     Correccion en impresion de resultados   |',/, &
     1X,'|                 por area.                               |',/, &
     1X,'|  2018/11/20     Se agrega sintonizacion de parametros   |',/, &
     1X,'|                 recomendada por soporte tecnico de CPLEX|',/, &
     1X,'|                 Sintonizacion interna de penalizaciones |',/, &
     1X,'|                 al modelo.                              |',/, &
     1X,'|                 Version 12.8 de CPLEX.                  |',/, &
     1X,'|                 Sintomizacion interna de penalizaciones.|',/, &
     1X,'|                 (solo para AUHE SIN).                   |',/, &
     1X,'|                 Mejora en la aproximacion de fun hidro. |',/, &
     1X,'|                 Mejora en calculo de precios en segundo |',/, &
     1X,'|                 problema (epsilon en reservas).         |',/, &
     1X,'|                 Restriccion de maximo numero de paros en|',/, &
     1X,'|                 el AUHE, por dia.                       |',/, &
     1X,'|  2018/11/26     Correccion de escritura de resultados   |',/, &
     1X,'|                 hidraulicos por dia (RDVAAU).           |',/, &
     1X,'|  2018/12/06     Escritura del porcentaje de unidades que|',/, &
     1X,'|                 son economicas (asignables), en el      |',/, &
     1X,'|                 escenario.                              |',/, &
     1X,'|  2019/01/29     Adecuacion en escalamiento de precios   |',/, &
     1X,'|                 de servicios conexos de acuerdo a la    |',/, &
     1X,'|                 curva de demanda de reservas (EXPOST).  |',/, &
     1X,'|                 Escritura de modelo LP a un archivo,    |',/, &
     1X,'|                 en la carpeta dirres (menos AUHE).      |',/, &
     1X,'|  2019/03/08     Se incorpora proteccion para impedir    |',/, &
     1X,'|                 que como solucion de precios la compo-  |',/, &
     1X,'|                 nente de perdidas sea nula.             |',/, &
     1X,'|                 Se corrige problema en logica de refi-  |',/, &
     1X,'|                 namiento de la funcion de generacion    |',/, &
     1X,'|                 hidro.                                  |',/, &
     1X,'|  2019/03/21     Se escribe a un archivo CSV los valores |',/, &
     1X,'|                 de sensibilidad de perdidas con respec- |',/, &
     1X,'|                 to a las inyecciones en los nodos.      |',/, &
     1X,'|  2019/04/02     Incremento de dimension en el numero    |',/, &
     1X,'|                 de unidades no programables, de 450 a   |',/, &
     1X,'|                 500 unidades.                           |',/, &
     1X,'|                 Escritura a pantalla y bitacora del GAP |',/, &
     1X,'|                 obtenido, despues de cada solucion de   |',/, &
     1X,'|                 del problema MIP de asignacion (CPLEX). |',/, &
     1X,'|  2019/04/24     Escritura de los archivos CORTENODAL    |',/, &
     1X,'|                 EXCEDNODAL en la carpeta de archivos    |',/, &
     1X,'|                 CSV (registro historico).               |',/, &
     1X,'|                 Incorporacion de un porcentaje de la    |',/, &
     1X,'|                 demanda nodal lo cual sera la cota      |',/, &
     1X,'|                 superior de la variable de corte en el  |',/, &
     1X,'|                 nodo (solo AUHE).                       |',/, &
     1X,'|  2019/05/08     Correccion en la escritura del modo de  |',/, &
     1X,'|                 operacion en el archivo RESMODO para las|',/, &
     1X,'|                 unidades de rango continuo no disponi-  |',/, &
     1X,'|                 bles.                                   |',/, &
     1X,'|  2019/05/15     Modificar las restriciones de limita-   |',/, &
     1X,'|                 cion de consumo de combustible y limi-  |',/, &
     1X,'|                 taciones de energía a restricciones por |',/, &
     1X,'|                 rangos horarios.                        |',/, &
     1X,'|  2019/05/15     Correccion en la escritura del modo de  |',/, &
     1X,'|                 operacion en el archivo RESMODO para las|',/, &
     1X,'|                 unidades compensadores sincronos.       |',/, &
!     1X,'|  2019/05/27     Reserva de regulacion incluida en la    |',/, &
!     1X,'|                 reserva operativa.                      |',/, &
!     1X,'|  2019/05/31     Mantener reserva asignada en MDA para   |',/, &
!     1X,'|                 el AUGC.                                |',/, &
     1X,'|                                                         |',/, &
     1X,'+=========================================================+')
     
Close ( 999 )
Close ( 555 )

CALL CIEBIT
CALL INIT_PID

stop

end
 
    
    
! ---------------------------------------------------------------------
! Resuelve el problema de asignacion de unidades termo e hidro del    *
! Mercado en un Dia en Adelanto (AUHE), usando la tecnica de conjunto *
! activo para manejar la transmision.                                 *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre de 2018                                                     *
! ---------------------------------------------------------------------
Subroutine FormaResuleveSCA ( sistema, ite )

use ParAUHE
use ProblemaAUHE
use ParGloRed, only: bangruram, bangruramcopy

use symtypes
!use cplex_ifaces, only: CPXsetintparam, CPXsetdblparam   !Windows (comentarizar para Linux)
use cplex_cons, only:   CPX_ON, CPX_PARAM_SCRIND, CPX_OFF, CPX_PARAM_EPGAP, &
                        CPX_PARAM_SCAIND, CPX_PARAM_EPINT, CPX_PARAM_PROBE, &
                        CPX_PARAM_MIPEMPHASIS, CPX_PARAM_NUMERICALEMPHASIS, &
                        CPX_PARAM_EPRHS, CPX_PARAM_EPOPT, CPX_PARAM_TILIM,  &
                        CPX_PARAM_DIVETYPE, CPX_PARAM_NODEFILEIND,          &
                        CPX_PARAM_HEURFREQ
!use cplex_ifaces, only: CPXwriteprob    !Windows (comentarizar para Linux)

implicit none
integer                 CPXsetintparam, CPXsetdblparam, CPXwriteprob   !Linux (comentarizar para Windows)

integer ibanbit, ierror, ite, status, sistema, m, imprime, Semtemp ( 20 )

CHARACTER fecha_Ej*19

real*8 temp
character*16  aaux

ibanbit = 1
ierror = 0
ite = 0
bangruramcopy = bangruram
bangruram = 0
SiViolacion = 1
imprime = 0

! Tolerancia de GAP
temp = GAPCPLEX
status = CPXsetdblparam (enb, CPX_PARAM_EPGAP, temp)

! Maximo de tiempo de solucion para escenarios semanales
temp = LIMIT_TIME_LINEAR
status = CPXsetdblparam (enb, CPX_PARAM_TILIM, temp)

! escalamiento (0=default, -1=no, 1=agresivo)
!status = CPXsetintparam (enb, CPX_PARAM_SCAIND, 1)

! Probar valores de variables binarias antes de resolver el nodo raiz
! (-1=No probing, 0=AUtomatic(default), 1=Moderate, 2= Aggressive, 3= Very aggressive)
!status = CPXsetintparam (enb, CPX_PARAM_PROBE, 3);  

SemBandera = 0

! se forma y resuelve el problema de Asignacion y Despacho (MILP), sin perdidas
call FormaResuelveMILP ( ite, sistema )

!goto 5555

! Controls trade-offs between speed, feasibility, optimality, and moving bounds in MIP
!0      Balance optimality and feasibility; default
!1      Emphasize feasibility over optimality
!2      Emphasize optimality over feasibility
!3      Emphasize moving best bound
!4      Emphasize finding hidden feasible solutions
status = CPXsetintparam (enb, CPX_PARAM_MIPEMPHASIS, 1)

!Controls the MIP dive strategy. The MIP traversal strategy occasionally performs
!probing dives, where it looks ahead at both children nodes before deciding which
!node to choose. The default (automatic) setting lets CPLEX choose when to
!perform a probing dive, 1 (one) directs CPLEX never to perform probing dives, 2
!always to probe, 3 to spend more time exploring potential solutions that are
!similar to the current incumbent. Setting 2, always to probe, is helpful for
!finding integer solutions.
status = CPXsetintparam (enb, CPX_PARAM_DIVETYPE, 2)

!Used when working memory specified by the memory available for working storage
!parameter, (CPX_PARAM_WORKMEM, WorkMem) has been exceeded by the size of the
!tree. If the node file parameter is set to zero when the tree memory limit is
!reached, optimization is terminated. Otherwise, a group of nodes is removed from
!the in-memory set as needed. By default, CPLEX transfers nodes to node files
!when the in-memory set is larger than the current value of memory available for
!working storage, and it keeps the resulting node files in compressed form in
!memory. At settings 2 and 3, the node files are transferred to disk, in
!uncompressed and compressed form respectively, into the directory for working
!files (CPX_PARAM_WORKDIR, WorkDir), and CPLEX actively manages which nodes
!remain in memory for processing.
status = CPXsetintparam (enb, CPX_PARAM_NODEFILEIND, 3)

!Decides how often to apply the periodic heuristic. Setting the value to -1 turns
!off the periodic heuristic. Setting the value to 0 (zero), the default, applies
!the periodic heuristic at an interval chosen automatically. Setting the value to
!a positive number applies the heuristic at the requested node interval. For
!example, setting this parameter to 20 dictates that the heuristic be called at
!node 0, 20, 40, 60, etc.
status = CPXsetintparam (enb, CPX_PARAM_HEURFREQ, 50)

!Emphasizes precision in numerically unstable or difficult problems
!CPX_OFF        Do not emphasize numerical precision; default
!CPX_ON Exercise extreme caution in computation
!status = CPXsetintparam (enb, CPX_PARAM_NUMERICALEMPHASIS, CPX_ON)

!Specifies the amount by which an integer variable can be different from an 
!integer and still be considered feasible
!Any number from 0.0 to 0.5; default: 1e-05
temp = 0.0
status = CPXsetdblparam (enb, CPX_PARAM_EPINT, temp)

!Specifies the feasibility tolerance, that is, the degree to which values of the 
!basic variables calculated by the simplex method may violate their bounds
!Any number from 1e-9 to 1e-1; default: 1e-06
temp = 1.0000001e-09
status = CPXsetdblparam (enb, CPX_PARAM_EPRHS, temp)

!Influences the reduced-cost tolerance for optimality. This parameter governs
!how closely CPLEX must approach the theoretically optimal solution
!Any number from 1e-9 to 1e-1; default: 1e-06
temp = 1.0000001e-09
status = CPXsetdblparam (enb, CPX_PARAM_EPOPT, temp)

5555 continue
! Numero inicial de restricciones adicionales al problema MILP
m = NumResAsig
! Numero de restricciones adicionales al problema MILP
NumResAdi = 0
! Inicializa informacion sobre restricciones adicionales
InfRestAdi = 0
IntRestAdi = 0
IsenRestAdi = 'E'

! si no se desea incluir la estimacion de perdidas
if ( SiPerdidas .eq. 0 ) then

!   si se desean considerar restricciones de transmision
    if ( SiTransmision .eq. 1 ) then
!       hasta que no existan violaciones en transmision
        do while ( SiViolacion .eq. 1 )
            SemBandera = 0
!           se agregan restricciones de grupos de ramas violadas
            call AddLimFluRes ( sistema, m )
!           se resuelve el problema MILP con aproximacion de perdidas
            call ResuelveMILP ( 1, sistema, 0, 0 )
        enddo
!       Si no es el AUGC y se desea resolver el segundo problema
        if ( TipoEjecu .ne. 1 .and. SiSegProb .eq. 1 ) then
            SemBandera = 0
            bmensaje = fecha_Ej//' '//NomEjecu//'001 RESUELVE SEGUNDO PROBLEMA PARA PRECIOS'
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            Call FechaEjecucion (fecha_Ej)
!           se reseulve un segundo problema (despacho) para determinar precios
            call SegundoProblema ( ite, sistema, imprime )
        endif
!       si existen excedentes en la solucion
!        if ( SemBandera ( 2 ) .eq. 1 .and. SiElimExce .eq. 1 ) then
        if ( SemBandera ( 2 ) .eq. 1 .and. NumUniRC .le. 0 ) then
!        if ( SemBandera ( 2 ) .eq. 1 ) then
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' '//NomEjecu//'001 INTENTO DE ELIMINAR EXCEDENTES'
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            Call FechaEjecucion (fecha_Ej)
            Semtemp = SemBandera
            SemBandera = 0
            imprime = 1
            call EliminaExcedentes ( ite, sistema, imprime )
            if ( imprime .eq. 3 ) then
                bmensaje = fecha_Ej//' '//NomEjecu//'001 ESCENARIO CON EXCEDENTE REAL'
                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                Call FechaEjecucion (fecha_Ej)
                SemBandera = Semtemp
            endif
        endif
    endif
    
else

    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'001 ESTIMACION DE PERDIDAS EN TRANSMISION'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
!   se acotan superiormente las perdidas
!    call AcotaPerdidas ( sistema )
!   se hacen iteraciones para estimar perdidas
    ite = 1

    do while ( ite .le. IterPerdidas .or. ( SiTransmision .eq. 1 .and. SiViolacion .eq. 1) )
 !      se acotan superiormente las perdidas
        call AcotaPerdidas ( sistema )
        SemBandera = 0
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write ( Unirdes, * ) 'ITERACION DE PERDIDAS:', ite
        write ( Unirdes, * )
        write ( aaux, 5000 ) ite
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' '//NomEjecu//'001 ITERACION DE PERDIDAS :   '//aaux
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
!       se calculan perdidas y sensibilidades
!        call CalSnsPerIntervalo ( sistema, ite )
!       se agregan nuevas restricciones para aproximar perdidas en el MILP
        call RestPerdidas ( sistema, m )
!       Se modifica lado derecho de la restriccion de balance de potencia
!        call CambiaBalPot
!       si se desean considerar restricciones de transmision
        if ( SiTransmision .eq. 1 ) then
!           se actualiza lado derecho de las restricciones de transmision
!           (ramas ya antes violadas)
            call ActualizaRestRama ( sistema )
!           si existe violacion de nuevas ramas
            if ( SiViolacion .eq. 1 ) then
!               se agregan restricciones de grupos de ramas violadas
                call AddLimFluRes ( sistema, m )
            endif
        endif
!       se resuelve el problema MILP con aproximacion de perdidas
        call ResuelveMILP ( ite, sistema, 0, 0 )
        ite = ite + 1
    enddo
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
 
!   Si no es el AUGC y se desea resolver el segundo problema
    if ( TipoEjecu .ne. 1 .and. SiSegProb .eq. 1 ) then
        SemBandera = 0
        bmensaje = fecha_Ej//' '//NomEjecu//'001 RESUELVE SEGUNDO PROBLEMA PARA PRECIOS'
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        Call FechaEjecucion (fecha_Ej)
!       Se reseulve un segundo problema (despacho) para determinar precios
        call SegundoProblema ( ite, sistema, imprime )
    endif
!   si existen excedentes en la solucion
!    if ( SemBandera ( 2 ) .eq. 1 .and. SiElimExce .eq. 1 ) then
    if ( SemBandera ( 2 ) .eq. 1 .and. NumUniRC .le. 0 ) then
!    if ( SemBandera ( 2 ) .eq. 1 ) then
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '//NomEjecu//'001 INTENTO DE ELIMINAR EXCEDENTES'
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        Call FechaEjecucion (fecha_Ej)
        Semtemp = SemBandera
        SemBandera = 0
        imprime = 1
        call EliminaExcedentes ( ite, sistema, imprime )
        if ( imprime .eq. 3 ) then
            bmensaje = fecha_Ej//' '//NomEjecu//'001 ESCENARIO CON EXCEDENTE REAL'
            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            Call FechaEjecucion (fecha_Ej)
            SemBandera = Semtemp
        endif
    endif
endif

if ( SiEscLP .eq. 1 .and. TipoEjecu .ne. 3 ) then
!   Se escribe el modelo actuaizado MILP a un archivo
    status = CPXwriteprob (enb, lpMILP, 'dirres/MDA.lp', 'LP')
!    status = CPXwriteprob (enb, lpMILP, 'MDA.sav', 'SAV')
endif
5000 FORMAT (I5)

return
end

    
! ---------------------------------------------------------------------
! Resuelve el problema de asignacion de unidades termo e hidro del    *
! Mercado en un Dia en Adelanto (AUHE).                               *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre de 2018                                                     *
! ---------------------------------------------------------------------
Subroutine FormaResuleveNCA ( sistema )

use ParAUHE
use ProblemaAUHE
use ParGloRed, only: bangruram

use symtypes
!use cplex_ifaces, only: CPXsetintparam, CPXsetdblparam   !Windows (comentarizar para Linux)
use cplex_cons, only:   CPX_ON, CPX_PARAM_SCRIND, CPX_OFF, CPX_PARAM_EPGAP, &
                        CPX_PARAM_SCAIND, CPX_PARAM_EPINT, CPX_PARAM_PROBE, &
                        CPX_PARAM_MIPEMPHASIS, CPX_PARAM_NUMERICALEMPHASIS, &
                        CPX_PARAM_EPRHS, CPX_PARAM_EPOPT, CPX_PARAM_TILIM

implicit none
integer                 CPXsetintparam, CPXsetdblparam   !Linux (comentarizar para Windows)

integer m, ibanbit, ierror, ite, status, sistema

CHARACTER fecha_Ej*19

real*8 temp
character*16 aaux
ibanbit = 1
ierror = 0
ite = 0

! Tolerancia de GAP
temp = GAPCPLEX
status = CPXsetdblparam (enb, CPX_PARAM_EPGAP, temp)

! Maximo de tiempo de solucion para escenarios semanales
temp = LIMIT_TIME_LINEAR
status = CPXsetdblparam (enb, CPX_PARAM_TILIM, temp)


! escalamiento (0=default, -1=no, 1=agresivo)
!status = CPXsetintparam (enb, CPX_PARAM_SCAIND, 1)

! Probar valores de variables binarias antes de resolver el nodo raiz
! (-1=No probing, 0=AUtomatic(default), 1=Moderate, 2= Aggressive, 3= Very aggressive)
!status = CPXsetintparam (enb, CPX_PARAM_PROBE, 3);  


! Controls trade-offs between speed, feasibility, optimality, and moving bounds in MIP
!0	Balance optimality and feasibility; default
!1	Emphasize feasibility over optimality
!2	Emphasize optimality over feasibility
!3	Emphasize moving best bound
!4	Emphasize finding hidden feasible solutions
status = CPXsetintparam (enb, CPX_PARAM_MIPEMPHASIS, 1)

! Emphasizes precision in numerically unstable or difficult problems
!CPX_OFF	Do not emphasize numerical precision; default
!CPX_ON	Exercise extreme caution in computation
status = CPXsetintparam (enb, CPX_PARAM_NUMERICALEMPHASIS, CPX_ON)

! Specifies the amount by which an integer variable can be different from an integer and still be considered feasible
! Any number from 0.0 to 0.5; default: 1e-05
temp = 0.0
status = CPXsetdblparam (enb, CPX_PARAM_EPINT, temp)

! Specifies the feasibility tolerance, that is, the degree to which values of the basic variables calculated by the simplex method may violate their bounds
! Any number from 1e-9 to 1e-1; default: 1e-06
temp = 1.0000001e-09
status = CPXsetdblparam (enb, CPX_PARAM_EPRHS, temp)

! Influences the reduced-cost tolerance for optimality. This parameter governs how closely CPLEX must approach the theoretically optimal solution
! Any number from 1e-9 to 1e-1; default: 1e-06
temp = 1.0000001e-09
status = CPXsetdblparam (enb, CPX_PARAM_EPOPT, temp)

SemBandera = 0

! se forma y resuelve el problema de Asignacion y Despacho (MILP), sin perdidas
call FormaResuelveMILP ( ite, sistema )

! Numero inicial de restricciones adicionales al problema MILP
m = NumResAsig
! Numero de restricciones adicionales al problema MILP
NumResAdi = 0
! Inicializa informacion sobre restricciones adicionales
InfRestAdi = 0
IntRestAdi = 0

! si se desea incluir la estimacion de perdidas
if ( SiPerdidas .gt. 0 ) then

    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'001 ESTIMACION DE PERDIDAS EN TRANSMISION'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
!   se acotan superiormente las perdidas
!    call AcotaPerdidas ( sistema )
!   se hacen iteraciones para estimar perdidas
    ite = 1
    do while ( ite .le. IterPerdidas )
!   se acotan superiormente las perdidas
    call AcotaPerdidas ( sistema )
        SemBandera = 0
        write ( Unirdes, * ) 'ITERACION DE PERDIDAS:', ite
        write ( Unirdes, * )
        write ( aaux, 5000 ) ite
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' '//NomEjecu//'001 ITERACION DE PERDIDAS :   '//aaux
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
!       se agregan nuevas restricciones para aproximar perdidas en el MILP
        call RestPerdidas ( sistema, m )
!       Se modifica lado derecho de la restriccion de balance de potencia
!        call CambiaBalPot
!       se resuelve el problema MILP con aproximacion de perdidas
        call ResuelveMILP ( ite, sistema, 0, 0 )
!       se calculan perdidas y sensibilidades
!        call CalSnsPerIntervalo ( sistema, ite )
        ite = ite + 1
    enddo
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    
!   Si no es el AUGC y se desea resolver el segundo problema
    if ( TipoEjecu .ne. 1 .and. SiSegProb .eq. 1 ) then
        SemBandera = 0
        bmensaje = fecha_Ej//' '//NomEjecu//'001 RESUELVE SEGUNDO PROBLEMA PARA PRECIOS'
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        Call FechaEjecucion (fecha_Ej)
!       se reseulve un segundo problema (despacho) para determinar precios
        call SegundoProblema ( ite, sistema, 1 )
    endif
endif

5000 FORMAT (I5)

return
end
    
    
    
! ---------------------------------------------------------------------
! Abrir archivos de debugger para escribir asignaciones, despachos    *
! y variables duales                                                  *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero del 2015                                                      *
! ---------------------------------------------------------------------
Subroutine AbreArchivos ( sistema )
use ParAUHE
use ParGloRed, only: NumGruRamSis

Implicit none

INTEGER   sistema, uunidad, IERROR

character*1 ssistema

! Se concatena el numero de subsistema al nombre de los archivos de debugger
Write( ssistema, '(I1)' )  sistema

! Abre archivos de resultados

! si hay unidades de rango discontinuo en el sistema
if ( NumUniRC .gt. 0 ) then
!   Generacion de uniaddes de rango continuo
    uunidad = 500 + sistema
    UnichauRC = uunidad
    OPEN ( UNIT = UnichauRC, FILE = RUT_RES//'r_chauRC'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1250 )
!   Resultados de reserva por zona
    uunidad = 540 + sistema
    UnirznuRC = uunidad
    OPEN ( UNIT = UnirznuRC, FILE = RUT_RES//'r_UniResZonaRC'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )
!   Resultados de reserva por sistema
    uunidad = 570 + sistema
    UnirsnuRC = uunidad
    OPEN ( UNIT = UnirsnuRC, FILE = RUT_RES//'r_UniResSisRC'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )
endif

! si hay unidades de rango discontinuo en el sistema
if ( NumUniRD .gt. 0 ) then
!   Generacion de uniaddes de rango discontinuo
    uunidad = 505 + sistema
    UnichauRD = uunidad
    OPEN ( UNIT = UnichauRD, FILE = RUT_RES//'r_chauRD'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1250 )
!   Resultados de reserva por zona
    uunidad = 640 + sistema
    UnirznuRD = uunidad
    OPEN ( UNIT = UnirznuRD, FILE = RUT_RES//'r_UniResZonaRD'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )
!   Resultados de reserva por sistema
    uunidad = 670 + sistema
    UnirsnuRD = uunidad
    OPEN ( UNIT = UnirsnuRD, FILE = RUT_RES//'r_UniResSisRD'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )
endif
    
! Despachos y duales de balance por intervalo
uunidad = 520 + sistema
Unirdes = uunidad
OPEN ( UNIT = Unirdes, FILE = RUT_RES//'r_desphora'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )

! Despachos y duales de balance por intervalo
uunidad = 610 + sistema
UnirCar = uunidad
OPEN ( UNIT = UnirCar, FILE = RUT_RES//'r_cargas'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )

! Resultados de reserva por zona
uunidad = 530 + sistema
Unirzn = uunidad
OPEN ( UNIT = Unirzn, FILE = RUT_RES//'r_CENACE_Zona'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )

! Resultados de reserva por sistema
uunidad = 560 + sistema
Unirsn = uunidad
OPEN ( UNIT = Unirsn, FILE = RUT_RES//'r_CENACE_Sis'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )

! Resultados de costos marginales por region
uunidad = 100 + sistema
Unimarreg = uunidad
OPEN ( UNIT = Unimarreg, FILE = RUT_RES//'r_margreg'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )

! si hay unidades de rango discontinuo en el sistema
if ( NumGruRamSis ( sistema ) .gt. 0 ) then
!   Resultados de flujos por grupos de rama
    uunidad = 700 + sistema
    UniFlujo = uunidad
    OPEN ( UNIT = UniFlujo, FILE = RUT_RES//'r_FlujosGpos'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )
endif

! si hay grupos de unidades termo con limitaciones de energia
if ( NumGruUTer .gt. 0 ) then
!if ( NResEner .gt. 0 ) then
!   Resultados de flujos por grupos de rama
    uunidad = 710 + sistema
    UniGLimT = uunidad
    OPEN ( UNIT = UniGLimT, FILE = RUT_RES//'r_GTermLimE'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )
endif

return
end

! ---------------------------------------------------------------------
! Escribir estado de semaforos para describrir la calidad de la       *
! solucion del AUHE, cuando no corrio el algoritmo.                    *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio del 2015                                                      *
! ---------------------------------------------------------------------
Subroutine EscSemaforosError
use ParAUHE

implicit none

! Se ecribe resultado de semaforos
write ( UniSemaf, * ) 1,',',' Corte de energia'
write ( UniSemaf, * ) 1,',',' Excedente de energia'
write ( UniSemaf, * ) 1,',',' Escasez de reservas'
write ( UniSemaf, * ) 1,',',' Infactibilidad en transmision'
write ( UniSemaf, * ) 1,',',' Violacion de limites de energia termo'
write ( UniSemaf, * ) 1,',',' Violacion de limites de energia hidro'
write ( UniSemaf, * ) 1,',',' Problema de optimizacion infactible'
!write ( UniSemaf, * ) 1,',',' Violacion de limites de combustible'

return
end

    
subroutine EliminaExcedentes ( ite, sistema, imprime )
! ---------------------------------------------------------------------
! Se intenta eliminar excedentes al fijar los valores de perdidas y   *
! no permitir excedentes.                                             *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre de 2015                                                   *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE
use ParGloRed, only: NumNodSis, PerIntervalo
use symtypes

!use cplex_ifaces, only: CPXchgbds , CPXwriteprob      !Windows (comentarizar para Linux)
!use cplex_forman, only: addrowsCPX      !Windows (comentarizar para Linux)


implicit none

integer                 CPXwriteprob, CPXaddrows    !Linux (comentarizar para Windows)
integer                 CPXchgbds      !Linux (comentarizar para Windows)
 
integer d, ite, n, sistema, imprime
integer status, ibanbit, i, cnt, indices


integer ierror

REAL*8     bd 
character*1 lu
character*1 ssistema
CHARACTER fecha_Ej*19

DIMENSION   bd      ( maxvarMILP )
DIMENSION   lu      ( maxvarMILP )
DIMENSION   indices ( maxvarMILP )

data status  / 0 /

ibanbit = 1
cnt = 0

! para cada intervalo
do i = 1, NTINTR
!   se fijan las perdidas
    cnt = cnt + 1
    indices ( cnt ) = IPERD + i - 2
!    bd ( cnt ) = xMILP ( IPERD + i - 1 )
    bd ( cnt ) = PerIntervalo(i)
    lu ( cnt ) = 'U'
!   se eliminan excedentes
!   para todos los nodos
    do n = 1, NumNodSis ( sistema )
        cnt = cnt + 1
        indices ( cnt ) = IEXC + n + (i-1)*NumNodSis ( sistema ) - 2
        bd ( cnt ) = 0.0
        lu ( cnt ) = 'B'
    enddo
!   se eliminan cortes
!   para todas las cargas
    do d = 1 , NumOferDem
        cnt = cnt + 1
        indices ( cnt ) = IDF + d + (i-1)*NumOferDem - 2
        bd ( cnt ) = 0.0
        lu ( cnt ) = 'B'
    enddo
enddo

! se actualizan cotas superiores de variables de asignacion
status = CPXchgbds (enb, lpMILP, cnt, indices, lu, bd)
if ( status .ne. 0) then
    write (*,*) ' Error al actualizar cotas de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError    
!   algoritmo no termina bien
    call SalidaError
    stop
end if

write( ssistema, '(I1)' )  sistema
!Se escribe el modelo actuaizado MILP a un archivo
!status = CPXwriteprob (enb, lpMILP, 'me'//ssistema//'.lp', 'LP')
!status = CPXwriteprob (enb, lpMILP, 'me'//ssistema//'.sav', 'SAV')

! se resuelve el problema
call ResuelveMILP ( ite, sistema, imprime, 0 )

return
end

subroutine SegundoProblema ( ite, sistema, imprime )
! ---------------------------------------------------------------------
! Se cambian las penalizaciones de variables artificiales para        *
! para determinar los precios de energia y servicios conexos.         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre de 2018                                                  *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE
use ParGloRed, only: NumGruRamSis, NumNodSis, PrecioTope
use symtypes

!use cplex_ifaces, only: CPXchgbds , CPXchgobj, CPXwriteprob, CPXchgrhs      !Windows (comentarizar para Linux)

implicit none

integer                 CPXchgbds , CPXchgobj, CPXwriteprob, CPXchgrhs     !Linux (comentarizar para Windows)
 
integer br, d, e, ite, m, modo, n, o, u, sistema, imprime
integer r, s, status, ibanbit, i, indices, segmarg

integer ierror, kr10, k10, ks, kr, ColVarC, bres

REAL*8     bd, NewValC, reqsig
!REAL*8  epsireserva, epsilonreserva, epsilon
REAL*8  epsireserva, epsilon


character*1 lu
character*1 ssistema
CHARACTER fecha_Ej*19

DIMENSION   bd      ( maxvarMILP )
DIMENSION   lu      ( maxvarMILP )
DIMENSION   indices ( maxvarMILP )
DIMENSION   NewValC ( maxvarMILP)
DIMENSION   ColVarC ( maxvarMILP )

data status  / 0 /

! Epsilon de reservas en casos factibles
data epsireserva / 1.0E-05 /

! Epsilon de reservas en casos infactibles o estresados 
! (Es el que leeria de parametros de sintonizacion)
!data epsilonreserva / 5.0E-03 /

!return
ibanbit = 1

! si la solucion es en variables enteras
if ( TipoProblema .eq. 1 ) then
!   se fija la asignacion de las unidades obtenida previamente
	call FijaAsignacion
endif

! se determinan nuevos valores de penalizacion
! Se determina el precio del corte de carga
CostoCorte = PrecioTope  !PrecioTopeEner 

! Se determina el precio de infactibilidad en transmision
PenRamas = CostoCorte * (1.03)

! Se determina el precio de infactibilidad en limites de energia hidro
PenEnerEmb = CostoCorte

! Se determina el precio de infactibilidad en limites de energia termo
PenEnerUTerm = CostoCorte

if ( TipoEscPre .eq. 1 ) then

! 	Se determina el precio de infactibilidad en reserva de regulacion por zona
!	PreResReg = PrecioTopeEner !PrecioTopeReg
    do r = 1, NumGruRes*SiOferComResZona
        do i = 1, NTINTR
            do s = 1, NumBloRReg
!                PreResReg (r, s, i) = min ( PrecioTopeEner, dualresrez(r,i) )
                PreResReg (r, s, i) = min ( PrecioTope, dualresrez(r,i) )
            enddo
        enddo
    enddo

! 	Se determina el precio de infactibilidad en reserva rodante por zona
!	PreResRR10 = PrecioTopeEner
    do r = 1, NumGruRes*SiOferComResZona
        do i = 1, NTINTR
            do s = 1, NumBloRR10
!                PreResRR10 (r, s, i) = min ( PrecioTopeEner, dualresr10z(r,i) )
                PreResReg (r, s, i) = min ( PrecioTope, dualresr10z(r,i) )
            enddo
        enddo
    enddo

! 	Se determina el precio de infactibilidad en reserva operativa por zona
!	PreResR10 = PrecioTopeEner
    do r = 1, NumGruRes*SiOferComResZona
        do i = 1, NTINTR
            do s = 1, NumBloRR10
!                PreResR10 (r, s, i) = min ( PrecioTopeEner, dualres10z(r,i) )
               PreResR10 (r, s, i) = min ( PrecioTope, dualres10z(r,i) )
            enddo
        enddo
    enddo

! 	Se determina el precio de infactibilidad en reserva suplementaria por zona
!	PreResSup = PrecioTopeEner
    do r = 1, NumGruRes*SiOferComResZona
        do i = 1, NTINTR
            do s = 1, NumBloRR10
!                PreResSup (r, s, i) = min ( PrecioTopeEner, dualressz(r,i) )
                PreResSup (r, s, i) = min ( PrecioTope, dualressz(r,i) )
            enddo
        enddo
    enddo

! 	Se determina el precio de infactibilidad en reserva de regulacion por sistema
	PreResRegS = PrecioTope !PrecioTopeEner
    
! 	Se determina el precio de infactibilidad en reserva rodante por sistema
	PreResRR10S = PrecioTope !PrecioTopeEner

! 	Se determina el precio de infactibilidad en reserva operativa por sistema
	PreResR10S = PrecioTope !PrecioTopeEner

! 	Se determina el precio de infactibilidad en reserva suplementaria por sistema
	PreResSupS = PrecioTope !PrecioTopeEner

endif

! Se cambian las penalizaciones y cotas superiores
m = 0
do i = 1, NTINTR
    
goto 3333
!   Despacho de unidades de rango continuo
    do u = 1, NumUniRC
        m = m + 1
!       variable
        indices ( m ) = IGRC + u + (i-1)*NumUniRC - 2
        bd ( m ) = xMILP ( IGRC + u + (i-1)*NumUniRC - 1 ) - 1.0e-6
        lu ( m ) = 'L'
    enddo

    !   Despacho de unidades de rango discontinuo
    do u = 1, NumUniRD
        do modo = 1, NumModRD(u)
            m = m + 1
    !       variable
            indices ( m ) = IGRD + INIURDI ( u, i ) + modo - 2
            bd ( m ) = xMILP ( IGRD + INIURDI ( u, i ) + modo - 1 ) - 1.0e-6
            lu ( m ) = 'L'
        enddo
    enddo

    ! Despacho de unidades hidro
    do u = 1, NumUniHid
        m = m + 1
    !   variable
        indices ( m ) = IGH + u + (i-1)*NumUniHid - 2
        bd ( m ) = xMILP ( IGH + u + (i-1)*NumUniHid - 1 ) - 1.0e-6
        lu ( m ) = 'L'
    enddo

    ! Despacho de unidades renovables
    do u = 1, NumUniRE
        m = m + 1
    !   variable
        indices ( m ) = IGRE + u + (i-1)*NumUniRE - 2
        bd ( m ) = xMILP ( IGRE + u + (i-1)*NumUniRE - 1 ) - 1.0e-6
        lu ( m ) = 'L'
    enddo

3333 continue
     

!   Corte de carga
    do d = 1, NumOferDem
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = CostoCorte*SiCorte 
        ColVarC ( m )  = IDF + d + (i-1)*NumOferDem - 2
!       cota superior
        indices ( m ) = IDF + d + (i-1)*NumOferDem - 2
        bd ( m ) = xMILP ( IDF + d + (i-1)*NumOferDem - 1 )
        if ( xMILP ( IDF + d + (i-1)*NumOferDem - 1 ) .gt. 0.0 ) then
            bd ( m ) = xMILP ( IDF + d + (i-1)*NumOferDem - 1 ) + 1.0e-5
        endif
        lu ( m ) = 'U'
    enddo
!   para todos los nodos
    do n = 1, NumNodSis ( sistema )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = CostoExced*SiExced 
        ColVarC ( m )  = IEXC + n + (i-1)*NumNodSis ( sistema ) - 2
!       cota superior
        indices ( m ) = IEXC + n + (i-1)*NumNodSis ( sistema ) - 2
        bd ( m ) = xMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 )
        if ( xMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 ) .gt. 0.0 ) then
            bd ( m ) = xMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 ) + 1.0e-5
        endif
        lu ( m ) = 'U'
    enddo
!   variable de perdidas
    m = m + 1
!   coeficientes de penalizacion
    NewValC ( m )  = CostoExced + 1.0  ! 1.0 
!    if ( nomsis(sistema) .eq. 'BCA' ) then
!        NewValC ( m )  = 0.0
!    endif
    ColVarC ( m )  = IPERD + i - 2
!   cota superior
    indices ( m ) = IPERD + i - 2
    bd ( m ) = xMILP ( IPERD + i - 1 ) + 1.0e-05
    lu ( m ) = 'U'
!   Restricciones de transmision
    do br = 1, NumGruRamSis (sistema)
!       variable artificial de excedente de flujo
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = PenRamas*SiArtTrans 
        ColVarC ( m )  = IAEF + br + (i-1)*NumGruRamSis (sistema) - 2
!       cota superior
        indices ( m ) = IAEF + br + (i-1)*NumGruRamSis (sistema) - 2
        bd ( m ) = xMILP ( IAEF + br + (i-1)*NumGruRamSis (sistema) - 1 )
        if ( bd ( m )  .gt. 0.0 ) then
            bd ( m ) = xMILP ( IAEF + br + (i-1)*NumGruRamSis (sistema) - 1 ) + 1.0e-05
        endif
        lu ( m ) = 'U'
!       variable artificial de excedente de contraflujo
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = PenRamas*SiArtTrans 
        ColVarC ( m )  = IACF + br + (i-1)*NumGruRamSis (sistema) - 2
!       cota superior
        indices ( m ) = IACF + br + (i-1)*NumGruRamSis (sistema) - 2
        bd ( m ) = xMILP ( IACF + br + (i-1)*NumGruRamSis (sistema) - 1 )
        if ( bd ( m )  .gt. 0.0 ) then
            bd ( m ) = xMILP ( IACF + br + (i-1)*NumGruRamSis (sistema) - 1 ) + 1.0e-05
        endif
        lu ( m ) = 'U'
    enddo
enddo

! produccion de energia para grupos de energia termica
do o = 1 , NumGruUTer*SiEnerTer
    m = m + 1
!   coeficientes de penalizacion
    NewValC ( m )  = PenEnerUTerm 
    ColVarC ( m )  = IARGT + o - 2
!   cota superior
    indices ( m ) = IARGT + o - 2
    bd ( m ) = xMILP ( IARGT + o - 1 )
    if ( bd ( m ) .gt. 0.0 ) then
        bd ( m ) = xMILP ( IARGT + o - 1 ) + 1.0e-5
    endif
    lu ( m ) = 'U'
enddo

! produccion de energia por embalse
do e = 1 , NumEmbalses*SiEnerHid
    m = m + 1
!   coeficientes de penalizacion
    NewValC ( m )  = PenEnerEmb 
    ColVarC ( m )  = IAREE + e - 2
!   cota superior
    indices ( m ) = IAREE + e - 2
    bd ( m ) = xMILP ( IAREE + e - 1 )
    if ( bd ( m ) .gt. 0.0 ) then
        bd ( m ) = xMILP ( IAREE + e - 1 ) + 1.0e-5
    endif
    lu ( m ) = 'U'
enddo

! Ofertas de compra de reserva del CENACE por zona
! para los grupos de reserva
kr10 = ICARR10G
k10 = ICAR10G
ks = ICARSG
kr = ICARRG
do r = 1, NumGruRes*SiOferComResZona
!   para todos los intervalos
    do i = 1, NTINTR
        if ( dualresr10z ( r, i ) .gt. PrecioTopeEner*1.2 ) then
!        if ( dualresr10z ( r, i ) .gt. PrecioTope*1.2 ) then
            epsilon = epsireserva
        else
            epsilon = epsilonreserva
        endif
        segmarg = 0
!       para los requerimientos de reserva rodante de 10 minutos
        do s = 1, NumBloRR10
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResRR10 (r, s, i)
            ColVarC ( m )  = kr10 - 1
!           cota superior
            indices ( m ) = kr10 - 1
            bd ( m ) = xMILP ( kr10 )
            if ( s .lt. NumBloRR10 ) then
                if ( xMILP ( kr10 ) .lt. ReqResR10 (r, s, i) .and. segmarg .eq. 0 ) then
                    segmarg = s
                endif
                bd ( m ) = xMILP ( kr10 ) 
                lu ( m ) = 'B'
             else 
                if ( segmarg .eq. 0 ) segmarg = s  
                bd ( m ) = xMILP ( kr10 ) - epsilon
                NewValC ( m )  = -PreResRR10 (r, segmarg, i)
                lu ( m ) = 'L'
            endif
            kr10 = kr10 + 1
        enddo
        segmarg = 0
        if ( dualres10z(r,i) .lt. PrecioTopeEner*1.2 ) then
!        if ( dualres10z(r,i) .lt. PrecioTope*1.2 ) then
            epsilon = epsireserva
        else
            epsilon = epsilonreserva
        endif
!       para los requerimientos de reserva de 10 minutos
        do s = 1, NumBloR10
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResR10 (r, s, i)
            ColVarC ( m )  = k10 - 1
!           cota superior
            indices ( m ) = k10 - 1
            bd ( m ) = xMILP ( k10 )
            if ( s .lt. NumBloR10 )  then
               if ( xMILP ( k10 ) .lt. ReqRes10 (r, s, i) .and. segmarg .eq. 0 ) then
                   segmarg = s
               endif
               bd ( m ) = xMILP ( k10 )
               lu ( m ) = 'B'
            else
               if ( segmarg .eq. 0 ) segmarg = s  
               NewValC ( m )  = -PreResR10 (r, segmarg, i)
               bd ( m ) = xMILP ( k10 ) - epsilon
               lu ( m ) = 'L'
            endif
            k10 = k10 + 1
        enddo
        segmarg = 0
        if ( dualressz( r, i ) .lt. PrecioTopeEner*1.2 ) then
!        if ( dualressz( r, i ) .lt. PrecioTope*1.2 ) then
            epsilon = epsireserva
        else
            epsilon = epsilonreserva
        endif
!       para los requerimientos de reserva suplementaria
        do s = 1, NumBloRSu
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResSup (r, s, i)
            ColVarC ( m )  = ks - 1
!           cota superior
            indices ( m ) = ks - 1
            bd ( m ) = xMILP ( ks )
            if ( s .lt. NumBloRSu )  then
               bd ( m ) = xMILP ( ks ) 
               if ( xMILP ( ks ) .lt. ReqResSup (r, s, i) .and. segmarg .eq. 0 ) then
                   segmarg = s
               endif
               lu ( m ) = 'B'
            else
               if ( segmarg .eq. 0 ) segmarg = s  
               NewValC ( m )  = -PreResSup (r, segmarg, i)
               bd ( m ) = xMILP ( ks ) - epsilon
               lu ( m ) = 'L'
            endif
            ks = ks + 1
        enddo
!       para los requerimientos de reserva de regulacion secundaria
        segmarg = 0
        if ( dualresrez(r,i) .lt. PrecioTopeEner*1.2 ) then
!        if ( dualresrez(r,i) .lt. PrecioTope*1.2 ) then
            epsilon = epsireserva
        else
            epsilon = epsilonreserva
        endif
        do s = 1, NumBloRReg
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResReg (r, s, i)
            ColVarC ( m )  = kr - 1
!           cota superior
            indices ( m ) = kr - 1
            if ( s .lt. NumBloRReg )  then
               bd ( m ) = xMILP ( kr )
               if ( xMILP ( kr )  .lt. ReqResReg (r, s, i) .and. segmarg .eq. 0 ) then
                  segmarg = s
               endif
               lu ( m ) = 'B'
            else
               if ( segmarg .eq. 0 ) segmarg = s 
               NewValC ( m )  = -PreResReg (r, s, i)
               lu ( m ) = 'L'
               bd ( m ) = xMILP ( kr ) - epsilon
            endif
            kr = kr + 1
        enddo
    enddo
enddo

! para el sistema
kr10 = ICARR10S
k10 = ICAR10S
ks = ICARSS
kr = ICARRS
! para todos los intervalos
do i = 1, NTINTR*SiOferComResSis
    
    bres = 0
!   para los requerimientos de reserva rodante de 10 minutos
    do s = 1, NumBloRR10
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResRR10S (sistema, s, i)
        ColVarC ( m )  = kr10 - 1
!       cota superior
        indices ( m ) = kr10 - 1
        bd ( m ) = xMILP ( kr10 )
        if ( bres .eq. 0 .and. ReqResR10S (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRR10 )  then
                reqsig =  ReqResR10S (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( kr10 ) - ReqResR10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            else if (  ReqResR10S (sistema, s, i) .gt. xMILP ( kr10 ) .and. s .lt. NumBloRR10  ) then
                bd ( m ) = max(xMILP ( kr10 ) - ReqResR10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        kr10 = kr10 + 1
    enddo
    bres = 0
!   para los requerimientos de reserva de 10 minutos
    do s = 1, NumBloR10
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResR10S (sistema, s, i)
        ColVarC ( m )  = k10 - 1
!       cota superior
        indices ( m ) = k10 - 1
        bd ( m ) = xMILP ( k10 )
        if ( bres .eq. 0 .and. ReqRes10S (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloR10 )  then
                reqsig =  ReqRes10S (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( k10 ) - ReqRes10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            else if (  ReqRes10S (sistema, s, i) .gt. xMILP ( k10 ) .and. s .lt. NumBloR10  ) then
                bd ( m ) = max(xMILP ( k10 ) - ReqRes10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        k10 = k10 + 1
    enddo
    bres = 0
!   para los requerimientos de reserva suplementaria
    do s = 1, NumBloRSu
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResSupS (sistema, s, i)
        ColVarC ( m )  = ks - 1
!       cota superior
        indices ( m ) = ks - 1
        bd ( m ) = xMILP ( ks )
        if ( bres .eq. 0 .and. ReqResSupS (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRSu )  then
                reqsig =  ReqResSupS (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( ks ) - ReqResSupS (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            else if (  ReqResSupS (sistema, s, i) .gt. xMILP ( ks ) .and. s .lt. NumBloRSu  ) then
                bd ( m ) = max(xMILP ( ks ) - ReqResSupS (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        ks = ks + 1
    enddo
    bres = 0
!   para los requerimientos de reserva de regulacion secundaria
    do s = 1, NumBloRReg
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResRegS (sistema, s, i)
        ColVarC ( m )  = kr - 1
!       cota superior
        indices ( m ) = kr - 1
        bd ( m ) = xMILP ( kr )
        if ( bres .eq. 0 .and. ReqResRegS (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRReg )  then
                reqsig =  ReqResRegS (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( kr ) - 1.0e-5, 0.0 )
                bres = 1
            else if (  ReqResRegS (sistema, s, i) .gt. xMILP ( kr ) .and. s .lt. NumBloRReg  ) then
                bd ( m ) = max(xMILP ( kr ) - 1.0e-5, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        kr = kr + 1
    enddo
enddo


! Se cambian coeficientes
status = CPXchgobj (enb, lpMILP, m, ColVarC, NewValC )
if ( status .ne. 0 ) then
    write (*,*) ' Error al actualizar coeficientes de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

! se actualizan cotas superiores de variables de asignacion
status = CPXchgbds (enb, lpMILP, m, indices, lu, bd)
if ( status .ne. 0) then
    write (*,*) ' Error al actualizar cotas de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError    
    stop
end if

write( ssistema, '(I1)' )  sistema
if ( SiEscLP .eq. 1 .and. TipoEjecu .ne. 3 ) then
!   Se escribe el modelo actuaizado MILP a un archivo
    status = CPXwriteprob (enb, lpMILP, 'dirres/MDA.lp', 'LP')
!    status = CPXwriteprob (enb, lpMILP, 'MDA.sav', 'SAV')
endif

! se resuelve el problema
call ResuelveMILP ( ite, sistema, imprime, 1 )

return
end
    
subroutine SegundoProblema_new ( ite, sistema, imprime )
! ---------------------------------------------------------------------
! Se cambian las penalizaciones de variables artificiales para        *
! para determinar los precios de energia y servicios conexos.         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre de 2018                                                  *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE
use ParGloRed, only: NumGruRamSis, NumNodSis, PrecioTope
use symtypes

!use cplex_ifaces, only: CPXchgbds , CPXchgobj, CPXwriteprob, CPXchgrhs      !Windows (comentarizar para Linux)

implicit none

integer                 CPXchgbds , CPXchgobj, CPXwriteprob, CPXchgrhs     !Linux (comentarizar para Windows)
 
integer br, d, e, ite, m, modo, n, o, u, sistema, imprime
integer r, s, status, ibanbit, i, indices, segmarg

integer ierror, kr10, k10, ks, kr, ColVarC, bres

REAL*8     bd, NewValC, reqsig
!REAL*8  epsireserva, epsilonreserva, epsilon
REAL*8  epsireserva, epsilon


character*1 lu
character*1 ssistema
CHARACTER fecha_Ej*19

DIMENSION   bd      ( maxvarMILP )
DIMENSION   lu      ( maxvarMILP )
DIMENSION   indices ( maxvarMILP )
DIMENSION   NewValC ( maxvarMILP)
DIMENSION   ColVarC ( maxvarMILP )

data status  / 0 /

! Epsilon de reservas en casos factibles
data epsireserva / 1.0E-05 /

! Epsilon de reservas en casos infactibles o estresados 
! (Es el que leeria de parametros de sintonizacion)
!data epsilonreserva / 5.0E-03 /

!return
ibanbit = 1

! si la solucion es en variables enteras
if ( TipoProblema .eq. 1 ) then
!   se fija la asignacion de las unidades obtenida previamente
	call FijaAsignacion
endif

! se determinan nuevos valores de penalizacion
! Se determina el precio del corte de carga
CostoCorte = PrecioTopeEner 

! Se determina el precio de infactibilidad en transmision
PenRamas = CostoCorte * (1.03)

! Se determina el precio de infactibilidad en limites de energia hidro
PenEnerEmb = CostoCorte

! Se determina el precio de infactibilidad en limites de energia termo
PenEnerUTerm = CostoCorte

if ( TipoEscPre .eq. 1 ) then

	PrecioTopeReg = PrecioTopeEner

! 	Se determina el precio de infactibilidad en reserva de regulacion por zona
!	PreResReg = PrecioTopeEner !PrecioTopeReg
    do r = 1, NumGruRes*SiOferComResZona
        do i = 1, NTINTR
            do s = 1, NumBloRReg
                PreResReg (r, s, i) = min ( PrecioTopeEner, dualresrez(r,i) )
!                PreResReg (r, s, i) = min ( PrecioTope, dualresrez(r,i) )
            enddo
        enddo
    enddo

! 	Se determina el precio de infactibilidad en reserva rodante por zona
	PreResRR10 = PrecioTopeEner

! 	Se determina el precio de infactibilidad en reserva operativa por zona
	PreResR10 = PrecioTopeEner

! 	Se determina el precio de infactibilidad en reserva suplementaria por zona
	PreResSup = PrecioTopeEner

! 	Se determina el precio de infactibilidad en reserva de regulacion por sistema
	PreResRegS = PrecioTopeEner
    
! 	Se determina el precio de infactibilidad en reserva rodante por sistema
	PreResRR10S = PrecioTopeEner

! 	Se determina el precio de infactibilidad en reserva operativa por sistema
	PreResR10S = PrecioTopeEner

! 	Se determina el precio de infactibilidad en reserva suplementaria por sistema
	PreResSupS = PrecioTopeEner

endif

! Se cambian las penalizaciones y cotas superiores
m = 0
do i = 1, NTINTR
    
goto 3333
!   Despacho de unidades de rango continuo
    do u = 1, NumUniRC
        m = m + 1
!       variable
        indices ( m ) = IGRC + u + (i-1)*NumUniRC - 2
        bd ( m ) = xMILP ( IGRC + u + (i-1)*NumUniRC - 1 )
        lu ( m ) = 'U'
    enddo

    !   Despacho de unidades de rango discontinuo
    do u = 1, NumUniRD
        do modo = 1, NumModRD(u)
            m = m + 1
    !       variable
            indices ( m ) = IGRD + INIURDI ( u, i ) + modo - 2
            bd ( m ) = xMILP ( IGRD + INIURDI ( u, i ) + modo - 1 )
            lu ( m ) = 'U'
        enddo
    enddo

    ! Despacho de unidades hidro
    do u = 1, NumUniHid
        m = m + 1
    !   variable
        indices ( m ) = IGH + u + (i-1)*NumUniHid - 2
        bd ( m ) = xMILP ( IGH + u + (i-1)*NumUniHid - 1 )
        lu ( m ) = 'U'
    enddo

    ! Despacho de unidades renovables
    do u = 1, NumUniRE
        m = m + 1
    !   variable
        indices ( m ) = IGRE + u + (i-1)*NumUniRE - 2
        bd ( m ) = xMILP ( IGRE + u + (i-1)*NumUniRE - 1 )
        lu ( m ) = 'U'
    enddo

3333 continue
     
!   Corte de carga
    do d = 1, NumOferDem
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = CostoCorte*SiCorte 
        ColVarC ( m )  = IDF + d + (i-1)*NumOferDem - 2
!       cota superior
        indices ( m ) = IDF + d + (i-1)*NumOferDem - 2
        bd ( m ) = xMILP ( IDF + d + (i-1)*NumOferDem - 1 )
        if ( xMILP ( IDF + d + (i-1)*NumOferDem - 1 ) .gt. 0.0 ) then
            bd ( m ) = xMILP ( IDF + d + (i-1)*NumOferDem - 1 ) + 1.0e-5
        endif
        lu ( m ) = 'U'
    enddo
!   para todos los nodos
    do n = 1, NumNodSis ( sistema )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = CostoExced*SiExced 
        ColVarC ( m )  = IEXC + n + (i-1)*NumNodSis ( sistema ) - 2
!       cota superior
        indices ( m ) = IEXC + n + (i-1)*NumNodSis ( sistema ) - 2
        bd ( m ) = xMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 )
        if ( xMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 ) .gt. 0.0 ) then
            bd ( m ) = xMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 ) + 1.0e-5
        endif
        lu ( m ) = 'U'
    enddo
!   variable de perdidas
!    m = m + 1
!   coeficientes de penalizacion
!    NewValC ( m )  = CostoExced + 1.0  ! 1.0 
!    if ( nomsis(sistema) .eq. 'BCA' ) then
!        NewValC ( m )  = 0.0
!    endif
!    ColVarC ( m )  = IPERD + i - 2
!   cota superior
!    indices ( m ) = IPERD + i - 2
!    bd ( m ) = xMILP ( IPERD + i - 1 ) + epsilonper
!    lu ( m ) = 'U'
!   Restricciones de transmision
    do br = 1, NumGruRamSis (sistema)
!       variable artificial de excedente de flujo
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = PenRamas*SiArtTrans 
        ColVarC ( m )  = IAEF + br + (i-1)*NumGruRamSis (sistema) - 2
!       cota superior
        indices ( m ) = IAEF + br + (i-1)*NumGruRamSis (sistema) - 2
        bd ( m ) = xMILP ( IAEF + br + (i-1)*NumGruRamSis (sistema) - 1 )
        if ( bd ( m )  .gt. 0.0 ) then
            bd ( m ) = xMILP ( IAEF + br + (i-1)*NumGruRamSis (sistema) - 1 ) + 1.0e-05
        endif
        lu ( m ) = 'U'
!       variable artificial de excedente de contraflujo
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = PenRamas*SiArtTrans 
        ColVarC ( m )  = IACF + br + (i-1)*NumGruRamSis (sistema) - 2
!       cota superior
        indices ( m ) = IACF + br + (i-1)*NumGruRamSis (sistema) - 2
        bd ( m ) = xMILP ( IACF + br + (i-1)*NumGruRamSis (sistema) - 1 )
        if ( bd ( m )  .gt. 0.0 ) then
            bd ( m ) = xMILP ( IACF + br + (i-1)*NumGruRamSis (sistema) - 1 ) + 1.0e-05
        endif
        lu ( m ) = 'U'
    enddo
enddo

! produccion de energia para grupos de energia termica
do o = 1 , NumGruUTer*SiEnerTer
    m = m + 1
!   coeficientes de penalizacion
    NewValC ( m )  = PenEnerUTerm 
    ColVarC ( m )  = IARGT + o - 2
!   cota superior
    indices ( m ) = IARGT + o - 2
    bd ( m ) = xMILP ( IARGT + o - 1 )
    if ( bd ( m ) .gt. 0.0 ) then
        bd ( m ) = xMILP ( IARGT + o - 1 ) + 1.0e-5
    endif
    lu ( m ) = 'U'
enddo

! produccion de energia por embalse
do e = 1 , NumEmbalses*SiEnerHid
    m = m + 1
!   coeficientes de penalizacion
    NewValC ( m )  = PenEnerEmb 
    ColVarC ( m )  = IAREE + e - 2
!   cota superior
    indices ( m ) = IAREE + e - 2
    bd ( m ) = xMILP ( IAREE + e - 1 )
    if ( bd ( m ) .gt. 0.0 ) then
        bd ( m ) = xMILP ( IAREE + e - 1 ) + 1.0e-5
    endif
    lu ( m ) = 'U'
enddo

! Ofertas de compra de reserva del CENACE por zona
! para los grupos de reserva
kr10 = ICARR10G
k10 = ICAR10G
ks = ICARSG
kr = ICARRG
do r = 1, NumGruRes*SiOferComResZona
!   para todos los intervalos
    do i = 1, NTINTR
        if ( dualresr10z ( r, i ) .gt. PrecioTopeEner*1.2 ) then
!        if ( dualresr10z ( r, i ) .gt. PrecioTope*1.2 ) then
            epsilon = epsireserva
        else
            epsilon = epsilonreserva
        endif
        segmarg = 0
!       para los requerimientos de reserva rodante de 10 minutos
        do s = 1, NumBloRR10
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResRR10 (r, s, i)
            ColVarC ( m )  = kr10 - 1
!           cota superior
            indices ( m ) = kr10 - 1
            bd ( m ) = xMILP ( kr10 )
            if ( s .lt. NumBloRR10 ) then
                if ( xMILP ( kr10 ) .lt. ReqResR10 (r, s, i) .and. segmarg .eq. 0 ) then
                    segmarg = s
                endif
                bd ( m ) = xMILP ( kr10 ) 
                lu ( m ) = 'B'
             else 
                if ( segmarg .eq. 0 ) segmarg = s  
                bd ( m ) = xMILP ( kr10 ) - epsilon
                NewValC ( m )  = -PreResRR10 (r, segmarg, i)
                lu ( m ) = 'L'
            endif
            kr10 = kr10 + 1
        enddo
        segmarg = 0
        if ( dualres10z(r,i) .lt. PrecioTopeEner*1.2 ) then
!        if ( dualres10z(r,i) .lt. PrecioTope*1.2 ) then
            epsilon = epsireserva
        else
            epsilon = epsilonreserva
        endif
!       para los requerimientos de reserva de 10 minutos
        do s = 1, NumBloR10
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResR10 (r, s, i)
            ColVarC ( m )  = k10 - 1
!           cota superior
            indices ( m ) = k10 - 1
            bd ( m ) = xMILP ( k10 )
            if ( s .lt. NumBloR10 )  then
               if ( xMILP ( k10 ) .lt. ReqRes10 (r, s, i) .and. segmarg .eq. 0 ) then
                   segmarg = s
               endif
               bd ( m ) = xMILP ( k10 )
               lu ( m ) = 'B'
            else
               if ( segmarg .eq. 0 ) segmarg = s  
               NewValC ( m )  = -PreResR10 (r, segmarg, i)
               bd ( m ) = xMILP ( k10 ) - epsilon
               lu ( m ) = 'L'
            endif
            k10 = k10 + 1
        enddo
        segmarg = 0
        if ( dualressz( r, i ) .lt. PrecioTopeEner*1.2 ) then
!        if ( dualressz( r, i ) .lt. PrecioTope*1.2 ) then
            epsilon = epsireserva
        else
            epsilon = epsilonreserva
        endif
!       para los requerimientos de reserva suplementaria
        do s = 1, NumBloRSu
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResSup (r, s, i)
            ColVarC ( m )  = ks - 1
!           cota superior
            indices ( m ) = ks - 1
            bd ( m ) = xMILP ( ks )
            if ( s .lt. NumBloRSu )  then
               bd ( m ) = xMILP ( ks ) 
               if ( xMILP ( ks ) .lt. ReqResSup (r, s, i) .and. segmarg .eq. 0 ) then
                   segmarg = s
               endif
               lu ( m ) = 'B'
            else
               if ( segmarg .eq. 0 ) segmarg = s  
               NewValC ( m )  = -PreResSup (r, segmarg, i)
               bd ( m ) = xMILP ( ks ) - epsilon
               lu ( m ) = 'L'
            endif
            ks = ks + 1
        enddo
!       para los requerimientos de reserva de regulacion secundaria
        segmarg = 0
        if ( dualresrez(r,i) .lt. PrecioTopeEner*1.2 ) then
!        if ( dualresrez(r,i) .lt. PrecioTope*1.2 ) then
            epsilon = epsireserva
        else
            epsilon = epsilonreserva
        endif
        do s = 1, NumBloRReg
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResReg (r, s, i)
            ColVarC ( m )  = kr - 1
!           cota superior
            indices ( m ) = kr - 1
            if ( s .lt. NumBloRReg )  then
               bd ( m ) = xMILP ( kr )
               if ( xMILP ( kr )  .lt. ReqResReg (r, s, i) .and. segmarg .eq. 0 ) then
                  segmarg = s
               endif
               lu ( m ) = 'B'
            else
               if ( segmarg .eq. 0 ) segmarg = s 
               NewValC ( m )  = -PreResReg (r, s, i)
               lu ( m ) = 'L'
               bd ( m ) = xMILP ( kr ) - epsilon
            endif
            kr = kr + 1
        enddo
    enddo
enddo

! para el sistema
kr10 = ICARR10S
k10 = ICAR10S
ks = ICARSS
kr = ICARRS
! para todos los intervalos
do i = 1, NTINTR*SiOferComResSis
    
    bres = 0
!   para los requerimientos de reserva rodante de 10 minutos
    do s = 1, NumBloRR10
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResRR10S (sistema, s, i)
        ColVarC ( m )  = kr10 - 1
!       cota superior
        indices ( m ) = kr10 - 1
        bd ( m ) = xMILP ( kr10 )
        if ( bres .eq. 0 .and. ReqResR10S (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRR10 )  then
                reqsig =  ReqResR10S (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( kr10 ) - ReqResR10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            else if (  ReqResR10S (sistema, s, i) .gt. xMILP ( kr10 ) .and. s .lt. NumBloRR10  ) then
                bd ( m ) = max(xMILP ( kr10 ) - ReqResR10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        kr10 = kr10 + 1
    enddo
    bres = 0
!   para los requerimientos de reserva de 10 minutos
    do s = 1, NumBloR10
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResR10S (sistema, s, i)
        ColVarC ( m )  = k10 - 1
!       cota superior
        indices ( m ) = k10 - 1
        bd ( m ) = xMILP ( k10 )
        if ( bres .eq. 0 .and. ReqRes10S (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloR10 )  then
                reqsig =  ReqRes10S (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( k10 ) - ReqRes10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            else if (  ReqRes10S (sistema, s, i) .gt. xMILP ( k10 ) .and. s .lt. NumBloR10  ) then
                bd ( m ) = max(xMILP ( k10 ) - ReqRes10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        k10 = k10 + 1
    enddo
    bres = 0
!   para los requerimientos de reserva suplementaria
    do s = 1, NumBloRSu
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResSupS (sistema, s, i)
        ColVarC ( m )  = ks - 1
!       cota superior
        indices ( m ) = ks - 1
        bd ( m ) = xMILP ( ks )
        if ( bres .eq. 0 .and. ReqResSupS (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRSu )  then
                reqsig =  ReqResSupS (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( ks ) - ReqResSupS (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            else if (  ReqResSupS (sistema, s, i) .gt. xMILP ( ks ) .and. s .lt. NumBloRSu  ) then
                bd ( m ) = max(xMILP ( ks ) - ReqResSupS (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        ks = ks + 1
    enddo
    bres = 0
!   para los requerimientos de reserva de regulacion secundaria
    do s = 1, NumBloRReg
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResRegS (sistema, s, i)
        ColVarC ( m )  = kr - 1
!       cota superior
        indices ( m ) = kr - 1
        bd ( m ) = xMILP ( kr )
        if ( bres .eq. 0 .and. ReqResRegS (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRReg )  then
                reqsig =  ReqResRegS (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( kr ) - 1.0e-5, 0.0 )
                bres = 1
            else if (  ReqResRegS (sistema, s, i) .gt. xMILP ( kr ) .and. s .lt. NumBloRReg  ) then
                bd ( m ) = max(xMILP ( kr ) - 1.0e-5, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        kr = kr + 1
    enddo
enddo


! Se cambian coeficientes
status = CPXchgobj (enb, lpMILP, m, ColVarC, NewValC )
if ( status .ne. 0 ) then
    write (*,*) ' Error al actualizar coeficientes de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

! se actualizan cotas superiores de variables de asignacion
status = CPXchgbds (enb, lpMILP, m, indices, lu, bd)
if ( status .ne. 0) then
    write (*,*) ' Error al actualizar cotas de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError    
    stop
end if

write( ssistema, '(I1)' )  sistema
if ( SiEscLP .eq. 1 .and. TipoEjecu .ne. 3 ) then
!   Se escribe el modelo actuaizado MILP a un archivo
    status = CPXwriteprob (enb, lpMILP, 'dirres/MDA.lp', 'LP')
!    status = CPXwriteprob (enb, lpMILP, 'MDA.sav', 'SAV')
endif

! se resuelve el problema
call ResuelveMILP ( ite, sistema, imprime, 1 )

return
end


subroutine SegundoProblema_old ( ite, sistema, imprime )
! ---------------------------------------------------------------------
! Se cambian las penalizaciones de variables artificiales para        *
! para determinar los precios de energia y servicios conexos.         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio del 2017                                                      *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE
use ParGloRed, only: NumGruRamSis, NumNodSis
use symtypes

!use cplex_ifaces, only: CPXchgbds , CPXchgobj, CPXwriteprob      !Windows (comentarizar para Linux)

implicit none

integer                 CPXchgbds , CPXchgobj, CPXwriteprob     !Linux (comentarizar para Windows)
 
integer br, d, e, ite, m, n, o, sistema, imprime
integer r, s, status, ibanbit, i, indices

integer ierror, kr10, k10, ks, kr, ColVarC, bres

REAL*8     bd, NewValC, reqsig
character*1 lu
character*1 ssistema
CHARACTER fecha_Ej*19

DIMENSION   bd      ( maxvarMILP )
DIMENSION   lu      ( maxvarMILP )
DIMENSION   indices ( maxvarMILP )
DIMENSION   NewValC ( maxvarMILP)
DIMENSION   ColVarC ( maxvarMILP )

data status  / 0 /

ibanbit = 1

! se fija la asignacion de las unidades obtenida previamente
call FijaAsignacion

! se determinan nuevos valores de penalizacion
! Se determina el precio del corte de carga
CostoCorte = PrecioTopeEner 

! Se determina el precio de infactibilidad en transmision
PenRamas = CostoCorte * (1.03)

! Se determina el precio de infactibilidad en limites de energia hidro
PenEnerEmb = CostoCorte

! Se determina el precio de infactibilidad en limites de energia termo
PenEnerUTerm = CostoCorte

if ( TipoEscPre .eq. 1 ) then

	PrecioTopeReg = PrecioTopeEner !max ( PrecioTopeReg, PrecioTopeRod, PrecioTopeSup )

! 	Se determina el precio de infactibilidad en reserva de regulacion por zona
	PreResReg = PrecioTopeEner !PrecioTopeReg

! 	Se determina el precio de infactibilidad en reserva rodante por zona
	PreResRR10 = PrecioTopeEner !PrecioTopeRod

! 	Se determina el precio de infactibilidad en reserva operativa por zona
	PreResR10 = PrecioTopeEner !PrecioTopeOper

! 	Se determina el precio de infactibilidad en reserva suplementaria por zona
	PreResSup = PrecioTopeEner !PrecioTopeSup

! 	Se determina el precio de infactibilidad en reserva de regulacion por sistema
	PreResRegS = PrecioTopeEner !PrecioTopeReg|

! 	Se determina el precio de infactibilidad en reserva rodante por sistema
	PreResRR10S = PrecioTopeEner !PrecioTopeRod

! 	Se determina el precio de infactibilidad en reserva operativa por sistema
	PreResR10S = PrecioTopeEner !PrecioTopeOper

! 	Se determina el precio de infactibilidad en reserva suplementaria por sistema
	PreResSupS = PrecioTopeEner !PrecioTopeSup

endif

! Se cambian las penalizaciones y cotas superiores
m = 0
do i = 1, NTINTR
!   Corte de carga
    do d = 1, NumOferDem
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = CostoCorte*SiCorte 
        ColVarC ( m )  = IDF + d + (i-1)*NumOferDem - 2
!       cota superior
        indices ( m ) = IDF + d + (i-1)*NumOferDem - 2
        bd ( m ) = xMILP ( IDF + d + (i-1)*NumOferDem - 1 )
        if ( xMILP ( IDF + d + (i-1)*NumOferDem - 1 ) .gt. 0.0 ) then
            bd ( m ) = xMILP ( IDF + d + (i-1)*NumOferDem - 1 ) + 1.0e-5
        endif
        lu ( m ) = 'U'
    enddo
!   para todos los nodos
    do n = 1, NumNodSis ( sistema )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = CostoExced*SiExced 
        ColVarC ( m )  = IEXC + n + (i-1)*NumNodSis ( sistema ) - 2
!       cota superior
        indices ( m ) = IEXC + n + (i-1)*NumNodSis ( sistema ) - 2
        bd ( m ) = xMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 )
        if ( xMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 ) .gt. 0.0 ) then
            bd ( m ) = xMILP ( IEXC + n + (i-1)*NumNodSis ( sistema ) - 1 ) + 1.0e-5
        endif
        lu ( m ) = 'U'
    enddo
!   variable de perdidas
    m = m + 1
!   coeficientes de penalizacion
    NewValC ( m )  = 1.0 
    ColVarC ( m )  = IPERD + i - 2
!   cota superior
    indices ( m ) = IPERD + i - 2
    bd ( m ) = xMILP ( IPERD + i - 1 )
    lu ( m ) = 'U'
!   Restricciones de transmision
    do br = 1, NumGruRamSis (sistema)
!       variable artificial de excedente de flujo
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = PenRamas*SiArtTrans 
        ColVarC ( m )  = IAEF + br + (i-1)*NumGruRamSis (sistema) - 2
!       cota superior
        indices ( m ) = IAEF + br + (i-1)*NumGruRamSis (sistema) - 2
        bd ( m ) = xMILP ( IAEF + br + (i-1)*NumGruRamSis (sistema) - 1 )
        if ( bd ( m )  .gt. 0.0 ) then
            bd ( m ) = xMILP ( IAEF + br + (i-1)*NumGruRamSis (sistema) - 1 ) - 1.0e-04
        endif
        lu ( m ) = 'L'
!       variable artificial de excedente de contraflujo
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = PenRamas*SiArtTrans 
        ColVarC ( m )  = IACF + br + (i-1)*NumGruRamSis (sistema) - 2
!       cota superior
        indices ( m ) = IACF + br + (i-1)*NumGruRamSis (sistema) - 2
        bd ( m ) = xMILP ( IACF + br + (i-1)*NumGruRamSis (sistema) - 1 )
        if ( bd ( m )  .gt. 0.0 ) then
            bd ( m ) = xMILP ( IAEF + br + (i-1)*NumGruRamSis (sistema) - 1 ) - 1.0e-04
        endif
        lu ( m ) = 'L'
    enddo
enddo

! produccion de energia para grupos de energia termica
do o = 1 , NumGruUTer*SiEnerTer
    m = m + 1
!   coeficientes de penalizacion
    NewValC ( m )  = PenEnerUTerm 
    ColVarC ( m )  = IARGT + o - 2
!   cota superior
    indices ( m ) = IARGT + o - 2
    bd ( m ) = max( xMILP ( IARGT + o - 1 )- 1.0e-5, 0.0 )
    lu ( m ) = 'L'
enddo

! produccion de energia por embalse
do e = 1 , NumEmbalses*SiEnerHid
    m = m + 1
!   coeficientes de penalizacion
    NewValC ( m )  = PenEnerEmb 
    ColVarC ( m )  = IAREE + e - 2
!   cota superior
    indices ( m ) = IAREE + e - 2
    bd ( m ) = max( xMILP ( IAREE + e - 1 ) - 1.0e-5, 0.0 )
    lu ( m ) = 'L'
enddo

! Ofertas de compra de reserva del CENACE por zona
! para los grupos de reserva
kr10 = ICARR10G
k10 = ICAR10G
ks = ICARSG
kr = ICARRG
do r = 1, NumGruRes*SiOferComResZona
!   para todos los intervalos
    do i = 1, NTINTR
        bres = 0
!       para los requerimientos de reserva rodante de 10 minutos
        do s = 1, NumBloRR10
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResRR10 (r, s, i)
            ColVarC ( m )  = kr10 - 1
!           cota superior
            indices ( m ) = kr10 - 1
            bd ( m ) = xMILP ( kr10 )
            if ( bres .eq. 0 .and. ReqResR10 (r, s, i) .gt. 0.0 ) then
                 reqsig = 0.0
                 if ( s .lt. NumBloRR10 )  then
                     reqsig =  ReqResR10 (r, s+1, i)
                 endif
                 if ( reqsig .eq. 0 ) then
                     bd ( m ) = max(xMILP ( kr10 ) - ReqResR10 (r, s, i)*0.005, 0.0)
                     bres = 1
                 else if (  ReqResR10 (r, s, i) .gt. xMILP ( kr10 ) .and. s .lt. NumBloRR10  ) then
                     bd ( m ) = max(xMILP ( kr10 ) - ReqResR10 (r, s, i)*0.005, 0.0)
                     bres = 1
                 endif
            endif
            lu ( m ) = 'L'
            kr10 = kr10 + 1
        enddo
        bres = 0
!       para los requerimientos de reserva de 10 minutos
        do s = 1, NumBloR10
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResR10 (r, s, i)
            ColVarC ( m )  = k10 - 1
!           cota superior
            indices ( m ) = k10 - 1
            bd ( m ) = xMILP ( k10 )
            if ( bres .eq. 0 .and. ReqRes10 (r, s, i) .gt. 0.0 ) then
                 reqsig = 0.0
                 if ( s .lt. NumBloR10 )  then
                     reqsig =  ReqRes10 (r, s+1, i)
                 endif
                 if ( reqsig .eq. 0 ) then
                     bd ( m ) = max(xMILP ( k10 )- ReqRes10 (r, s, i)*0.005, 0.0)
                     bres = 1
                 else if (  ReqRes10 (r, s, i) .gt. xMILP ( k10 ) .and. s .lt. NumBloR10  ) then
                     bd ( m ) = max(xMILP ( k10 ) - ReqRes10 (r, s, i)*0.005, 0.0)
                     bres = 1
                 endif
            endif
            lu ( m ) = 'L'
            k10 = k10 + 1
        enddo
        bres = 0
!       para los requerimientos de reserva suplementaria
        do s = 1, NumBloRSu
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResSup (r, s, i)
            ColVarC ( m )  = ks - 1
!           cota superior
            indices ( m ) = ks - 1
            bd ( m ) = xMILP ( ks )
            if ( bres .eq. 0 .and. ReqResSup (r, s, i) .gt. 0.0 ) then
                 reqsig = 0.0
                 if ( s .lt. NumBloRSu )  then
                     reqsig =  ReqResSup (r, s+1, i)
                 endif
                 if ( reqsig .eq. 0 ) then
                     bd ( m ) = max(xMILP ( ks ) - ReqResSup (r, s, i)*0.005, 0.0)
                     bres = 1
                 else if (  ReqResSup (r, s, i) .gt. xMILP ( ks ) .and. s .lt. NumBloRSu  ) then
                     bd ( m ) = max(xMILP ( ks ) - ReqResSup (r, s, i)*0.005, 0.0)
                     bres = 1
                 endif
            endif
            lu ( m ) = 'L'
            ks = ks + 1
        enddo
!       para los requerimientos de reserva de regulacion secundaria
        bres = 0
        do s = 1, NumBloRReg
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResReg (r, s, i)
            ColVarC ( m )  = kr - 1
!           cota superior
            indices ( m ) = kr - 1
            bd ( m ) = xMILP ( kr )
            if ( bres .eq. 0 .and. ReqResReg (r, s, i) .gt. 0.0 ) then
                 reqsig = 0.0
                 if ( s .lt. NumBloRReg )  then
                     reqsig =  ReqResReg (r, s+1, i)
                 endif
                 if ( reqsig .eq. 0 ) then
                     bd ( m ) = max(xMILP ( kr ) - 1.0e-5, 0.0)
                     bres = 1
                 else if (  ReqResReg (r, s, i) .gt. xMILP ( kr ) .and. s .lt. NumBloRReg  ) then
                     bd ( m ) = max(xMILP ( kr ) - 1.0e-5, 0.0)
                     bres = 1
                 endif
            endif
            lu ( m ) = 'L'
            kr = kr + 1
        enddo
    enddo
enddo

! para el sistema
kr10 = ICARR10S
k10 = ICAR10S
ks = ICARSS
kr = ICARRS
! para todos los intervalos
do i = 1, NTINTR*SiOferComResSis
    
    bres = 0
!   para los requerimientos de reserva rodante de 10 minutos
    do s = 1, NumBloRR10
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResRR10S (sistema, s, i)
        ColVarC ( m )  = kr10 - 1
!       cota superior
        indices ( m ) = kr10 - 1
        bd ( m ) = xMILP ( kr10 )
        if ( bres .eq. 0 .and. ReqResR10S (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRR10 )  then
                reqsig =  ReqResR10S (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( kr10 ) - ReqResR10S (sistema, s, i)*0.005, 0.0 )
                bres = 1
            else if (  ReqResR10S (sistema, s, i) .gt. xMILP ( kr10 ) .and. s .lt. NumBloRR10  ) then
                bd ( m ) = max(xMILP ( kr10 ) - ReqResR10S (sistema, s, i)*0.005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        kr10 = kr10 + 1
    enddo
    bres = 0
!   para los requerimientos de reserva de 10 minutos
    do s = 1, NumBloR10
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResR10S (sistema, s, i)
        ColVarC ( m )  = k10 - 1
!       cota superior
        indices ( m ) = k10 - 1
        bd ( m ) = xMILP ( k10 )
        if ( bres .eq. 0 .and. ReqRes10S (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloR10 )  then
                reqsig =  ReqRes10S (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( k10 ) - ReqRes10S (sistema, s, i)*0.005, 0.0 )
                bres = 1
            else if (  ReqRes10S (sistema, s, i) .gt. xMILP ( k10 ) .and. s .lt. NumBloR10  ) then
                bd ( m ) = max(xMILP ( k10 ) - ReqRes10S (sistema, s, i)*0.005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        k10 = k10 + 1
    enddo
    bres = 0
!   para los requerimientos de reserva suplementaria
    do s = 1, NumBloRSu
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResSupS (sistema, s, i)
        ColVarC ( m )  = ks - 1
!       cota superior
        indices ( m ) = ks - 1
        bd ( m ) = xMILP ( ks )
        if ( bres .eq. 0 .and. ReqResSupS (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRSu )  then
                reqsig =  ReqResSupS (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( ks ) - ReqResSupS (sistema, s, i)*0.005, 0.0 )
                bres = 1
            else if (  ReqResSupS (sistema, s, i) .gt. xMILP ( ks ) .and. s .lt. NumBloRSu  ) then
                bd ( m ) = max(xMILP ( ks ) - ReqResSupS (sistema, s, i)*0.005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        ks = ks + 1
    enddo
    bres = 0
!   para los requerimientos de reserva de regulacion secundaria
    do s = 1, NumBloRReg
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResRegS (sistema, s, i)
        ColVarC ( m )  = kr - 1
!       cota superior
        indices ( m ) = kr - 1
        bd ( m ) = xMILP ( kr )
        if ( bres .eq. 0 .and. ReqResRegS (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRReg )  then
                reqsig =  ReqResRegS (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( kr ) - 1.0e-5, 0.0 )
                bres = 1
            else if (  ReqResRegS (sistema, s, i) .gt. xMILP ( kr ) .and. s .lt. NumBloRReg  ) then
                bd ( m ) = max(xMILP ( kr ) - 1.0e-5, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        kr = kr + 1
    enddo
enddo

!do i = 1, m                         
!   ColVarC ( i ) = ColVarC ( i ) / 1.0e5
!enddo

! Se cambian coeficientes
status = CPXchgobj (enb, lpMILP, m, ColVarC, NewValC )
if ( status .ne. 0 ) then
    write (*,*) ' Error al actualizar coeficientes de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

! se actualizan cotas superiores de variables de asignacion
status = CPXchgbds (enb, lpMILP, m, indices, lu, bd)
if ( status .ne. 0) then
    write (*,*) ' Error al actualizar cotas de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError    
    stop
end if

write( ssistema, '(I1)' )  sistema
!Se escribe el modelo actuaizado MILP a un archivo
!status = CPXwriteprob (enb, lpMILP, 'SP'//ssistema//'.lp', 'LP')
!status = CPXwriteprob (enb, lpMILP, 'SP'//ssistema//'.sav', 'SAV')

! se resuelve el problema
call ResuelveMILP ( ite, sistema, imprime, 1 )

return
end


!* ------------------------------------------------------------------------------    
!* Escala precios de servicios conexos cuando el segundo problema es infactible *
!* ------------------------------------------------------------------------------ 
subroutine EscalaPreciosConexos                     
! ---------------------------------------------------------------------
! Se cambian las penalizaciones de variables artificiales para        *
! para determinar los precios de energia y servicios conexos.         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero de 2019                                                       *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE
use ParGloRed, only: PrecioTope

Implicit none

integer r, i, s, kr10, k10, ks, kr, smarg

real*8 preciodeseado, facesc, umbral

! Inicializa duales escalados
!dualresr10zesc = dualresr10z
!dualres10zesc = dualres10z
!dualresszesc = dualressz
!dualresrezesc = dualresrez

umbral = PrecioTope ! *base ! Cuando se corre el segundo problema no se multiplica por la base

kr10 = ICARR10G
k10 = ICAR10G
ks = ICARSG
kr = ICARRG

do r = 1, NumGruRes*SiOferComResZona
!   para todos los intervalos
    do i = 1, NTINTR
        
!       para los requerimientos de reserva suplementaria
        if ( dualresszesc ( r, i ) .gt. umbral ) then
            smarg = 0
            do s = 1, NumBloRSu
!               Identifica el segmento marginal
                if ( smarg .eq. 0 ) then
                    if ( ( ReqResSup (r, s, i) - xMILP ( ks ) ) .gt. 0.0 .or. s .eq.  NumBloRSu ) then
                        preciodeseado = PreResSup (r, s, i)
                        smarg = s
                    endif
                endif
                ks = ks + 1
            enddo
!           Si es el MDA o AUHE
            if ( TipoEjecu .eq. 0 .or. TipoEjecu .eq. 3 ) then
                preciodeseado = PrecioTope ! PrecioTopeEner
            endif
            facesc =  preciodeseado/dualresszesc ( r, i )
            dualresszesc ( r, i ) = dualresszesc ( r, i ) * facesc
        else
            do s = 1, NumBloRSu
                ks = ks + 1
            enddo
        endif        
!       para los requerimientos de reserva de 10 minutos
        if ( dualres10zesc ( r, i ) .gt. umbral ) then
            smarg = 0
            do s = 1, NumBloR10
!               Identifica el segmento marginal
                if ( smarg .eq. 0 ) then
                    if ( ( ReqRes10 (r, s, i) - xMILP ( k10 ) ) .gt. 0.0 .or. s .eq.  NumBloR10 ) then
                        preciodeseado = PreResR10 (r, s, i)
                        smarg = s
                    endif
                endif
                k10 = k10 + 1
            enddo
!           Si es el MDA o AUHE
            if ( TipoEjecu .eq. 0 .or. TipoEjecu .eq. 3 ) then
                preciodeseado = PrecioTope !PrecioTopeEner
            endif
            if ( preciodeseado .lt. ( preciodeseado - dualresszesc ( r, i ) ) ) then
              preciodeseado = preciodeseado + dualresszesc ( r, i )
            endif
            preciodeseado = preciodeseado - dualresszesc ( r, i )
            facesc =  preciodeseado/dualres10zesc ( r, i )
            dualres10zesc ( r, i ) =   dualres10zesc ( r, i ) * facesc
        else
            do s = 1, NumBloR10
                k10 = k10 + 1
            enddo
        endif
!       para los requerimientos de reserva rodante de 10 minutos
        if ( dualresr10zesc ( r, i ) .gt. umbral ) then
            smarg = 0
            do s = 1, NumBloRR10
!               Identifica el segmento marginal
                if ( smarg .eq. 0 ) then
                    if ( ( ReqResR10 (r, s, i) - xMILP ( kr10 ) ) .gt. 0.0 .or. s .eq.  NumBloRR10 ) then
                        preciodeseado = PreResRR10 (r, s, i)
                        smarg = s
                    endif
                endif
               kr10 = kr10 + 1
            enddo
!           Si es el MDA o AUHE
            if ( TipoEjecu .eq. 0 .or. TipoEjecu .eq. 3 ) then
                preciodeseado = PrecioTope !PrecioTopeEner
            endif
            if ( preciodeseado .lt. ( dualres10zesc( r, i ) + dualresszesc( r, i ) ) ) then
                preciodeseado = preciodeseado + dualres10zesc( r, i ) + dualresszesc( r, i )
            endif
            preciodeseado =  preciodeseado - dualres10zesc( r, i ) - dualresszesc( r, i )
            facesc =  preciodeseado/dualresr10zesc ( r, i )
            dualresr10zesc ( r, i ) = dualresr10zesc ( r, i ) * facesc
        else
            do s = 1, NumBloRR10
               kr10 = kr10 + 1
            enddo
        endif
        
        ! Verifica si hay marginales de reserva de regulación altos
        if ( dualresrezesc ( r, i ) .gt. umbral ) then
!           para los requerimientos de reserva de regulacion
            smarg = 0
            do s = 1, NumBloRReg
!               Identifica el segmento marginal
                if ( smarg .eq. 0 ) then
                    if ( ( ReqResReg (r, s, i) - xMILP ( kr ) ) .gt. 0.0 .or. s .eq.  NumBloRReg ) then
                        preciodeseado = PreResReg (r, s, i)
                        smarg = s
                    endif
                endif
                kr = kr + 1
            enddo
!           Si es el MDA o AUHE
            if ( TipoEjecu .eq. 0 .or. TipoEjecu .eq. 3 ) then
                preciodeseado = PrecioTope !PrecioTopeEner
            endif
!            if ( preciodeseado .lt. ( dualresr10zesc ( r, i ) + dualres10zesc( r, i ) + dualresszesc( r, i )  )  ) then
!                preciodeseado = preciodeseado + dualresr10zesc ( r, i ) + dualres10zesc( r, i ) + dualresszesc( r, i )  
!            endif
            facesc =  preciodeseado/dualresrezesc( r, i )
            dualresrezesc ( r, i ) = dualresrezesc( r, i ) * facesc
        else
            do s = 1, NumBloRReg
                kr = kr + 1
            enddo
        endif

    enddo        
enddo

return
end 


subroutine AcotaPerdidas ( sistema )
! ---------------------------------------------------------------------
! Se acota superiormente la variable de perdidas, conforme a la       *
! estimacion del problema de flujos.                                  *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Marzo de 2018                                                       *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE
use ParGloRed, only: PerIntervalo
use symtypes

!use cplex_ifaces, only: CPXchgbds , CPXwriteprob      !Windows (comentarizar para Linux)


implicit none

integer                 CPXwriteprob, CPXaddrows    !Linux (comentarizar para Windows)
integer                 CPXchgbds      !Linux (comentarizar para Windows)
 
integer sistema
integer status, ibanbit, i, cnt, indices


integer ierror

REAL*8     bd 
character*1 lu
character*1 ssistema
CHARACTER fecha_Ej*19

DIMENSION   bd      ( maxvarMILP )
DIMENSION   lu      ( maxvarMILP )
DIMENSION   indices ( maxvarMILP )

data status  / 0 /

ibanbit = 1
cnt = 0

! para cada intervalo
do i = 1, NTINTR
!   se fijan las perdidas
    cnt = cnt + 1
    indices ( cnt ) = IPERD + i - 2
    bd ( cnt ) = 5000.0/Base
    if ( nomsis(sistema) .eq. 'BCA' ) then
        bd ( cnt ) = PerIntervalo(i)*1.02
    endif
    lu ( cnt ) = 'U'
enddo

! se actualizan cotas superiores de variables de asignacion
status = CPXchgbds (enb, lpMILP, cnt, indices, lu, bd)
if ( status .ne. 0) then
    write (*,*) ' Error al actualizar cotas de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError    
!   algoritmo no termina bien
    call SalidaError
    stop
end if

write( ssistema, '(I1)' )  sistema
!Se escribe el modelo actuaizado MILP a un archivo
!status = CPXwriteprob (enb, lpMILP, 'bp'//ssistema//'.lp', 'LP')
!status = CPXwriteprob (enb, lpMILP, 'bp'//ssistema//'.sav', 'SAV')

return
end


subroutine SalidaError
! ---------------------------------------------------------------------
! Escribe bandera de terminacion anormal del algoritmo.               *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Agosto del 2017                                                     *
! ---------------------------------------------------------------------

rewind(555)
write(555,*) '1'

return
end


subroutine AsignaOri
! ---------------------------------------------------------------------
! Guardar las cotas de las variables de asignacion en el modelo MILP  *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Febrero de 2019                                                     *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE
use symtypes

implicit none
 
integer i, modo, u

lbOMILP = 0
ubOMILP = 0

! para todas las unidades de rango continuo
do u = 1, NumUniRC
!   para cada intervalo
    do i = 1, NTINTR
        lbOMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = lbMILP ( IARC + u + (i-1)*NumUniRC - 1 )
        ubOMILP ( IARC + u + (i-1)*NumUniRC - 1 ) = ubMILP ( IARC + u + (i-1)*NumUniRC - 1 )
    enddo
enddo
! para todas las unidades de rango discontinuo
do u = 1, NumUniRD
!   para cada intervalo
    do i = 1, NTINTR
!       para todos los modos
        do modo = 1, NumModRD ( u )
            lbOMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = lbMILP ( IARD + INIURDI ( u, i ) + modo - 1 )
            ubOMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) = ubMILP ( IARD + INIURDI ( u, i ) + modo - 1 )
        enddo
    enddo
enddo
! para todas las unidades hidro
do u = 1 , NumUniHid
!   Para todos los intervalos
    do i = 1 , NTINTR
        lbOMILP ( IAH + u + (i-1)*NumUniHid - 1  ) = lbMILP ( IAH + u + (i-1)*NumUniHid - 1  )
        ubOMILP ( IAH + u + (i-1)*NumUniHid - 1  ) = ubMILP ( IAH + u + (i-1)*NumUniHid - 1  )
    enddo
enddo
! para todas las unidades renovables
do u = 1 , NumUniRE
!   Para todos los intervalos
    do i = 1 , NTINTR
        lbOMILP ( IARE + u + (i-1)*NumUniRE - 1 ) = lbMILP ( IARE + u + (i-1)*NumUniRE - 1  )
        ubOMILP ( IARE + u + (i-1)*NumUniRE - 1  ) = ubMILP ( IARE + u + (i-1)*NumUniRE - 1  )
    enddo
enddo

return
end
