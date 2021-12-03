
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
! Septiembre de 2020                                                  *
! ---------------------------------------------------------------------
Program AUCHT

use ParAUHE
use ProblemaAUHE
use ParGloRed, only: NumGruRamSis, RamActiva

use symtypes
!use cplex_ifaces, only: CPXsetintparam, CPXsetlogfilename   !Windows (comentarizar para Linux)
use cplex_cons, only:   CPX_ON, CPX_PARAM_SCRIND, CPX_OFF, CPX_PARAM_CLONELOG

implicit none

integer                 CPXsetintparam, CPXsetlogfilename   !Linux (comentarizar para Windows)

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

status = CPXsetlogfilename (enb, "./dirres/Optimizador.log", "w");

ibanbit = 1
ierror = 0
RamActiva = 0

status = CPXsetintparam(enb, CPX_PARAM_CLONELOG, -1)

Call FechaEjecucion (fecha_Ej)
BMensaje = fecha_Ej//' '//NomEjecu//'001 VERSION 1.0.2'
call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
Call FechaEjecucion (fecha_Ej)
bmensaje = fecha_Ej//' '//NomEjecu//'001 INICIA ASIGNACION DE UNIDADES '//NomEjecu
Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )

! escitura al archivo Version_ control del ejcutable
OPEN ( UNIT = 999, FILE = RUT_RES//'Version_'//trim(NomEjecu)//'.res', IOSTAT = IERROR, &
       STATUS='UNKNOWN', RECORDSIZE = 250                )
     write( 999, 100)

! se crean los archivos de resultados del MDA (AUGC, AUHE, EXPOST)
call CreaArchRes

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

!       si existe transmision en el subsistema
        if ( NumGruRamSis ( sistema )*SiTransmision .gt. 0 ) then
            if ( SiSolucionInicial .eq. 0 ) then
                OPEN ( UNIT = 55, FILE = rut_res//'CONJACTIV.csv', IOSTAT = IERROR,  STATUS='unknown', RECORDSIZE = 250 )
                close (UNIT = 55, status = 'delete')
                OPEN ( UNIT = 55, FILE = rut_res//'CONJACTIV.csv', ACCESS = 'APPEND', IOSTAT = IERROR,  STATUS='unknown', RECORDSIZE = 250 )
            endif
        endif
        
!       se detrminan dimensiones conforme a los parametros de sintonizacion
        call DefineDimensionesMILP
        
!       se abren los archivos de debugger
        call AbreArchivos ( sistema )

!       Se forma y resuleve el problema de asignacion de unidades (AUHE) con conjunto activo
        call FormaResuleveSCA ( sistema, ite )
		 
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
write ( UniSemaf, * ) SemBandera ( 8 ),',',' Violacion de limites unidades hidro'
!write ( UniSemaf, * ) SemBandera ( 9 ),',',' Violacion de limites de combustible'

4321 continue
close (55)
5100 FORMAT (I3)
5101 FORMAT (F12.2)


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
     1X,'|  2019/05/08     Correccion en la escritura del modo de  |',/, &
     1X,'|                 operacion en el archivo RESMODO para las|',/, &
     1X,'|                 unidades de rango continuo no disponi-  |',/, &
     1X,'|                 bles.                                   |',/, &
!     1X,'|  2019/05/15     Modificar las restriciones de limita-   |',/, &
!     1X,'|                 cion de consumo de combustible y limi-  |',/, &
!     1X,'|                 taciones de energía a restricciones por |',/, &
!     1X,'|                 rangos horarios.                        |',/, &
     1X,'|  2019/05/15     Correccion en la escritura del modo de  |',/, &
     1X,'|                 operacion en el archivo RESMODO para las|',/, &
     1X,'|                 unidades compensadores sincronos.       |',/, &
!     1X,'|  2019/06/19     Adecuacion del manejo de ofertas de     |',/, &
!     1X,'|                 importacion y exportacion.              |',/, &
     1X,'|  2019/07/08     Correccion en la escritura de las altu- |',/, &
     1X,'|                 ras netas de los embalses en el archivo |',/, &
     1X,'|                 CARGAH_1.                               |',/, &
     1X,'|  2019/07/26     Incremento de dimension en arreglos de  |',/, &
     1X,'|                 la representacion de la funcion de ge-  |',/, &
     1X,'|                 neracion hidro.                         |',/, &
     1X,'|  2019/08/07     Correccion en calculo de desviacion es- |',/, &
     1X,'|                 tandar de precios marginales regionales,|',/, &
     1X,'|                 y bandera para manejo de restricciones  |',/, &
     1X,'|                 de energia.                             |',/, &
     1X,'|  2019/08/09     Uso de nueva version del CPLEX (version |',/, &
     1X,'|                 129).                                   |',/, &
     1X,'|  2019/08/22     Cambio de valor de parametro CPLEX para |',/, &
     1X,'|                 administrar mejor la memoria.           |',/, &
     1X,'|  2019/08/23     Se corrige logica para formar apunta-   |',/, &
     1X,'|                 dores de inicio de variables de rango   |',/, &
     1X,'|                 operativo.                              |',/, &
     1X,'|  2019/10/10     Se adicionan mensajes a bitacora con    |',/, &
     1X,'|                 informacion del GAP absoluto CPLEX.     |',/, &
     1X,'|  2019/10/08     Se incorpora opcion de usar solucion    |',/, &
     1X,'|                 inicial (solo AUHE).                    |',/, &
     1X,'|  2019/10/15     Ajuste de tolerancia para factibilidad  |',/, &
     1X,'|                 en el problema de despacho.             |',/, &
     1X,'|  2019/10/25     Se activa bandera de reserva de regu-   |',/, &
     1X,'|                 lacion anidada.                         |',/, &
     1X,'|  2019/12/12     No se permite excedentes en nodos con   |',/, &
     1X,'|                 nivel de tension menor a 1 kv.          |',/, &
     1X,'|                 Incorporacion de un porcentaje de la    |',/, &
     1X,'|                 demanda nodal lo cual sera la cota      |',/, &
     1X,'|                 superior de la variable de corte en los |',/, &
     1X,'|                 nodos.                                  |',/, &
     1X,'|                 Se desactiva la opcion para escribir a  |',/, &
     1X,'|                 un archivo el modelo MILP.              |',/, &
     1X,'|                 Se incorpora un semaforo a la IHM para  |',/, &
     1X,'|                 indicar cuando algun limite de despa-   |',/, &
     1X,'|                 cho de una unidad hidro se ha modifi-   |',/, &
     1X,'|                 cado, por factibilidad hidraulica.      |',/, &
!     1X,'|                 Mantener reserva asignada en MDA para   |',/, &
!     1X,'|                 el AUGC.                                |',/, &
!     1X,'|                 Relajamiento interno de la reserva      |',/, &
!     1X,'|                 rodante Rod10 y Regul cuando se desea   |',/, &
!     1X,'|                 mantener las reservas de MDA, antes de  |',/, &
!     1X,'|                 caer en infactibilidad matematica.      |',/, &
!     1X,'|                 Incorporar la opcion para que el MDA    |',/, &
!     1X,'|                 pueda considerar la asignacion manual   |',/, &
!     1X,'|                 de reservas de regulacion.              |',/, &
     1X,'|                 Se incorpora la creacion de todos los   |',/, &
     1X,'|                 archivos csv de resultados, indepen-    |',/, &
     1X,'|                 dientementemente de los parametros de   |',/, &
     1X,'|                 ejecucion seleccionados.                |',/, &
     1X,'|  2020/01/24     Se limpian todos los archivos csv de    |',/, &
     1X,'|                 resultados.                             |',/, &
!     1X,'|                 Se incluye bandera para decidir mante-  |',/, &
!     1X,'|                 ner reservas no rodantes.               |',/, &
!     1X,'|                 Se incluye bandera para escribir los    |',/, &
!     1X,'|                 resultados de la red electrica a archi- |',/, &
!     1X,'|                 vos CSV.                                |',/, &
     1X,'|  2020/02/24     Se corrige logica cuando se usa solu-   |',/, &
     1X,'|                 cion inicial y se desactiva un grupo    |',/, &
     1X,'|                 de ramas de transmision.                |',/, &
     1X,'|  2020/02/27     Se agrega la restriccion adicional para |',/, &
     1X,'|                 el caso de UPC en modo PIE-LIE.         |',/, &
     1X,'|  2020/03/02     Se ajusta el manejo de las condiciones  |',/, &
     1X,'|                 iniciales en el caso de que una unidad  |',/, &
     1X,'|                 se encuentre en proceso de arranque.    |',/, &
     1X,'|  2020/03/18     Se habilita la restriccion y valida-    |',/, &
     1X,'|                 cion de las restricciones de rampa de   |',/, &
     1X,'|                 10 y 30 minutos, solo para BCA.         |',/, &
     1X,'|  2020/03/25     Se corrige escalamiento de servicios    |',/, &
     1X,'|                 conexos (no se estaba considerando el   |',/, &
     1X,'|                 anidamiento de la reserva de regulacion.|',/, &
     1X,'|                 Se mejora el desempeño de la solucion   |',/, &
     1X,'|                 del segundo problema de precios, ante   |',/, &
     1X,'|                 escasez de reserva.                     |',/, &
     1X,'|                 Se adecua la escritura de los archivos  |',/, &
     1X,'|                 quiceminutales, ante la inicializacion  |',/, &
     1X,'|                 de todos los archivos de resultados.    |',/, &
     1X,'|  2020/04/04     Se corrige actualizacion de alturas de  |',/, &
     1X,'|                 embalses, ante escenarios de mas o      |',/, &
     1X,'|                 menos de 24 intervalos de planeacion.   |',/, &
     1X,'|  2020/04/17     Se corrige formato de escritura en la   |',/, &
     1X,'|                 rutina RestUPC (de enteros de dos       |',/, &
     1X,'|                 cifras, se cambia a tres cifras).       |',/, &
     1X,'|  2020/05/24     Se mejora la precision de tolerancias   |',/, &
     1X,'|                 utilizadas en el algoritmo.             |',/, &
!     1X,'|                 Se activa asignacion manual de reserva  |',/, &
!     1X,'|                 de regulacion en AUGC.                  |',/, &
!     1X,'|                 Se corrige impresion de resultados en   |',/, &
!     1X,'|                 archivo RESUMEN_UNIDADES (uso reserva)  |',/, &
     1X,'|                 Corregir calculo de maximo a regular por|',/, &
     1X,'|                 rango de regulacion.                    |',/, &
     1X,'|                 Incluir la logica para el manejo de dos |',/, &
     1X,'|                 rangos de regulacion.                   |',/, &
     1X,'|                 Incluir mensajes a ususario de errores  |',/, &
     1X,'|                 en los archivos de entrada.             |',/, &
     1X,'|                 Incluir manejo de islas electricas      |',/, &
     1X,'|                 debido a licencias de transmision.      |',/, &
     1X,'|  2020/07/08     Aumentar el tamaño de la cadena que     |',/, &
     1X,'|                 contiene el nombre de RAMA a 30         |',/, &
     1X,'|                 caracteres, para ecribir los archivos   |',/, &
     1X,'|                 FLUPOTACTRAMA_1.csv,PERPOTACTRAMA_1.csv |',/, &
     1X,'|  2020/07/29     Se incrementa la dimension de las uni-  |',/, &
     1X,'|                 dades renovables, de 60 a 100.          |',/, &
     1X,'|                 Corregir calculo de maximo a regular por|',/, &
     1X,'|                 rango de regulacion.                    |',/, &
     1X,'|                 Incluir la logica para el manejo de dos |',/, &
     1X,'|                 rangos de regulacion.                   |',/, &
     1X,'|                 Incluir mensajes a ususario de errores  |',/, &
     1X,'|                 en los archivos de entrada.             |',/, &
     1X,'|                 Incluir manejo de islas electricas      |',/, &
     1X,'|                 debido a licencias de transmision.      |',/, &
     1X,'|  2020/07/30     Aumentar el tamaño de la cadena que     |',/, &
     1X,'|                 contiene el nombre de RAMA a 30         |',/, &
     1X,'|                 caracteres, para ecribir los archivos   |',/, &
     1X,'|                 FLUPOTACTRAMA_1.csv,PERPOTACTRAMA_1.csv |',/, &
     1X,'|                 Corregir el uso de la variable de corte |',/, &
     1X,'|                 de carga en el modelo.                  |',/, &
     1X,'|  2020/09/02     Se incrementa el máximo numero de uni-  |',/, &
     1X,'|                 dades no programables (de 500 a 550).   |',/, &
!     1X,'|  2020/09/04     Se valida el minimo solicitado a regu-  |',/, &
!     1X,'|                 lar para las unidades termo e hidro,    |',/, &
!     1X,'|                 considerando lo maximo posible a regular|',/, &
!     1X,'|  2020/09/10     Se agrega la bandera para decidir el    |',/, &
!     1X,'|                 escribir a un archivo el modelo MILP.   |',/, &
     1X,'|  2020/09/10     Se permite en EXPOST considerar ofer-   |',/, &
     1X,'|                 tas de compra de energia variables o    |',/, &
     1X,'|                 sensibles al precio.                    |',/, &
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

logical interrumpe ! URIEL

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

! si no se utilizo solucion inicial
if ( SiSolucionInicial .eq. 0 ) then
!   Numero de restricciones adicionales al problema MILP
    NumResAdi = 0
!   Inicializa informacion sobre restricciones adicionales
    InfRestAdi = 0
    IntRestAdi = 0
    IsenRestAdi = 'E'
endif

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

!   URIEL	 ********
	GAPperdidas = 0.0 ! URIEL
!   URIEL GAP DE PERDIDAS REQUERIDO 	********
	!GAPperreq = 0.001 ! URIEL
	
    interrumpe = .TRUE.  ! URIEL
!    do while ( ite .le. IterPerdidas .or. ( SiTransmision .eq. 1 .and. SiViolacion .eq. 1) ) 
    do while ( interrumpe == .TRUE.  .or. ( SiTransmision .eq. 1 .and. SiViolacion .eq. 1) ) ! URIEL 	********
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
		
!       URIEL SI SE CUMPLE EL REQUERIMEINTO DE GAP DE PERDIDAS	********
		if (GAPperdidas.lt.GAPperreq)then
			interrumpe = .FALSE.
		   !se ejecuta el MILP por última vez
		   IterPerdidas = ite
		    call ResuelveMILP ( ite, sistema, 0, 0 ) 
			print *, "GAPperdidas FINAL = " ,GAPperdidas, ",GAP PERDIDAS REQUERIDO = " ,GAPperreq
		endif
		GAPperdidas=0
!       URIEL SI SE CUMPLE EL REQUERIMIENTO DE GAP DE PERDIDAS  ********
		
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
CostoCorte = PrecioTope  ! PrecioTopeEner ! 

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
            do s = 1, NumBloRReg( i )
                PreResReg (r, s, i) = min ( PrecioTopeEner, dualresrez(r,i) )
!                PreResReg (r, s, i) = min ( PrecioTope, dualresrez(r,i) )
            enddo
        enddo
    enddo

! 	Se determina el precio de infactibilidad en reserva rodante por zona
!	PreResRR10 = PrecioTopeEner
    do r = 1, NumGruRes*SiOferComResZona
        do i = 1, NTINTR
            do s = 1, NumBloRR10( i )
                PreResRR10 (r, s, i) = min ( PrecioTopeEner, dualresr10z(r,i) )
!                PreResReg (r, s, i) = min ( PrecioTope, dualresr10z(r,i) )
            enddo
        enddo
    enddo

! 	Se determina el precio de infactibilidad en reserva operativa por zona
!	PreResR10 = PrecioTopeEner
    do r = 1, NumGruRes*SiOferComResZona
        do i = 1, NTINTR
            do s = 1, NumBloRR10( i )
                PreResR10 (r, s, i) = min ( PrecioTopeEner, dualres10z(r,i) )
!               PreResR10 (r, s, i) = min ( PrecioTope, dualres10z(r,i) )
            enddo
        enddo
    enddo

! 	Se determina el precio de infactibilidad en reserva suplementaria por zona
!	PreResSup = PrecioTopeEner
    do r = 1, NumGruRes*SiOferComResZona
        do i = 1, NTINTR
            do s = 1, NumBloRR10( i )
                PreResSup (r, s, i) = min ( PrecioTopeEner, dualressz(r,i) )
!                PreResSup (r, s, i) = min ( PrecioTope, dualressz(r,i) )
            enddo
        enddo
    enddo

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
        do s = 1, NumBloRR10( i )
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResRR10 (r, s, i)
            ColVarC ( m )  = kr10 - 1
!           cota superior
            indices ( m ) = kr10 - 1
            bd ( m ) = xMILP ( kr10 )
            lu ( m ) = 'B'
            if ( xMILP ( kr10 ) .lt. ReqResR10 (r, s, i) .and. segmarg .eq. 0 ) then
               segmarg = s
            endif
            if ( s .eq. NumBloRR10( i ) .and. segmarg .eq. 0 ) segmarg = s  
            if ( s .eq. segmarg ) then
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
        do s = 1, NumBloR10( i )
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResR10 (r, s, i)
            ColVarC ( m )  = k10 - 1
!           cota superior
            indices ( m ) = k10 - 1
            bd ( m ) = xMILP ( k10 )
            lu ( m ) = 'B'
            if ( xMILP ( k10 ) .lt. ReqRes10 (r, s, i) .and. segmarg .eq. 0 ) then
               segmarg = s
            endif
            if ( s .eq. NumBloR10( i ) .and. segmarg .eq. 0 ) segmarg = s
            if ( s .eq. segmarg ) then
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
        do s = 1, NumBloRSu( i )
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResSup (r, s, i)
            ColVarC ( m )  = ks - 1
!           cota superior
            indices ( m ) = ks - 1
            bd ( m ) = xMILP ( ks )
            lu ( m ) = 'B'
            if ( xMILP ( ks ) .lt. ReqResSup (r, s, i) .and. segmarg .eq. 0 ) then
               segmarg = s
            endif
            if ( s .eq.  NumBloRSu( i ) .and. segmarg .eq. 0 ) segmarg = s 
            if ( s .eq. segmarg ) then
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
        do s = 1, NumBloRReg( i )
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResReg (r, s, i)
            ColVarC ( m )  = kr - 1
!           cota superior
            indices ( m ) = kr - 1
            bd ( m ) = xMILP ( kr )
            lu ( m ) = 'B'
            if ( xMILP ( kr )  .lt. ReqResReg (r, s, i) .and. segmarg .eq. 0 ) then
               segmarg = s
            endif
            if ( s .eq. NumBloRReg( i ) .and. segmarg .eq. 0 ) then
                segmarg = s
            endif
            if ( s .eq. segmarg ) then
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
    do s = 1, NumBloRR10( i )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResRR10S (sistema, s, i)
        ColVarC ( m )  = kr10 - 1
!       cota superior
        indices ( m ) = kr10 - 1
        bd ( m ) = xMILP ( kr10 )
        if ( bres .eq. 0 .and. ReqResR10S (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRR10( i ) )  then
                reqsig =  ReqResR10S (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( kr10 ) - ReqResR10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            else if (  ReqResR10S (sistema, s, i) .gt. xMILP ( kr10 ) .and. s .lt. NumBloRR10( i )  ) then
                bd ( m ) = max(xMILP ( kr10 ) - ReqResR10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        kr10 = kr10 + 1
    enddo
    bres = 0
!   para los requerimientos de reserva de 10 minutos
    do s = 1, NumBloR10( i )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResR10S (sistema, s, i)
        ColVarC ( m )  = k10 - 1
!       cota superior
        indices ( m ) = k10 - 1
        bd ( m ) = xMILP ( k10 )
        if ( bres .eq. 0 .and. ReqRes10S (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloR10( i ) )  then
                reqsig =  ReqRes10S (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( k10 ) - ReqRes10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            else if (  ReqRes10S (sistema, s, i) .gt. xMILP ( k10 ) .and. s .lt. NumBloR10( i )  ) then
                bd ( m ) = max(xMILP ( k10 ) - ReqRes10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        k10 = k10 + 1
    enddo
    bres = 0
!   para los requerimientos de reserva suplementaria
    do s = 1, NumBloRSu( i )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResSupS (sistema, s, i)
        ColVarC ( m )  = ks - 1
!       cota superior
        indices ( m ) = ks - 1
        bd ( m ) = xMILP ( ks )
        if ( bres .eq. 0 .and. ReqResSupS (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRSu( i ) )  then
                reqsig =  ReqResSupS (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( ks ) - ReqResSupS (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            else if (  ReqResSupS (sistema, s, i) .gt. xMILP ( ks ) .and. s .lt. NumBloRSu( i )  ) then
                bd ( m ) = max(xMILP ( ks ) - ReqResSupS (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        ks = ks + 1
    enddo
    bres = 0
!   para los requerimientos de reserva de regulacion secundaria
    do s = 1, NumBloRReg( i )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResRegS (sistema, s, i)
        ColVarC ( m )  = kr - 1
!       cota superior
        indices ( m ) = kr - 1
        bd ( m ) = xMILP ( kr )
        if ( bres .eq. 0 .and. ReqResRegS (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRReg( i ) )  then
                reqsig =  ReqResRegS (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( kr ) - 1.0e-5, 0.0 )
                bres = 1
            else if (  ReqResRegS (sistema, s, i) .gt. xMILP ( kr ) .and. s .lt. NumBloRReg( i )  ) then
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
            do s = 1, NumBloRReg( i )
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
        do s = 1, NumBloRR10( i )
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResRR10 (r, s, i)
            ColVarC ( m )  = kr10 - 1
!           cota superior
            indices ( m ) = kr10 - 1
            bd ( m ) = xMILP ( kr10 )
            if ( s .lt. NumBloRR10( i ) ) then
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
        do s = 1, NumBloR10( i )
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResR10 (r, s, i)
            ColVarC ( m )  = k10 - 1
!           cota superior
            indices ( m ) = k10 - 1
            bd ( m ) = xMILP ( k10 )
            if ( s .lt. NumBloR10( i ) )  then
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
        do s = 1, NumBloRSu( i )
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResSup (r, s, i)
            ColVarC ( m )  = ks - 1
!           cota superior
            indices ( m ) = ks - 1
            bd ( m ) = xMILP ( ks )
            if ( s .lt. NumBloRSu( i ) )  then
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
        do s = 1, NumBloRReg( i )
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResReg (r, s, i)
            ColVarC ( m )  = kr - 1
!           cota superior
            indices ( m ) = kr - 1
            if ( s .lt. NumBloRReg( i ) )  then
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
    do s = 1, NumBloRR10( i )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResRR10S (sistema, s, i)
        ColVarC ( m )  = kr10 - 1
!       cota superior
        indices ( m ) = kr10 - 1
        bd ( m ) = xMILP ( kr10 )
        if ( bres .eq. 0 .and. ReqResR10S (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRR10( i ) )  then
                reqsig =  ReqResR10S (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( kr10 ) - ReqResR10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            else if (  ReqResR10S (sistema, s, i) .gt. xMILP ( kr10 ) .and. s .lt. NumBloRR10( i )  ) then
                bd ( m ) = max(xMILP ( kr10 ) - ReqResR10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        kr10 = kr10 + 1
    enddo
    bres = 0
!   para los requerimientos de reserva de 10 minutos
    do s = 1, NumBloR10( i )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResR10S (sistema, s, i)
        ColVarC ( m )  = k10 - 1
!       cota superior
        indices ( m ) = k10 - 1
        bd ( m ) = xMILP ( k10 )
        if ( bres .eq. 0 .and. ReqRes10S (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloR10( i ) )  then
                reqsig =  ReqRes10S (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( k10 ) - ReqRes10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            else if (  ReqRes10S (sistema, s, i) .gt. xMILP ( k10 ) .and. s .lt. NumBloR10( i )  ) then
                bd ( m ) = max(xMILP ( k10 ) - ReqRes10S (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        k10 = k10 + 1
    enddo
    bres = 0
!   para los requerimientos de reserva suplementaria
    do s = 1, NumBloRSu( i )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResSupS (sistema, s, i)
        ColVarC ( m )  = ks - 1
!       cota superior
        indices ( m ) = ks - 1
        bd ( m ) = xMILP ( ks )
        if ( bres .eq. 0 .and. ReqResSupS (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRSu( i ) )  then
                reqsig =  ReqResSupS (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( ks ) - ReqResSupS (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            else if (  ReqResSupS (sistema, s, i) .gt. xMILP ( ks ) .and. s .lt. NumBloRSu( i )  ) then
                bd ( m ) = max(xMILP ( ks ) - ReqResSupS (sistema, s, i)*0.000005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        ks = ks + 1
    enddo
    bres = 0
!   para los requerimientos de reserva de regulacion secundaria
    do s = 1, NumBloRReg( i )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResRegS (sistema, s, i)
        ColVarC ( m )  = kr - 1
!       cota superior
        indices ( m ) = kr - 1
        bd ( m ) = xMILP ( kr )
        if ( bres .eq. 0 .and. ReqResRegS (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRReg( i ) )  then
                reqsig =  ReqResRegS (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( kr ) - 1.0e-5, 0.0 )
                bres = 1
            else if (  ReqResRegS (sistema, s, i) .gt. xMILP ( kr ) .and. s .lt. NumBloRReg( i )  ) then
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
        do s = 1, NumBloRR10( i )
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResRR10 (r, s, i)
            ColVarC ( m )  = kr10 - 1
!           cota superior
            indices ( m ) = kr10 - 1
            bd ( m ) = xMILP ( kr10 )
            if ( bres .eq. 0 .and. ReqResR10 (r, s, i) .gt. 0.0 ) then
                 reqsig = 0.0
                 if ( s .lt. NumBloRR10( i ) )  then
                     reqsig =  ReqResR10 (r, s+1, i)
                 endif
                 if ( reqsig .eq. 0 ) then
                     bd ( m ) = max(xMILP ( kr10 ) - ReqResR10 (r, s, i)*0.005, 0.0)
                     bres = 1
                 else if (  ReqResR10 (r, s, i) .gt. xMILP ( kr10 ) .and. s .lt. NumBloRR10( i )  ) then
                     bd ( m ) = max(xMILP ( kr10 ) - ReqResR10 (r, s, i)*0.005, 0.0)
                     bres = 1
                 endif
            endif
            lu ( m ) = 'L'
            kr10 = kr10 + 1
        enddo
        bres = 0
!       para los requerimientos de reserva de 10 minutos
        do s = 1, NumBloR10( i )
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResR10 (r, s, i)
            ColVarC ( m )  = k10 - 1
!           cota superior
            indices ( m ) = k10 - 1
            bd ( m ) = xMILP ( k10 )
            if ( bres .eq. 0 .and. ReqRes10 (r, s, i) .gt. 0.0 ) then
                 reqsig = 0.0
                 if ( s .lt. NumBloR10( i ) )  then
                     reqsig =  ReqRes10 (r, s+1, i)
                 endif
                 if ( reqsig .eq. 0 ) then
                     bd ( m ) = max(xMILP ( k10 )- ReqRes10 (r, s, i)*0.005, 0.0)
                     bres = 1
                 else if (  ReqRes10 (r, s, i) .gt. xMILP ( k10 ) .and. s .lt. NumBloR10( i )  ) then
                     bd ( m ) = max(xMILP ( k10 ) - ReqRes10 (r, s, i)*0.005, 0.0)
                     bres = 1
                 endif
            endif
            lu ( m ) = 'L'
            k10 = k10 + 1
        enddo
        bres = 0
!       para los requerimientos de reserva suplementaria
        do s = 1, NumBloRSu( i )
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResSup (r, s, i)
            ColVarC ( m )  = ks - 1
!           cota superior
            indices ( m ) = ks - 1
            bd ( m ) = xMILP ( ks )
            if ( bres .eq. 0 .and. ReqResSup (r, s, i) .gt. 0.0 ) then
                 reqsig = 0.0
                 if ( s .lt. NumBloRSu( i ) )  then
                     reqsig =  ReqResSup (r, s+1, i)
                 endif
                 if ( reqsig .eq. 0 ) then
                     bd ( m ) = max(xMILP ( ks ) - ReqResSup (r, s, i)*0.005, 0.0)
                     bres = 1
                 else if (  ReqResSup (r, s, i) .gt. xMILP ( ks ) .and. s .lt. NumBloRSu( i )  ) then
                     bd ( m ) = max(xMILP ( ks ) - ReqResSup (r, s, i)*0.005, 0.0)
                     bres = 1
                 endif
            endif
            lu ( m ) = 'L'
            ks = ks + 1
        enddo
!       para los requerimientos de reserva de regulacion secundaria
        bres = 0
        do s = 1, NumBloRReg( i )
            m = m + 1
!           coeficientes de penalizacion
            NewValC ( m )  = -PreResReg (r, s, i)
            ColVarC ( m )  = kr - 1
!           cota superior
            indices ( m ) = kr - 1
            bd ( m ) = xMILP ( kr )
            if ( bres .eq. 0 .and. ReqResReg (r, s, i) .gt. 0.0 ) then
                 reqsig = 0.0
                 if ( s .lt. NumBloRReg( i ) )  then
                     reqsig =  ReqResReg (r, s+1, i)
                 endif
                 if ( reqsig .eq. 0 ) then
                     bd ( m ) = max(xMILP ( kr ) - 1.0e-5, 0.0)
                     bres = 1
                 else if (  ReqResReg (r, s, i) .gt. xMILP ( kr ) .and. s .lt. NumBloRReg( i )  ) then
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
    do s = 1, NumBloRR10( i )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResRR10S (sistema, s, i)
        ColVarC ( m )  = kr10 - 1
!       cota superior
        indices ( m ) = kr10 - 1
        bd ( m ) = xMILP ( kr10 )
        if ( bres .eq. 0 .and. ReqResR10S (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRR10( i ) )  then
                reqsig =  ReqResR10S (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( kr10 ) - ReqResR10S (sistema, s, i)*0.005, 0.0 )
                bres = 1
            else if (  ReqResR10S (sistema, s, i) .gt. xMILP ( kr10 ) .and. s .lt. NumBloRR10( i )  ) then
                bd ( m ) = max(xMILP ( kr10 ) - ReqResR10S (sistema, s, i)*0.005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        kr10 = kr10 + 1
    enddo
    bres = 0
!   para los requerimientos de reserva de 10 minutos
    do s = 1, NumBloR10( i )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResR10S (sistema, s, i)
        ColVarC ( m )  = k10 - 1
!       cota superior
        indices ( m ) = k10 - 1
        bd ( m ) = xMILP ( k10 )
        if ( bres .eq. 0 .and. ReqRes10S (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloR10( i ) )  then
                reqsig =  ReqRes10S (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( k10 ) - ReqRes10S (sistema, s, i)*0.005, 0.0 )
                bres = 1
            else if (  ReqRes10S (sistema, s, i) .gt. xMILP ( k10 ) .and. s .lt. NumBloR10( i )  ) then
                bd ( m ) = max(xMILP ( k10 ) - ReqRes10S (sistema, s, i)*0.005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        k10 = k10 + 1
    enddo
    bres = 0
!   para los requerimientos de reserva suplementaria
    do s = 1, NumBloRSu( i )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResSupS (sistema, s, i)
        ColVarC ( m )  = ks - 1
!       cota superior
        indices ( m ) = ks - 1
        bd ( m ) = xMILP ( ks )
        if ( bres .eq. 0 .and. ReqResSupS (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRSu( i ) )  then
                reqsig =  ReqResSupS (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( ks ) - ReqResSupS (sistema, s, i)*0.005, 0.0 )
                bres = 1
            else if (  ReqResSupS (sistema, s, i) .gt. xMILP ( ks ) .and. s .lt. NumBloRSu( i )  ) then
                bd ( m ) = max(xMILP ( ks ) - ReqResSupS (sistema, s, i)*0.005, 0.0 )
                bres = 1
            endif
        endif
        lu ( m ) = 'L'
        ks = ks + 1
    enddo
    bres = 0
!   para los requerimientos de reserva de regulacion secundaria
    do s = 1, NumBloRReg( i )
        m = m + 1
!       coeficientes de penalizacion
        NewValC ( m )  = -PreResRegS (sistema, s, i)
        ColVarC ( m )  = kr - 1
!       cota superior
        indices ( m ) = kr - 1
        bd ( m ) = xMILP ( kr )
        if ( bres .eq. 0 .and. ReqResRegS (sistema, s, i) .gt. 0.0 ) then
            reqsig = 0.0
            if ( s .lt. NumBloRReg( i ) )  then
                reqsig =  ReqResRegS (sistema, s+1, i)
            endif
            if ( reqsig .eq. 0 ) then
                bd ( m ) = max(xMILP ( kr ) - 1.0e-5, 0.0 )
                bres = 1
            else if (  ReqResRegS (sistema, s, i) .gt. xMILP ( kr ) .and. s .lt. NumBloRReg( i )  ) then
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
! Marzo de 2020                                                       *
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

!umbral = PrecioTope ! *base ! Cuando se corre el segundo problema no se multiplica por la base
umbral = PrecioTopeEner ! *base ! Cuando se corre el segundo problema no se multiplica por la base

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
            do s = 1, NumBloRSu( i )
!               Identifica el segmento marginal
                if ( smarg .eq. 0 ) then
                    if ( ( ReqResSup (r, s, i) - xMILP ( ks ) ) .gt. 0.0 .or. s .eq.  NumBloRSu( i ) ) then
                        preciodeseado = PreResSup (r, s, i)
                        smarg = s
                    endif
                endif
                ks = ks + 1
            enddo
!           Si es el MDA o AUHE
            if ( TipoEjecu .eq. 0 .or. TipoEjecu .eq. 3 ) then
!            if ( TipoEjecu .eq. 0 ) then
                preciodeseado = PrecioTopeEner ! PrecioTope ! 
            endif
            facesc =  preciodeseado/dualresszesc ( r, i )
            dualresszesc ( r, i ) = dualresszesc ( r, i ) * facesc
        else
            do s = 1, NumBloRSu( i )
                ks = ks + 1
            enddo
        endif        
!       para los requerimientos de reserva de 10 minutos
        if ( dualres10zesc ( r, i ) .gt. umbral ) then
            smarg = 0
            do s = 1, NumBloR10( i )
!               Identifica el segmento marginal
                if ( smarg .eq. 0 ) then
                    if ( ( ReqRes10 (r, s, i) - xMILP ( k10 ) ) .gt. 0.0 .or. s .eq.  NumBloR10( i ) ) then
                        preciodeseado = PreResR10 (r, s, i)
                        smarg = s
                    endif
                endif
                k10 = k10 + 1
            enddo
!           Si es el MDA o AUHE
            if ( TipoEjecu .eq. 0 .or. TipoEjecu .eq. 3 ) then
!            if ( TipoEjecu .eq. 0 ) then
                preciodeseado = PrecioTopeEner  ! PrecioTope ! 
            endif
            if ( preciodeseado .lt. ( preciodeseado - dualresszesc ( r, i ) ) ) then
              preciodeseado = preciodeseado + dualresszesc ( r, i )
            endif
            preciodeseado = preciodeseado - dualresszesc ( r, i )
            facesc =  preciodeseado/dualres10zesc ( r, i )
            dualres10zesc ( r, i ) =   dualres10zesc ( r, i ) * facesc
        else
            do s = 1, NumBloR10( i )
                k10 = k10 + 1
            enddo
        endif
!       para los requerimientos de reserva rodante de 10 minutos
        if ( dualresr10zesc ( r, i ) .gt. umbral ) then
            smarg = 0
            do s = 1, NumBloRR10( i )
!               Identifica el segmento marginal
                if ( smarg .eq. 0 ) then
                    if ( ( ReqResR10 (r, s, i) - xMILP ( kr10 ) ) .gt. 0.0 .or. s .eq.  NumBloRR10( i ) ) then
                        preciodeseado = PreResRR10 (r, s, i)
                        smarg = s
                    endif
                endif
               kr10 = kr10 + 1
            enddo
!           Si es el MDA o AUHE
            if ( TipoEjecu .eq. 0 .or. TipoEjecu .eq. 3 ) then
!            if ( TipoEjecu .eq. 0 ) then
                preciodeseado = PrecioTopeEner ! PrecioTope ! 
            endif
            if ( preciodeseado .lt. ( dualres10zesc( r, i ) + dualresszesc( r, i ) ) ) then
                preciodeseado = preciodeseado + dualres10zesc( r, i ) + dualresszesc( r, i )
            endif
            preciodeseado =  preciodeseado - dualres10zesc( r, i ) - dualresszesc( r, i )
            facesc =  preciodeseado/dualresr10zesc ( r, i )
            dualresr10zesc ( r, i ) = dualresr10zesc ( r, i ) * facesc
        else
            do s = 1, NumBloRR10( i )
               kr10 = kr10 + 1
            enddo
        endif
        
        ! Verifica si hay marginales de reserva de regulación altos
        if ( dualresrezesc ( r, i ) .gt. umbral ) then
!           para los requerimientos de reserva de regulacion
            smarg = 0
            do s = 1, NumBloRReg( i )
!               Identifica el segmento marginal
                if ( smarg .eq. 0 ) then
                    if ( ( ReqResReg (r, s, i) - xMILP ( kr ) ) .gt. 0.0 .or. s .eq.  NumBloRReg( i ) ) then
                        preciodeseado = PreResReg (r, s, i)
                        smarg = s
                    endif
                endif
                kr = kr + 1
            enddo
!           Si es el MDA o AUHE
            if ( TipoEjecu .eq. 0 .or. TipoEjecu .eq. 3 ) then
!            if ( TipoEjecu .eq. 0 ) then
                preciodeseado = PrecioTopeEner ! PrecioTope ! 
            endif
            if ( preciodeseado .lt. ( dualresr10zesc ( r, i ) + dualres10zesc( r, i ) + dualresszesc( r, i )  )  .and. SiRegEnRod .eq. 1 ) then
                preciodeseado = preciodeseado + dualresr10zesc ( r, i ) + dualres10zesc( r, i ) + dualresszesc( r, i )  
            endif
            preciodeseado =  preciodeseado - (dualresr10zesc ( r, i ) + dualres10zesc( r, i ) + dualresszesc( r, i ))*SiRegEnRod
            facesc =  preciodeseado/dualresrezesc( r, i )
            dualresrezesc ( r, i ) = dualresrezesc( r, i ) * facesc
        else
            do s = 1, NumBloRReg( i )
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


subroutine CreaArchRes
! ---------------------------------------------------------------------
! Crear los archivos csv de resultados del algoritmo.                 *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero de 2020                                                       *
! ---------------------------------------------------------------------
use ParAUHE

Implicit none

INTEGER ierror

character*1 ssistema

ierror = 0

 OPEN ( UNIT = 111, FILE = trim(rut_dat_1)//'CORTENODAL_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000 )
 OPEN ( UNIT = 2, FILE = trim(rut_dat_1)//'EXCEDNODAL_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000 )
 OPEN ( UNIT = 3, FILE = trim(rut_dat_1)//'RESCSTOPARR_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 4, FILE = trim(rut_dat_1)//'BALANCES_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 350 )
 OPEN ( UNIT = 5, FILE = trim(rut_dat_1)//'RESCARGAS_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 6, FILE = trim(rut_dat_1)//'RESRERO10Z_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 7, FILE = trim(rut_dat_1)//'RESRE10Z_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 8, FILE = trim(rut_dat_1)//'RESRESUZ_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 9, FILE = trim(rut_dat_1)//'RESRERESEZ_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 10, FILE = trim(rut_dat_1)//'RESRESSIS_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 11, FILE = trim(rut_dat_1)//'GRUPOSRAMASRES_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 12, FILE = trim(rut_dat_1)//'RAMASENLRES_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 13, FILE = trim(rut_dat_1)//'GPOUTERRES_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 300 )
 OPEN ( UNIT = 14, FILE = trim(rut_dat_1)//'RESREGIONES_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )
 OPEN ( UNIT = 15, FILE = trim(rut_dat_1)//'RESGEN_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 16, FILE = trim(rut_dat_1)//'RESMODO_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 17, FILE = trim(rut_dat_1)//'UNIMARG_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 18, FILE = trim(rut_dat_1)//'RESAREAS_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 400 )
 OPEN ( UNIT = 19, FILE = trim(rut_dat_1)//'RESRERO10U_1.csv', IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 20, FILE = trim(rut_dat_1)//'RESRENRO10U_1.csv', IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 21, FILE = trim(rut_dat_1)//'RESREROSUU_1.csv', IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 22, FILE = trim(rut_dat_1)//'RESRENROSUU_1.csv', IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 23, FILE = trim(rut_dat_1)//'RESRERESEU_1.csv', IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 24, FILE = trim(rut_dat_1)//'GENEMBRES_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 300 )
 OPEN ( UNIT = 25, FILE = trim(rut_dat_1)//'GENEMBHORRES_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )
 OPEN ( UNIT = 26, FILE = trim(rut_dat_1)//'ASIGN_MDA_RC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 27, FILE = trim(rut_dat_1)//'ASIGN_MDA_RD.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 28, FILE = trim(rut_dat_1)//'ASIGN_AUGC_RC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 29, FILE = trim(rut_dat_1)//'DISPO_AUGC_RC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 30, FILE = trim(rut_dat_1)//'LSUNIT_AUGC_RC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 31, FILE = trim(rut_dat_1)//'LIUNIT_AUGC_RC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 32, FILE = trim(rut_dat_1)//'ResAUGC_RC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 33, FILE = trim(rut_dat_1)//'ASIGN_AUGC_RD.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 34, FILE = trim(rut_dat_1)//'DISPO_AUGC_RD.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 35, FILE = trim(rut_dat_1)//'LSUNIT_AUGC_RD.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 36, FILE = trim(rut_dat_1)//'LIUNIT_AUGC_RD.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 37, FILE = trim(rut_dat_1)//'ResAUGC_RD.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 38, FILE = trim(rut_dat_1)//'GPOUTER_AUGC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 39, FILE = trim(rut_dat_1)//'ASIGN_AUGC_H.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 40, FILE = trim(rut_dat_1)//'DISPO_AUGC_H.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 41, FILE = trim(rut_dat_1)//'ResAUGC_H.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 42, FILE = trim(rut_dat_1)//'LIMENEREMB_AUGC.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 43, FILE = trim(rut_dat_1)//'ASIGN_AUGC_RE.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 44, FILE = trim(rut_dat_1)//'DISPO_AUGC_RE.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 45, FILE = trim(rut_dat_1)//'ResAUGC_RE.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 46, FILE = trim(rut_dat_1)//'RESTRANS_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
 OPEN ( UNIT = 47, FILE = trim(rut_dat_1)//'VOLUMH_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000 )
 OPEN ( UNIT = 48, FILE = trim(rut_dat_1)//'CARGAH_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000 )
 OPEN ( UNIT = 49, FILE = trim(rut_dat_1)//'RDVAAU_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 400 )
 OPEN ( UNIT = 50, FILE = trim(rut_dat_1)//'VASOEN2_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 3000 )
 OPEN ( UNIT = 51, FILE = trim(rut_dat_1)//'REVAAU_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 400 )
 OPEN ( UNIT = 52, FILE = trim(rut_dat_1)//'VIAHO_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000 )
 OPEN ( UNIT = 53, FILE = trim(rut_dat_1)//'RESGASD_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 1350 )
 OPEN ( UNIT = 54, FILE = trim(rut_dat_1)//'CONSUMOXU_1.csv',IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 6500 )
 OPEN ( UNIT = 55, FILE = trim(rut_dat_1)//'ANGVOLNODAL_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000 )
 OPEN ( UNIT = 56, FILE = trim(rut_dat_1)//'SNSPERDNODALES_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000 )
 OPEN ( UNIT = 57, FILE = trim(rut_dat_1)//'FLUPOTACTRAMA_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000 )
 OPEN ( UNIT = 58, FILE = trim(rut_dat_1)//'PERPOTACTRAMA_1.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000 )
 OPEN ( UNIT = 59, FILE = trim(rut_dat_1)//'SNSFLUGRUPOSRAMAS.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000 )
 OPEN ( UNIT = 60, FILE = trim(rut_dat_1)//'PMR_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )
 OPEN ( UNIT = 61, FILE = trim(rut_dat_1)//'PMRGEN_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )
 OPEN ( UNIT = 62, FILE = trim(rut_dat_1)//'PMRPER_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )
 OPEN ( UNIT = 63, FILE = trim(rut_dat_1)//'PMRCON_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )
 OPEN ( UNIT = 64, FILE = trim(rut_dat_1)//'PRECIOS_ZONALES_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )
 OPEN ( UNIT = 65, FILE = trim(rut_dat_1)//'PRECIOS_ZONALES_ORI_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )
 OPEN ( UNIT = 66, FILE = trim(rut_dat_1)//'PRECIOS_NODALES_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )
 OPEN ( UNIT = 67, FILE = trim(rut_dat_1)//'PRECIOS_NODALES_ORI_1.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )

! Limpia archivos

 write ( 111 , * ) ''
 rewind ( 111 )
 write ( 1 , * ) ''
 rewind ( 1 )
 write ( 2 , * ) ''
 rewind ( 2 )
 write ( 3 , * ) ''
 rewind ( 3 )
 write ( 4 , * ) ''
 rewind ( 4 )
 write ( 5 , * ) ''
 rewind ( 5 )
 write ( 6 , * ) ''
 rewind ( 6 )
 write ( 7 , * ) ''
 rewind ( 7 )
 write ( 8 , * ) ''
 rewind ( 8 )
 write ( 9 , * ) ''
 rewind ( 9 )
 write ( 10 , * ) ''
 rewind ( 10 )
 write ( 11 , * ) ''
 rewind ( 11 )
 write ( 12 , * ) ''
 rewind ( 12 )
 write ( 13 , * ) ''
 rewind ( 13 )
 write ( 14 , * ) ''
 rewind ( 14 )
 write ( 15 , * ) ''
 rewind ( 15 )
 write ( 16 , * ) ''
 rewind ( 16 )
 write ( 17 , * ) ''
 rewind ( 17 )
 write ( 18 , * ) ''
 rewind ( 18 )
 write ( 19 , * ) ''
 rewind ( 19 )
 write ( 20 , * ) ''
 rewind ( 20 )
 write ( 21 , * ) ''
 rewind ( 21 )
 write ( 22 , * ) ''
 rewind ( 22 )
 write ( 23 , * ) ''
 rewind ( 23 )
 write ( 24 , * ) ''
 rewind ( 24 )
 write ( 25 , * ) ''
 rewind ( 25 )
 write ( 26 , * ) ''
 rewind ( 26 )
 write ( 27 , * ) ''
 rewind ( 27 )
 write ( 28 , * ) ''
 rewind ( 28 )
 write ( 29 , * ) ''
 rewind ( 29 )
 write ( 30 , * ) ''
 rewind ( 30 )
 write ( 31 , * ) ''
 rewind ( 31 )
 write ( 32 , * ) ''
 rewind ( 32 )
 write ( 33 , * ) ''
 rewind ( 33 )
 write ( 34 , * ) ''
 rewind ( 34 )
 write ( 35 , * ) ''
 rewind ( 35 )
 write ( 36 , * ) ''
 rewind ( 36 )
 write ( 37 , * ) ''
 rewind ( 37 )
 write ( 38 , * ) ''
 rewind ( 38 )
 write ( 39 , * ) ''
 rewind ( 39 )
 write ( 40 , * ) ''
 rewind ( 40 )
 write ( 41 , * ) ''
 rewind ( 41 )
 write ( 42 , * ) ''
 rewind ( 42 )
 write ( 43 , * ) ''
 rewind ( 43 )
 write ( 44 , * ) ''
 rewind ( 44 )
 write ( 45 , * ) ''
 rewind ( 45 )
 write ( 46 , * ) ''
 rewind ( 46 )
 write ( 47 , * ) ''
 rewind ( 47 )
 write ( 48 , * ) ''
 rewind ( 48 )
 write ( 49 , * ) ''
 rewind ( 49 )
 write ( 50 , * ) ''
 rewind ( 50 )
 write ( 51 , * ) ''
 rewind ( 51 )
 write ( 52 , * ) ''
 rewind ( 52 )
 write ( 53 , * ) ''
 rewind ( 53 )
 write ( 54 , * ) ''
 rewind ( 54 )
 write ( 55 , * ) ''
 rewind ( 55 )
 write ( 56 , * ) ''
 rewind ( 56 )
 write ( 57 , * ) ''
 rewind ( 57 )
 write ( 58 , * ) ''
 rewind ( 58 )
 write ( 59 , * ) ''
 rewind ( 59 )
 write ( 60 , * ) ''
 rewind ( 60 )
 write ( 61 , * ) ''
 rewind ( 61 )
 write ( 62 , * ) ''
 rewind ( 62 )
 write ( 63 , * ) ''
 rewind ( 63 )
 write ( 64 , * ) ''
 rewind ( 64 )
 write ( 65 , * ) ''
 rewind ( 65 )
 write ( 66 , * ) ''
 rewind ( 66 )
 write ( 67 , * ) ''
 rewind ( 67 )


 CLOSE ( 111 )
 CLOSE ( 2 )
 CLOSE ( 3 )
 CLOSE ( 4 )
 CLOSE ( 5 )
 CLOSE ( 6 )
 CLOSE ( 7 )
 CLOSE ( 8 )
 CLOSE ( 9 )
 CLOSE ( 10 )
 CLOSE ( 11 )
 CLOSE ( 12 )
 CLOSE ( 13 )
 CLOSE ( 14 )
 CLOSE ( 15 )
 CLOSE ( 16 )
 CLOSE ( 17 )
 CLOSE ( 18 )
 CLOSE ( 19 )
 CLOSE ( 20 )
 CLOSE ( 21 )
 CLOSE ( 22 )
 CLOSE ( 23 )
 CLOSE ( 24 )
 CLOSE ( 25 )
 CLOSE ( 26 )
 CLOSE ( 27 )
 CLOSE ( 28 )
 CLOSE ( 29 )
 CLOSE ( 30 )
 CLOSE ( 31 )
 CLOSE ( 32 )
 CLOSE ( 33 )
 CLOSE ( 34 )
 CLOSE ( 35 )
 CLOSE ( 36 )
 CLOSE ( 37 )
 CLOSE ( 38 )
 CLOSE ( 39 )
 CLOSE ( 40 )
 CLOSE ( 41 )
 CLOSE ( 42 )
 CLOSE ( 43 )
 CLOSE ( 44 )
 CLOSE ( 45 )
 CLOSE ( 46 )
 CLOSE ( 47 )
 CLOSE ( 48 )
 CLOSE ( 49 )
 CLOSE ( 50 )
 CLOSE ( 51 )
 CLOSE ( 52 )
 CLOSE ( 53 )
 CLOSE ( 54 )
 CLOSE ( 55 )
 CLOSE ( 56 )
 CLOSE ( 57 )
 CLOSE ( 58 )
 CLOSE ( 59 )
 CLOSE ( 60 )
 CLOSE ( 61 )
 CLOSE ( 62 )
 CLOSE ( 63 )
 CLOSE ( 64 )
 CLOSE ( 65 )
 CLOSE ( 66 )
 CLOSE ( 67 )


return
end

