!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Realiza las lecturas de los datos necesarios para el AUHE       *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Noviembre 2014                        *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                   Junio 2019                            *
!**************************************************************************
SUBROUTINE Lecturas_AUHE

use ParAUHE
use ProblemaAUHE

integer i, ierror, ibanbit, errleca
character*3000 letaux
character*5 letr

ierror = 0
letaux = 'letaux'
ibanbit = 1

!Se lee archivo de parametros
call LeeParAUHE

!Se leen nodos del sistema electrico
call lee_nodos

!Se leen datos de generadores con rago continuo de operacion
call LecUnidadesRC

!Se leen datos de cargas
call data_cargas

!Se leen datos de unidades no programables
call data_noprog

!Se leen potencias activas de nodos de intercambios
call lee_nodo_intercambio

!Se abre archivo de bitacora de lecturas
call Abre_Archivos_Generales_1

! Se leen dimensiones hidro
call Lectura_Dimension_Hidro_1

! Lee informacion de la cuenca y crea apuntadores (relacion con embalses, nombre de la cuenca)
!call Lectura_Cuenca_1
call Lectura_Cuenca

! Lee informacion de los embalses (nombre del embalse, no. vias convergentes, divergentes, curva 9 puntos altura vs volumen),
! namino
!call Lectura_Vasos_1
call Lectura_Vasos

!   Lee informacion de plantas hidro (no. de unidades, via de desfogue, etc)
!call Lectura_Plah_1
call Lectura_Plah

!Se leen datos de unidades y sistema hidro
call data_hidro

! si se desea considerar el modelado hiraulico
if ( SiModHid .eq. 1 ) then

!   Forma relacion unidades pertenecientes al embalse y no. total de embalses del sistema
    call unidad_embalse 

!   Se leen datos del sistema hidrologico
    call Lecturas_Hidro_1
    
endif

!Escalamiento; de GWh a MWh y luego a PU
LimIEnerEmb = LimIEnerEmb * ( 1000.0 / base )
LimSEnerEmb = LimSEnerEmb * ( 1000.0 / base )

PotMinUniH = PotMinUniH / Base
PotMaxUniH = PotMaxUniH / Base
PotMinRUniH = PotMinRUniH / Base
PotMaxRUniH = PotMaxRUniH / Base

!Se leen datos de generadores con rago discontinuo de operacion
call data_rd

!Se leen datos de generadores renovables intermitentes
call data_reno

!Se leen datos de zonas de reserva y requerimientos de servicios conexos de CENACE
call conexos_cenace

! Si hay restricciones de limitacion de energia
errleca = 0
if ( SiEnerTer .eq. 1 ) then
goto 5555
    ! -----------------------------
    ! * Se leen datos de GPOUTER *
    ! -----------------------------
    OPEN (UNIT = 116, FILE = rut_dat_1( 1 : long_ruta )//'GPOUTER.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
    
    if ( ierror .eq. 0 ) then
        i = 0
        ! Lee información hasta encontrar fin de información
        do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
	        read ( 116, 100, iostat = ierror ) letaux
	        if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
                i = i + 1
                read ( letaux, *, iostat = errleca )  ggrupo, NGpoResEner ( i ), NomGpoResEner ( i ), HIResEner ( i ), HFResEner ( i ), LInfResEner ( i ), LSupResEner ( i ), ActResEner ( i )
                if ( errleca .ne. 0 ) go to 1001
	        endif
        enddo
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO GPOUTER.csv'
       call ParaProceso ( bmensaje )
    end if

1001 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 ) then
        write ( letr, "(i5)" ) i
        call Elimina_blancos ( letr, 5)
        bmensaje = 'ERROR DE LECTURA ARCHIVOS GPOUTER.csv'
        call EnviaMensajeError ( bmensaje )
        Bmensaje = 'GRUPO: ['//trim(letr)//'] '//trim(NomGpoResEner ( i ))
       call ParaProceso ( bmensaje )
    endif    
    NresEner = i
    
    CLOSE ( UNIT = 116 )

    write ( 1, 100 ) '------------------------------------------------'
    write ( 1, 100 ) 'LISTA DE RESTRICCIONES CON LIMITACION DE ENERGIA'
    write ( 1, 100 ) '------------------------------------------------'
    !Para todas las restricciones
    write ( 1, 100 ) 'Restriccion  Grupo    NomGrupo   HoraIni   HoraFin     LInfEner      LSupEner   Activo'
    do i = 1, NresEner
        write ( 1, 200 ) i, NGpoResEner ( i ), NomGpoResEner ( i ), HIResEner ( i ), HFResEner ( i ), LInfResEner ( i ), LSupResEner ( i ), ActResEner ( i )
    end do

5555 continue
     
!   Se leen datos de grupos de unidades termicas con limitaciones de energia y requerimientos del CENACE
    call grupos_cenace
!    call grupos_cenace_new

!   Escalamiento
    LInfResEner = ( LInfResEner * 1000.00 ) / Base
    LSupResEner = ( LSupResEner * 1000.00 ) / Base  

endif

if ( SiGpoGas .eq. 1 ) then
!   Se leen datos de grupos de unidades termicas con limitaciones de consumo de combustible
!    call LeeGpoGas_new
   call LeeGpoGas
endif

! Valida generación y carga en islas muertas
call ValidaGeneradoresEnIslasMuertas

! si el escenario no es de un EXPOST
!if ( TipoEjecu .ne. 2 ) then
!   Se calcula el limite inferior de las ofertas de servicios conexos de TODAS las unidades
    call calc_conexos
!endif

! se determinan los bloques de requerimientos
call BloReqRes

!Se llama la subrutina de validación de rampas para unidades de rango continuo
call Valid_Rampa_RC

!Se llama la subrutina de validación de rampas para unidades de rango discontinuo
call Valid_Rampa_RD

!call ValidaNumeroMinimoUniRegulacion

!Se lee información de grupos de generadores que no se pueden arrancar simultaneamente
call LeeGruposArranque

!Se lee informacion de unidades de propiedad conjunta
call LeeUPC

! Se leen grupos de unidades de importacion
!call LeeGposImportacion

! Se leen grupos de cragas de exportacion
!call LeeGposExportacion

100 format ( a )
200 format ( 2(i3,8x), a12, 2(I3, 7x), 2(f9.2, 5x), i3 )
END SUBROUTINE Lecturas_AUHE


    
    
    
    
!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Apertura de archivos de debugger y bitacora y obtencion de      *
!         ruta de lectura desde un archivo *.dat                          *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Noviembre 2014                        *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
    
SUBROUTINE Abre_Bit_Lec

use ParAUHE, only: rut_res, rut_ent, rut_dat_1, long_ruta, NomEjecu
!
IMPLICIT NONE
      
integer ierror, i
	 
character * 40 nomruta 
!
		 
OPEN (UNIT = 2, DEFAULTFILE=rut_ent, FILE='ctrl_s_csv.dat', STATUS='OLD', IOSTAT=ierror)
!--------------------------------------------
!* Lee los datos del archivo ctrl_s_csv.dat *
!--------------------------------------------  	  
IF( ierror .NE. 0 ) THEN
    print  *, ' No se pudo abrir el archivo de path para lecturas'         
ELSE
    READ( 2,'(A)') nomruta         
    i = 1
    DO WHILE( (i .LE. 40) .AND. ( nomruta ( i : i ) .NE. ' ') )
        i = i + 1
    END DO
    long_ruta = i - 1
END IF
         
!--------------------------------------------------
! * Especifica la ruta de los archivos de entrada *
!--------------------------------------------------
rut_dat_1 = nomruta ( 1 : long_ruta )   ! Ruta de los datos de entrada 
        
CLOSE ( unit = 2 )


END SUBROUTINE Abre_Bit_Lec

!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Se leen datos de Subsistemas                                    *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!                                Noviembre 2014                           *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************

Subroutine LeeSubsistemas

Use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, indsis, nomsis, EstadoIsla, SlackIsla, &
                  numsis_act, numsis, maxsis

Implicit none

integer ierror, i, ierr, errleca

integer ibanbit

character*250 letaux

character*5 letr

! Abre archivos de datos de subistemas

OPEN ( UNIT = 4, FILE = rut_dat_1( 1 : long_ruta )//'AUSUBSIS.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 250 )

ibanbit = 1

ierror = 0

i = 0

!Inicialización de variables
indsis = 0
nomsis = ''
EstadoIsla = 0
numsis_act = 0
!
errleca = 0
! Lee información de division hasta encontrar fin de informacion
if ( ierror .eq. 0 ) then
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )

      read ( 4, 100, iostat = ierror ) letaux

      if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
        i = i + 1
        if ( i .gt. maxsis ) then
           bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
           call EnviaMensajeError ( bmensaje )
           bmensaje = "DE SUBSISTEMAS"
           call ParaProceso ( bmensaje )
        endif
        read ( letaux, *, iostat = ierr ) indsis(i), nomsis(i), EstadoIsla(i), SlackIsla(i)
	    if ( ierr .ne. 0 ) then
           bmensaje = '!!! AuSeg '//'Error en lectura de archivo AUSUBSIS.csv, registro '// letaux
           Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
          i = i - 1
	    endif
      endif

    enddo
else
   bmensaje = 'ERROR DE LECTURA ARCHIVO AUSUBSIS.csv'
   call ParaProceso ( bmensaje )
end if

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0  ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO AUSUBSIS.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'SUBSISTEMA: ['//trim(letr)//'] '//trim( nomsis(i))
   call ParaProceso ( bmensaje )
endif

numsis = i

! se determina el numero de sistemas activos
numsis_act = 0
write ( 1, 100 ) 'No. Sistema Activo 1/0'
do i = 1, numsis
   if ( EstadoIsla(i) .eq. 1 ) then
      numsis_act = numsis_act + 1
      write ( 1, 200 ) indsis(i), nomsis(i), EstadoIsla(i)
   endif
enddo

write ( 1, * ) ''

100 format ( a )
200 format ( i1, 3x, a8, 4x, i1)

close ( unit = 4 )

end subroutine LeeSubsistemas
    
    
!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Lee datos de numero de intervalos y duracion del intervalo      *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Noviembre 2014                        *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!        Jose Luis Ceciliano        Agosto 2019                           *
!**************************************************************************
    
SUBROUTINE data_horizo

use ParAUHE, only: rut_dat_1, NTINTR, long_ruta, durintr, bmensaje, TipoEjecu, &
                  NomEjecu, durdia, intdia, TipoLec, RUT_RES, MAXDIA
use ProblemaAUHE

IMPLICIT NONE

integer dia, i, ierror, ibanbit, error, dias

CHARACTER fecha_Ej*19

character*250 letaux
character*4 modelo, sistema

!Inicializacion de variables
NTINTR = 0
durintr = 0
durdia = 0
intdia = 0
ierror = 0
ibanbit = 0

! ------------------------------
! * Se leen datos de HORIZOAUHE *
! ------------------------------

OPEN (UNIT = 4, FILE = rut_dat_1( 1 : long_ruta )//'HORIZONTE.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

OPEN (UNIT = 3, FILE = rut_dat_1( 1 : long_ruta )//'HORIZOAUHE.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
if ( ierror .ne. 0 ) then
 close (3)
 ierror = 0
 OPEN (UNIT = 3, FILE = rut_dat_1( 1 : long_ruta )//'HORIZOMDA.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
endif   

if ( ierror .eq. 0 ) then
    read ( 3, 100, iostat = ierror ) letaux
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
	    ! Guarda información del horizonte
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
		    read ( letaux, * )  NTINTR, durintr, TipoEjecu, durdia
	    endif
	    read ( 3, 100, iostat = ierror ) letaux
    enddo
else	
	ibanbit = 1
    ierror = 0
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'LEC ERROR DE LECTURA ARCHIVO HORIZOAUHE.csv'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

! se lee la fecha de horizonte
read ( 4, 100, iostat = ierror ) letaux
if ( ierror .eq. 0 ) then
    read ( letaux, * )  HORIZO, dias, modelo, sistema
else
    ibanbit = 1
    ierror = 0
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'LEC ERROR DE LECTURA ARCHIVO HORIZOAUHE.csv'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
endif

CLOSE ( UNIT = 3 )
CLOSE ( UNIT = 4 )

TipoLec = 'MDA'
! Tipo de ejecucion
SELECT CASE ( TipoEjecu )
!   MDA
    CASE ( 0 )
        NomEjecu = 'MDA'
!   AUGC
    CASE ( 1 )
        NomEjecu = 'AUGC'
!   EXPOST
    CASE ( 2 )
        NomEjecu = 'EXPOST'
!   AUHE
    CASE ( 3 )
        NomEjecu = 'AUHE'
        TipoLec = 'AUHE'
END SELECT

!----------------------------------------
!* Abre archivo de bitacora de lecturas *
!----------------------------------------

OPEN ( UNIT = 1, FILE = rut_res//'Data_'//trim(NomEjecu)//'.res', IOSTAT = error, STATUS='UNKNOWN', RECORDSIZE = 3024 )
    
! -------------------------------
! * Se leen datos de INTERVADIA *
! -------------------------------

OPEN (UNIT = 3, FILE = rut_dat_1( 1 : long_ruta )//'INTERVADIA.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

if ( ierror .eq. 0 ) then
    i = 0
    read ( 3, 100, iostat = ierror ) letaux
    ! Lee informacion hasta encontrar fin de archivo
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
	    ! Guarda información del horizonte
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
		    read ( letaux, * )  dia, intdia ( i )
	    endif
	    read ( 3, 100, iostat = ierror ) letaux
    enddo
else	
	ibanbit = 1
    ierror = 0
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'LEC ERROR DE LECTURA ARCHIVO INTERVADIA.csv'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

CLOSE ( UNIT = 3 )

write ( 1, 200 ) 'No. Intervalos:', NTINTR
write ( 1, 200 ) 'Duración (min):', durintr
!write ( 1, 200 ) 'Tipo Ejecucion:', TipoEjecu
write ( 1, 200 ) 'No. de dias   :', durdia
write ( 1, * ) ''

do i = durdia+1, MAXDIA
    intdia ( i ) = 0
enddo

100 format ( a250 )
200 format ( a15, x, i3)

END SUBROUTINE data_horizo
   
    

    
!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Lee datos de nodos del sistema                                  *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Noviembre 2014                        *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                   Julio 2017                            *
!**************************************************************************
    
SUBROUTINE lee_nodos

use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, nodo_area, nodo_subsis, &
                  NumNodos, IslaGenRC, NumUniRC, nodorc, unidadrc_area, &
                  nombunirc, tiunidrc , proprc, &
                  NomEjecu, TipoLec, kvbase,maxnod

use ParGloRed, only: regnod, disnodini, nomnod

IMPLICIT NONE

integer ierror, dummy, i, errleca

character*250 letaux

character*3 letb, letc, letre

character*5 letr

!Inicializacion de variables
nodo_area = ''
nodo_subsis = ''
NumNodos = 0
!

! --------------------------
! * Se leen datos de NODOS *
! --------------------------

OPEN (UNIT = 6, FILE = rut_dat_1( 1 : long_ruta )//'NODOS'//trim(TipoLec)//'.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
errleca = 0; letaux=''    
if ( ierror .eq. 0 ) then
    i = 0
    read ( 6, 100, iostat = ierror ) letaux
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
        ! call QuitaComas ( letaux, len_trim(letaux) )
        i = i + 1
        if ( i .gt. maxnod ) then
           bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
           call EnviaMensajeError ( bmensaje )
           bmensaje = "DE NODOS"
           call ParaProceso ( bmensaje )
        endif
        read ( letaux, *, iostat = errleca )  dummy, nomnod ( i ), letb, dummy, nodo_area ( i ), letc, kvbase(i), nodo_subsis ( i ), letre, letr, regnod(i), disnodini(i) 
        read ( 6, 100, iostat = ierror ) letaux
    enddo
else   
   bmensaje = 'ERROR DE LECTURA ARCHIVO NODOS'//trim(TipoLec)//'.csv'
   call ParaProceso ( bmensaje )
end if

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0  ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO NODOS'//trim(TipoLec)//'.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'NODO: ['//trim(letr)//'] '//trim( nomnod ( i ))
   call ParaProceso ( bmensaje )
endif

NumNodos = i

CLOSE ( UNIT = 6 )

100 format ( a )

END SUBROUTINE lee_nodos
    
!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Lee datos de cargas y las relacionas con los subsistemas        *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Noviembre 2014                        *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
    
SUBROUTINE data_cargas

use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, NumNodos, NumOferDem, nombcar, &
                  propcar, nodocar, carga_area, isladem, numsis, nodo_subsis, &
                  estadoisla, nodo_area, nomsis, correspcar, DemFija, ntintr, Base, &
                  OferComDem, maxsegde, NumBloDem, PreComEner, CarGruRes, maxgrure, &
                  OferRes10, PreOferRes10, OferResS, PreOferResS, maxint, maxdem, &
                  NomEjecu, TipoEjecu, maxzondist, maxnodist, maxdem, NoNodDisCar, &
                  Tempnodocar, facdistcar, ExpEnt, maxcar
!
IMPLICIT NONE

integer ierror, dummy, tempnodcar ( maxint ), carga, i, nodo, subsistema, intervalo, &
        bloque, k, ierror_1, numseg, auxiliar ( maxgrure ), zona, NoDi, ZonaInd, NodDisZona ( maxzondist, maxnodist, maxint ), NoNodDisZona ( maxzondist, maxint ), &
        contador, IndiceTemp, NodDist, Indice ( maxzondist ), nodomax ( maxdem ), Exportacion

character*3000 letaux, letaux_1, letaux_2, letaux_4, letaux_5

character*20 nombre, propietario

integer  segmento, TipoDemAUHE, errleca, errlecb, errlecc, ierror_2

logical entro, YaEsta

character*1 let

character*5 letr

real*8 FacDisCar ( maxzondist, maxnodist, maxint )


!Inicializacion de variables
nombre = ''
propietario = ''
dummy = 0
tempnodcar = 0
carga = 0
i = 0
nombcar = ''
propcar = ''
nodocar = 0
carga_area = ''
IslaDem = 0
DemFija = 0.0
Indice = 0
IndiceTemp = 0
ierror_1 = 0
NodDisZona = 0
NoNodDisZona = 0
nodomax = 0
ExpEnt = 0
Exportacion = 0

!
write ( 1, 100 ) '--------------------' 
write ( 1, 100 ) 'CARGAS PARTICIPANTES' 
write ( 1, 100 ) '--------------------'
! --------------------------------
! * Se leen datos de CARGAS      *
! * Se leen datos de ZONASRESCAR *
! * Se leen datos de NODOSCAR    *
! --------------------------------

OPEN (UNIT = 12, FILE = rut_dat_1( 1 : long_ruta )//'CARGAS.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 108, FILE = rut_dat_1( 1 : long_ruta )//'ZONASRESCAR.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 192, FILE = rut_dat_1( 1 : long_ruta )//'NODOSCAR.csv', IOSTAT = IERROR_2, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0; errlecc = 0
if ( ierror .eq. 0  .and. ierror_1 .eq. 0 .and. ierror_2 .eq. 0 ) then
!    write ( 1, 100 ) 'Carga   Nombre      Propietario          Nodo     Area        Subsistema      Nodo Dist      Indice   Exportacion' 
    write ( 1, 100 ) 'Carga   Nombre      Propietario          Nodo     Area        Subsistema      Nodo Dist      Indice   Exportacion' 
    ! Lee informacion hasta encontrar fin de informacion
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	    read ( 12, 100, iostat = ierror ) letaux
        read ( 108, 100, iostat = ierror ) letaux_1
        read ( 192, 100, iostat = ierror ) letaux_2
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. len_trim(letaux_1) .ne. 0 .and. len_trim(letaux_2) .ne. 0 ) then
!            read ( letaux, * )  nombre, propietario, let, let, NodDist, IndiceTemp, Exportacion
            read ( letaux, *, iostat = errleca)  nombre, propietario, let, let, NodDist, IndiceTemp
            read ( letaux_1, *, iostat = errlecb )  ( auxiliar ( zona ), zona = 1, maxgrure )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1001
            !Si tiene nodos distribuidos se leen archivos de nodos distribuidos
            if ( NodDist .eq. 0 ) then
                read ( letaux_2, *, iostat = errlecc )  ( tempnodcar ( intervalo ), intervalo = 1, ntintr )
                if ( errlecc .ne. 0  ) go to 1001
                i = i + 1
                if ( i .gt. maxcar ) then
                   bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
                   call EnviaMensajeError ( bmensaje )
                   bmensaje = "DE CARGAS"
                   call ParaProceso ( bmensaje )
                endif
                !Hacer para todos los nodos
                do nodo = 1, NumNodos
                    !Se asigna subsitema con el nodo del primer intervalo
                    if ( tempnodcar ( 1 ) .eq. nodo ) then
                        !Hacer para todos los subsistemas
                        do subsistema = 1, numsis
                            if ( nodo_subsis ( nodo ) .eq. nomsis ( subsistema ) ) then
                                !Ver si el subsistema esta activo
                                if ( EstadoIsla ( subsistema ) .eq. 1 ) then
                                    carga = carga + 1
                                    IslaDem ( carga ) = subsistema
                                    !se asigna area con el nodo del primer intervalo
                                    carga_area ( carga ) = nodo_area ( nodo )
                                    nombcar ( carga ) = nombre                                
                                    propcar ( carga ) = propietario                                
                                    !nodocar ( carga, : ) = tempnodcar ( : )
                                    !Numero de nodos distribuidos por carga
                                    NoNodDisCar ( carga, : ) = 1
                                    Tempnodocar ( carga, 1, : ) = tempnodcar ( : )
                                    facdistcar ( carga, 1, : ) = 1
                                    correspcar ( carga ) = i
                                    ExpEnt ( carga ) = Exportacion
                                    CarGruRes ( carga, : ) =  auxiliar
                                    !se escribe a debugger
                                    write ( 1, 200 ) carga, nombcar ( carga ), propcar ( carga ), Tempnodocar ( carga, 1, 1 ), carga_area ( carga ), nomsis ( IslaDem ( carga ) ), NodDist, IndiceTemp, ExpEnt ( carga )
                                    exit
                                end if
                            end if
                        end do
                    end if
                end do
            else
                !revisar si ya se leyo el archivo con nodos distribuidos
                YaEsta = .false.
                do ZonaInd = 1, maxzondist
                    if ( Indice ( ZonaInd ) .ne. IndiceTemp ) then
                        if ( Indice ( ZonaInd ) .eq. 0 .and. YaEsta .eq. .false. ) then
                            Indice ( ZonaInd ) = IndiceTemp
                            !leer
                            contador = 0
                            write ( let, 1500 ) IndiceTemp
                            OPEN (UNIT = 200, FILE = trim(rut_dat_1)//'FACDISCAR'//let//'.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)
                            OPEN (UNIT = 201, FILE = trim(rut_dat_1)//'NODDISCAR'//let//'.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)
                            do while ( ierror_1 .eq. 0 .and. len_trim(letaux_4) .ne. 0 )        
	                            read ( 200, 100, iostat = ierror_1 ) letaux_4
                                read ( 201, 100, iostat = ierror_1 ) letaux_5
                                if ( ierror_1 .eq. 0 .and. len_trim(letaux_4) .ne. 0 ) then
                                    contador = contador + 1
                                    !read ( letaux_4, * )  NodDisZona ( IndiceTemp, contador ), ( FacDisCar ( IndiceTemp, contador, intervalo ), intervalo = 1, ntintr )
                                    read ( letaux_4, * )  ( FacDisCar ( IndiceTemp, contador, intervalo ), intervalo = 1, ntintr )
                                    read ( letaux_5, * )  ( NodDisZona ( IndiceTemp, contador, intervalo ), intervalo = 1, ntintr )
                                end if
                            end do
                            !Determinar el numero de nodos distribuidos por intervalo
                            !Hacer para todos los intervalos
                            do intervalo = 1, ntintr
                                !hacer para el maximo de nodos
                                do nodo = 1, contador
                                    if ( NodDisZona ( IndiceTemp, nodo, intervalo ) .eq. 0 ) then
                                        NoNodDisZona ( IndiceTemp, intervalo ) = nodo - 1
                                        go to 311
                                    else
                                        NoNodDisZona ( IndiceTemp, intervalo ) = contador
                                    end if
                                end do
311                         end do
                            close ( unit = 200 )
                            close ( unit = 201 )
                            exit
                        end if
                    else
                        YaEsta = .true.
                    end if
                end do
                i = i + 1
                !Hacer para todos los nodos
                do nodo = 1, NumNodos
                    !Se asigna subsitema con el nodo del primer intervalo
                    if ( NodDisZona ( IndiceTemp, 1, 1 ) .eq. nodo  ) then
                        !Hacer para todos los subsistemas
                        do subsistema = 1, numsis
                            if ( nodo_subsis ( nodo ) .eq. nomsis ( subsistema ) ) then
                                !Ver si el subsistema esta activo
                                if ( EstadoIsla ( subsistema ) .eq. 1 ) then
                                    carga = carga + 1
                                    IslaDem ( carga ) = subsistema
                                    !se asigna area con el nodo del primer intervalo
                                    carga_area ( carga ) = nodo_area ( nodo )
                                    nombcar ( carga ) = nombre                                
                                    propcar ( carga ) = propietario                                
                                    !!!!!!!!!!!!!!!!!!!!!!!!!!
                                    !Hacer para todos los intervalos
                                    do intervalo = 1, ntintr
                                        !Numero de nodos distribuidos por unidad de rango continuo
                                        NoNodDisCar ( carga, intervalo ) = NoNodDisZona ( IndiceTemp, intervalo )
                                        !NoNodDisCar ( carga ) = NoNodDisZona ( IndiceTemp, intervalo )
                                        !hacer para todos los nodos distribuidos
                                        do NoDi = 1, NoNodDisZona ( IndiceTemp, intervalo )
                                            if ( NoDi .gt. nodomax ( carga ) ) then
                                                nodomax ( carga ) = NoDi
                                            end if
                                            !Tempnodorc ( unidad, NoDi, : ) = NodDisZona ( IndiceTemp, NoDi )
                                            !facdistgen ( unidad, NoDi, : ) = FacDisGen ( IndiceTemp, NoDi, : )
                                            Tempnodocar ( carga, NoDi, intervalo ) = NodDisZona ( IndiceTemp, NoDi, intervalo )
                                            facdistcar( carga, NoDi, intervalo ) = FacDisCar ( IndiceTemp, NoDi, intervalo )
                                        end do                                    
                                    end do
                                    !!!!!!!!!!!!!!!!!!!!!!!!!
                                    correspcar ( carga ) = i
                                    CarGruRes ( carga, : ) =  auxiliar
                                    !se escribe a debugger
!!!                                    write ( 1, 200 ) carga, nombcar ( carga ), propcar ( carga ), tempnodocar ( carga, 1, 1 ), carga_area ( carga ), nomsis ( IslaDem ( carga ) ), NodDist, IndiceTemp
                                    exit
                                end if
                            end if
                        end do
                    end if
                end do
            end if    
	    endif
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO CARGAS.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO ZONASRESCAR.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_2 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSCAR.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0 ) then
    write ( letr, "(i5)" ) carga
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS CARGAS.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO ZONASRESCAR.csv'
       call EnviaMensajeError ( bmensaje )
   endif
   if ( errlecc .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSCAR.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = 'CARGA: ['//trim(letr)//'] '//trim(nombcar ( carga ))
   call ParaProceso ( bmensaje )
endif

NumOferDem = carga

CLOSE ( UNIT = 12 )
CLOSE ( UNIT = 108 )
close ( UNIT = 192 )

write ( 1, 100 ) ''

write ( 1, 100 ) '------------------------------'
write ( 1, 100 ) 'NODOS DE CARGAS PARTICIPANTES'
write ( 1, 100 ) '------------------------------'
!Hacer para todoas las cargas
do carga = 1, NumOferDem
    bloque = ntintr / 24
    do k = 1, bloque
!!!        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
!!!        write ( 1, 1400 ) carga, nombcar ( carga ), ( Tempnodocar ( carga, 1, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
!!!        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
!!!        write ( 1, 1400 ) carga, nombcar ( carga ), ( Tempnodocar ( carga, 1, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
    end if
end do

write ( 1, 100 ) ''

ierror = 0
i = 0

write ( 1, 100 ) '---------------------------------'
write ( 1, 100 ) 'FACTORES DE DITRIBUCION DE CARGA'
write ( 1, 100 ) '---------------------------------'
!Hacer para todoas las unidades de rango continuo
do carga = 1, NumOferDem
    !hacer para todos los nodos distribuidos
    do NoDi = 1, nodomax ( carga ) !NoNodDisCar ( carga )
        bloque = ntintr / 24
        do k = 1, bloque
!!!            write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
!!!            write ( 1, 1400 ) carga, 'Nodo:', ( Tempnodocar ( carga, NoDi, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
!!!            write ( 1, 1600 ) carga, 'FDC :', ( facdistcar ( carga, NoDi, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
!!!            write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
!!!            write ( 1, 1400 ) carga, 'Nodo:', ( Tempnodocar ( carga, NoDi, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
!!!            write ( 1, 1600 ) carga, 'FDC :', ( facdistcar ( carga, NoDi, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end if
    end do
end do

write ( 1, 100 ) ''

TipoDemAUHE = 1 ! (Tipo de Demanda para el AUHE: 1 = Pronostico, 0 = Ofertas de compra)
ierror = 0
i = 0

! TipoEjecu = 0 (MDA), TipoEjecu = 1 (AUGC), TipoEjecu = 2 (EXPOST), TipoEjecu = 3 (AUHE)

!Se leen datos de demanda dependiendo si es el MDA, AUGC, EXPOST o AUHE
if ( TipoEjecu .eq. 0 .or. ( TipoEjecu .eq. 3 .and. TipoDemAUHE .eq. 0 ) ) then
    !Es el MDA
    ! -----------------------------
    ! * Se leen datos de CARFIJA *
    ! -----------------------------
    write ( 1, 100 ) '------------'
    write ( 1, 100 ) 'DEMANDA FIJA'
    write ( 1, 100 ) '------------'

    OPEN (UNIT = 13, FILE = rut_dat_1( 1 : long_ruta )//'CARFIJA.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
     
    errleca = 0
    if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
	    read ( 13, 100, iostat = ierror ) letaux
        do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
            i = i + 1
            !Hacer para todoas las unidades de rango continuo
            do carga = 1, NumOferDem
                if ( correspcar ( carga ) .eq. i ) then
                    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
                        read ( letaux, *, iostat = errleca )  ( DemFija ( carga, intervalo ), intervalo = 1, ntintr )
                        if ( errleca .ne. 0 ) go to 1002
                        bloque = ntintr / 24
                        do k = 1, bloque
!!!                            write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
!!!                            write ( 1, 400 ) carga, nombcar ( carga ), ( DemFija ( carga, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                        end do
                        if ( ntintr - 24 * bloque .gt. 0 ) then
!!!                            write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
!!!                            write ( 1, 400 ) carga, nombcar ( carga ), ( DemFija ( carga, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                        end if
                    end if
                end if
            end do
	        read ( 13, 100, iostat = ierror ) letaux
        end do
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO CARFIJA.csv'
       call ParaProceso ( bmensaje )
    end if

1002 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0  ) then
        write ( letr, "(i5)" ) i
        call Elimina_blancos ( letr, 5)
        bmensaje = 'ERROR DE LECTURA ARCHIVO CARFIJA.csv'
        call EnviaMensajeError ( bmensaje )
        Bmensaje = 'CARGA: ['//trim(letr)//'] '//trim( nombcar ( carga ))
       call ParaProceso ( bmensaje )
    endif

    CLOSE ( UNIT = 13 )

    write ( 1, 100 ) ''

    !Escalamiento 
    DemFija = DemFija / Base

    !DemFija ( :, 12 ) = 0.0

    ierror = 0
    ierror_1 = 0
    i = 0
    ! ------------------------------
    ! * Se leen datos de POTCOCAR *
    ! * Se leen datos de PRECOCAR *
    ! ------------------------------
    write ( 1, 100 ) '-------------------------------------------------'
    write ( 1, 100 ) 'OFERTA DE COMPRA DE ENERGIA CARGAS PARTICIPANTES'
    write ( 1, 100 ) '-------------------------------------------------'

    OPEN (UNIT = 102, FILE = rut_dat_1( 1 : long_ruta )//'POTCOCAR.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
    OPEN (UNIT = 103, FILE = rut_dat_1( 1 : long_ruta )//'PRECOCAR.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 3000)
    errleca = 0; errlecb = 0
    if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    	read ( 102, 100, iostat = ierror ) letaux
        read ( 103, 100, iostat = ierror_1 ) letaux_1
        do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim ( letaux_1 ) .ne. 0 )        
            i = i + 1
            entro = .false.
            !Hacer para todoas las cargas participantes
            do carga = 1, NumOferDem
                segmento = 1
                if ( correspcar ( carga ) .eq. i ) then
                    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim(letaux_1) .ne. 0 ) then
                        entro = .true.
                        read ( letaux, *, iostat = errleca )  ( OferComDem ( carga, segmento, intervalo ), intervalo = 1, ntintr )
                        read ( letaux_1, *, iostat = errlecb )  ( PreComEner ( carga, segmento, intervalo ), intervalo = 1, ntintr )
                        if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1003
                        !Hacer para los segmentos del segundo en adelante
                        do segmento = 2, maxsegde
                            read ( 102, 100, iostat = ierror ) letaux
                            read ( letaux, * )  ( OferComDem ( carga, segmento, intervalo ), intervalo = 1, ntintr )
                            read ( 103, 100, iostat = ierror ) letaux_1
                            read ( letaux_1, * )  ( PreComEner ( carga, segmento, intervalo ), intervalo = 1, ntintr )
                        end do                    
                    end if
                end if
                if ( carga .eq. NumOferDem .and. entro .eq. .false. ) then
                    do segmento = 2, maxsegde
                        read ( 102, 100, iostat = ierror ) letaux                    
                        read ( 103, 100, iostat = ierror ) letaux_1
                    end do                    
                end if
            end do
	        read ( 102, 100, iostat = ierror ) letaux
            read ( 103, 100, iostat = ierror_1 ) letaux_1
        end do
    else	
      if ( ierror .ne. 0 ) then   
         bmensaje = 'ERROR DE LECTURA ARCHIVO POTCOCAR.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      if ( ierror_1 .ne. 0 ) then   
         bmensaje = 'ERROR DE LECTURA ARCHIVO PRECOCAR.csv'
         call EnviaMensajeError ( bmensaje )
      endif
       bmensaje = ' ' 
       call ParaProceso ( bmensaje )
    end if

1003 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 .or. errlecb .ne. 0  ) then
        write ( letr, "(i5)" ) carga
        call Elimina_blancos ( letr, 5)
        if ( errleca .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVOS POTCOCAR.csv'
           call EnviaMensajeError ( bmensaje )
        endif
       if ( errlecb .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO PRECOCAR.csv'
           call EnviaMensajeError ( bmensaje )
       endif
        Bmensaje = 'CARGA: ['//trim(letr)//'] '//trim(nombcar ( carga ))
       call ParaProceso ( bmensaje )
    endif

    CLOSE ( UNIT = 102 )
    CLOSE ( UNIT = 103 )

    !Escribir a bitacora
    !Hacer para todas las cargas participantes
    do carga = 1, NumOferDem
        !Hacer para todos los bloques de 24 intervalos
        bloque = ntintr / 24
        do k = 1, bloque
!!!            write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
            !Hacer para todos los segmentos
            do segmento = 1, maxsegde
!!!                write ( 1, 500 ) carga, nombcar ( carga ), 'MW', ( OferComDem ( carga, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
!!!                write ( 1, 500 ) carga, nombcar ( carga ), '$/MWh', ( PreComEner ( carga, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do        
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
!!!            write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
            do segmento = 1, maxsegde
!!!                write ( 1, 500 ) carga, nombcar ( carga ), 'MW', ( OferComDem ( carga, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
!!!                write ( 1, 500 ) carga, nombcar ( carga ), '$/MWh', ( PreComEner ( carga, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr )
            end do
        end if
    end do

    !Escalamiento
    OferComDem = OferComDem / Base
    PreComEner = PreComEner * Base

    !OferComDem ( :, :, 12 ) = 0.0

    !Calcular cuantos segmentos se ofertaron por carga y por intervalo
    do carga = 1, NumOferDem
        !Hacer para todos los intervalos
        do intervalo = 1, ntintr
            numseg = 0
            !Hacer para todos los segmentos
            do segmento = 1, maxsegde
                !Encontrar segmento con oferta de potencia cero
                if ( OferComDem ( carga, segmento, intervalo ) .ne. 0 ) then
                    numseg = numseg + 1
                end if            
            end do
            NumBloDem   ( carga, intervalo ) = numseg
        end do
    end do

    write ( 1, 100 ) ''

    ierror = 0
    ierror_1 = 0
    i = 0
else
    OferComDem = 0.0
    PreComEner = 0.0
    ! -----------------------------
    ! * Se leen datos de PRODEM *
    ! -----------------------------
    write ( 1, 100 ) '---------------------'
    write ( 1, 100 ) 'PRONOSTICO DE DEMANDA'
    write ( 1, 100 ) '----------------------'

    OPEN (UNIT = 13, FILE = rut_dat_1( 1 : long_ruta )//'PRODEM.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
     
    errleca = 0
    if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	        read ( 13, 100, iostat = ierror ) letaux
            i = i + 1
            !Hacer para todoas las unidades de rango continuo
            do carga = 1, NumOferDem
                if ( correspcar ( carga ) .eq. i ) then
                    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
                        read ( letaux, *, iostat = errleca )  ( DemFija ( carga, intervalo ), intervalo = 1, ntintr )
                        if ( errleca .ne. 0 ) go to 1004
                        bloque = ntintr / 24
                        do k = 1, bloque
!!!                            write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
!!!                            write ( 1, 400 ) carga, nombcar ( carga ), ( DemFija ( carga, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                        end do
                        if ( ntintr - 24 * bloque .gt. 0 ) then
!!!                            write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
!!!                            write ( 1, 400 ) carga, nombcar ( carga ), ( DemFija ( carga, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                        end if
                    end if
                end if
            end do
        end do
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO PRODEM.csv'
       call ParaProceso ( bmensaje )
    end if

1004 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0  ) then
        write ( letr, "(i5)" ) i
        call Elimina_blancos ( letr, 5)
        bmensaje = 'ERROR DE LECTURA ARCHIVO PRODEM.csv'
        call EnviaMensajeError ( bmensaje )
        Bmensaje = 'CARGA: ['//trim(letr)//'] '//trim( nombcar ( carga ))
       call ParaProceso ( bmensaje )
    endif
    
    CLOSE ( UNIT = 13 )

    write ( 1, 100 ) ''

    !Escalamiento 
    DemFija = DemFija / Base

    !DemFija ( :, 12 ) = 0.0

!    if ( TipoEjecu .eq. 3 ) then
    if ( TipoEjecu .eq. 3 .or. TipoEjecu .eq. 2 ) then !Version de prueba Expost Angel (10/09/2020)

        ierror = 0
        ierror_1 = 0
        i = 0
        ! ------------------------------
        ! * Se leen datos de POTCOCAR *
        ! * Se leen datos de PRECOCAR *
        ! ------------------------------
        write ( 1, 100 ) '-------------------------------------------------'
        write ( 1, 100 ) 'OFERTA DE COMPRA DE ENERGIA CARGAS PARTICIPANTES'
        write ( 1, 100 ) '-------------------------------------------------'

        OPEN (UNIT = 102, FILE = rut_dat_1( 1 : long_ruta )//'POTCOCAR.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
        OPEN (UNIT = 103, FILE = rut_dat_1( 1 : long_ruta )//'PRECOCAR.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 3000)
     
        errleca = 0; errlecb = 0
        if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
        ! Lee información hasta encontrar fin de información
            do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim ( letaux_1 ) .ne. 0 )        
	            read ( 102, 100, iostat = ierror ) letaux
                read ( 103, 100, iostat = ierror_1 ) letaux_1
                i = i + 1
                entro = .false.
                !Hacer para todoas las cargas participantes
                do carga = 1, NumOferDem
                    segmento = 1
                    if ( correspcar ( carga ) .eq. i ) then
                        if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim(letaux_1) .ne. 0 ) then
                            entro = .true.
                            read ( letaux, *, iostat = errleca)  ( OferComDem ( carga, segmento, intervalo ), intervalo = 1, ntintr )
                            read ( letaux_1, *, iostat = errlecb )  ( PreComEner ( carga, segmento, intervalo ), intervalo = 1, ntintr )
                            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1005
                            !Hacer para los segmentos del segundo en adelante
                            do segmento = 2, maxsegde
                                read ( 102, 100, iostat = ierror ) letaux
                                read ( letaux, * )  ( OferComDem ( carga, segmento, intervalo ), intervalo = 1, ntintr )
                                read ( 103, 100, iostat = ierror ) letaux_1
                                read ( letaux_1, * )  ( PreComEner ( carga, segmento, intervalo ), intervalo = 1, ntintr )
                            end do                    
                        end if
                    end if
                    if ( carga .eq. NumOferDem .and. entro .eq. .false. ) then
                        do segmento = 2, maxsegde
                            read ( 102, 100, iostat = ierror ) letaux                    
                            read ( 103, 100, iostat = ierror ) letaux_1
                        end do                    
                    end if
                end do
            end do
        else	
          if ( ierror .ne. 0 ) then   
             bmensaje = 'ERROR DE LECTURA ARCHIVO POTCOCAR.csv'
             call EnviaMensajeError ( bmensaje )
          endif
          if ( ierror_1 .ne. 0 ) then   
             bmensaje = 'ERROR DE LECTURA ARCHIVO PRECOCAR.csv'
             call EnviaMensajeError ( bmensaje )
          endif
          bmensaje = ' ' 
          call ParaProceso ( bmensaje )
        end if

1005 continue

        ! Verifica que no existan errores de lectura
        if ( errleca .ne. 0 .or. errlecb .ne. 0  ) then
            write ( letr, "(i5)" ) carga
            call Elimina_blancos ( letr, 5)
            if ( errleca .ne. 0 ) then
               bmensaje = 'ERROR DE LECTURA ARCHIVOS POTCOCAR.csv'
               call EnviaMensajeError ( bmensaje )
            endif
           if ( errlecb .ne. 0 ) then
               bmensaje = 'ERROR DE LECTURA ARCHIVO PRECOCAR.csv'
               call EnviaMensajeError ( bmensaje )
           endif
            Bmensaje = 'CARGA: ['//trim(letr)//'] '//trim(nombcar ( carga ))
           call ParaProceso ( bmensaje )
        endif
        
        CLOSE ( UNIT = 102 )
        CLOSE ( UNIT = 103 )

        !Escribir a bitacora
        !Hacer para todas las cargas participantes
        do carga = 1, NumOferDem
            !Hacer para todos los bloques de 24 intervalos
            bloque = ntintr / 24
            do k = 1, bloque
                write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
                !Hacer para todos los segmentos
                do segmento = 1, maxsegde
                    write ( 1, 500 ) carga, nombcar ( carga ), 'MW', ( OferComDem ( carga, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                    write ( 1, 500 ) carga, nombcar ( carga ), '$/MWh', ( PreComEner ( carga, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                end do        
            end do
            if ( ntintr - 24 * bloque .gt. 0 ) then
                write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
                do segmento = 1, maxsegde
                    write ( 1, 500 ) carga, nombcar ( carga ), 'MW', ( OferComDem ( carga, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                    write ( 1, 500 ) carga, nombcar ( carga ), '$/MWh', ( PreComEner ( carga, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr )
                end do
            end if
        end do

        !Escalamiento
        OferComDem = OferComDem / Base
        PreComEner = PreComEner * Base

        !OferComDem ( :, :, 12 ) = 0.0

        !Calcular cuantos segmentos se ofertaron por carga y por intervalo
        do carga = 1, NumOferDem
            !Hacer para todos los intervalos
            do intervalo = 1, ntintr
                numseg = 0
                !Hacer para todos los segmentos
                do segmento = 1, maxsegde
                    !Encontrar segmento con oferta de potencia cero
                    if ( OferComDem ( carga, segmento, intervalo ) .ne. 0 ) then
                        numseg = numseg + 1
                    end if            
                end do
                NumBloDem   ( carga, intervalo ) = numseg
            end do
        end do

        write ( 1, 100 ) ''

    endif

end if


! ---------------------------------
! * Se leen datos de POTRES10CAR *
! * Se leen datos de PRERES10CAR *
! ---------------------------------
!Potencia oferta de reserva de 10 min cargas participantes
! -----------------------------------------
!Precio oferta de reserva rodante de 10 min unidades hidro
write ( 1, 100 ) '----------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE RESERVA DE 10 MIN DE CARGAS PARTICIPANTES '
write ( 1, 100 ) '----------------------------------------------------'

! Abre archivo de datos de generadores
OPEN (UNIT = 104, FILE = rut_dat_1( 1 : long_ruta )//'POTRES10CAR.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 105, FILE = rut_dat_1( 1 : long_ruta )//'PRERES10CAR.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim ( letaux_1 ) .ne. 0 )       
        read ( 104, 100, iostat = ierror ) letaux
        read ( 105, 100, iostat = ierror_1 ) letaux_1
        i = i + 1
        !Hacer para todoas las cargas
        do carga = 1, NumOferDem
            if ( correspcar ( carga ) .eq. i ) then
                if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim(letaux_1) .ne. 0 ) then
                    read ( letaux, *, iostat = errleca )  ( OferRes10 ( carga, intervalo ), intervalo = 1, ntintr )
                    read ( letaux_1, *, iostat = errlecb )  ( PreOferRes10 ( carga, intervalo ), intervalo = 1, ntintr )
                    if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1006
                    bloque = ntintr / 24
                    do k = 1, bloque
!!!                        write ( 1, 600 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
!!!                        write ( 1, 700 ) carga, nombcar ( carga ), 'Pot:', ( OferRes10 ( carga, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
!!!                        write ( 1, 700 ) carga, nombcar ( carga ), '$  :', ( PreOferRes10 ( carga, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                    end do
                    if ( ntintr - 24 * bloque .gt. 0 ) then
!!!                        write ( 1, 600 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
!!!                        write ( 1, 700 ) carga, nombcar ( carga ), 'Pot:', ( OferRes10 ( carga, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
!!!                        write ( 1, 700 ) carga, nombcar ( carga ), '$  :', ( PreOferRes10 ( carga, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                    end if
                end if
            end if
        end do
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO POTRES10CAR.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERES10CAR.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  bmensaje = ' ' 
  call ParaProceso ( bmensaje )
end if

1006 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0  ) then
   write ( letr, "(i5)" ) carga
   call Elimina_blancos ( letr, 5)
   if ( errleca .ne. 0 ) then
      bmensaje = 'ERROR DE LECTURA ARCHIVOS POTRES10CAR.csv'
      call EnviaMensajeError ( bmensaje )
   endif
   if ( errlecb .ne. 0 ) then
      bmensaje = 'ERROR DE LECTURA ARCHIVO PRERES10CAR.csv'
      call EnviaMensajeError ( bmensaje )
   endif
   Bmensaje = 'CARGA: ['//trim(letr)//'] '//trim(nombcar ( carga ))
   call ParaProceso ( bmensaje )
endif
        
CLOSE ( UNIT = 104 )
CLOSE ( UNIT = 105 )

write ( 1, 100 ) ''

!Escalamiento
OferRes10 = OferRes10 / Base
PreOferRes10 = PreOferRes10 * Base

ierror = 0
ierror_1 = 0
i = 0

! ---------------------------------
! * Se leen datos de POTRESSUCAR *
! * Se leen datos de PRERESSUCAR *
! ---------------------------------
!Potencia oferta de reserva suplementaria cargas participantes
! -----------------------------------------
!Precio oferta de reserva suplementaria cargas participantes
write ( 1, 100 ) '-------------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE RESERVA SUPLEMENTARIA DE CARGAS PARTICIPANTES '
write ( 1, 100 ) '-------------------------------------------------------'

! Abre archivo de datos de generadores
OPEN (UNIT = 106, FILE = rut_dat_1( 1 : long_ruta )//'POTRESSUCAR.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 107, FILE = rut_dat_1( 1 : long_ruta )//'PRERESSUCAR.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim ( letaux_1 ) .ne. 0 )       
        read ( 106, 100, iostat = ierror ) letaux
        read ( 107, 100, iostat = ierror_1 ) letaux_1
        i = i + 1
        !Hacer para todoas las cargas
        do carga = 1, NumOferDem
            if ( correspcar ( carga ) .eq. i ) then
                if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim(letaux_1) .ne. 0 ) then
                    read ( letaux, *, iostat = errleca )  ( OferResS ( carga, intervalo ), intervalo = 1, ntintr )
                    read ( letaux_1, *, iostat = errlecb )  ( PreOferResS ( carga, intervalo ), intervalo = 1, ntintr )
                    if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1007
                    bloque = ntintr / 24
                    do k = 1, bloque
!!!                        write ( 1, 600 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
!!!                        write ( 1, 700 ) carga, nombcar ( carga ), 'Pot:', ( OferResS ( carga, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
!!!                        write ( 1, 700 ) carga, nombcar ( carga ), '$  :', ( PreOferResS ( carga, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                    end do
                    if ( ntintr - 24 * bloque .gt. 0 ) then
!!!                        write ( 1, 600 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
!!!                        write ( 1, 700 ) carga, nombcar ( carga ), 'Pot:', ( OferResS ( carga, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
!!!                        write ( 1, 700 ) carga, nombcar ( carga ), '$  :', ( PreOferResS ( carga, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                    end if
                end if
            end if
        end do
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESSUCAR.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESSUCAR.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  bmensaje = ' ' 
  call ParaProceso ( bmensaje )
end if

1007 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0  ) then
   write ( letr, "(i5)" ) carga
   call Elimina_blancos ( letr, 5)
   if ( errleca .ne. 0 ) then
      bmensaje = 'ERROR DE LECTURA ARCHIVOS POTRESSUCAR.csv'
      call EnviaMensajeError ( bmensaje )
   endif
   if ( errlecb .ne. 0 ) then
      bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESSUCAR.csv'
      call EnviaMensajeError ( bmensaje )
   endif
   Bmensaje = 'CARGA: ['//trim(letr)//'] '//trim(nombcar ( carga ))
   call ParaProceso ( bmensaje )
endif
        
CLOSE ( UNIT = 106 )
CLOSE ( UNIT = 107 )

write ( 1, 100 ) ''

!Escalamiento
OferResS = OferResS / Base
PreOferResS = PreOferResS * Base

ierror = 0
ierror_1 = 0
i = 0

100 format ( a )
!200 format ( i4, 5x, a12, 4x, a5, 12x, i4, 5x, a9, 3x, a5, 18x, i1, 18x, i2 )  
200 format ( i4, 5x, a12, 4x, a5, 12x, i4, 5x, a9, 3x, a5, 12x, i1, 18x, i2, 5x, i2 )  
300 format ( a10, x, i3, x, a1, i3 )      
400 format ( i4, x, a12, x, 24 (f9.2, 2x) )    
500 format ( i4, x, a12, x, a5, x, 24 (f9.2, 2x) )    
600  format ( a10, x, i3, x, a1, i3 )      
700  format ( i4, x, a12, x, a5, x, 24 (f9.2, 2x) )
1400 format ( i4, x, a12, x, 24 (i6, 2x) )
1500 format ( i1 )
1600 format ( i3, x, a12, x, 24 (f6.3, 2x) )    


END SUBROUTINE data_cargas

!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Lee datos de generadores con rangos continuos de                *
!         operacion y relaciona unidades de                               *
!         de generacion con los subsistemas                               * 
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Noviembre 2014                        *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                   Julio 2017                            *
!**************************************************************************
    
SUBROUTINE data_noprog

use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, numnodos, numsis, nodo_subsis, &
                  nomsis, estadoisla, IslaGenNPR, unidadnpr_area, nodo_area, &
                  nombuninpr, tiunidnpr, propnpr, nodonpr, correspnpr, NumUniNPR, &
                  PotNPR, ntintr, Base, maxint, maxunpr, TipoLec, &
                  NomEjecu, Tempnodonpr, NoNodDisNPR, facdistnpr, maxzondist, maxnodist
!
IMPLICIT NONE

integer ierror, dummy, tempnodnpr ( maxint ), i, nodo, subsistema, unidad, intervalo, &
        bloque, k, NoDi, ZonaInd, NodDisZona ( maxzondist, maxnodist, maxint ), contador, ierror_1, Indice ( maxzondist ), &
        IndiceTemp, NodDist, NoNodDisZona ( maxzondist, maxint ), nodomax ( maxunpr ), errleca, errlecb

character*3000 letaux, letaux_1, letaux_4, letaux_5 

character*20 nombre, propietario

character*7 tipo

character*1 let

character*5 letr

real*8 FacDisGen ( maxzondist, maxnodist, maxint )

logical YaEsta

!Inicializacion de variables
unidad = 0
i = 0
ierror = 0
nombre = ''
tipo = ''
propietario = ''
dummy = 0
tempnodnpr = 0
IslaGenNPR = 0
unidadnpr_area = ''
nombuninpr = ''
tiunidnpr = ''
propnpr = ''
nodonpr = 0
correspnpr = 0
NumUniNPR = 0
NodDist = 0
Indice = 0
IndiceTemp = 0
ierror_1 = 0
NodDisZona = 0
NoNodDisZona = 0
nodomax = 0
!

! ---------------------------
! * Se leen datos de PLNOPR *
! ---------------------------
write ( 1, 100 ) '------------------------' 
write ( 1, 100 ) 'UNIDADES NO PROGRAMABLES' 
write ( 1, 100 ) '------------------------'
OPEN (UNIT = 14, FILE = rut_dat_1( 1 : long_ruta )//'PLNOPR'//trim(TipoLec)//'.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 193, FILE = rut_dat_1( 1 : long_ruta )//'NODOSPLNOPR.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0
if ( ierror .eq. 0 ) then
    write ( 1, 100 ) 'Unidad  Nombre          Tipo  Propietario    Nodo     Area        Subsistema      Nodo Dist.               Indice' 
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	    read ( 14, 100, iostat = ierror ) letaux
        read ( 193, 100, iostat = ierror ) letaux_1
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. len_trim(letaux_1) .ne. 0 ) then
            read ( letaux, *, iostat = errleca )  nombre, tipo, propietario, let, let, NodDist, IndiceTemp
            if ( errleca .ne. 0 ) go to 1001
            !Si tiene nodos distribuidos se leen archivos de nodos distribuidos
            if ( NodDist .eq. 0 ) then
                read ( letaux_1, *, iostat = errlecb )  ( tempnodnpr ( intervalo ), intervalo = 1, ntintr )
                if ( errlecb .ne. 0 ) go to 1001
                i = i + 1
                if ( i .gt. maxunpr ) then
                   bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
                   call EnviaMensajeError ( bmensaje )
                   bmensaje = "DE UNIDADES NO PROGRAMABLES"
                   call ParaProceso ( bmensaje )
                endif
                
                !Hacer para todos los nodos
                do nodo = 1, NumNodos
                    !Se asigna subsistema con el nodo del primer intervalo
                    if ( tempnodnpr ( 1 ) .eq. nodo ) then
                        !Hacer para todos los subsistemas
                        do subsistema = 1, numsis
                            if ( nodo_subsis ( nodo ) .eq. nomsis ( subsistema ) ) then
                                !Ver si el subsistema esta activo
                                if ( EstadoIsla ( subsistema ) .eq. 1 ) then
                                    unidad = unidad + 1
                                    IslaGenNPR ( unidad ) = subsistema
                                    !se asigna area con el nodo del primer intervalo
                                    unidadnpr_area ( unidad ) = nodo_area ( nodo )
                                    nombuninpr ( unidad ) = nombre
                                    tiunidnpr ( unidad ) = tipo
                                    propnpr ( unidad ) = propietario
                                    !nodonpr ( unidad, : ) = tempnodnpr ( : )                                
                                    NoNodDisNPR( unidad, : ) = 1
                                    Tempnodonpr ( unidad, 1, : ) = tempnodnpr ( : )
                                    facdistnpr ( unidad, 1, : ) = 1
                                    correspnpr ( unidad ) = i
                                    !se escribe a debugger
                                    write ( 1, 200 ) unidad, nombuninpr ( unidad ), tiunidnpr ( unidad ), propnpr ( unidad ), tempnodonpr ( unidad, 1, 1 ), unidadnpr_area ( unidad ), nomsis ( IslaGenNPR ( unidad ) ), NodDist, IndiceTemp
                                    exit
                                end if
                            end if
                        end do
                    end if
                end do
            else
                !revisar si ya se leyo el archivo con nodos distribuidos
                YaEsta = .false.
                do ZonaInd = 1, maxzondist
                    if ( Indice ( ZonaInd ) .ne. IndiceTemp ) then
                        if ( Indice ( ZonaInd ) .eq. 0 .and. YaEsta .eq. .false. ) then
                            Indice ( ZonaInd ) = IndiceTemp
                            !leer
                            contador = 0
                            write ( let, 1500 ) IndiceTemp
                            OPEN (UNIT = 200, FILE = trim(rut_dat_1)//'FACDISGEN'//let//'.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)
                            OPEN (UNIT = 201, FILE = trim(rut_dat_1)//'NODDISGEN'//let//'.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)
                            do while ( ierror_1 .eq. 0 .and. len_trim(letaux_4) .ne. 0 )        
	                            read ( 200, 100, iostat = ierror_1 ) letaux_4
                                read ( 201, 100, iostat = ierror_1 ) letaux_5
                                if ( ierror_1 .eq. 0 .and. len_trim(letaux_4) .ne. 0 ) then
                                    contador = contador + 1
                                    !read ( letaux_4, * )  NodDisZona ( IndiceTemp, contador ), ( FacDisGen ( IndiceTemp, contador, intervalo ), intervalo = 1, ntintr )
                                    read ( letaux_4, * )  ( FacDisGen ( IndiceTemp, contador, intervalo ), intervalo = 1, ntintr )
                                    read ( letaux_5, * )  ( NodDisZona ( IndiceTemp, contador, intervalo ), intervalo = 1, ntintr )
                                end if
                            end do
                            !Determinar el numero de nodos distribuidos por intervalo
                            !Hacer para todos los intervalos
                            do intervalo = 1, ntintr
                                !hacer para el maximo de nodos
                                do nodo = 1, contador
                                    if ( NodDisZona ( IndiceTemp, nodo, intervalo ) .eq. 0 ) then
                                        NoNodDisZona ( IndiceTemp, intervalo ) = nodo - 1
                                        go to 311
                                    else
                                        NoNodDisZona ( IndiceTemp, intervalo ) = contador
                                    end if
                                end do
311                         end do
                            close ( unit = 200 )
                            close ( unit = 201 )
                            exit
                        end if
                    else
                        YaEsta = .true.
                    end if
                end do
                i = i + 1
                !Hacer para todos los nodos
                do nodo = 1, NumNodos
                    !Se asigna subsistema con el nodo del primer intervalo
                    !if ( tempnodnpr ( 1 ) .eq. nodo ) then
                    !Se revisa pertenencia al subsitema con el primer nodo del primer intervalo
                    if ( NodDisZona ( IndiceTemp, 1, 1 ) .eq. nodo ) then
                        !Hacer para todos los subsistemas
                        do subsistema = 1, numsis
                            if ( nodo_subsis ( nodo ) .eq. nomsis ( subsistema ) ) then
                                !Ver si el subsistema esta activo
                                if ( EstadoIsla ( subsistema ) .eq. 1 ) then
                                    unidad = unidad + 1
                                    IslaGenNPR ( unidad ) = subsistema
                                    !se asigna area con el nodo del primer intervalo
                                    unidadnpr_area ( unidad ) = nodo_area ( nodo )
                                    nombuninpr ( unidad ) = nombre
                                    tiunidnpr ( unidad ) = tipo
                                    propnpr ( unidad ) = propietario
                                    !!!!!!!!!!!!!!
                                    !Hacer para todos los intervalos
                                    do intervalo = 1, ntintr
                                        !Numero de nodos distribuidos por unidad de rango continuo
                                        NoNodDisNPR ( unidad, intervalo ) = NoNodDisZona ( IndiceTemp, intervalo )
                                        !NoNodDisNPR ( unidad ) = NoNodDisZona ( IndiceTemp, intervalo )
                                        !hacer para todos los nodos distribuidos
                                        do NoDi = 1, NoNodDisZona ( IndiceTemp, intervalo )
                                            if ( NoDi .gt. nodomax ( unidad ) ) then
                                                nodomax ( unidad ) = NoDi
                                            end if
                                            !Tempnodorc ( unidad, NoDi, : ) = NodDisZona ( IndiceTemp, NoDi )
                                            !facdistgen ( unidad, NoDi, : ) = FacDisGen ( IndiceTemp, NoDi, : )
                                            Tempnodonpr ( unidad, NoDi, intervalo ) = NodDisZona ( IndiceTemp, NoDi, intervalo )
                                            facdistnpr ( unidad, NoDi, intervalo ) = FacDisGen ( IndiceTemp, NoDi, intervalo )
                                        end do                                    
                                    end do
                                    !!!!!!!!!!!!!!!!!
                                    correspnpr ( unidad ) = i
                                    !se escribe a debugger
                                    write ( 1, 200 ) unidad, nombuninpr ( unidad ), tiunidnpr ( unidad ), propnpr ( unidad ), tempnodonpr ( unidad, 1, 1 ), unidadnpr_area ( unidad ), nomsis ( IslaGenNPR ( unidad ) ), NodDist, IndiceTemp
                                    exit
                                end if
                            end if
                        end do
                    end if
                end do
            end if
	    endif
    enddo
else	
  if ( ierror .ne. 0 ) then   
      bmensaje = 'ERROR DE LECTURA ARCHIVO '//'PLNOPR'//trim(TipoLec)//'.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSPLNOPR.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  bmensaje = ' ' 
  call ParaProceso ( bmensaje )
end if

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0  ) then
   write ( letr, "(i5)" ) i
   call Elimina_blancos ( letr, 5)
   if ( errleca .ne. 0 ) then
      bmensaje = 'ERROR DE LECTURA ARCHIVO '//'PLNOPR'//trim(TipoLec)//'.csv'
      call EnviaMensajeError ( bmensaje )
   endif
   if ( errlecb .ne. 0 ) then
      bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSPLNOPR.csv'
      call EnviaMensajeError ( bmensaje )
   endif
   Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombuninpr ( i ))
   call ParaProceso ( bmensaje )
endif

NumUniNPR = unidad

CLOSE ( UNIT = 14 )
close ( UNIT = 193 )

write ( 1, 100 ) ''

write ( 1, 100 ) '----------------------------------'
write ( 1, 100 ) 'NODOS DE UNIDADES NO PROGRAMABLES'
write ( 1, 100 ) '----------------------------------'
!Hacer para todoas las UNIDADES no programables
do unidad = 1, NumUniNPR
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        write ( 1, 1400 ) unidad, nombuninpr ( unidad ), ( Tempnodonpr ( unidad, 1, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        write ( 1, 1400 ) unidad, nombuninpr ( unidad ), ( Tempnodonpr ( unidad, 1, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
    end if
end do

write ( 1, 100 ) ''

ierror = 0
i = 0

write ( 1, 100 ) '---------------------------------------------------'
write ( 1, 100 ) 'FACTORES DE DITRIBUCION DE GENERACION NO PROGRAMABLE'
write ( 1, 100 ) '---------------------------------------------------'
!Hacer para todoas las UNIDADES no programables
do unidad = 1, NumUniNPR
    !hacer para todos los nodos distribuidos
    do NoDi = 1, nodomax ( unidad ) !NoNodDisNPR ( unidad )
        bloque = ntintr / 24
        do k = 1, bloque
            write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
            write ( 1, 1400 ) unidad, 'Nodo:', ( Tempnodonpr ( unidad, NoDi, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 1600 ) unidad, 'FDG :', ( facdistnpr ( unidad, NoDi, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
            write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
            write ( 1, 1400 ) unidad, 'Nodo:', ( Tempnodonpr ( unidad, NoDi, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 1600 ) unidad, 'FDG :', ( facdistnpr ( unidad, NoDi, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end if
    end do
end do

write ( 1, 100 ) ''

ierror = 0
i = 0

! ------------------------------
! * Se leen datos de POTPLNOPR *
! ------------------------------
write ( 1, 100 ) '------------------------------------'
write ( 1, 100 ) 'POTENCIA DE GENERACION NO PROGRAMABLE'
write ( 1, 100 ) '------------------------------------'

OPEN (UNIT = 15, FILE = rut_dat_1( 1 : long_ruta )//'POTPLNOPR.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)

errleca = 0
if ( ierror .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	    read ( 15, 100, iostat = ierror ) letaux
        i = i + 1
        !Hacer para todoas las unidades de rango continuo
        do unidad = 1, NumUniNPR
            if ( correspnpr ( unidad ) .eq. i ) then
                if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
                    read ( letaux, *, iostat = errleca )  ( PotNPR ( unidad, intervalo ), intervalo = 1, ntintr )
                    if ( errleca .ne. 0 ) go to 1002
                    bloque = ntintr / 24
                    do k = 1, bloque
                        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
                        write ( 1, 400 ) unidad, nombuninpr ( unidad ), ( PotNPR ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                    end do
                    if ( ntintr - 24 * bloque .gt. 0 ) then
                        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
                        write ( 1, 400 ) unidad, nombuninpr ( unidad ), ( PotNPR ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                    end if
                end if
            end if
        end do
    end do
else	
  if ( ierror .ne. 0 ) then   
      bmensaje = 'ERROR DE LECTURA ARCHIVO POTPLNOPR.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  bmensaje = ' ' 
  call ParaProceso ( bmensaje )
end if

1002 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
   write ( letr, "(i5)" ) i
   call Elimina_blancos ( letr, 5)
   bmensaje = 'ERROR DE LECTURA ARCHIVO POTPLNOPR.csv'
   call EnviaMensajeError ( bmensaje )
   Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombuninpr ( i ))
   call ParaProceso ( bmensaje )
endif
CLOSE ( UNIT = 15 )

write ( 1, 100 ) ''

!Escalamiento
PotNPR = PotNPR / Base

!PotNPR = 0.0

100 format ( a )
200 format ( i3, 5x, a12, 4x, a2, 4x, a3, 12x, i4, 5x, a9, 3x, a5, 18x, i1, 18x, i2 )    
300 format ( a10, x, i3, x, a1, i3 )      
400 format ( i3, x, a12, x, 24 (f9.2, 2x) )   
1400 format ( i3, x, a12, x, 24 (i6, 2x) )    
1500 format ( i1 )
1600 format ( i3, x, a12, x, 24 (f6.3, 2x) )    

     
END SUBROUTINE data_noprog
    

    
    !
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Valida las ofertas de servicios conexos de TODOS los            *
!         participantes del mercado                                       *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Noviembre 2014                        *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
    
SUBROUTINE calc_conexos

use ParAUHE, only: RamEmer10RC, PotMaxGRC, PotMinGRC, OferResR10RC, CalOferResR10RC, NumUniRC, &
                  ntintr, CalOferResRxRC, CalOferResRegRC, RamEmerxRC, RamRegRC, OferResRxRC, &
                  OferResRegRC, CalOferResNR10RC, PotSincNR10URC, OferResNR10RC, CalOferResNRxRC, &
                  PotSincNRSURC, OferResNRxRC, nombunirc, PreVenResR10RC, PreVenResRegRC, PreVenResRxRC, &
                  PreVenResNR10RC, PreVenResNRxRC, base, NumUniHid, CalOferResR10H, CalOferResRxH, &
                  CalOferResReGH, CalOferResNR10H, CalOferResNRxH, RamEmer10H, PotMaxUniH, PotMinUniH, &
                  OferResR10H, OferResRxH, OferResRegH, RamEmerxH, RamRegH, nombunih, PreVenResR10H, &
                  PreVenResRxH, PreVenResRegH, PreVenResNR10H, OferResNRxH, PreVenResNRxH, OferResNR10H, &
                  CalOferResR10RD, CalOferResRxRD, CalOferResRegRD, CalOferResNR10RD, CalOferResNRxRD, &
                  NumUniRD, NumModRD, RamEmer10RD, PotMaxGRD, PotMinGRD, OferResRxRD, RamEmerxRD, OferResR10RD, &
                  RamRegRD, PotSincNR10URD, PotSincNRSURD, nombunird, OferResRegRD, PreVenResR10RD, PreVenResRxRD, &
                  PreVenResRegRD, OferResNR10RD, PreVenResNR10RD, OferResNRxRD, PreVenResNRxRD, &
                  NoRaOpRC, RaOpSupRC, RaOpInfRC, CalOferRegROH
use ProblemaAUHE

IMPLICIT NONE

integer unidad, intervalo, bloque, k, modo
integer ro

CalOferResR10RC = 0.0
CalOferResRxRC = 0.0
CalOferResRegRC = 0.0
CalOferResNR10RC = 0.0
CalOferResNRxRC = 0.0
CalOferRegRORC = 0.0

CalOferResR10H = 0.0
CalOferResRxH = 0.0
CalOferResReGH = 0.0
CalOferResNR10H = 0.0
CalOferResNRxH = 0.0
CalOferRegROH = 0.0

CalOferResR10RD = 0.0
CalOferResRxRD = 0.0
CalOferResRegRD = 0.0
CalOferResNR10RD = 0.0
CalOferResNRxRD = 0.0

!Hacer para todas las unidades de rango continuo
do unidad = 1, NumUniRC
    if ( unidad == 234 ) then
        continue
    endif
    !Hacer para todos los intervalos
    do intervalo = 1, ntintr
        if ( dispourc ( unidad, intervalo ) .eq. 1 ) then
            !Reserva rodante de 10 min
            CalOferResR10RC ( unidad, intervalo ) = min ( RamEmer10RC ( unidad ), PotMaxGRC ( unidad, intervalo ) - PotMinGRC ( unidad, intervalo ), OferResR10RC ( unidad, intervalo ) )
            !Reserva rodante suplementaria
            CalOferResRxRC ( unidad, intervalo ) = min ( RamEmerxRC ( unidad ), PotMaxGRC ( unidad, intervalo ) - PotMinGRC ( unidad, intervalo ), OferResRxRC ( unidad, intervalo ) )
            !Reserva regulacion secundaria
            if ( SiLimReg .eq. 0 ) then
                CalOferResRegRC ( unidad, intervalo ) = min ( RamRegRC ( unidad ), 0.5*(PotMaxGRC ( unidad, intervalo ) - PotMinGRC ( unidad, intervalo )), OferResRegRC ( unidad, intervalo ) )
            else
                CalOferResRegRC ( unidad, intervalo ) = min ( RamRegRC ( unidad ), 0.5*(PotMaxRRC ( unidad, intervalo ) - PotMinRRC ( unidad, intervalo )), OferResRegRC ( unidad, intervalo ) )
            endif                
            !Limite minimo de reserva no rodante de 10 min
            CalOferResNR10RC ( unidad, intervalo ) = min ( PotSincNR10URC ( unidad ), PotMinGRC ( unidad, intervalo ) )
            !Limite minimo de reserva no rodante suplementaria
            CalOferResNRxRC ( unidad, intervalo ) = min ( PotSincNRSURC ( unidad ), PotMinGRC ( unidad, intervalo ) )
        endif
    end do
end do

! se determina maximo de reserva de regulacion para unidades con bandas prohibidas
if ( SiBandProh .eq. 1 ) then
!   unidades de rango continuo
    do unidad = 1 , NumUniRC
!       si la unidad tiene bandas prohibidas
        if ( NoRaOpRC ( unidad ) .gt. 0 ) then
    !           para todos los intervalos
                do intervalo = 1, ntintr
!                   si la unidad esta disponible
                    if ( dispourc ( unidad, intervalo ) .eq. 1 ) then
        !               para los rangos de la unidad
                        do ro = 1, NoRaOpRC ( unidad )
        !                   oferta de reserva regulacion secundaria
                            CalOferRegRORC ( unidad, ro, intervalo ) = min ( RamRegRC ( unidad ), (RaOpSupRC ( unidad, ro, intervalo ) - RaOpInfRC ( unidad, ro, intervalo ))*0.5, &
                                                                        OferResRegRC ( unidad, intervalo ) )
                        enddo
                    endif
                enddo
        endif
    enddo
endif

write ( 1, 100 ) '-------------------------------------------------------'
write ( 1, 100 ) 'CALCULO DE SERVICIOS CONEXOS DE UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '-------------------------------------------------------'


do unidad = 1, NumUniRC
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot RR10min     :', ( CalOferResR10RC ( unidad, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), '$               :', ( PreVenResR10RC ( unidad, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot RRSUmin     :', ( CalOferResRxRC ( unidad, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), '$               :', ( PreVenResRxRC ( unidad, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot RRegSec     :', ( CalOferResRegRC ( unidad, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), '$               :', ( PreVenResRegRC ( unidad, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot Max RNR10min:', ( OferResNR10RC ( unidad, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot Min RNR10min:', ( CalOferResNR10RC ( unidad, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), '$               :', ( PreVenResNR10RC ( unidad, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot Max RNRSUmin:', ( OferResNRxRC ( unidad, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot Min RNRSUmin:', ( CalOferResNRxRC ( unidad, intervalo) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), '$               :', ( PreVenResNRxRC ( unidad, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
        
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot RR10min     :', ( CalOferResR10RC ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), '$               :', ( PreVenResR10RC ( unidad, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot RRSUmin     :', ( CalOferResRxRC ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), '$               :', ( PreVenResRxRC ( unidad, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot RRegSec     :', ( CalOferResRegRC ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), '$               :', ( PreVenResRegRC ( unidad, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot Max RNR10min:', ( OferResNR10RC ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot Min RNR10min:', ( CalOferResNR10RC ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), '$               :', ( PreVenResNR10RC ( unidad, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot Max RNRSUmin:', ( OferResNRxRC ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot Min RNRSUmin:', ( CalOferResNRxRC ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunirc ( unidad ), '$               :', ( PreVenResNRxRC ( unidad, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
    end if
end do

write ( 1, 100 ) ''

!Hacer para todas las unidades hidro
do unidad = 1, NumUniHid
    !Hacer para todos los intervalos
    do intervalo = 1, ntintr
        if ( dispouh (unidad, intervalo ) .eq. 1 ) then
            !Reserva rodante de 10 min
            CalOferResR10H ( unidad, intervalo ) = min ( RamEmer10H ( unidad ), PotMaxUniH ( unidad, intervalo ) - PotMinUniH ( unidad, intervalo ), OferResR10H ( unidad, intervalo ) )
            !Reserva rodante suplementaria
            CalOferResRxH ( unidad, intervalo ) = min ( RamEmerxH ( unidad ), PotMaxUniH ( unidad, intervalo ) - PotMinUniH ( unidad, intervalo ), OferResRxH ( unidad, intervalo ) )
            !Reserva regulacion secundaria
            !Reserva regulacion secundaria
            if ( SiLimReg .eq. 0 ) then
                CalOferResRegH ( unidad, intervalo ) = min ( RamRegH ( unidad ), 0.5*(PotMaxUniH ( unidad, intervalo ) - PotMinUniH ( unidad, intervalo )), OferResRegH ( unidad, intervalo ) )
            else
                CalOferResRegH ( unidad, intervalo ) = min ( RamRegH ( unidad ), 0.5*(PotMaxRUniH ( unidad, intervalo ) - PotMinRUniH ( unidad, intervalo )), OferResRegH ( unidad, intervalo ) )
            endif
            !Limite minimo de reserva no rodante de 10 min
            CalOferResNR10H ( unidad, intervalo ) = PotMinUniH ( unidad, intervalo ) 
            !Limite minimo de reserva no rodante suplementaria
            CalOferResNRxH ( unidad, intervalo ) = PotMinUniH ( unidad, intervalo ) 
        endif
    end do
end do

! se determina maximo de reserva de regulacion para unidades con bandas prohibidas
if ( SiBandProh .eq. 1 ) then
!   unidades de rango continuo
    do unidad = 1 , NumUniHid
!       si la unidad tiene bandas prohibidas
        if ( NoRaOpH ( unidad ) .gt. 0 ) then
!           para todos los intervalos
            do intervalo = 1, ntintr
                if ( dispouh (unidad, intervalo ) .eq. 1 ) then
    !               para los rangos de la unidad
                    do ro = 1, NoRaOpH ( unidad )
    !                   oferta de reserva regulacion secundaria
                        CalOferRegROH ( unidad, ro, intervalo ) = min ( RamRegH ( unidad ), (RaOpSupH ( unidad, ro, intervalo ) - RaOpInfH ( unidad, ro, intervalo ))*0.5, &
                                                                    OferResRegH ( unidad, intervalo ) )
                    enddo
                endif
            enddo
        endif
    enddo
endif

write ( 1, 100 ) '-----------------------------------------------'
write ( 1, 100 ) 'CALCULO DE SERVICIOS CONEXOS DE UNIDADES HIDRO'
write ( 1, 100 ) '-----------------------------------------------'


do unidad = 1, NumUniHid
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot RR10min     :', ( CalOferResR10H ( unidad, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), '$               :', ( PreVenResR10H ( unidad, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunih( unidad ), 'Pot RRSUmin     :', ( CalOferResRxH ( unidad, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), '$               :', ( PreVenResRxH ( unidad, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot RRegSec     :', ( CalOferResRegH ( unidad, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), '$               :', ( PreVenResRegH ( unidad, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot Max RNR10min:', ( OferResNR10H ( unidad, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot Min RNR10min:', ( CalOferResNR10H ( unidad, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), '$               :', ( PreVenResNR10H ( unidad, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot Max RNRSUmin:', ( OferResNRxH ( unidad, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot Min RNRSUmin:', ( CalOferResNRxH ( unidad, intervalo) * Base, intervalo = 24 * k - 23 , 24 * k  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), '$               :', ( PreVenResNRxH ( unidad, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
        
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot RR10min     :', ( CalOferResR10H ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), '$               :', ( PreVenResR10H ( unidad, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot RRSUmin     :', ( CalOferResRxH ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), '$               :', ( PreVenResRxH ( unidad, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot RRegSec     :', ( CalOferResRegH ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), '$               :', ( PreVenResRegH ( unidad, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot Max RNR10min:', ( OferResNR10H ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot Min RNR10min:', ( CalOferResNR10H ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), '$               :', ( PreVenResNR10H ( unidad, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot Max RNRSUmin:', ( OferResNRxH ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot Min RNRSUmin:', ( CalOferResNRxH ( unidad, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
        write ( 1, 600 ) unidad, nombunih ( unidad ), '$               :', ( PreVenResNRxH ( unidad, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
    end if
end do

write ( 1, 100 ) ''

!Hacer para todas las unidades de rango discontinuo
do unidad = 1, NumUniRD
    !Hacer para todos los modos
    do modo = 1, NumModRD ( unidad )
        !Hacer para todos los intervalos
        do intervalo = 1, ntintr
            if ( DispoURD ( unidad , modo, intervalo ).eq. 1 ) then
                !Reserva rodante de 10 min
                CalOferResR10RD ( unidad, modo, intervalo ) = min ( RamEmer10RD ( unidad, modo ), PotMaxGRD ( unidad, modo, intervalo ) - PotMinGRD ( unidad, modo, intervalo ), OferResR10RD ( unidad, modo, intervalo ) )
                !Reserva rodante suplementaria
                CalOferResRxRD ( unidad, modo, intervalo ) = min ( RamEmerxRD ( unidad, modo ), PotMaxGRD ( unidad, modo, intervalo ) - PotMinGRD ( unidad, modo, intervalo ), OferResRxRD ( unidad, modo, intervalo ) )
                !Reserva regulacion secundaria
                CalOferResRegRD ( unidad, modo, intervalo ) = min ( RamRegRD ( unidad, modo ), PotMaxGRD ( unidad, modo, intervalo ) - PotMinGRD ( unidad, modo, intervalo ), OferResRegRD ( unidad, modo, intervalo ) )
                !Limite minimo de reserva no rodante de 10 min
                CalOferResNR10RD ( unidad, modo, intervalo ) = min ( PotSincNR10URD ( unidad, modo ), PotMinGRD ( unidad, modo, intervalo ) )
                !Limite minimo de reserva no rodante suplementaria
                CalOferResNRxRD ( unidad, modo, intervalo ) = min ( PotSincNRSURD ( unidad, modo ), PotMinGRD ( unidad, modo, intervalo ) )
            endif
        end do
    end do
end do

write ( 1, 100 ) '----------------------------------------------------------'
write ( 1, 100 ) 'CALCULO DE SERVICIOS CONEXOS DE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '----------------------------------------------------------'


do unidad = 1, NumUniRD
    do modo = 1, NumModRD ( unidad )
        bloque = ntintr / 24
        do k = 1, bloque
            write ( 1, 800 ) 'Modo', modo
            write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot RR10min     :', ( CalOferResR10RD ( unidad, modo, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), '$               :', ( PreVenResR10RD ( unidad, modo, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot RRSUmin     :', ( CalOferResRxRD ( unidad, modo, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), '$               :', ( PreVenResRxRD ( unidad, modo, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot RRegSec     :', ( CalOferResRegRD ( unidad, modo, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), '$               :', ( PreVenResRegRD ( unidad, modo, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot Max RNR10min:', ( OferResNR10RD ( unidad, modo, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot Min RNR10min:', ( CalOferResNR10RD ( unidad, modo, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), '$               :', ( PreVenResNR10RD ( unidad, modo, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot Max RNRSUmin:', ( OferResNRxRD ( unidad, modo, intervalo ) * Base, intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot Min RNRSUmin:', ( CalOferResNRxRD ( unidad, modo, intervalo) * Base, intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), '$               :', ( PreVenResNRxRD ( unidad, modo, intervalo ) / Base, intervalo = 24 * k - 23 , 24 * k  )
        end do
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        do modo = 1, NumModRD ( unidad )
            write ( 1, 800 ) 'Modo', modo
            write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot RR10min     :', ( CalOferResR10RD ( unidad, modo, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), '$               :', ( PreVenResR10RD ( unidad, modo, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot RRSUmin     :', ( CalOferResRxRD ( unidad, modo, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), '$               :', ( PreVenResRxRD ( unidad, modo, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot RRegSec     :', ( CalOferResRegRD ( unidad, modo, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), '$               :', ( PreVenResRegRD ( unidad, modo, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot Max RNR10min:', ( OferResNR10RD ( unidad, modo, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot Min RNR10min:', ( CalOferResNR10RD ( unidad, modo, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), '$               :', ( PreVenResNR10RD ( unidad, modo, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot Max RNRSUmin:', ( OferResNRxRD ( unidad, modo, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), 'Pot Min RNRSUmin:', ( CalOferResNRxRD ( unidad, modo, intervalo ) * Base, intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 600 ) unidad, nombunird ( unidad ), '$               :', ( PreVenResNRxRD ( unidad, modo, intervalo )/ Base, intervalo = 24 * bloque + 1 , ntintr  )
        end do
    end if
end do

write ( 1, 100 ) ''

100  format ( a )
300  format ( a10, x, i3, x, a1, i3 )      
600  format ( i3, x, a12, x, a17, x, 24 (f9.2, 2x) )   
800  format ( a4, x, i1 ) 
        
end subroutine calc_conexos
    
    
!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Se leen zonas de reserva y requerimientos de servicios conexos  *
!         solicitdos por el CENACE                                        *
!         Se forman arreglos de trabajo para relacionar unidades y        *
!         cargas zonas de reserva                                         *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Diciembre 2014                        *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
    
SUBROUTINE conexos_cenace

use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, NomZonaRes, NumZonaRes, NumGruRes, &
                  maxgrure, ApunURCxZona, UniRCxZona, NumURCxZona, NumUniRC, UniGruResRC, &
                  IslaGenRC, maxsegce, NumBloRR10, ReqResR10, ntintr, PreResRR10, NumBloR10, &
                  NumBloRSu ,NumBloRReg, ReqRes10, PreResR10, ReqResSup, PreResSup, ReqResReg, &
                  PreResReg, ReqResR10S, PreResRR10S, ReqRes10S, PreResR10S, ReqResSupS, PreResSupS, &
                  ReqResRegS, PreResRegS, numsis, EstadoIsla, Nomsis, nombunirc, Base, ApunUHxZona, &
                  NumUniHid, UniHxZona, UniGruResH, NumUHxZona, IslaGenH, nombunih, NumUniRD, UniGruResRD, &
                  NumURDxZona, UniRDxZona, ApunURDxZona, IslaGenRD, nombunird, NumCarxZona, CarxZona, ApunCarxZona, &
                  NumOferDem, CarGruRes, IslaDem, nombcar, NomEjecu, NminRegZ, zona_subsis, maxgrure
use ProblemaAUHE

IMPLICIT NONE

integer ierror, i, zona, consecutivo_RC, unidad, NumZonaConsec ( maxgrure ), zzona, bloque, & 
        intervalo, sistema, k, segmento, localidad,  consecutivo_H, consecutivo, consecutivo_RD, consecutivo_Car, carga

integer errleca, errlecb, errlecc, errlecd, errlece, errlecf, errlecg, errlech, errleci, ierror_1, ierror_2, ierror_3, &
       ierror_4, ierror_5, ierror_6, ierror_7, ierror_8

character*20 nombre

character*3000 letaux, letaux_1, letaux_2, letaux_3, letaux_4, letaux_5, letaux_6, letaux_7, letaux_8

logical entro

!Inicializacion de variables
i = 0
ierror = 0
NomZonaRes = ''
nombre = ''
zona = 0
UniRCxZona = 0
NumURCxZona = 0
NumGruRes = 0
unidad = 0
zona_subsis = 0
NumZonaConsec = 0
zzona = 0
bloque = 0
NumBloRR10 = 0
intervalo = 0
ReqResR10 = 0.0
PreResRR10 = 0.0
ReqRes10 = 0.0
PreResR10 = 0.0 
ReqResSup = 0.0 
PreResSup = 0.0 
ReqResReg = 0.0
PreResReg = 0.0 
ReqResR10S = 0.0 
PreResRR10S = 0.0 
ReqRes10S = 0.0 
PreResR10S = 0.0
ReqResSupS = 0.0
PreResSupS = 0.0
ReqResRegS = 0.0
PreResRegS = 0.0
localidad = 0
NumUHxZona = 0
UniHxZona = 0
UniRDxZona = 0
NumURDxZona = 0
NumCarxZona = 0
NumBloRR10 = maxsegce
NumBloR10 = maxsegce
NumBloRSu = maxsegce
NumBloRReg = maxsegce
NminRegZ = 0

!Hacer para el maximo numero de zonas de reserva
consecutivo_RC = 0
consecutivo_RD = 0
consecutivo_H = 0
consecutivo_Car = 0
ApunURCxZona ( 1 ) = 1
ApunURDxZona ( 1 ) = 1
ApunUHxZona ( 1 ) = 1
ApunCarxZona ( 1 ) = 1
do zona = 1, maxgrure
    entro = .false.
    !Hacer para todas las unidades activas de rango continuo
    do unidad = 1, NumUniRC
        if ( UniGruResRC ( unidad, zona ) .eq. 1 ) then
            if ( entro .eq. .false. ) then
                NumGruRes = NumGruRes + 1
                entro = .true.
            end if
            NumURCxZona ( NumGruRes ) = NumURCxZona ( NumGruRes ) + 1
            consecutivo_RC = consecutivo_RC + 1
            UniRCxZona ( consecutivo_RC ) = unidad
        end if        
    end do
    if ( entro .eq. .true. ) then 
        NumZonaConsec ( NumGruRes ) = zona
        if ( UniRCxZona ( ApunURCxZona ( NumGruRes ) ) .ne. 0 ) then
            zona_subsis ( NumGruRes ) = IslaGenRC ( UniRCxZona ( ApunURCxZona ( NumGruRes ) ) )
        end if
        ApunURCxZona ( NumGruRes + 1 ) = ApunURCxZona ( NumGruRes ) + NumURCxZona ( NumGruRes )
        ApunURDxZona ( NumGruRes + 1 ) = ApunURDxZona ( NumGruRes ) + NumURDxZona ( NumGruRes )
        ApunUHxZona ( NumGruRes + 1 ) = ApunUHxZona ( NumGruRes ) + NumUHxZona ( NumGruRes )
        ApunCarxZona ( NumGruRes + 1 ) = ApunCarxZona ( NumGruRes ) + NumCarxZona ( NumGruRes )
     end if
    !Hacer para todas las unidades activas de rango discontinuo
    do unidad = 1, NumUniRD
        if ( UniGruResRD ( unidad, zona ) .eq. 1 ) then
            if ( entro .eq. .false. ) then
                NumGruRes = NumGruRes + 1
                entro = .true.
            end if
            NumURDxZona ( NumGruRes ) = NumURDxZona ( NumGruRes ) + 1
            consecutivo_RD = consecutivo_RD + 1
            UniRDxZona ( consecutivo_RD ) = unidad
        end if        
    end do
    if ( entro .eq. .true. ) then 
        NumZonaConsec ( NumGruRes ) = zona
        if ( UniRDxZona ( ApunURDxZona ( NumGruRes ) ) .ne. 0 ) then
            zona_subsis ( NumGruRes ) = IslaGenRD ( UniRDxZona ( ApunURDxZona ( NumGruRes ) ) )
        end if
        ApunURCxZona ( NumGruRes + 1 ) = ApunURCxZona ( NumGruRes ) + NumURCxZona ( NumGruRes )
        ApunURDxZona ( NumGruRes + 1 ) = ApunURDxZona ( NumGruRes ) + NumURDxZona ( NumGruRes )
        ApunUHxZona ( NumGruRes + 1 ) = ApunUHxZona ( NumGruRes ) + NumUHxZona ( NumGruRes )
        ApunCarxZona ( NumGruRes + 1 ) = ApunCarxZona ( NumGruRes ) + NumCarxZona ( NumGruRes )
    end if
    
    !Hacer para todas las unidades activas hidro
    do unidad = 1, NumUniHid
        if ( UniGruResH ( unidad, zona ) .eq. 1 ) then
            if ( entro .eq. .false. ) then
                NumGruRes = NumGruRes + 1
                entro = .true.
            end if
            NumUHxZona ( NumGruRes ) = NumUHxZona ( NumGruRes ) + 1
            consecutivo_H = consecutivo_H + 1
            UniHxZona ( consecutivo_H ) = unidad
        end if        
    end do
    if ( entro .eq. .true. ) then !.and. NumUHxZona ( NumGruRes ) .ne. 0 ) then    
        NumZonaConsec ( NumGruRes ) = zona
        if ( UniHxZona ( ApunUHxZona ( NumGruRes ) ) .ne. 0 ) then
            zona_subsis ( NumGruRes ) = IslaGenH ( UniHxZona ( ApunUHxZona ( NumGruRes ) ) )
        end if
        ApunURCxZona ( NumGruRes + 1 ) = ApunURCxZona ( NumGruRes ) + NumURCxZona ( NumGruRes )
        ApunURDxZona ( NumGruRes + 1 ) = ApunURDxZona ( NumGruRes ) + NumURDxZona ( NumGruRes )
        ApunUHxZona ( NumGruRes + 1 ) = ApunUHxZona ( NumGruRes ) + NumUHxZona ( NumGruRes )
        ApunCarxZona ( NumGruRes + 1 ) = ApunCarxZona ( NumGruRes ) + NumCarxZona ( NumGruRes )
    end if
    !Hacer para todas las cargas participantes
    do carga = 1, NumOferDem
        if ( CarGruRes ( carga, zona ) .eq. 1 ) then
            if ( entro .eq. .false. ) then
                NumGruRes = NumGruRes + 1
                entro = .true.
            end if
            NumCarxZona ( NumGruRes ) = NumCarxZona ( NumGruRes ) + 1
            consecutivo_Car = consecutivo_Car + 1
            CarxZona ( consecutivo_Car ) = carga
        end if        
    end do
    if ( entro .eq. .true. ) then 
        NumZonaConsec ( NumGruRes ) = zona
        if ( CarxZona ( ApunCarxZona ( NumGruRes ) ) .ne. 0 ) then
            zona_subsis ( NumGruRes ) = IslaDem ( CarxZona ( ApunCarxZona ( NumGruRes ) ) )
        end if
        ApunURCxZona ( NumGruRes + 1 ) = ApunURCxZona ( NumGruRes ) + NumURCxZona ( NumGruRes )
        ApunURDxZona ( NumGruRes + 1 ) = ApunURDxZona ( NumGruRes ) + NumURDxZona ( NumGruRes )
        ApunUHxZona ( NumGruRes + 1 ) = ApunUHxZona ( NumGruRes ) + NumUHxZona ( NumGruRes )
        ApunCarxZona ( NumGruRes + 1 ) = ApunCarxZona ( NumGruRes ) + NumCarxZona ( NumGruRes )
     end if
end do

if ( NumGruRes .gt. maxgrure ) then
   bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
   call EnviaMensajeError ( bmensaje )
   bmensaje = "DE GRUPOS RESERVA"
   call ParaProceso ( bmensaje )
endif

! -----------------------------
! * Se leen datos de ZONASRES *
! -----------------------------
OPEN (UNIT = 32, FILE = rut_dat_1( 1 : long_ruta )//'ZONASRES.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	    read ( 32, 100, iostat = ierror ) letaux
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            read ( letaux, *, iostat = errleca  )  zona, nombre
            if ( errleca .ne. 0 ) go to 1001
            !Hacer para las zonas activas por sistema
            do zzona = 1, NumGruRes
                if ( zona .eq. NumZonaConsec ( zzona ) ) then
                    NomZonaRes ( zzona ) = nombre
                    NumZonaRes ( zzona ) = zona
                    exit
                end if     
            end do
	    endif
    enddo
else	
   bmensaje = 'ERROR DE LECTURA ARCHIVO ZONASRES.csv'
   call ParaProceso ( bmensaje )
end if

1001 continue
     
if ( errleca .ne. 0  ) then
   bmensaje = 'ERROR DE LECTURA ARCHIVOS ZONASRES.csv'
   call ParaProceso ( bmensaje )
endif
CLOSE ( UNIT = 32 )

i = 1
consecutivo = 1
letaux_8 = 'letaux_8'

!--------------------------------------------------------------------------------------------
!*Se leen requerimientos de reserva rodante de diez minutos del CENACE por zona de reserva  *
!*Se leen requerimientos de reserva de diez minutos del CENACE por zona de reserva          *
!*Se leen requerimientos de reserva suplementaria del CENACE por zona de reserva            *
!*Se leen requerimientos de reserva de regulacion secundaria del CENACE por zona de reserva *
!*Se leen requerimientos de minimo de unidades con reserva de regulacion por zona de reserva*
!--------------------------------------------------------------------------------------------
ierror_8 = 0
OPEN (UNIT = 34, FILE = rut_dat_1( 1 : long_ruta )//'RRERO10Z.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 35, FILE = rut_dat_1( 1 : long_ruta )//'PRERO10Z.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 36, FILE = rut_dat_1( 1 : long_ruta )//'RRE10Z.csv', IOSTAT = IERROR_2, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 37, FILE = rut_dat_1( 1 : long_ruta )//'PRE10Z.csv', IOSTAT = IERROR_3, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 38, FILE = rut_dat_1( 1 : long_ruta )//'RRESUZ.csv', IOSTAT = IERROR_4, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 39, FILE = rut_dat_1( 1 : long_ruta )//'PRESUZ.csv', IOSTAT = IERROR_5, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 40, FILE = rut_dat_1( 1 : long_ruta )//'RRERESEZ.csv', IOSTAT = IERROR_6, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 41, FILE = rut_dat_1( 1 : long_ruta )//'PRERESEZ.csv', IOSTAT = IERROR_7, STATUS='OLD', RECORDSIZE = 250)
if ( SiResRegDis .eq. 1 ) then
    OPEN (UNIT = 42, FILE = rut_dat_1( 1 : long_ruta )//'MINUNIRESREZ.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
endif
 
errleca = 0; errlecb = 0; errlecc = 0; errlecd = 0; errlece = 0; errlecf = 0; errlecg = 0; errlech = 0; errleci = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 .and. ierror_2 .eq. 0 .and. ierror_3 .eq. 0.and. ierror_4 .eq. 0.and. ierror_5 .eq. 0 &
     .and. ierror_6 .eq. 0 .and. ierror_7 .eq. 0 .and. ierror_8 .eq. 0) then
    ! Lee información hasta encontrar fin de información

    do while ( ierror .eq. 0 ) 
        if ( i .eq. NumZonaConsec ( consecutivo ) ) then
            do bloque = 1, maxsegce
                !se almacena y se lee el primer bloque
                letaux = ''; letaux_1 = ''; letaux_1 = ''; letaux_2 = ''; letaux_3 = ''; letaux_4 = ''; letaux_5 = ''; letaux_6 = ''; letaux_7 = ''
                read ( 34, 100, iostat = ierror ) letaux
                read ( 35, 100, iostat = ierror ) letaux_1
                read ( 36, 100, iostat = ierror ) letaux_2
                read ( 37, 100, iostat = ierror ) letaux_3
                read ( 38, 100, iostat = ierror ) letaux_4
                read ( 39, 100, iostat = ierror ) letaux_5
                read ( 40, 100, iostat = ierror ) letaux_6
                read ( 41, 100, iostat = ierror ) letaux_7
                if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. len_trim(letaux_1) .ne. 0 .and. len_trim(letaux_2) .ne. 0 .and. &
                      len_trim(letaux_3) .ne. 0 .and. len_trim(letaux_4) .ne. 0 .and. len_trim(letaux_5) .ne. 0 .and. len_trim(letaux_6) .ne. 0 .and. &
                      len_trim(letaux_7) .ne. 0 .and. len_trim(letaux_8) .ne. 0 ) then
                     read ( letaux, * , iostat = errleca )  ( ReqResR10 ( consecutivo, bloque, intervalo ), intervalo = 1, ntintr )
                     read ( letaux_1, *, iostat = errlecb )  ( PreResRR10 ( consecutivo, bloque, intervalo ), intervalo = 1, ntintr )
                     read ( letaux_2, *, iostat = errlecc )  ( ReqRes10 ( consecutivo, bloque, intervalo ), intervalo = 1, ntintr )
                     read ( letaux_3, *, iostat = errlecd )  ( PreResR10 ( consecutivo, bloque, intervalo ), intervalo = 1, ntintr )
                     read ( letaux_4, *, iostat = errlece )  ( ReqResSup ( consecutivo, bloque, intervalo ), intervalo = 1, ntintr )
                     read ( letaux_5, *, iostat = errlecf )  ( PreResSup ( consecutivo, bloque, intervalo ), intervalo = 1, ntintr )
                     read ( letaux_6, *, iostat = errlecg )  ( ReqResReg ( consecutivo, bloque, intervalo ), intervalo = 1, ntintr )
                     read ( letaux_7, *, iostat = errlech )  ( PreResReg ( consecutivo, bloque, intervalo ), intervalo = 1, ntintr )
                     if ( errleca .ne. 0 .or. errlecb .ne. 0  .or. errlecc .ne. 0  .or. errlecd .ne. 0  .or. errlece .ne. 0  .or. errlecf .ne. 0 &
                          .or. errlecg .ne. 0  .or. errlech .ne. 0 ) go to 1002
                end if
            end do
            i = i + 1
            consecutivo = consecutivo + 1
        else
            do bloque = 1, maxsegce
                !Se descartan los bloques
                read ( 34, 100, iostat = ierror ) letaux 
                read ( 35, 100, iostat = ierror ) letaux_1 
                read ( 36, 100, iostat = ierror ) letaux_2 
                read ( 37, 100, iostat = ierror ) letaux_3 
                read ( 38, 100, iostat = ierror ) letaux_4 
                read ( 39, 100, iostat = ierror ) letaux_5 
                read ( 40, 100, iostat = ierror ) letaux_6 
                read ( 41, 100, iostat = ierror ) letaux_7 
            end do
            i = i + 1
        end if
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRERO10Z.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERO10Z.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   if ( ierror_2 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRE10Z.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( ierror_3 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRE10Z.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( ierror_4 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRESUZ.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( ierror_5 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRESUZ.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( ierror_6 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRERESEZ.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_7 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESEZ.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( ierror_8 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO MINUNIRESREZ.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

if ( SiResRegDis .eq. 1 ) then
    do zona = 1, NumGruRes
        read ( 42, 100, iostat = ierror ) letaux_8
        if ( ierror .eq. 0 .and. len_trim(letaux_8) .ne. 0 ) then
    !        read ( letaux_8, * )  ( NminRegZ ( zona ), zona = 1, NumGruRes )
            read ( letaux_8, *, iostat = errleci )  ( NminRegZ ( zona, i ), i = 1, NTINTR )
            if ( errleci .ne. 0 ) go to 1002
        else	
          bmensaje = 'ERROR DE LECTURA ARCHIVO MINUNIRESREZ.csv'
          call ParaProceso ( bmensaje )
        end if
    enddo
endif

1002 continue
 
if ( errleca .ne. 0 .or. errlecb .ne. 0  .or. errlecc .ne. 0  .or. errlecd .ne. 0  .or. errlece .ne. 0  .or. errlecf .ne. 0 &
                          .or. errlecg .ne. 0  .or. errlech .ne. 0 .or. errleci .ne. 0 ) then
  if ( errleca .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRERO10Z.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( errlecb .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERO10Z.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   if ( errlecc .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRE10Z.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if (errlecd .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRE10Z.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( errlece .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRESUZ.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( errlecf .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRESUZ.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( errlecg .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRERESEZ.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( errlech.ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESEZ.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( errleci .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO MINUNIRESREZ.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
endif
    
CLOSE ( UNIT = 34 )
CLOSE ( UNIT = 35 )
CLOSE ( UNIT = 36 )
CLOSE ( UNIT = 37 )
CLOSE ( UNIT = 38 )
CLOSE ( UNIT = 39 )
CLOSE ( UNIT = 40 )
CLOSE ( UNIT = 41 )
if ( SiResRegDis .eq. 1 ) then
    CLOSE ( UNIT = 42 )
endif

i = 0

!------------------------------------------------------------------------------------
!*Se leen requerimientos de reserva rodante de diez minutos del CENACE por sistema  *
!*Se leen requerimientos de reserva de diez minutos del CENACE por sistema          *
!*Se leen requerimientos de reserva suplementaria del CENACE por sistema            *
!*Se leen requerimientos de reserva de regulacion secundaria del CENACE por sistema *
!------------------------------------------------------------------------------------

OPEN (UNIT = 42, FILE = rut_dat_1( 1 : long_ruta )//'RRERO10S.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 43, FILE = rut_dat_1( 1 : long_ruta )//'PRERO10S.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 44, FILE = rut_dat_1( 1 : long_ruta )//'RRE10S.csv', IOSTAT = IERROR_2, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 45, FILE = rut_dat_1( 1 : long_ruta )//'PRE10S.csv', IOSTAT = IERROR_3, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 46, FILE = rut_dat_1( 1 : long_ruta )//'RRESUS.csv', IOSTAT = IERROR_4, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 47, FILE = rut_dat_1( 1 : long_ruta )//'PRESUS.csv', IOSTAT = IERROR_5, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 48, FILE = rut_dat_1( 1 : long_ruta )//'RRERESES.csv', IOSTAT = IERROR_6, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 49, FILE = rut_dat_1( 1 : long_ruta )//'PRERESES.csv', IOSTAT = IERROR_7, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0; errlecc = 0; errlecd = 0; errlece = 0; errlecf = 0; errlecg = 0; errlech = 0; errleci = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 .and. ierror_2 .eq. 0 .and. ierror_3 .eq. 0.and. ierror_4 .eq. 0.and. ierror_5 .eq. 0 &
     .and. ierror_6 .eq. 0 .and. ierror_7 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 ) 
        do sistema = 1, numsis
            if ( EstadoIsla ( sistema ) .eq. 1 ) then
                i = i + 1
                do bloque = 1, maxsegce
                    !se almacena y se lee el primer bloque
                    read ( 42, 100, iostat = ierror ) letaux
                    read ( 43, 100, iostat = ierror ) letaux_1
                    read ( 44, 100, iostat = ierror ) letaux_2
                    read ( 45, 100, iostat = ierror ) letaux_3
                    read ( 46, 100, iostat = ierror ) letaux_4
                    read ( 47, 100, iostat = ierror ) letaux_5
                    read ( 48, 100, iostat = ierror ) letaux_6
                    read ( 49, 100, iostat = ierror ) letaux_7
                     if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. len_trim(letaux_1) .ne. 0 .and. len_trim(letaux_2) .ne. 0 .and. &
                          len_trim(letaux_3) .ne. 0 .and. len_trim(letaux_4) .ne. 0 .and. len_trim(letaux_5) .ne. 0 .and. len_trim(letaux_6) .ne. 0 .and. &
                          len_trim(letaux_7) .ne. 0 ) then
                         read ( letaux, *, iostat = errleca )  ( ReqResR10S ( i, bloque, intervalo ), intervalo = 1, ntintr )
                         read ( letaux_1, *, iostat = errlecb )  ( PreResRR10S ( i, bloque, intervalo ), intervalo = 1, ntintr )
                         read ( letaux_2, *, iostat = errlecc )  ( ReqRes10S ( i, bloque, intervalo ), intervalo = 1, ntintr )
                         read ( letaux_3, *, iostat = errlecd )  ( PreResR10S ( i, bloque, intervalo ), intervalo = 1, ntintr )
                         read ( letaux_4, *, iostat = errlece )  ( ReqResSupS ( i, bloque, intervalo ), intervalo = 1, ntintr )
                         read ( letaux_5, *, iostat = errlecf )  ( PreResSupS ( i, bloque, intervalo ), intervalo = 1, ntintr )
                         read ( letaux_6, *, iostat = errlecg )  ( ReqResRegS ( i, bloque, intervalo ), intervalo = 1, ntintr )
                         read ( letaux_7, *, iostat = errlech )  ( PreResRegS ( i, bloque, intervalo ), intervalo = 1, ntintr )
                         if ( errleca .ne. 0 .or. errlecb .ne. 0  .or. errlecc .ne. 0  .or. errlecd .ne. 0  .or. errlece .ne. 0  .or. errlecf .ne. 0 &
                              .or. errlecg .ne. 0  .or. errlech .ne. 0 ) go to 1003
                         
                    end if
                end do
            else
                do bloque = 1, maxsegce
                    !Se descartan los bloques
                    read ( 42, 100, iostat = ierror ) letaux 
                    read ( 43, 100, iostat = ierror ) letaux_1 
                    read ( 44, 100, iostat = ierror ) letaux_2 
                    read ( 45, 100, iostat = ierror ) letaux_3 
                    read ( 46, 100, iostat = ierror ) letaux_4 
                    read ( 47, 100, iostat = ierror ) letaux_5 
                    read ( 48, 100, iostat = ierror ) letaux_6 
                    read ( 49, 100, iostat = ierror ) letaux_7 
                end do
            end if
        end do
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRERO10S.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERO10S.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   if ( ierror_2 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRE10S.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( ierror_3 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRE10S.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( ierror_4 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRESUS.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( ierror_5 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRESUS.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( ierror_6 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRERESES.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_7 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESES.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1003 continue
     
if ( errleca .ne. 0 .or. errlecb .ne. 0  .or. errlecc .ne. 0  .or. errlecd .ne. 0  .or. errlece .ne. 0  .or. errlecf .ne. 0 &
                          .or. errlecg .ne. 0  .or. errlech .ne. 0 ) then
  if ( errleca .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRERO10Z.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( errlecb .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERO10Z.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   if ( errlecc .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRE10Z.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if (errlecd .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRE10Z.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( errlece .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRESUZ.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( errlecf .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRESUZ.csv'
     call EnviaMensajeError ( bmensaje )
   endif
  if ( errlecg .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RRERESEZ.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( errlech.ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESEZ.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
endif
                        
CLOSE ( UNIT = 42 )
CLOSE ( UNIT = 43 )
CLOSE ( UNIT = 44 )
CLOSE ( UNIT = 45 )
CLOSE ( UNIT = 46 )
CLOSE ( UNIT = 47 )
CLOSE ( UNIT = 48 )
CLOSE ( UNIT = 49 )

!Se escriben datos de servicios conexos
write ( 1, 100 ) '-----------------------------------------------------'
write ( 1, 100 ) 'REQUERIMIENTO DE SERVICIOS CONEXOS DEL CENACE POR ZONA'
write ( 1, 100 ) '-----------------------------------------------------'

!Hacer para todos los sistemas
do zzona = 1, NumGruRes
    !Hacer para todos los bloques de 24 intervalos
    bloque = ntintr / 24
    write ( 1, 100 ) 'RESERVA RODANTE DE DIEZ MINUTOS'
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        !Hacer para todos los segmentos
        do segmento = 1, maxsegce
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), 'MW', ( ReqResR10 ( zzona, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), '$/MWh', ( PreResRR10 ( zzona, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do        
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        do segmento = 1, maxsegce
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), 'MW', ( ReqResR10 ( zzona, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), '$/MWh', ( PreResRR10 ( zzona, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end do
    end if
    write ( 1, 100 ) 'RESERVA DE DIEZ MINUTOS'
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        !Hacer para todos los segmentos
        do segmento = 1, maxsegce
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), 'MW', ( ReqRes10 ( zzona, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), '$/MWh', ( PreResR10 ( zzona, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do        
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        do segmento = 1, maxsegce
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), 'MW', ( ReqRes10 ( zzona, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), '$/MWh', ( PreResR10 ( zzona, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end do
    end if
    write ( 1, 100 ) 'RESERVA SUPLEMENTARIA'
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        !Hacer para todos los segmentos
        do segmento = 1, maxsegce
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), 'MW', ( ReqResSup ( zzona, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), '$/MWh', ( PreResSup ( zzona, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do        
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        do segmento = 1, maxsegce
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), 'MW', ( ReqResSup ( zzona, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), '$/MWh', ( PreResSup ( zzona, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end do
    end if
    write ( 1, 100 ) 'RESERVA DE REGULACION SECUNDARIA'
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        !Hacer para todos los segmentos
        do segmento = 1, maxsegce
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), 'MW', ( ReqResReg ( zzona, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), '$/MWh', ( PreResReg ( zzona, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do        
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        do segmento = 1, maxsegce
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), 'MW', ( ReqResReg ( zzona, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 500 ) zzona, NomZonaRes ( zzona ), '$/MWh', ( PreResReg ( zzona, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end do
    end if
    write ( 1, 100 ) ''
!    write ( 1, 600 ) zzona, NomZonaRes ( zzona ), 'Nz', NminRegZ ( zzona)
    write ( 1, 600 ) zzona, NomZonaRes ( zzona ), 'Nz', NminRegZ ( zzona, 1 )
    write ( 1, 100 ) ''
end do

write ( 1, 100 ) ''

i = 0

!Escalamiento
ReqResR10 = ReqResR10 / Base
PreResRR10 = PreResRR10 * Base
ReqRes10 = ReqRes10 / Base
PreResR10 = PreResR10 * Base
ReqResSup = ReqResSup / Base
PreResSup = PreResSup * Base
ReqResReg = ReqResReg / Base
PreResReg = PreResReg * Base


write ( 1, 100 ) '--------------------------------------------------------'
write ( 1, 100 ) 'REQUERIMIENTO DE SERVICIOS CONEXOS DEL CENACE POR SISTEMA'
write ( 1, 100 ) '--------------------------------------------------------'

!Hacer para todas las zonas
do sistema = 1, numsis
    if ( EstadoIsla ( sistema ) .eq. 1 ) then
        i = i + 1
        !Hacer para todos los bloques de 24 intervalos
        bloque = ntintr / 24
        write ( 1, 100 ) 'RESERVA RODANTE DE DIEZ MINUTOS'
        do k = 1, bloque
            write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
            !Hacer para todos los segmentos
            do segmento = 1, maxsegce
                write ( 1, 500 ) sistema, Nomsis ( sistema ), 'MW', ( ReqResR10S ( i, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                write ( 1, 500 ) sistema, Nomsis ( sistema ), '$/MWh', ( PreResRR10S ( i, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do        
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
            write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
            do segmento = 1, maxsegce
                write ( 1, 500 ) sistema, Nomsis ( sistema ), 'MW', ( ReqResR10S ( i, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                write ( 1, 500 ) sistema, Nomsis ( sistema ), '$/MWh', ( PreResRR10S ( i, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end do
        end if
        write ( 1, 100 ) 'RESERVA DE DIEZ MINUTOS'
        do k = 1, bloque
            write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
            !Hacer para todos los segmentos
            do segmento = 1, maxsegce
                write ( 1, 500 ) sistema, Nomsis ( sistema ), 'MW', ( ReqRes10S ( i, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                write ( 1, 500 ) sistema, Nomsis ( sistema ), '$/MWh', ( PreResR10S ( i, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do        
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
            write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
            do segmento = 1, maxsegce
                write ( 1, 500 ) sistema, Nomsis ( sistema ), 'MW', ( ReqRes10S ( i, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr )
                write ( 1, 500 ) sistema, Nomsis ( sistema ), '$/MWh', ( PreResR10S ( i, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end do
        end if
        write ( 1, 100 ) 'RESERVA SUPLEMENTARIA'
        do k = 1, bloque
            write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
            !Hacer para todos los segmentos
            do segmento = 1, maxsegce
                write ( 1, 500 ) sistema, Nomsis ( sistema ), 'MW', ( ReqResSupS ( i, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                write ( 1, 500 ) sistema, Nomsis ( sistema ), '$/MWh', ( PreResSupS ( i, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do        
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
            write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
            do segmento = 1, maxsegce
                write ( 1, 500 ) sistema, Nomsis ( sistema ), 'MW', ( ReqResSupS ( i, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                write ( 1, 500 ) sistema, Nomsis ( sistema ), '$/MWh', ( PreResSupS ( i, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end do
        end if
        write ( 1, 100 ) 'RESERVA DE REGULACION SECUNDARIA'
        do k = 1, bloque
            write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
            !Hacer para todos los segmentos
            do segmento = 1, maxsegce
                write ( 1, 500 ) sistema, Nomsis ( sistema ), 'MW', ( ReqResRegS ( i, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                write ( 1, 500 ) sistema, Nomsis ( sistema ), '$/MWh', ( PreResRegS ( i, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do        
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
            write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
            do segmento = 1, maxsegce
                write ( 1, 500 ) sistema, Nomsis ( sistema ), 'MW', ( ReqResRegS ( i, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                write ( 1, 500 ) sistema, Nomsis ( sistema ), '$/MWh', ( PreResRegS ( i, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end do
        end if
    end if
end do

write ( 1, 100 ) ''

!Escalamiento
ReqResR10S = ReqResR10S / Base
PreResRR10S = PreResRR10S * Base
ReqRes10S = ReqRes10S / Base
PreResR10S = PreResR10S * Base
ReqResSupS = ReqResSupS / Base
PreResSupS = PreResSupS * Base
ReqResRegS = ReqResRegS / Base
PreResRegS = PreResRegS * Base


!Imprime la lista de unidades por zona
write ( 1, 100 ) '-------------------------------------------------------'
write ( 1, 100 ) 'LISTA DE UNIDADES DE RANGO CONTINUO POR ZONA DE RESERVA'
write ( 1, 100 ) '-------------------------------------------------------'
!Hacer para todas las zonas activas
do zona = 1, NumGruRes
    if ( NumURCxZona ( zona ) .ne. 0 ) then
        write ( 1, 100 ) ''
        write ( 1, 200 ) NomZonaRes ( zona )
        !localidad
        do localidad = ApunURCxZona ( zona ), ApunURCxZona ( zona ) + NumURCxZona ( zona ) - 1
            write ( 1, 400 ) UniRCxZona ( localidad ), nombunirc ( UniRCxZona ( localidad ) )
            continue
        end do
    end if
end do

!Imprime la lista de unidades por zona
write ( 1, 100 ) '----------------------------------------------------------'
write ( 1, 100 ) 'LISTA DE UNIDADES DE RANGO DISCONTINUO POR ZONA DE RESERVA'
write ( 1, 100 ) '----------------------------------------------------------'
!Hacer para todas las zonas activas
do zona = 1, NumGruRes
    if ( NumURDxZona ( zona ) .ne. 0 ) then
        write ( 1, 100 ) ''
        write ( 1, 200 ) NomZonaRes ( zona )
        !localidad
        do localidad = ApunURDxZona ( zona ), ApunURDxZona ( zona ) + NumURDxZona ( zona ) - 1
            write ( 1, 400 ) UniRDxZona ( localidad ), nombunird ( UniRDxZona ( localidad ) )
            continue
        end do
    end if
end do

!Imprime la lista de unidades por zona
write ( 1, 100 ) '-------------------------------------------'
write ( 1, 100 ) 'LISTA DE UNIDADES HIDRO POR ZONA DE RESERVA'
write ( 1, 100 ) '-------------------------------------------'
!Hacer para todas las zonas activas
do zona = 1, NumGruRes
    !localidad
    if ( NumUHxZona ( zona ) .ne. 0 ) then
        write ( 1, 100 ) ''
        write ( 1, 200 ) NomZonaRes ( zona )
        do localidad = ApunUhxZona ( zona ), ApunUhxZona ( zona ) + NumUhxZona ( zona ) - 1
            write ( 1, 400 ) UnihxZona ( localidad ), nombunih ( UnihxZona ( localidad ) )
            continue
        end do
    end if
end do

!Imprime la lista de cargas por zona
write ( 1, 100 ) '--------------------------------------------------'
write ( 1, 100 ) 'LISTA DE CARGAS PARTICIPANTES POR ZONA DE RESERVA'
write ( 1, 100 ) '--------------------------------------------------'
!Hacer para todas las zonas activas
do zona = 1, NumGruRes
    if ( NumCarxZona ( zona ) .ne. 0 ) then
        write ( 1, 100 ) ''
        write ( 1, 200 ) NomZonaRes ( zona )
        !localidad
        do localidad = ApunCarxZona ( zona ), ApunCarxZona ( zona ) + NumCarxZona ( zona ) - 1
            write ( 1, 400 ) CarxZona ( localidad ), nombcar ( CarxZona ( localidad ) )
            continue
        end do
    end if
end do




100 format ( a )
200 format ( a15 )    
300 format ( a10, x, i3, x, a1, i3 )      
400 format ( i3, 5x, a12 )      
500 format ( i3, x, a12, x, a5, x, 24 (f9.2, 2x) )    
600 format ( i3, x, a12, x, a5, x, i4 )    

end subroutine conexos_cenace
    
!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Lee datos de cargas y las relacionas con los subsistemas        *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Noviembre 2014                        *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                   Julio 2017                            *
!**************************************************************************
    
SUBROUTINE lee_nodo_intercambio

use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, NumNodInt, NodoInt, IslaNodInt, &
                  NodInt_area, PotNodInt, ntintr, NumNodos, NumSis, nodo_subsis, &
                  NomSis, EstadoIsla, Nodo_Area, maxint, base, maxnod, &
                  NomEjecu, TipoLec
!
IMPLICIT NONE

integer ierror, tempnodint ( maxint ), intervalo, nodo, subsistema, i, bloque, k, errleca, errlecb, ierror_1

real*8 potinttemp ( maxint )

character*3000 letaux, letaux_1

character*1 dummy

character*5 letr

!Inicializacion de variables
tempnodint = 0
potinttemp = 0.0
NumNodInt = 0
NodoInt = 0
IslaNodInt = 0
NodInt_area = ''
PotNodInt = 0.0
intervalo = 0
nodo = 0
bloque = 0
subsistema = 0
k = 0
!
write ( 1, 100 ) '---------------------------------------' 
write ( 1, 100 ) 'NODOS DE INTERCAMBIO DE POTENCIA ACTIVA' 
write ( 1, 100 ) '---------------------------------------'
! ------------------------------
! * Se leen datos de NOINPA    *
! * Se leen datos de NODOSINPA *
! ------------------------------

OPEN (UNIT = 50, FILE = rut_dat_1( 1 : long_ruta )//'NOINPA'//trim(TipoLec)//'.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 194, FILE = rut_dat_1( 1 : long_ruta )//'NODOSINPA.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then    
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	    read ( 50, 100, iostat = ierror ) letaux
        read ( 194, 100, iostat = ierror ) letaux_1
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. len_trim(letaux_1) .ne. 0 ) then
            read ( letaux, *, iostat = errleca ) dummy, ( potinttemp ( intervalo ), intervalo = 1, ntintr )
            read ( letaux_1, *, iostat = errlecb ) ( tempnodint ( intervalo ), intervalo = 1, ntintr )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1001
            !Hacer para todos los nodos
            do nodo = 1, NumNodos
                !Se asigna subsistema con el nodo del primer intervalo
                if ( tempnodint ( 1 ) .eq. nodo ) then
                    !Hacer para todos los subsistemas
                    do subsistema = 1, numsis
                        if ( nodo_subsis ( nodo ) .eq. nomsis ( subsistema ) ) then
                            !Ver si el subsistema esta activo
                            if ( EstadoIsla ( subsistema ) .eq. 1 ) then
                                NumNodInt = NumNodInt + 1                                
                                NodoInt ( NumNodInt, : ) = tempnodint ( : )
                                IslaNodInt ( NumNodInt ) = subsistema
                                NodInt_area ( NumNodInt ) = nodo_area ( nodo )     
                                do i = 1, ntintr
                                    PotNodInt ( NumNodInt, i ) = potinttemp ( i )
                                end do
                                !se escribe a debugger
                                write ( 1, 100 ) 'Numero   Nodo  Subsistema      Area' 
                                write ( 1, 200 ) NumNodInt, NodoInt ( NumNodInt, 1 ), IslaNodInt ( NumNodInt ), NodInt_area ( NumNodInt )
                                bloque = ntintr / 24
                                do k = 1, bloque
                                    write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
                                    write ( 1, 400 ) ( PotNodInt ( NumNodInt, intervalo ), intervalo = 24 * k - 23 , 24 * k )
                                end do
                                if ( ntintr - 24 * bloque .gt. 0 ) then
                                    write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
                                    write ( 1, 400 ) ( PotNodInt ( NumNodInt, intervalo ), intervalo = 24 * bloque + 1 , ntintr )
                                end if
                                exit
                            end if
                        end if
                    end do
                end if
            end do
	    endif
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO NOINPA'//trim(TipoLec)//'.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSINPA.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) NumNodInt
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS NOINPA'//trim(TipoLec)//'.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSINPA.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = ''
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 50 )
close ( UNIT = 194 )

write ( 1, 100 ) ''

write ( 1, 100 ) '------------------------------'
write ( 1, 100 ) 'NODOS DE INTERCAMBIO POR HORA'
write ( 1, 100 ) '------------------------------'
!Hacer para todos los nodos de intercambio
do nodo = 1, NumNodInt
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        write ( 1, 500 ) nodo, ( NodoInt ( nodo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        write ( 1, 500 ) nodo, ( NodoInt ( nodo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
    end if
end do

write ( 1, 100 ) ''

ierror = 0
i = 0


!Escalamiento 
PotNodInt = PotNodInt / Base


!PotNodInt = 0.0

100 format ( a )
200 format ( i4, 5x, i4, 4x, i2, 12x, a20 )  
300  format ( a10, x, i3, x, a1, i3 )      
400  format ( 24 (f9.2, 2x) )    
500 format ( 25 (i4, 2x) )          


END SUBROUTINE lee_nodo_intercambio


    
!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Calcula la disponibilidad y la coordinabilidad de las unidades  *
!         Hidro en base a sus ofertas de energia                          *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Enero 2015                            *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
    
SUBROUTINE Dispo_Coord_H

use ParAUHE, only: DispoUH, CoordUH, NumUniHid, ntintr, PotMaxUniH, PotMinUniH, &
                  nombunih, Base, CompSincH

use ProblemaAUHE, only: SiModHid

use ParAuHeHidro, only: potfijah

IMPLICIT NONE

integer unidad, intervalo, bloque, k

!DispoUH = 0
CoordUH = 0
potfijah = 0.0

!Hacer para todas las unidades de rango continuo
do unidad = 1, NumUniHid
    !Hacer para todos los intervalos
    do intervalo = 1, ntintr
        !Para que unidad sea no disponible su potencia maxima esta en cero
!        if ( ( PotMaxUniH ( unidad, intervalo ) .eq. 0.0 .or. CompSincH ( unidad, intervalo ) .eq. 1 ) .and. SiModHid .eq. 1 ) then
        if ( PotMaxUniH ( unidad, intervalo ) .eq. 0.0 .or. CompSincH ( unidad, intervalo ) .eq. 1  ) then
            !No disponible
            DispoUH ( unidad, intervalo ) = 0
        else
            !Disponible
!            DispoUH ( unidad, intervalo ) = 1
        end if
!       si es un compensador sincrono
        if ( CompSincH ( unidad, intervalo ) .eq. 2 ) then
            PotMinUniH ( unidad, intervalo ) = 0
        endif
        !Para que unidad sea no coordinable su potencia minima y maxima son iguales
        if ( PotMaxUniH ( unidad, intervalo ) .eq. PotMinUniH ( unidad, intervalo ) ) then
            !No coordinable
            CoordUH ( unidad, intervalo ) = 0
            potfijah ( unidad, intervalo ) = PotMinUniH ( unidad, intervalo )
        else
            !Coordinable
            CoordUH ( unidad, intervalo ) = 1
        end if        
    end do
end do

!write ( 1, 100 ) '-----------------------------'
!write ( 1, 100 ) 'DISPONIBILIDAD UNIDADES HIDRO'
!write ( 1, 100 ) '-----------------------------'
do unidad = 1, NumUniHid*0
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        write ( 1, 900 ) unidad, nombunih ( unidad ), 'DISP:', ( DispoUH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        write ( 1, 900 ) unidad, nombunih ( unidad ), 'DISP:', ( DispoUH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
    end if
end do

!write ( 1, 100 ) ''

write ( 1, 100 ) '------------------------------'
write ( 1, 100 ) 'COORDINABILIDAD UNIDADES HIDRO'
write ( 1, 100 ) '------------------------------'
do unidad = 1, NumUniHid
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        write ( 1, 900 ) unidad, nombunih ( unidad ), 'COOR:', ( CoordUH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        write ( 1, 900 ) unidad, nombunih ( unidad ), 'COOR:', ( CoordUH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
    end if
end do

write ( 1, 100 ) ''

100  format ( a )
300  format ( a10, x, i3, x, a1, i3 )      
900  format ( i3, x, a15, x, a4, x, 24 (i1, 2x) )  

end subroutine Dispo_Coord_H
    

    
!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Calcula la disponibilidad y la coordinabilidad de los mdoos     *
!         de las unidades de rango continuo en base a sus ofertas         *
!         de energia                                                      *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Febrero 2015                          *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
    
SUBROUTINE Dispo_Coord_RD

use ParAUHE, only: DispoURD, CoordURD, NumUniRD, ntintr, PotMaxGRD, PotMinGRD, &
                  nombunird, NumModRD
!
IMPLICIT NONE

integer unidad, modo, intervalo, bloque, k


!Hacer para todas las unidades de rango discontinuo
do unidad = 1, NumUniRD
    !Hacer para todos los modos de operacion
    do modo = 1, NumModRD(unidad)
        !Hacer para todos los intervalos
        do intervalo = 1, ntintr
            !Para que el modo de una unidad sea no disponible su potencia maxima esta en cero
            if ( PotMaxGRD ( unidad, modo, intervalo ) .eq. 0.0 ) then
                !No disponible
                DispoURD ( unidad, modo, intervalo ) = 0
            else
                !Disponible
                DispoURD ( unidad, modo, intervalo ) = 1
            end if
            !Para que el modod de una unidad sea no coordinable su potencia minima y maxima son iguales
            if ( PotMaxGRD ( unidad, modo, intervalo ) .eq. PotMinGRD ( unidad, modo, intervalo ) ) then
                !No coordinable
                CoordURD ( unidad, modo, intervalo ) = 0
            else
                !Coordinable
                CoordURD ( unidad, modo, intervalo ) = 1
            end if        
        end do
    end do
end do

!Siempre poner disponible y coordinable el modo apagado
DispoURD ( :, 1, : ) = 1
CoordURD ( :, 1, : ) = 1

write ( 1, 100 ) '-----------------------------------------------------'
write ( 1, 100 ) 'DISPONIBILIDAD DE MODOS DE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '-----------------------------------------------------'
!Hacer para todas las unidades
do unidad = 1, NumUniRD
    !Hacer para todos los modos
    do modo = 1, NumModRD(unidad)
        bloque = ntintr / 24
        do k = 1, bloque
            if ( modo .eq. 1 ) then
                write ( 1, 600 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
                write ( 1, 800 ) 'Modo', modo
                write ( 1, 900 ) unidad, nombunird ( unidad ), 'DISP:', ( DispoURD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            else
                write ( 1, 800 ) 'Modo', modo
                write ( 1, 1300 ) 'DISP:', ( DispoURD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end if
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
            if ( modo .eq. 1 ) then
                write ( 1, 600 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
                write ( 1, 800 ) 'Modo', modo
                write ( 1, 900 ) unidad, nombunird ( unidad ), 'DISP:', ( DispoURD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            else
                write ( 1, 800 ) 'Modo', modo
                write ( 1, 1300 ) 'DISP:', ( DispoURD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end if
        end if
    end do
end do

write ( 1, 100 ) ''

write ( 1, 100 ) '------------------------------------------------------'
write ( 1, 100 ) 'COORDINABILIDAD DE MODOS DE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '------------------------------------------------------'
!Hacer para todas las unidades
do unidad = 1, NumUniRD
    !Hacer para todos los modos
    do modo = 1, NumModRD(unidad)
        bloque = ntintr / 24
        do k = 1, bloque
            if ( modo .eq. 1 ) then
                write ( 1, 600 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
                write ( 1, 800 ) 'Modo', modo
                write ( 1, 900 ) unidad, nombunird ( unidad ), 'COOR:', ( CoordURD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            else
                write ( 1, 800 ) 'Modo', modo
                write ( 1, 1300 ) 'COOR:', ( CoordURD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end if
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
            if ( modo .eq. 1 ) then
                write ( 1, 600 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
                write ( 1, 800 ) 'Modo', modo
                write ( 1, 900 ) unidad, nombunird ( unidad ), 'COOR:', ( CoordURD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            else
                write ( 1, 800 ) 'Modo', modo
                write ( 1, 1300 ) 'COOR:', ( CoordURD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end if
        end if
    end do
end do

write ( 1, 100 ) ''

100 format ( a )
600  format ( a10, x, i3, x, a1, i3 )     
800  format ( a4, x, i1 )        
900  format ( i3, x, a5, x, a5, x, 24 (i1, 2x) )  
1300 format ( 10x, a5, x, 24 (i1, 2x) ) 
     
end subroutine Dispo_Coord_RD
    



    
!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Se leen grupos termicos con limitacion de energia  y            * 
!         requerimientos de los grupos solicitdos por el CENACE           *
!         Se forman arreglos de trabajo para relacionar unidades y        *
!         grupos con limitacion de energia                                *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Marzo 2015                            *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
    
SUBROUTINE grupos_cenace

use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, NumUniRC, NumGruUTer, NumURCxGrupo, &
                  NumURDxGrupo, ApunURCxGrupo, ApunURDxGrupo, maxgrute, UniGruTerRC, &
                  UniGruTerRD, UniRCxGrupo, IslaGenRC, NumUniRD, UniRDxGrupo, IslaGenRD, &
                  LimEnerIUTermo, LimEnerSUTermo, NomGpoTer, NumGpoTer, base, nombunirc, &
                  nombunird, NomEjecu, DURDIA, maxdia

!
IMPLICIT NONE

integer d, dia, ierror, i, grupo, unidad, consecutivo_RC, consecutivo_RD, NumGrupoConsec ( maxgrute ), &
        grupo_subsis ( maxgrute ), ggrupo, localidad, errleca

real*8 minimo (maxdia), maximo (maxdia)

character*20 nombre

character*3000 letaux

character*5 letr

logical entro

!Inicializacion de variables
i = 0
ierror = 0
grupo = 0
unidad = 0
NumGruUTer = 0
NumURCxGrupo = 0
NumURDxGrupo = 0
grupo_subsis = 0 
localidad = 0

!Hacer para el maximo numero de zonas de reserva
consecutivo_RC = 0
consecutivo_RD = 0

ApunURCxGrupo ( 1 ) = 1
ApunURDxGrupo ( 1 ) = 1

do grupo = 1, maxgrute
    entro = .false.
    !Hacer para todas las unidades activas de rango continuo
    do unidad = 1, NumUniRC
        if ( UniGruTerRC ( unidad, grupo ) .eq. 1 ) then
            if ( entro .eq. .false. ) then
                NumGruUTer = NumGruUTer + 1
                entro = .true.
            end if
            NumURCxGrupo ( NumGruUTer ) = NumURCxGrupo ( NumGruUTer ) + 1
            consecutivo_RC = consecutivo_RC + 1
            UniRCxGrupo ( consecutivo_RC ) = unidad
        end if        
    end do
    if ( entro .eq. .true. ) then 
        NumGrupoConsec ( NumGruUTer ) = grupo
        if ( UniRCxGrupo ( ApunURCxGrupo ( NumGruUTer ) ) .ne. 0 ) then
            grupo_subsis ( NumGruUTer ) = IslaGenRC ( UniRCxGrupo ( ApunURCxGrupo ( NumGruUTer ) ) )
        end if
        ApunURCxGrupo ( NumGruUTer + 1 ) = ApunURCxGrupo ( NumGruUTer ) + NumURCxGrupo ( NumGruUTer )
        ApunURDxGrupo ( NumGruUTer + 1 ) = ApunURDxGrupo ( NumGruUTer ) + NumURDxGrupo ( NumGruUTer )
     end if
    !Hacer para todas las unidades activas de rango discontinuo
    do unidad = 1, NumUniRD
        if ( UniGruTerRD ( unidad, grupo ) .eq. 1 ) then
            if ( entro .eq. .false. ) then
                NumGruUTer = NumGruUTer + 1
                entro = .true.
            end if
            NumURDxGrupo ( NumGruUTer ) = NumURDxGrupo ( NumGruUTer ) + 1
            consecutivo_RD = consecutivo_RD + 1
            UniRDxGrupo ( consecutivo_RD ) = unidad
        end if        
    end do
    if ( entro .eq. .true. ) then 
        NumGrupoConsec ( NumGruUTer ) = grupo
        if ( UniRDxGrupo ( ApunURDxGrupo ( NumGruUTer ) ) .ne. 0 ) then
            grupo_subsis ( NumGruUTer ) = IslaGenRD ( UniRDxGrupo ( ApunURDxGrupo ( NumGruUTer ) ) )
        end if
        ApunURCxGrupo ( NumGruUTer + 1 ) = ApunURCxGrupo ( NumGruUTer ) + NumURCxGrupo ( NumGruUTer )
        ApunURDxGrupo ( NumGruUTer + 1 ) = ApunURDxGrupo ( NumGruUTer ) + NumURDxGrupo ( NumGruUTer )
    end if  
end do

! -----------------------------
! * Se leen datos de GPOUTER *
! -----------------------------
OPEN (UNIT = 116, FILE = rut_dat_1( 1 : long_ruta )//'GPOUTER.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	    read ( 116, 100, iostat = ierror ) letaux
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            read ( letaux, *, iostat = errleca )  grupo, nombre,( minimo (dia), maximo (dia) , dia = 1, durdia )
            if ( errleca .ne. 0 ) go to 1001
            !Hacer para las zonas activas por sistema
            do ggrupo = 1, NumGruUTer
                if ( grupo .eq. NumGrupoConsec ( ggrupo ) ) then
                    NomGpoTer ( ggrupo ) = nombre
                    NumGpoTer ( ggrupo ) = grupo
                    do d = 1, DURDIA
                        LimEnerIUTermo ( ggrupo, d ) = minimo ( d )
                        LimEnerSUTermo ( ggrupo, d ) = maximo ( d )
                    enddo
                    exit
                end if     
            end do
	    endif
    enddo
else	
   bmensaje = 'ERROR DE LECTURA ARCHIVO GPOUTER.csv'
   call ParaProceso ( bmensaje )
end if

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) grupo
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO GPOUTER.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'GRUPO: ['//trim(letr)//'] '//trim(nombre)
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 116 )

!Imprime la lista de unidades por grupo con limitacion de energia
write ( 1, 100 ) '-----------------------------------------------------'
write ( 1, 100 ) 'LISTA DE UNIDADES POR GRUPO CON LIMITACION DE ENERGIA'
write ( 1, 100 ) '-----------------------------------------------------'
!Hacer para todas las zonas activas
do grupo = 1, NumGruUTer
    if ( NumURCxGrupo ( grupo ) .ne. 0 .or. NumURDxGrupo ( grupo ) .ne. 0 ) then
        write ( 1, 100 ) ''
        write ( 1, 100 ) 'Nombre          Minimos  Maximos (GWH)'
        write ( 1, 200 ) NomGpoTer ( grupo ), ( LimEnerIUTermo (grupo,dia), LimEnerSUTermo (grupo, dia) , dia = 1, durdia )
        !localidad
        if ( NumURCxGrupo ( grupo ) .ne. 0 ) then
            do localidad = ApunURCxGrupo ( grupo ), ApunURCxGrupo ( grupo ) + NumURCxGrupo ( grupo ) - 1
                write ( 1, 300 ) UniRCxGrupo ( localidad ), nombunirc ( UniRCxGrupo ( localidad ) )               
            end do
        end if
        if ( NumURDxGrupo ( grupo ) .ne. 0 ) then
            do localidad = ApunURDxGrupo ( grupo ), ApunURDxGrupo ( grupo ) + NumURDxGrupo ( grupo ) - 1
                write ( 1, 300 ) UniRDxGrupo ( localidad ), nombunird ( UniRDxGrupo ( localidad ) )               
            end do
        end if
    end if
end do

!Escalamiento

LimEnerIUTermo = ( LimEnerIUTermo * 1000.00 ) / Base
LimEnerSUTermo = ( LimEnerSUTermo * 1000.00 ) / Base  

100 format ( a )
200 format ( a12, x, 7( 2(f9.2, 2x)) )
300 format ( i3, x, a12 ) 

end subroutine grupos_cenace


!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Se leen restricciones de unidades con limitacion de energia.    * 
!         requerimientos de los grupos solicitdos por el CENACE           *
!         Se forman arreglos de trabajo para relacionar unidades y        *
!         restrcciones con limitacion de energia                          *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Marzo 2015                            *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!        Jose Luis Ceciliano Meza   Junio 2019                            *
!**************************************************************************
    
SUBROUTINE grupos_cenace_new

use ParAUHE

IMPLICIT NONE

integer ierror, i, grupo, unidad, consecutivo_RC, consecutivo_RD, NumGrupoConsec ( maxgrute ), &
        grupo_subsis ( maxgrute ), localidad

logical entro

!Inicializacion de variables
i = 0
ierror = 0
grupo = 0
unidad = 0
NumGruUTer = 0
NumURCxGrupo = 0
NumURDxGrupo = 0
grupo_subsis = 0 
localidad = 0

!Hacer para el maximo numero de zonas de reserva
consecutivo_RC = 0
consecutivo_RD = 0

ApunURCxGrupo ( 1 ) = 1
ApunURDxGrupo ( 1 ) = 1

do grupo = 1, maxgrute
    entro = .false.
    !Hacer para todas las unidades activas de rango continuo
    do unidad = 1, NumUniRC
        if ( UniGruTerRC ( unidad, grupo ) .eq. 1 ) then
            if ( entro .eq. .false. ) then
                NResEner = NResEner + 1
                entro = .true.
            end if
            NumURCxGrupo ( NResEner ) = NumURCxGrupo ( NResEner ) + 1
            consecutivo_RC = consecutivo_RC + 1
            UniRCxGrupo ( consecutivo_RC ) = unidad
        end if        
    end do
    if ( entro .eq. .true. ) then 
        NumGrupoConsec ( NResEner ) = grupo
        if ( UniRCxGrupo ( ApunURCxGrupo ( NResEner ) ) .ne. 0 ) then
            grupo_subsis ( NResEner ) = IslaGenRC ( UniRCxGrupo ( ApunURCxGrupo ( NResEner ) ) )
        end if
        ApunURCxGrupo ( NResEner + 1 ) = ApunURCxGrupo ( NResEner ) + NumURCxGrupo ( NResEner )
        ApunURDxGrupo ( NResEner + 1 ) = ApunURDxGrupo ( NResEner ) + NumURDxGrupo ( NResEner )
     end if
    !Hacer para todas las unidades activas de rango discontinuo
    do unidad = 1, NumUniRD
        if ( UniGruTerRD ( unidad, grupo ) .eq. 1 ) then
            if ( entro .eq. .false. ) then
                NResEner = NResEner + 1
                entro = .true.
            end if
            NumURDxGrupo ( NResEner ) = NumURDxGrupo ( NResEner ) + 1
            consecutivo_RD = consecutivo_RD + 1
            UniRDxGrupo ( consecutivo_RD ) = unidad
        end if        
    end do
    if ( entro .eq. .true. ) then 
        NumGrupoConsec ( NResEner ) = grupo
        if ( UniRDxGrupo ( ApunURDxGrupo ( NResEner ) ) .ne. 0 ) then
            grupo_subsis ( NResEner ) = IslaGenRD ( UniRDxGrupo ( ApunURDxGrupo ( NResEner ) ) )
        end if
        ApunURCxGrupo ( NResEner + 1 ) = ApunURCxGrupo ( NResEner ) + NumURCxGrupo ( NResEner )
        ApunURDxGrupo ( NResEner + 1 ) = ApunURDxGrupo ( NResEner ) + NumURDxGrupo ( NResEner )
    end if  
end do

!Imprime la lista de unidades por grupo con limitacion de energia
write ( 1, 100 ) '-----------------------------------------------------'
write ( 1, 100 ) 'LISTA DE UNIDADES POR GRUPO CON LIMITACION DE ENERGIA'
write ( 1, 100 ) '-----------------------------------------------------'
do grupo = 1, NresEner
    if ( NumURCxGrupo ( grupo ) .ne. 0 .or. NumURDxGrupo ( grupo ) .ne. 0 ) then
        write ( 1, * ) '   Grupo: ', grupo
        !localidad
        if ( NumURCxGrupo ( grupo ) .ne. 0 ) then
            do localidad = ApunURCxGrupo ( grupo ), ApunURCxGrupo ( grupo ) + NumURCxGrupo ( grupo ) - 1
                write ( 1, 300 ) UniRCxGrupo ( localidad ), nombunirc ( UniRCxGrupo ( localidad ) )               
            end do
        end if
        if ( NumURDxGrupo ( grupo ) .ne. 0 ) then
            do localidad = ApunURDxGrupo ( grupo ), ApunURDxGrupo ( grupo ) + NumURDxGrupo ( grupo ) - 1
                write ( 1, 300 ) UniRDxGrupo ( localidad ), nombunird ( UniRDxGrupo ( localidad ) )               
            end do
        end if
    end if
end do


100 format ( a )
200 format ( 2(i3,8x), a12, 2(I3, 7x), 2(f9.2, 5x), i3 )
300 format ( i3, x, a12 ) 

end subroutine grupos_cenace_new

! ---------------------------------------------------------------------
! Se leen parametros de sintonia en el problema de asignacion         *
! (MILP).                                                             *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre de 2020                                                  *
! ---------------------------------------------------------------------
Subroutine LeeParAUHE

use ParAUHE
use ProblemaAUHE

Implicit none

integer ierror, ierr
real*8  porcentaje


character*500 letaux

! Abre archivos de parametros de sintonizacion

OPEN ( UNIT = 10, FILE = rut_dat_1( 1 : long_ruta )//trim(TipoLec)//'PRMTS.csv', IOSTAT = IERROR, &
       STATUS='UNKNOWN', RECORDSIZE = 500 		                  )


! Lee información de parametros hasta encontrar fin de información
if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then


! Considerar Unidades de Rango Continuo ( 1 = si, 0 = no )
  SiUniRC = 1

! Resolver el problema en variables enteras ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) TipoProblema

! Considerar Unidades de Rango Discontinuo ( 1 = si, 0 = no )
  SiUniRD = 1

! Considerar escalar precios de servicios conexos ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiEscalaConex
  if ( TipoEjecu .ne. 3 ) then
      SiEscalaConex = 1
  endif

! Considerar Unidades Hidro ( 1 = si, 0 = no )
  SiUniH = 1

! Considerar determinacion de penalizaciones ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiCalPen

  if ( TipoEjecu .lt. 3 ) then
	  SiCalPen = 0
  else
      if ( nomsis(1) .ne. 'SIN' ) then
		  SiCalPen = 0
      endif
  endif

! Considerar Unidades Renovables ( 1 = si, 0 = no )
  SiUniRE = 1

! Considerar escribir el modelo LP ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiEscLP
  SiEscLP = 0

! Considerar Ofertas de demanda ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiOferDem
! si es el AUGC o EXPOST
!  if ( TipoEjecu .eq. 1 .or. TipoEjecu .eq. 2 ) then
  if ( TipoEjecu .eq. 1 ) then !Version de prueba Expost Angel (10/09/2020)
      SiOferDem = 0
  endif

! Resolver segundo problema para ajuste de precios ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiSegProb

! Considerar Ofertas de reserva ( 1 = si, 0 = no )
  SiOferRes = 1

! Escribir a archivo el detalle de cortes/excdentes por nodo ( 1 = si, 0 = no )
  SiEscCorExc = 1

! Considerar Ofertas de compra de reserva por zona ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiOferComResZona

! Considerar uso de una solucion inicial ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiOferComResSis
!  read ( letaux, *, iostat = ierr ) SiSolucionInicial

! Considerar Ofertas de compra de reserva por sistema ( 1 = si, 0 = no )
!  SiOferComResSis = 0
  SiSolucionInicial = 0

! Considerar Ofertas de demanda controlable ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiOferDemCon

! Considerar limitaciones de energia hidro ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiEnerHid
  !SiEnerHid = 1

! Considerar limitaciones de energia termo ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiEnerTer

! Considerar limitaciones de transmision ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiTransmision

! Considerar perdidas en transmision ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiPerdidas

! Tipo de ofertas a considerar ( 1 = ofertas de costo a generacion minima, 0 = ofertas de costo en vacio )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) TipoOferta
  
! Tipo de escalamiento de precios a considerar ( 1 = maxima oferta incremental, 0 = curva de demanda de reservas )
  TipoEscPre = 1
  if ( TipoEjecu .eq. 2 ) then
!  if ( TipoEjecu .eq. 2 .or. TipoEjecu .eq. 0 ) then
      TipoEscPre = 0
  endif
  
! Numero de iteraciones para aproximar perdidas en transmision
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) IterPerdidas

! Considerar cortes de carga ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiCorte

! Considerar excedentes ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiExced

! Considerar artificiales transmision ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiArtTrans

! Precio del corte de carga
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) CostoCorte  
  CostoCorte = CostoCorte*Base

! Precio del excedente de energia
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) CostoExced  
  CostoExced = CostoExced*Base

! Costo por no satisfacer una politica de energia Hidro
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) PenEnerEmb 
  PenEnerEmb = PenEnerEmb*Base

! Costo por no satisfacer una politica de energia Termo
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) PenEnerUTerm 
  PenEnerUTerm = PenEnerUTerm*Base

! Costo por violar restricciones de transmision
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) PenRamas  
  PenRamas = PenRamas*Base

! Considerar bandas prohibidas de operacion ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiBandproh

! Porcentaje de cercania a la solucion optima de un problema de asignacion y despacho (MILP) en %
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) GAPCPLEX
  GAPCPLEX = GAPCPLEX/100.0

! Tiempo maximo (segundos) de ejecucion para resolver un problema de asignacion y despacho (MILP)
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) LIMIT_TIME_LINEAR

! Precio de infactibilidad en reserva de regulacion
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) PrecioTopeReg
  PrecioTopeReg = PrecioTopeReg*Base
  
! Precio de infactibilidad en reserva rodante
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) PrecioTopeRod
  PrecioTopeRod = PrecioTopeRod*Base
  
! Precio de infactibilidad en reserva operativa
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) PrecioTopeOper
  PrecioTopeOper = PrecioTopeOper*Base
  
! Precio de infactibilidad en reserva suplementaria
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) PrecioTopeSup
  PrecioTopeSup = PrecioTopeSup*Base

! Considerar limites de regulacion ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiLimReg

! Considerar arranque no simultaneo de unidades ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiArrNoSimul

! Considerar modelado hidraulico ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiModHid

! Considerar reserva de regulacion distribuida en zonas ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiResRegDis

! pocentaje para minima reserva de regulacion
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) porcentaje

! Considerar limites de combustible ( 1 = si, 0 = no )
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) SiGpoGas

! porcentaje de la demanda nodal para permitir el corte
!  read ( 10, 100, iostat = ierror ) letaux
!  read ( letaux, *, iostat = ierr ) PorcenCorte

  PorcenCorte = 1.0

! Costo por no satisfacer un limite de combustible
!  read ( 10, 100, iostat = ierror ) letaux
!  read ( letaux, *, iostat = ierr ) PenLimComb 
!  PenLimComb = PenLimComb*Base
  PenLimComb = PenEnerUTerm

! Mantener reservas de MDA en AUGC ( 1 = si, 0 = no )
!  read ( 10, 100, iostat = ierror ) letaux
!  read ( letaux, *, iostat = ierr ) SiMantReservas
  SiMantReservas = 0

! Relajar reserva de regulacion ( 1 = si, 0 = no )
!  read ( 10, 100, iostat = ierror ) letaux
!  read ( letaux, *, iostat = ierr ) SiRelReg
  SiRelReg = 0

! Relajar reserva rodante ( 1 = si, 0 = no )
!  read ( 10, 100, iostat = ierror ) letaux
!  read ( letaux, *, iostat = ierr ) SiRelRod
  SiRelRod = 0

! Costo por relajar reserva de regulacion
!  read ( 10, 100, iostat = ierror ) letaux
!  read ( letaux, *, iostat = ierr ) PenRelReg
!  PenRelReg = PenRelReg*Base
  PenRelReg = CostoCorte

! Costo por relajar reserva rodante
!  read ( 10, 100, iostat = ierror ) letaux
!  read ( letaux, *, iostat = ierr ) PenRelRod
!  PenRelRod = PenRelRod*Base
  PenRelRod = CostoCorte

! Proteger reserva no rodante ( 1 = si, 0 = no )
!  read ( 10, 100, iostat = ierror ) letaux
!  read ( letaux, *, iostat = ierr ) SiRelNRod
  SiRelNRod = 0

! Escribir resultados de la red electrica a archivos CSV ( 1 = si, 0 = no )
!  read ( 10, 100, iostat = ierror ) letaux
!  read ( letaux, *, iostat = ierr ) SiEscResRed
  SiEscResRed = 0
! Epsilon para relajar requerimientos de reserva en segundo problema
!  read ( 10, 100, iostat = ierror ) letaux
!  read ( letaux, *, iostat = ierr ) epsilonreserva
  epsilonreserva = 1.0E-05

! Considerar refinamiento de modelado hidraulico ( 1 = si, 0 = no )
  SiRefHid = 1

!Reserva de regulacion contribuye a la reserva rodante ( 1 = si, 0 = no )
 SiRegEnRod = 1

! Considerar relajar reservas rodantes en AUGC ( 1 = si, 0 = no )
 if ( TipoEjecu .eq. 1 ) then
     if ( SiMantReservas .eq. 0 ) then
         SiRelReg = 0
         SiRelRod = 0
         SiRelNRod = 0
     endif
 endif

 ! Si es un MDA
 if ( TipoEjecu .eq. 0 ) then
    SiMantReservas = 0
    SiRelRod = 0
 endif

  ! Si es un AUGC y no se desea mantener reservas
 if ( TipoEjecu .eq. 1 .and. SiMantReservas .eq. 0 ) then
    SiRelRod = 0
 endif

! URIEL LEE GAP DE PERDIDAS REQUERIDO 
  read ( 10, 100, iostat = ierror ) letaux
  read ( letaux, *, iostat = ierr ) GAPperreq
  
endif

close ( 10 )

100 format ( a )
return

end


!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Se leen datos de grupos de generadores que no se arrancan       *
!         en forma simultanea                                             *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!                                Mayo 2016                                *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************

Subroutine LeeGruposArranque

Use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, NumGruArr, NomGruArr, numunigruarr, unigruarr

Implicit none

integer ierror, i, k, kmax, ierr

integer ibanbit

character*20 let

character*250 letaux

! Abre archivos de datos de grupos de generadores que no pueden ser arrancados simultaneamente

OPEN ( UNIT = 4, FILE = rut_dat_1( 1 : long_ruta )//'GPOARR_RC.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 250 )

ibanbit = 1

ierror = 0

i = 0


! Lee información de division hasta encontrar fin de informacion
do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )

  read ( 4, 100, iostat = ierror ) letaux

  if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
    i = i + 1
    read ( letaux, *, iostat = ierr ) let, kmax
    read ( letaux, *, iostat = ierr ) NomGruArr(i), numunigruarr(i), ( unigruarr(i,k), k = 1, kmax )
	if ( ierr .ne. 0 ) then
       bmensaje = '!!! AuSeg '//'Error en lectura de archivo GPOARR_RC.csv, registro '// letaux
       Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
      i = i - 1
	endif
  endif

enddo

NumGruArr = i


100 format ( a )

close ( unit = 4 )

end subroutine LeeGruposArranque
    

!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!       I N S T I T U T O   D E    I N V E S T I G A C I O N E S          *
!                                                                         *
!                       E L E C T R I C A S                               *
!                                                                         *
!      D I V I S I O N   D E   S I S T E M A S   E L E C T R I C O S      *
!                                                                         *
!    D E P A R T A M E N T O   D E   A N A L I S I S   D E   R E D E S    *
!                                                                         *
!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Se leen datos de unidades de propiedad conjunta                 *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!                                Diciembre 2016                           *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************

Subroutine LeeUPC_old

Use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, NumUPC, indiceup, &
                  indiceuc1, indiceuc2

Implicit none

integer ierror, i, ierr

integer ibanbit

character*20 let

character*250 letaux

! Abre archivos de datos de unidades de propiedad conjunta

OPEN ( UNIT = 4, FILE = rut_dat_1( 1 : long_ruta )//'UPCONJRC.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 250 )

ibanbit = 1
ierror = 0
i = 0

! Lee información de division hasta encontrar fin de informacion
do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )

  read ( 4, 100, iostat = ierror ) letaux
  if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
    i = i + 1
    read ( letaux, *, iostat = ierr ) let, indiceup(i), let, indiceuc1(i), let, indiceuc2(i)
	if ( ierr .ne. 0 .and. ierr .ne. -1 ) then
       bmensaje = '!!! AuSeg '//'Error en lectura de archivo UPCONJRC.csv, registro '// letaux
       Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
      i = i - 1
	endif
  endif

enddo

NumUPC = i

100 format ( a )

close ( unit = 4 )

end subroutine LeeUPC_old


! ---------------------------------------------------------------------
! Se lee informacion de grupos de unidades con limitacion de          *
! grupos de combustible (gas).                                        *
!                                                                     *
! Instituto Nacional de Electricidad y Energias Limpias               *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre de 2017                                                   *
! ---------------------------------------------------------------------   
SUBROUTINE LeeGpoGas

use ParAUHE

Implicit none

integer d, dia, ierror, i, grupo, unidad, consecutivo_RC, consecutivo_RD, NumGrupoConsec ( maxgrute ), &
        grupo_subsis ( maxgrute ), ggrupo, localidad, auxiliar ( maxgrute ), numgpo, bandera, errleca

real*8 minimo (maxdia), maximo (maxdia)

character*20 nombre

character*3000 letaux

character*5 letr

logical entro

!Inicializacion de variables
i = 0
ierror = 0
grupo = 0
unidad = 0
NumGruGas = 0
NumURCxGpoGas = 0
NumURDxGpoGas = 0
grupo_subsis = 0 
localidad = 0
letaux = 'letaux'

consecutivo_RC = 0
consecutivo_RD = 0

ApunURCxGpoGas ( 1 ) = 1
ApunURDxGpoGas ( 1 ) = 1

! -----------------------------
! * Se leen datos de GPOGASRC *
! -----------------------------
OPEN (UNIT = 117, FILE = rut_dat_1( 1 : long_ruta )//'GPOGASRC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee informacion hasta encontrar fin de archivo
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
        read ( 117, 100, iostat = ierror ) letaux
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
            read ( letaux, *, iostat = errleca )  ( auxiliar ( grupo ), grupo = 1, maxgrute )
            if ( errleca .ne. 0 ) go to 1001
            UniGpoGasRC ( i, : ) =  auxiliar
	    endif
    enddo
else	
   bmensaje = 'ERROR DE LECTURA ARCHIVO GPOGASRC.csv'
   call ParaProceso ( bmensaje )
end if

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO GPOGASRC.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'GRUPO: ['//trim(letr)//'] '//trim(nombre)
   call ParaProceso ( bmensaje )
end if

close (117)

! -----------------------------
! * Se leen datos de GPOGASRD *
! -----------------------------
letaux = 'letaux'
ierror = 0
i = 0
OPEN (UNIT = 118, FILE = rut_dat_1( 1 : long_ruta )//'GPOGASRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee informacion hasta encontrar fin de archivo
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
        read ( 118, 100, iostat = ierror ) letaux
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
            read ( letaux, *, iostat = errleca )  ( auxiliar ( grupo ), grupo = 1, maxgrute )
            if ( errleca .ne. 0 ) go to 1002
            UniGpoGasRD ( i, : ) =  auxiliar
	    endif
    enddo
else	
   bmensaje = 'ERROR DE LECTURA ARCHIVO GPOGASRD.csv'
   call ParaProceso ( bmensaje )
end if

1002 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO GPOGASRD.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'GRUPO: ['//trim(letr)//'] '//trim(nombre)
   call ParaProceso ( bmensaje )
end if

close (118)

! se forman apuntadores para identificar los grupos de unidades
do grupo = 1, maxgrute
    entro = .false.
    !Hacer para todas las unidades activas de rango continuo
    do unidad = 1, NumUniRC  
        if ( UniGpoGasRC ( unidad, grupo ) .eq. 1 ) then
            if ( entro .eq. .false. ) then
                NumGruGas = NumGruGas + 1
                entro = .true.
            end if
            NumURCxGpoGas ( NumGruGas ) = NumURCxGpoGas ( NumGruGas ) + 1
            consecutivo_RC = consecutivo_RC + 1
            UniRCxGpoGas ( consecutivo_RC ) = unidad
        end if        
    end do
    if ( entro .eq. .true. ) then 
        NumGrupoConsec ( NumGruGas ) = grupo
        if ( UniRCxGpoGas ( ApunURCxGpoGas ( NumGruGas ) ) .ne. 0 ) then
            grupo_subsis ( NumGruGas ) = IslaGenRC ( UniRCxGpoGas ( ApunURCxGpoGas ( NumGruGas ) ) )
        end if
        ApunURCxGpoGas ( NumGruGas + 1 ) = ApunURCxGpoGas ( NumGruGas ) + NumURCxGrupo ( NumGruGas )
        ApunURDxGpoGas ( NumGruGas + 1 ) = ApunURDxGpoGas ( NumGruGas ) + NumURDxGrupo ( NumGruGas )
     end if
    !Hacer para todas las unidades activas de rango discontinuo
    do unidad = 1, NumUniRD
        if ( UniGpoGasRD ( unidad, grupo ) .eq. 1 ) then
            if ( entro .eq. .false. ) then
                NumGruGas = NumGruGas + 1
                entro = .true.
            end if
            NumURDxGpoGas ( NumGruGas ) = NumURDxGpoGas ( NumGruGas ) + 1
            consecutivo_RD = consecutivo_RD + 1
            UniRDxGpoGas ( consecutivo_RD ) = unidad
        end if        
    end do
    if ( entro .eq. .true. ) then 
        NumGrupoConsec ( NumGruGas ) = grupo
        if ( UniRDxGpoGas ( ApunURDxGpoGas ( NumGruGas ) ) .ne. 0 ) then
            grupo_subsis ( NumGruGas ) = IslaGenRD ( UniRDxGpoGas ( ApunURDxGpoGas ( NumGruGas ) ) )
        end if
        ApunURCxGpoGas ( NumGruGas + 1 ) = ApunURCxGpoGas ( NumGruGas ) + NumURCxGpoGas ( NumGruGas )
        ApunURDxGpoGas ( NumGruGas + 1 ) = ApunURDxGpoGas ( NumGruGas ) + NumURDxGpoGas ( NumGruGas )
    end if  
end do

letaux = 'letaux'
ierror = 0

! -----------------------------
! * Se leen datos de GPOGAS *
! -----------------------------
OPEN (UNIT = 116, FILE = rut_dat_1( 1 : long_ruta )//'GPOGAS.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	    read ( 116, 100, iostat = ierror ) letaux
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            read ( letaux, *, iostat = errleca )  grupo, nombre, bandera, ( minimo (dia), maximo (dia) , dia = 1, maxdia )
            if ( errleca .ne. 0 ) go to 1003
            GpoAct ( grupo ) = bandera
            !Hacer para las zonas activas por sistema
            do ggrupo = 1, NumGruGas
                if ( grupo .eq. NumGrupoConsec ( ggrupo ) ) then
                    NomGpoGas ( ggrupo ) = nombre
                    numgpo = grupo
                    do d = 1, DURDIA
                        LimInfGas ( ggrupo, d ) = minimo ( d )
                        LimSupGas ( ggrupo, d ) = maximo ( d )
                    enddo
                    exit
                end if     
            end do
	    endif
    enddo
else	
   bmensaje = 'ERROR DE LECTURA ARCHIVO GPOGAS.csv'
   call ParaProceso ( bmensaje )
end if

1003 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) grupo
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO GPOGAS.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'GRUPO: ['//trim(letr)//'] '//trim(nombre)
   call ParaProceso ( bmensaje )
end if

CLOSE ( UNIT = 116 )

!Imprime la lista de unidades por grupo con limitacion de combustible
write ( 1, 100 ) '---------------------------------------------------------'
write ( 1, 100 ) 'LISTA DE UNIDADES POR GRUPO CON LIMITACION DE COMBUSTIBLE'
write ( 1, 100 ) '---------------------------------------------------------'
!Hacer para todas las zonas activas
do grupo = 1, NumGruGas
    if ( NumURCxGpoGas ( grupo ) .ne. 0 .or. NumURDxGpoGas ( grupo ) .ne. 0 ) then
        write ( 1, 100 ) ''
        write ( 1, 100 ) 'Nombre          Minimos  Maximos (MMBtu)'
        write ( 1, 200 ) NomGpoGas ( grupo ), ( LimInfGas (grupo,dia), LimSupGas (grupo, dia) , dia = 1, durdia )
        !localidad
        if ( NumURCxGpoGas ( grupo ) .ne. 0 ) then
            do localidad = ApunURCxGpoGas ( grupo ), ApunURCxGpoGas ( grupo ) + NumURCxGpoGas ( grupo ) - 1
                write ( 1, 300 ) UniRCxGpoGas ( localidad ), nombunirc ( UniRCxGpoGas ( localidad ) )               
            end do
        end if
        if ( NumURDxGpoGas ( grupo ) .ne. 0 ) then
            do localidad = ApunURDxGpoGas ( grupo ), ApunURDxGpoGas ( grupo ) + NumURDxGpoGas ( grupo ) - 1
                write ( 1, 300 ) UniRDxGpoGas ( localidad ), nombunird ( UniRDxGpoGas ( localidad ) )               
            end do
        end if
    end if
end do

!Escalamiento

LimInfGas = LimInfGas / Base
LimSupGas = LimSupGas / Base  

100 format ( a )
200 format ( a12, x, 7( 2(f9.2, 2x)) )
300 format ( i3, x, a12 ) 

end subroutine LeeGpoGas


! ---------------------------------------------------------------------
! Se lee informacion de generacion de las unidades en tiempo real y   *
! se fija a dicho valor la generacion del modelo, cuando es un EXPOST.*
!                                                                     *
! Instituto Nacional de Electricidad y Energias Limpias               *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Abril de 2019                                                       *
! ---------------------------------------------------------------------   
SUBROUTINE LeeGenTR

use ParAUHE
use ProblemaAUHE

Implicit none

integer ierror, i, modo, u, clave, s, last, errleca

real*8   GenTRRC ( maxurc, maxint), GenTRRD ( maxurd, maxint ), GenTRH ( maxuh, maxint ), GenTRRE ( maxure, maxint ), sumaB

character*20 nombre

character*3000 letaux

character*5 letr

!Inicializacion de variables
ierror = 0
GenTRRC = 0.0
GenTRRD = 0.0
GenTRH = 0.0
GenTRRE = 0.0

! --------------------------------------------
! * Se leen generaciones de tiempo real (TR) *
! --------------------------------------------
OPEN ( UNIT = 15, FILE = trim(rut_dat_1)//'GENTR.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
errleca = 0
if ( ierror .eq. 0 ) then
!   para todas las unidades de rango continuo
    do u = 1, NumUniRC
!       para todos los intervalos de planeacion
        read ( 15, 100, iostat = ierror ) letaux
        if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            read ( letaux, *, iostat = errleca )  nombre, clave, ( GenTRRC ( u, i ), i = 1, NTINTR )
            if ( errleca .ne. 0 ) go to 1001
        endif
    enddo
!   para todas las unidades de rango discontinuo
    do u = 1, NumUniRD
!       para todos los intervalos de planeacion
        read ( 15, 100, iostat = ierror ) letaux
        if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            read ( letaux, *, iostat = errleca )  nombre, clave, ( GenTRRD ( u, i ), i = 1, NTINTR )
            if ( errleca .ne. 0 ) go to 1001
        endif
    enddo
!   para todas las unidades hidro
    do u = 1, NumUniHid
!       para todos los intervalos de planeacion
        read ( 15, 100, iostat = ierror ) letaux
        if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            read ( letaux, *, iostat = errleca )  nombre, clave, ( GenTRH ( u, i ), i = 1, NTINTR )
            if ( errleca .ne. 0 ) go to 1001
        endif
    enddo
!   para todas las unidades renovables
    do u = 1, NumUniRE
!       para todos los intervalos de planeacion
        read ( 15, 100, iostat = ierror ) letaux
        if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            read ( letaux, *, iostat = errleca )  nombre, clave, ( GenTRRE ( u, i ), i = 1, NTINTR )
            if ( errleca .ne. 0 ) go to 1001
        endif
    enddo
else	
   bmensaje = 'ERROR DE LECTURA ARCHIVO GENTR.csv'
   call ParaProceso ( bmensaje )
end if

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) u
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO GENTR.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombre)
   call ParaProceso ( bmensaje )
end if

close (15)


! para todos los intervalos de planeacion
do i = 1, NTINTR
    
    ! Despacho de unidades de rango continuo
    do u = 1, NumUniRC
        if ( i .eq. 10 .and. u .eq. 176 ) then
            continue
        endif
        sumaB = 0.0
        last = 0
        if ( GenTRRC ( u, i )/Base .gt. PotMaxGRC ( u, i ) ) then
            PotMaxGRC ( u, i ) = GenTRRC ( u, i )/Base
!            write (*, *) 'RC', u, i
        endif
        if ( GenTRRC ( u, i )/Base .lt. PotMinGRC ( u, i ) ) then
            PotMinGRC ( u, i ) = GenTRRC ( u, i )/Base
!            write (*, *) 'RC', u, i
        endif
!       para todos los segmentos de curva de ofertas de venta
        do s = 1, NumBloVRC( u, i )
!           coeficiente de segmento de venta
	        sumaB = sumaB + OferVenEnerRC ( u, s, i )
            if ( OferVenEnerRC ( u, s, i ) .gt. 0.0 ) then
                last = s
            endif
        enddo
        if ( (PotMinGRC ( u, i ) + sumaB) .lt. GenTRRC ( u, i )/Base ) then
!            write (*, *) 'RC', u, i
            if ( last .eq. 0 ) then
                last = 1
                NumBloVRC ( u, i ) = 1
!               write (*, *) 'Oferta nula'
            endif
            OferVenEnerRC ( u, last, i ) = OferVenEnerRC ( u, last, i ) + GenTRRC ( u, i )/Base - (PotMinGRC ( u, i ) + sumaB)
        endif
        
    !   cota inferior de variable de generacion
        lbMILP ( IGRC + u + (i-1)*NumUniRC - 1 ) = GenTRRC ( u, i )/Base - 1.0e-5
    !   cota superior de variable de generacion
        ubMILP ( IGRC + u + (i-1)*NumUniRC - 1 ) = GenTRRC ( u, i )/Base
    enddo

    !   Despacho de unidades de rango discontinuo
    do u = 1, NumUniRD
        modo = 2
    !   cota inferior de variable de generacion
        lbMILP ( IGRD + INIURDI ( u, i ) + modo - 1 ) = GenTRRD ( u, i )/Base - 1.0e-5
    !   cota superior de variable de generacion
        ubMILP ( IGRD + INIURDI ( u, i ) + modo - 1 ) = GenTRRD ( u, i )/Base
    enddo

    ! Despacho de unidades hidro
    do u = 1, NumUniHid
        if ( GenTRH ( u, i )/Base .gt. PotMaxUniH ( u, i ) ) then
            PotMaxUniH ( u, i ) = GenTRH ( u, i )/Base
!            write (*, *) 'H', u, i
        endif
        if ( GenTRH ( u, i )/Base .lt. PotMinUniH ( u, i ) ) then
            PotMinUniH ( u, i ) = GenTRH ( u, i )/Base
!            write (*, *) 'H', u, i
        endif
    !   cota inferior de variable de generacion
        lbMILP ( IGH + u + (i-1)*NumUniHid - 1 ) = GenTRH ( u, i )/Base - 1.0e-5
    !   cota superior de variable de generacion
        ubMILP ( IGH + u + (i-1)*NumUniHid - 1 ) = GenTRH ( u, i )/Base
    enddo

    ! Despacho de unidades renovables
    do u = 1, NumUniRE
        sumaB = 0.0
        last = 0
        if ( GenTRRE ( u, i )/Base .gt. PotMaxGRE ( u, i ) ) then
            PotMaxGRE ( u, i ) = GenTRRE ( u, i )/Base
!            write (*, *) 'RE', u, i
        endif
        if ( GenTRRE ( u, i )/Base .lt. PotMinGRE ( u, i ) ) then
            PotMinGRE ( u, i ) = GenTRRE ( u, i )/Base
!            write (*, *) 'RE', u, i
        endif
!       para todos los segmentos de curva de ofertas de venta
        do s = 1, NumBloVRE( u, i )
!           coeficiente de segmento de venta
	        sumaB = sumaB + OferVenEnerRE ( u, s, i )
            if ( OferVenEnerRE ( u, s, i ) .gt. 0.0 ) then
                last = s
            endif
        enddo
        if ( (PotMinGRE ( u, i ) + sumaB) .lt. GenTRRE ( u, i )/Base ) then
!            write (*, *) 'RE', u, i
            if ( last .eq. 0 ) then
                last = 1
                NumBloVRE ( u, i ) = 1
!               write (*, *) 'Oferta nula'
            endif
            OferVenEnerRE ( u, last, i ) = OferVenEnerRE ( u, last, i ) + GenTRRE ( u, i )/Base - (PotMinGRE ( u, i ) + sumaB)
        endif
    !   cota inferior de variable de generacion
        lbMILP ( IGRE + u + (i-1)*NumUniRE - 1 ) = GenTRRE ( u, i )/Base - 1.0e-5
    !   cota superior de variable de generacion
        ubMILP ( IGRE + u + (i-1)*NumUniRE - 1 ) = GenTRRE ( u, i )/Base
    enddo
enddo


100 format ( a )

end subroutine LeeGenTR


! ---------------------------------------------------------------------
! Se lee informacion de grupos horarios de unidades con limitacion    *
! de combustible (gas, diesel, petroleo, carbon, etc).                *
!                                                                     *
! Instituto Nacional de Electricidad y Energias Limpias               *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Abril de 2019                                                       *
! ---------------------------------------------------------------------   
SUBROUTINE LeeGpoGas_New

use ParAUHE

Implicit none

integer ierror, i, grupo, unidad, consecutivo_RC, consecutivo_RD, NumGrupoConsec ( maxgrute ), &
        grupo_subsis ( maxgrute ), ggrupo, localidad, auxiliar ( maxgrute ), errleca

character*3000 letaux

character*5 letr

logical entro

!Inicializacion de variables
i = 0
ierror = 0
grupo = 0
unidad = 0
NumGruGas = 0
NumURCxGpoGas = 0
NumURDxGpoGas = 0
grupo_subsis = 0 
localidad = 0
letaux = 'letaux'

! -----------------------------
! * Se leen datos de GPOGAS *
! -----------------------------
OPEN (UNIT = 116, FILE = rut_dat_1( 1 : long_ruta )//'GPOGAS.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0
if ( ierror .eq. 0 ) then
    i = 0
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
	    read ( 116, 100, iostat = ierror ) letaux
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
            read ( letaux, *, iostat = errleca )  ggrupo, NGpoResComb ( i ), NomGpoResComb ( i ), HIResComb ( i ), HFResComb ( i ), LInfResComb ( i ), LSupResComb ( i ), ActResComb ( i )
            if ( errleca .ne. 0 ) go to 1001
	    endif
    enddo
else	
   bmensaje = 'ERROR DE LECTURA ARCHIVO GPOGAS.csv'
   call ParaProceso ( bmensaje )
end if

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO GPOGAS.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'GRUPO: ['//trim(letr)//'] '//trim(NomGpoResComb ( i ))
   call ParaProceso ( bmensaje )
end if

CLOSE ( UNIT = 116 )

NresComb = i
consecutivo_RC = 0
consecutivo_RD = 0

ApunURCxGpoGas ( 1 ) = 1
ApunURDxGpoGas ( 1 ) = 1

! -----------------------------
! * Se leen datos de GPOGASRC *
! -----------------------------
OPEN (UNIT = 117, FILE = rut_dat_1( 1 : long_ruta )//'GPOGASRC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee informacion hasta encontrar fin de archivo
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
        read ( 117, 100, iostat = ierror ) letaux
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
            read ( letaux, *, iostat = errleca )  ( auxiliar ( grupo ), grupo = 1, NresComb )
            if ( errleca .ne. 0 ) go to 1002
            UniGpoGasRC ( i, : ) =  auxiliar
	    endif
    enddo
else	
   bmensaje = 'ERROR DE LECTURA ARCHIVO GPOGASRC.csv'
   call ParaProceso ( bmensaje )
end if

1002 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO GPOGASRC.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'GRUPO: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
end if

close (117)

! -----------------------------
! * Se leen datos de GPOGASRD *
! -----------------------------
letaux = 'letaux'
ierror = 0
i = 0
OPEN (UNIT = 118, FILE = rut_dat_1( 1 : long_ruta )//'GPOGASRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee informacion hasta encontrar fin de archivo
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
        read ( 118, 100, iostat = ierror ) letaux
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
            read ( letaux, *, iostat = errleca )  ( auxiliar ( grupo ), grupo = 1, NresComb )
            if ( errleca .ne. 0 ) go to 1003
            UniGpoGasRD ( i, : ) =  auxiliar
	    endif
    enddo
else	
   bmensaje = 'ERROR DE LECTURA ARCHIVO GPOGASRD.csv'
   call ParaProceso ( bmensaje )
end if

1003 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO GPOGASRD.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'GRUPO: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
end if

close (118)

! se forman apuntadores para identificar los grupos de unidades
do grupo = 1, maxgrute
    entro = .false.
    !Hacer para todas las unidades activas de rango continuo
    do unidad = 1, NumUniRC  
        if ( UniGpoGasRC ( unidad, grupo ) .eq. 1 ) then
            if ( entro .eq. .false. ) then
                NumGruGas = NumGruGas + 1
                entro = .true.
            end if
            NumURCxGpoGas ( NumGruGas ) = NumURCxGpoGas ( NumGruGas ) + 1
            consecutivo_RC = consecutivo_RC + 1
            UniRCxGpoGas ( consecutivo_RC ) = unidad
        end if        
    end do
    if ( entro .eq. .true. ) then 
        NumGrupoConsec ( NumGruGas ) = grupo
        if ( UniRCxGpoGas ( ApunURCxGpoGas ( NumGruGas ) ) .ne. 0 ) then
            grupo_subsis ( NumGruGas ) = IslaGenRC ( UniRCxGpoGas ( ApunURCxGpoGas ( NumGruGas ) ) )
        end if
        ApunURCxGpoGas ( NumGruGas + 1 ) = ApunURCxGpoGas ( NumGruGas ) + NumURCxGrupo ( NumGruGas )
        ApunURDxGpoGas ( NumGruGas + 1 ) = ApunURDxGpoGas ( NumGruGas ) + NumURDxGrupo ( NumGruGas )
     end if
    !Hacer para todas las unidades activas de rango discontinuo
    do unidad = 1, NumUniRD
        if ( UniGpoGasRD ( unidad, grupo ) .eq. 1 ) then
            if ( entro .eq. .false. ) then
                NumGruGas = NumGruGas + 1
                entro = .true.
            end if
            NumURDxGpoGas ( NumGruGas ) = NumURDxGpoGas ( NumGruGas ) + 1
            consecutivo_RD = consecutivo_RD + 1
            UniRDxGpoGas ( consecutivo_RD ) = unidad
        end if        
    end do
    if ( entro .eq. .true. ) then 
        NumGrupoConsec ( NumGruGas ) = grupo
        if ( UniRDxGpoGas ( ApunURDxGpoGas ( NumGruGas ) ) .ne. 0 ) then
            grupo_subsis ( NumGruGas ) = IslaGenRD ( UniRDxGpoGas ( ApunURDxGpoGas ( NumGruGas ) ) )
        end if
        ApunURCxGpoGas ( NumGruGas + 1 ) = ApunURCxGpoGas ( NumGruGas ) + NumURCxGpoGas ( NumGruGas )
        ApunURDxGpoGas ( NumGruGas + 1 ) = ApunURDxGpoGas ( NumGruGas ) + NumURDxGpoGas ( NumGruGas )
    end if  
end do

!Imprime la lista de unidades por grupo con limitacion de combustible
write ( 1, 100 ) '---------------------------------------------------------'
write ( 1, 100 ) 'LISTA DE UNIDADES POR GRUPO CON LIMITACION DE COMBUSTIBLE'
write ( 1, 100 ) '---------------------------------------------------------'
!Hacer para todas las zonas activas
do grupo = 1, NumGruGas
    if ( NumURCxGpoGas ( grupo ) .ne. 0 .or. NumURDxGpoGas ( grupo ) .ne. 0 ) then
        write ( 1, 100 ) 'Grupo          '
        write ( 1, 200 ) grupo
        !localidad
        if ( NumURCxGpoGas ( grupo ) .ne. 0 ) then
            do localidad = ApunURCxGpoGas ( grupo ), ApunURCxGpoGas ( grupo ) + NumURCxGpoGas ( grupo ) - 1
                write ( 1, 300 ) UniRCxGpoGas ( localidad ), nombunirc ( UniRCxGpoGas ( localidad ) )               
            end do
        end if
        if ( NumURDxGpoGas ( grupo ) .ne. 0 ) then
            do localidad = ApunURDxGpoGas ( grupo ), ApunURDxGpoGas ( grupo ) + NumURDxGpoGas ( grupo ) - 1
                write ( 1, 300 ) UniRDxGpoGas ( localidad ), nombunird ( UniRDxGpoGas ( localidad ) )               
            end do
        end if
    end if
end do

write ( 1, 100 ) '----------------------------------------------------'
write ( 1, 100 ) 'LISTA DE RESTRICCIONES CON LIMITACION DE COMBUSTIBLE'
write ( 1, 100 ) '----------------------------------------------------'
!Para todas las restricciones
write ( 1, 100 ) 'Restriccion  Grupo    NomGrupo   HoraIni   HoraFin     LInfComb      LSupComb   Activo'
do i = 1, NresComb
    write ( 1, 200 ) i, NGpoResComb ( i ), NomGpoResComb ( i ), HIResComb ( i ), HFResComb ( i ), LInfResComb ( i ), LSupResComb ( i ), ActResComb ( i )
end do

!Escalamiento

LInfResComb = LInfResComb / Base
LSupResComb = LSupResComb / Base  

100 format ( a )
200 format ( 2(i3,8x), a12, 2(I3, 7x), 2(f9.2, 5x), i3 )
300 format ( i3, x, a12 ) 

end subroutine LeeGpoGas_New


Subroutine LeeAsigReservas
! ---------------------------------------------------------------------
! Se leen resultados de reservas asignadas en MDA por unidad, para    *
! mantener en AUGC.                                                   *
!                                                                     *
! Instituto Nacional de Electricidad y Energias Limpias               *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre de 2019                                                   *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

INTEGER   i, u, ierror, modo, tipo, ro, errleca

real*8    reserva ( maxint ), res, epsilon, min, maximo

character*12 nombre

character*3000 letaux

character*5 letr

letaux = 'letaux'
ierror = 0
epsilon = 0.005

! Abre archivos csv de resultados de reserva de regulacion asignada por unidad y por intervalo
OPEN ( 179, FILE = trim(rut_dat_1)//'D_RESRERESEU.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

errleca = 0
! para las unidades de rango continuo
do u = 1 , NumUniRC
    if ( u == 67 ) then
        continue
    endif
    read ( 179, 100, iostat = ierror ) letaux
    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
!       se lee asignacion de reservas de regulacion
        read ( letaux, *, iostat = errleca ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1001
        do i = 1,NTINTR
            if ( reserva ( i ) .gt. epsilon ) then
                res = (reserva ( i ) - epsilon) / Base
            else
                res = 0.0
            endif
            if ( res .gt. 0.0 ) then
                if ( DISPOURC (u, i ) .eq. 1 ) then
                    min = CalOferResRegRC (u, i)
                    do ro = 1, NoRaOpRC ( u )*SiBandProh
                        if ( min .gt. CalOferRegRORC ( u, ro, i ) .and. RaRegSupRC ( u, ro, i ) .gt. RaRegInfRC ( u, ro, i )) then
                            min = CalOferRegRORC ( u, ro, i )
                        endif
                    enddo
                    if ( res .gt. min ) then
                        write(234,*) 'regulacion, unidad:',nombunirc(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                    lbMILP ( IRRERC + u + (i-1)*NumUniRC - 1 ) =  res
                else
                    min = 0.0
                    write(234,*) 'regulacion, unidad:',nombunirc(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                    exit
                endif
            endif
        enddo
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRERESEU.csv'
       call ParaProceso ( bmensaje )
    end if
enddo

! para las unidades de rango discontinuo
do u = 1 , NumUniRD
    read ( 179, 100, iostat = ierror ) letaux
    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
!       se lee asignacion de reservas de regulacion
        read ( letaux, *, iostat = errleca  ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1001
        do i = 1,NTINTR
            modo = 2 !!! pendiente definir modo
            if ( reserva ( i ) .gt. epsilon ) then
                res = (reserva ( i ) - epsilon) / Base
            else
                res = 0.0
            endif
            if ( res .gt. 0.0 ) then
                if ( DISPOURD (u, modo, i ) .eq. 1 ) then
                    min = CalOferResRegRD (u, modo, i)
                    if ( res .gt. min ) then
                        write(234,*) 'regulacion, unidad:',nombunird(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                    lbMILP ( IRRERD + INIURDI ( u, i ) + modo - 1  ) =  res
                endif
            endif
        enddo
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRERESEU.csv'
       call ParaProceso ( bmensaje )
    endif
enddo
! para las unidades hidro
do u = 1 , NumUniHid
    read ( 179, 100, iostat = ierror ) letaux
    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
!       se lee asignacion de reservas de regulacion
        read ( letaux, *, iostat = errleca  ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1001
        do i = 1,NTINTR
            if ( reserva ( i ) .gt. epsilon ) then
                res = (reserva ( i ) - epsilon) / Base
            else
                res = 0.0
            endif
            if ( res .gt. 0.0 ) then
                if ( DISPOUH (u, i ) .eq. 1 ) then
                    min = CalOferResRegH (u, i)
                    do ro = 1, NoRaOpH ( u )*SiBandProh
                        if ( min .gt. CalOferRegROH ( u, ro, i ) .and. RaRegSupH ( u, ro, i ) .gt. RaRegInfH ( u, ro, i ) ) then
                            min = CalOferRegROH ( u, ro, i )
                        endif
                    enddo
                    if ( res .gt. min ) then
                        write(234,*) 'regulacion, unidad:',nombunih(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                    lbMILP ( IRREH + u + (i-1)*NumUniHid - 1 ) =  res
                endif
            endif
        enddo
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRERESEU.csv'
       call ParaProceso ( bmensaje )
    endif
enddo

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) u
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRERESEU.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif


! Abre archivos csv de resultados de reserva rodante de 10 minutos por unidad y por intervalo
OPEN ( 119, FILE = trim(rut_dat_1)//'D_RESRERO10U.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
errleca = 0
! para las unidades de rango continuo
do u = 1 , NumUniRC
    if ( u == 67 ) then
        continue
    endif
    read ( 119, 100, iostat = ierror ) letaux
    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
!       se lee asignacion de reservas rodante de 10
        read ( letaux, *, iostat = errleca ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1002
        do i = 1,NTINTR  
            if ( reserva ( i ) .gt. epsilon ) then
                res = (reserva ( i ) - epsilon) / Base
            else
                res = 0.0
            endif
            if ( res .gt. 0.0 ) then
                if ( DISPOURC (u, i ) .eq. 1 ) then
                    min = CalOferResR10RC (u, i)
                    if ( res .gt. min ) then
                        write(234,*) 'rodante 10, unidad:',nombunirc(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                else
                    res = 0.0
                endif
                lbMILP (IRR10RC + u + (i-1)*NumUniRC - 1 ) =  res
            endif
        enddo
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRERO10U.csv'
       call ParaProceso ( bmensaje )
    endif
enddo
! para las unidades de rango discontinuo
do u = 1 , NumUniRD
    read ( 119, 100, iostat = ierror ) letaux
    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
!       se lee asignacion de reservas rodante de 10
        read ( letaux, *, iostat = errleca ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1002
        do i = 1,NTINTR
            modo = 2 !!! pendiente definir modo
            if ( reserva ( i ) .gt. epsilon ) then
                res = (reserva ( i ) - epsilon) / Base
            else
                res = 0.0
            endif
            if ( res .gt. 0.0 ) then
                if ( DISPOURD (u, modo, i ) .eq. 1 ) then
                    min = CalOferResR10RD (u, modo, i)
                    if ( res .gt. min ) then
                        write(234,*) 'rodante 10, unidad:',nombunird(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                else
                    res = 0.0
                endif
                lbMILP (IRR10RD + INIURDI ( u, i ) + modo - 1 ) =  res
            endif
        enddo
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRERO10U.csv'
       call ParaProceso ( bmensaje )
    endif
enddo
! para las unidades hidro
do u = 1 , NumUniHid
    read ( 119, 100, iostat = ierror ) letaux
    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
!       se lee asignacion de reservas rodante de 10
        read ( letaux, *, iostat = errleca ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1002
        do i = 1,NTINTR
            if ( reserva ( i ) .gt. epsilon ) then
                res = (reserva ( i ) - epsilon) / Base
            else
                res = 0.0
            endif
            if ( res .gt. 0.0 ) then
                if ( DISPOUH (u, i ) .eq. 1 ) then
                    min = CalOferResR10H (u, i)
                    if ( res .gt. min ) then
                        write(234,*) 'rodante 10, unidad:',nombunih(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                else
                    res = 0.0
                endif
                lbMILP (IRR10H + u + (i-1)*NumUniHid - 1 ) =  res
            endif
        enddo
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRERO10U.csv'
       call ParaProceso ( bmensaje )
    endif
enddo

1002 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) u
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRERO10U.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif

! Abre archivos csv de resultados de reserva rodante suplementaria por unidad y por intervalo
OPEN ( 159, FILE = trim(rut_dat_1)//'D_RESREROSUU.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
errleca = 0

! para las unidades de rango continuo
do u = 1 , NumUniRC
    if ( u == 67 ) then
        continue
    endif
    read ( 159, 100, iostat = ierror ) letaux
    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
!       se lee asignacion de reservas rodante suplementaria
        read ( letaux, *, iostat = errleca  ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1003
        do i = 1,NTINTR
            if ( reserva ( i ) .gt. epsilon ) then
                res = (reserva ( i ) - epsilon) / Base
            else
                res = 0.0
            endif
            if ( res .gt. 0.0 ) then
                if ( DISPOURC (u, i ) .eq. 1 ) then
                    min = CalOferResRxRC (u, i)
                    if ( res .gt. min ) then
                        write(234,*) 'rodante sup, unidad:',nombunirc(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                else
                    res = 0.0
                endif
                lbMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 ) =  res
            endif
        enddo
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESREROSUU.csv'
       call ParaProceso ( bmensaje )
    endif
enddo
! para las unidades de rango discontinuo
do u = 1 , NumUniRD
    read ( 159, 100, iostat = ierror ) letaux
    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
!       se lee asignacion de reservas rodante suplementaria
        read ( letaux, *, iostat = errleca  ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1003
        do i = 1,NTINTR
            modo = 2 !!! pendiente definir modo
            if ( reserva ( i ) .gt. epsilon ) then
                res = (reserva ( i ) - epsilon) / Base
            else
                res = 0.0
            endif
            if ( res .gt. 0.0 ) then
                if ( DISPOURD (u, modo, i ) .eq. 1 ) then
                    min = CalOferResRxRD (u, modo, i)
                    if ( res .gt. min ) then
                        write(234,*) 'rodante sup, unidad:',nombunird(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                else
                    res = 0.0
                endif
                lbMILP ( IRRSRD + INIURDI ( u, i ) + modo - 1 ) =  res
            endif
        enddo
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESREROSUU.csv'
       call ParaProceso ( bmensaje )
    endif
enddo
! para las unidades hidro
do u = 1 , NumUniHid
    read ( 159, 100, iostat = ierror ) letaux
    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
!       se lee asignacion de reservas rodante suplementaria
        read ( letaux, *, iostat = errleca  ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1003
        do i = 1,NTINTR
            if ( reserva ( i ) .gt. epsilon ) then
                res = (reserva ( i ) - epsilon) / Base
            else
                res = 0.0
            endif
            if ( res .gt. 0.0 ) then
                if ( DISPOUH (u, i ) .eq. 1 ) then
                    min = CalOferResRxH (u, i)
                    if ( res .gt. min ) then
                        write(234,*) 'rodante sup, unidad:',nombunih(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                else
                    res = 0.0
                endif
                lbMILP ( IRRSH + u + (i-1)*NumUniHid - 1 ) =  res
            endif
        end do
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESREROSUU.csv'
       call ParaProceso ( bmensaje )
    endif
enddo

1003 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) u
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESREROSUU.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif

errleca = 0
! si se desea proteger reservas no rodantes
if ( SiRelNRod .eq. 1 ) then

    ! Abre archivos csv de resultados de reserva no rodante de 10 minutos por unidad y por intervalo
    OPEN ( 149, FILE = trim(rut_dat_1)//'D_RESRENRO10U.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
    ! para las unidades de rango continuo
    do u = 1 , NumUniRC
        read ( 149, 100, iostat = ierror ) letaux
        if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
    !       se lee asignacion de reservas no rodante de 10
            read ( letaux, *, iostat = errleca  ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
            if ( errleca .ne. 0 ) go to 1004
            do i = 1,NTINTR
                res = 0.0
    !           si la unidad esta disponible y obligada a operar
                if ( DispoURC (u, i) .eq. 1 .and. AsignURC (u, i) .eq. 0 ) then
                    res = 0.0
                endif
    !           si la unidad esta disponible y no obligada a operar
                if ( DispoURC (u, i) .eq. 1 .and. AsignURC (u, i) .eq. 1 ) then
                    res = (reserva ( i ) - epsilon) / Base
                endif
                if ( res .gt. 0.0 ) then
                    min = OferResNR10RC (u, i)
                    if ( res .gt. min ) then
                        write(234,*) 'no rodante 10, unidad:',nombunirc(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                    lbMILP (IRNR10RC + u + (i-1)*NumUniRC - 1 ) =  res
                    ubMILP ( IARC + u + (i-1)*NumUniRC - 1 ) =  0.0
                    lbMILP ( IARC + u + (i-1)*NumUniRC - 1 ) =  0.0
                endif
            enddo
        else	
           bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRENRO10U.csv'
           call ParaProceso ( bmensaje )
        endif
    enddo
    ! para las unidades de rango discontinuo
    do u = 1 , NumUniRD
        read ( 149, 100, iostat = ierror ) letaux
        if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
    !       se lee asignacion de reservas no rodante de 10
            read ( letaux, *, iostat = errleca  ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
            if ( errleca .ne. 0 ) go to 1004
            do i = 1,NTINTR
                modo = 2 !!! pendiente definir modo
                res = 0.0
    !           si la unidad esta disponible y obligada a operar
                if ( DispoURD (u, modo, i) .eq. 1 .and. AsignURD (u, modo, i) .eq. 0 ) then
                    res = 0.0
                endif
    !           si la unidad esta disponible y no obligada a operar
                if ( DispoURD (u, modo, i) .eq. 1 .and. AsignURD (u, modo, i) .eq. 1 ) then
                    res = (reserva ( i ) - epsilon) / Base
                endif
                if ( res .gt. 0.0 ) then
                    min = OferResNR10RD (u, modo, i)
                    if ( res .gt. min ) then
                        write(234,*) 'no rodante 10, unidad:',nombunird(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                    lbMILP ( IRNR10RD + INIURDI ( u, i ) + modo - 1 ) =  res
                    ubMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) =  0.0
                    lbMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) =  0.0
                endif
            enddo
        else	
           bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRENRO10U.csv'
           call ParaProceso ( bmensaje )
        endif
    enddo
    ! para las unidades hidro
    do u = 1 , NumUniHid
        read ( 149, 100, iostat = ierror ) letaux
        if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
    !       se lee asignacion de reservas no rodante de 10
            read ( letaux, *, iostat = errleca  ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
            if ( errleca .ne. 0 ) go to 1004
            do i = 1,NTINTR
                res = 0.0
    !           si la unidad esta disponible y obligada a operar
                if ( DispoUH (u, i) .eq. 1 .and. AsignUH (u, i) .eq. 0 ) then
                    res = 0.0
                endif
    !           si la unidad esta disponible y no obligada a operar
                if ( DispoUH (u, i) .eq. 1 .and. AsignUH (u, i) .eq. 1 ) then
                    res = (reserva ( i ) - epsilon) / Base
                endif
                if ( res .gt. 0.0 ) then
                    min = OferResNR10H (u, i)
                    if ( res .gt. min ) then
                        write(234,*) 'no rodante 10, unidad:',nombunih(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                    lbMILP (IRNR10H + u + (i-1)*NumUniHid - 1 ) =  res
                    ubMILP ( IAH + u + (i-1)*NumUniHid - 1 ) =  0.0
                    lbMILP ( IAH + u + (i-1)*NumUniHid - 1 ) =  0.0
                endif
            enddo
        else	
           bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRENRO10U.csv'
           call ParaProceso ( bmensaje )
        endif
    enddo

1004 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 ) then
        write ( letr, "(i5)" ) u
        call Elimina_blancos ( letr, 5)
        bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRENRO10U.csv'
        call EnviaMensajeError ( bmensaje )
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '
       call ParaProceso ( bmensaje )
    endif

! Abre archivos csv de resultados de reserva no rodante suplementaria por unidad y por intervalo
    OPEN ( 169, FILE = trim(rut_dat_1)//'D_RESRENROSUU.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )
    errleca = 0

    ! para las unidades de rango continuo
    do u = 1 , NumUniRC
        read ( 169, 100, iostat = ierror ) letaux
        if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
    !       se lee asignacion de reservas no rodante suplementaria
            read ( letaux, *, iostat = errleca ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
            if ( errleca .ne. 0 ) go to 1005
            do i = 1,NTINTR
                res = 0.0
    !           si la unidad esta disponible y obligada a operar
                if ( DispoURC (u, i) .eq. 1 .and. AsignURC (u, i) .eq. 0 ) then
                    res = 0.0
                endif
    !           si la unidad esta disponible y no obligada a operar
                if ( DispoURC (u, i) .eq. 1 .and. AsignURC (u, i) .eq. 1 ) then
                    res = (reserva ( i ) - epsilon) / Base
                endif
                if ( res .gt. 0.0 ) then
                    min = OferResNRxRC (u, i)
                    if ( res .gt. min ) then
                        write(234,*) 'no rodante 10, unidad:',nombunirc(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                    lbMILP (IRNRSRC + u + (i-1)*NumUniRC - 1 ) =  res
                    ubMILP ( IARC + u + (i-1)*NumUniRC - 1 ) =  0.0
                    lbMILP ( IARC + u + (i-1)*NumUniRC - 1 ) =  0.0
                endif
            enddo
        else	
           bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRENROSUU.csv'
           call ParaProceso ( bmensaje )
        endif
    enddo
    ! para las unidades de rango discontinuo
    do u = 1 , NumUniRD
        read ( 169, 100, iostat = ierror ) letaux
        if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
    !       se lee asignacion de reservas no rodante suplementaria
            read ( letaux, *, iostat = errleca  ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
            if ( errleca .ne. 0 ) go to 1005
            do i = 1,NTINTR
                modo = 2 !!! pendiente definir modo
                res = 0.0
    !           si la unidad esta disponible y obligada a operar
                if ( DispoURD (u, modo, i) .eq. 1 .and. AsignURD (u, modo, i) .eq. 0 ) then
                    res = 0.0
                endif
    !           si la unidad esta disponible y no obligada a operar
                if ( DispoURD (u, modo, i) .eq. 1 .and. AsignURD (u, modo, i) .eq. 1 ) then
                    res = (reserva ( i ) - epsilon) / Base
                endif
                if ( res .gt. 0.0 ) then
                    min = OferResNRxRD (u, modo, i)
                    if ( res .gt. min ) then
                        write(234,*) 'no rodante 10, unidad:',nombunird(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                    lbMILP ( IRNRSRD + INIURDI ( u, i ) + modo - 1 ) =  res
                    ubMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) =  0.0
                    lbMILP ( IARD + INIURDI ( u, i ) + modo - 1 ) =  0.0
                endif
            enddo
        else	
           bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRENROSUU.csv'
           call ParaProceso ( bmensaje )
        endif
    enddo
    ! para las unidades hidro
    do u = 1 , NumUniHid
        read ( 169, 100, iostat = ierror ) letaux
        if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
    !       se lee asignacion de reservas no rodante suplementaria
            read ( letaux, *, iostat = errleca  ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
            if ( errleca .ne. 0 ) go to 1005
            do i = 1,NTINTR
                res = 0.0
    !           si la unidad esta disponible y obligada a operar
                if ( DispoUH (u, i) .eq. 1 .and. AsignUH (u, i) .eq. 0 ) then
                    res = 0.0
                endif
    !           si la unidad esta disponible y no obligada a operar
                if ( DispoUH (u, i) .eq. 1 .and. AsignUH (u, i) .eq. 1 ) then
                    res = (reserva ( i ) - epsilon) / Base
                endif
                if ( res .gt. 0.0 ) then
                    min = OferResNRxH (u, i)
                    if ( res .gt. min ) then
                        write(234,*) 'no rodante 10, unidad:',nombunih(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                    lbMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 ) =  res
                    ubMILP ( IAH + u + (i-1)*NumUniHid - 1 ) =  0.0
                    lbMILP ( IAH + u + (i-1)*NumUniHid - 1 ) =  0.0
                endif
            enddo
        else	
           bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRENROSUU.csv'
           call ParaProceso ( bmensaje )
        endif
    enddo

    endif

    1005 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 ) then
        write ( letr, "(i5)" ) u
        call Elimina_blancos ( letr, 5)
        bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRENROSUU.csv'
        call EnviaMensajeError ( bmensaje )
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '
       call ParaProceso ( bmensaje )
    endif

! para las unidades de rango continuo
do u = 1 , NumUniRC
    if ( u == 67 ) then
        continue
    endif
    do i = 1,NTINTR
!       Se revisa que la reserva de regulacion + reserva rodante 10 + reserva rodante suplementaria, quepa en el rango operativo de la unidad
        res = PotMinGRC ( u, i ) + lbMILP ( IRRERC + u + (i-1)*NumUniRC - 1 ) + lbMILP (IRR10RC + u + (i-1)*NumUniRC - 1 ) + lbMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 )
        if ( res .gt. PotMaxGRC ( u, i ) ) then
            min = res - PotMaxGRC ( u, i )
            if ( min .le. lbMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 ) ) then
                lbMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 ) = lbMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 ) - min - epsilon
            else
                lbMILP ( IRRSRC + u + (i-1)*NumUniRC - 1 ) = 0.0
            endif
        endif
!       Se revisa que la reserva de regulacion + reserva rodante 10 sea menor a la rampa de 10 minutos de la unidad
!        res = lbMILP ( IRRERC + u + (i-1)*NumUniRC - 1 ) + lbMILP (IRR10RC + u + (i-1)*NumUniRC - 1 )
!        if ( res .gt. RamEmer10RC ( u ) ) then
!            min = res - RamEmer10RC ( u )
!            if ( min .le. lbMILP (IRR10RC + u + (i-1)*NumUniRC - 1 ) ) then
!                lbMILP (IRR10RC + u + (i-1)*NumUniRC - 1 ) = lbMILP (IRR10RC + u + (i-1)*NumUniRC - 1 ) - min - epsilon
!            else
!                lbMILP (IRR10RC + u + (i-1)*NumUniRC - 1 ) = 0.0
!            endif
!        endif
!       Se revisa que la reserva no rodante 10 + reserva no rodante suplementaria quepa en el rango operativo de la unidad
!       si es unidad disponible, asignable y cordinable en este periodo
        if ( DispoURC ( u , i ) .eq. 1 .and. CoordURC ( u , i ) .eq. 1 .and. AsignURC ( u , i ) .eq. 1 .and. SiRelNRod .eq. 1 ) then
            maximo =  max ( OferResNR10RC ( u, i ), OferResNRxRC ( u, i ) )
            res = lbMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 ) + lbMILP (IRNRSRC + u + (i-1)*NumUniRC - 1 )
            if ( res .gt. maximo ) then
                min = res - maximo
                if ( min .le. lbMILP ( IRNRSRC + u + (i-1)*NumUniRC - 1 ) ) then
                    lbMILP ( IRNRSRC + u + (i-1)*NumUniRC - 1 ) = lbMILP ( IRNRSRC + u + (i-1)*NumUniRC - 1 ) - min - epsilon
                else
                    min =  min - lbMILP (IRNRSRC + u + (i-1)*NumUniRC - 1 )
                    lbMILP ( IRNRSRC + u + (i-1)*NumUniRC - 1 ) = 0.0
                    if ( min .le. lbMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 ) ) then
                        lbMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 ) = lbMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 ) - min - epsilon
                    else
                        lbMILP ( IRNR10RC + u + (i-1)*NumUniRC - 1 ) = 0.0
                    endif
                endif
            endif
        endif
    enddo
enddo

! para las unidades hidro
do u = 1 , NumUniHid
    do i = 1,NTINTR
!       Se revisa que la reserva de regulacion + reserva rodante 10 + reserva rodante suplementaria, quepa en el rango operativo de la unidad
        res = PotMinUniH ( u, i ) + lbMILP ( IRREH + u + (i-1)*NumUniHid - 1 ) + lbMILP (IRR10H + u + (i-1)*NumUniHid - 1 ) + lbMILP ( IRRSH + u + (i-1)*NumUniHid - 1 )
        if ( res .gt. PotMaxUniH ( u, i ) ) then
            min = res - PotMaxUniH ( u, i )
            if ( min .le. lbMILP ( IRRSH + u + (i-1)*NumUniHid - 1 ) ) then
                lbMILP ( IRRSH + u + (i-1)*NumUniHid - 1 ) = lbMILP ( IRRSH + u + (i-1)*NumUniHid - 1 ) - min - epsilon
            else
                lbMILP ( IRRSH + u + (i-1)*NumUniHid - 1 ) = 0.0
            endif
        endif
!       Se revisa que la reserva de regulacion + reserva rodante 10 sea menor a la rampa de 10 minutos de la unidad
!        res = lbMILP ( IRREH + u + (i-1)*NumUniHid - 1 ) + lbMILP (IRR10H + u + (i-1)*NumUniHid - 1 )
!        if ( res .gt. RamEmer10H ( u ) ) then
!            min = res - RamEmer10H ( u )
!            if ( min .le. lbMILP (IRR10H + u + (i-1)*NumUniHid - 1 ) ) then
!                lbMILP (IRR10H + u + (i-1)*NumUniHid - 1 ) = lbMILP (IRR10H + u + (i-1)*NumUniHid - 1 ) - min - epsilon
!            else
!                lbMILP (IRR10H + u + (i-1)*NumUniHid - 1 ) = 0.0
!            endif
!        endif
!       Se revisa que la reserva no rodante 10 + reserva no rodante suplementaria quepa en el rango operativo de la unidad
!       si es unidad disponible, asignable y cordinable en este periodo
        if ( DispoUH ( u , i ) .eq. 1 .and. CoordUH ( u , i ) .eq. 1 .and. AsignUH ( u , i ) .eq. 1 .and. SiRelNRod .eq. 1 ) then
            maximo =  max ( OferResNR10H ( u, i ), OferResNRxH ( u, i ) )
            res = lbMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 ) + lbMILP (IRNRSH + u + (i-1)*NumUniHid - 1 )
            if ( res .gt. maximo ) then
                min = res - maximo
                if ( min .le. lbMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 ) ) then
                    lbMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 ) = lbMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 ) - min - epsilon
                else
                    min =  min - lbMILP (IRNRSH + u + (i-1)*NumUniHid - 1 )
                    lbMILP ( IRNRSH + u + (i-1)*NumUniHid - 1 ) = 0.0
                    if ( min .le. lbMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 ) ) then
                        lbMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 ) = lbMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 ) - min - epsilon
                    else
                        lbMILP ( IRNR10H + u + (i-1)*NumUniHid - 1 ) = 0.0
                    endif
                endif
            endif
        endif
    enddo
enddo

! para las unidades de rango discontinuo
do u = 1 , NumUniRD
!   para los modos de operacion, excepto el paro
    do modo = 2, NumModRD ( u )
        do i = 1,NTINTR
    !       Se revisa que la reserva de regulacion + reserva rodante 10 + reserva rodante suplementaria, quepa en el rango operativo de la unidad
            res = PotMinGRD ( u, modo, i ) + lbMILP ( IRRERD + INIURDI ( u, i ) + modo - 1 ) + lbMILP (IRR10RD + INIURDI ( u, i ) + modo - 1 ) + lbMILP ( IRRSRD + INIURDI ( u, i ) + modo - 1 )
            if ( res .gt. PotMaxGRD ( u, modo, i ) ) then
                min = res - PotMaxGRD ( u, modo, i )
                if ( min .le. lbMILP ( IRRSRD + INIURDI ( u, i ) + modo - 1 ) ) then
                    lbMILP ( IRRSRD + INIURDI ( u, i ) + modo - 1 ) = lbMILP ( IRRSRD + INIURDI ( u, i ) + modo - 1 ) - min - epsilon
                else
                    lbMILP ( IRRSRD + INIURDI ( u, i ) + modo - 1 ) = 0.0
                endif
            endif
    !       Se revisa que la reserva de regulacion + reserva rodante 10 sea menor a la rampa de 10 minutos de la unidad
!            res = lbMILP ( IRRERD + INIURDI ( u, i ) + modo - 1 ) + lbMILP ( IRR10RD + INIURDI ( u, i ) + modo - 1 )
!            if ( res .gt. RamEmer10RD ( u, modo ) ) then
!                min = res - RamEmer10RD ( u, modo )
!                if ( min .le. lbMILP (IRR10RD + INIURDI ( u, i ) + modo - 1 ) ) then
!                    lbMILP (IRR10RD + INIURDI ( u, i ) + modo - 1 ) = lbMILP (IRR10RD + INIURDI ( u, i ) + modo - 1 ) - min - epsilon
!                else
!                    lbMILP (IRR10RD + INIURDI ( u, i ) + modo - 1 ) = 0.0
!                endif
!            endif
        enddo
    enddo
enddo

close (179)
close (119)
close (159)
close (149)
close (169)

100 format ( a )

return
end

! ---------------------------------------------------------------------
! Se lee informacion de grupos de unidades de importacion             *
!                            .                                        *
!                                                                     *
! Instituto Nacional de Electricidad y Energias Limpias               *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Agosto de 2018                                                      *
! ---------------------------------------------------------------------   
SUBROUTINE LeeGposImportacion

use ParAUHE

Implicit none   

integer i, k, g, u, ierror, bloque, icuenta, errleca 

CHARACTER letr*5

character*3000 letaux

! -----------------------------
! * Se leen datos de GPOIMP *
! -----------------------------
OPEN (UNIT = 116, FILE = rut_dat_1( 1 : long_ruta )//'GPOIMP.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
errleca = 0
g = 0
if ( ierror .eq. 0 ) then
    read ( 116, 100, iostat = ierror ) letaux
    do while ( ierror .eq. 0 .and.  len_trim(letaux) .ne. 0 ) 
        g = g + 1
        read ( letaux, *, iostat = errleca ) NomGpoImp(g), NumUniGpoImp(g), ( PotMaxGpoImp (g, i),  i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1001
        read ( 116, 100, iostat = ierror ) letaux
    enddo
    
else
  bmensaje = 'ERROR DE LECTURA ARCHIVO GPOIMP.csv'
  call ParaProceso ( bmensaje )
endif

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
   write ( letr, "(i5)" ) g
   call Elimina_blancos ( letr, 5)
   bmensaje = 'ERROR DE LECTURA ARCHIVO GPOIMP.csv'
   call EnviaMensajeError ( bmensaje )
   Bmensaje = 'GRUPO: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 116 )

NumGpoImp = g
ApuUniGpoImp = 0
LisUniGpoImp = 0
icuenta = 1
errleca = 0 

write ( 1, * ) " "
write ( 1, * ) "---------------------------------"
write ( 1, * ) "LIMITES DE GRUPOS DE IMPORTACION "
write ( 1, * ) "---------------------------------"

if ( NumGpoImp .gt. 0 ) then
    do g = 1, NumGpoImp
        bloque = ntintr / 24
        do k = 1, bloque
          write ( 1, 200 ) NomGpoImp ( g ), NumUniGpoImp ( g ), ( PotMaxGpoImp (g, i),  i = 24 * k -23, 24*k  )
        enddo
    enddo

!   Lectura de unidades de gupos de exportación
    OPEN (UNIT = 117, FILE = rut_dat_1( 1 : long_ruta )//'GPOUNIIMP.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
    if ( ierror .eq. 0 ) then
       do g = 1, NumGpoImp
          write ( 1, * ) " "
          write ( 1, 300 ) NomGpoImp ( g )
          ApuUniGpoImp (g) = icuenta
          do u = 1, NumUniGpoImp ( g )
              read ( 117, 100, iostat = ierror ) letaux
              if ( ierror .eq. 0 ) then
                 read ( letaux, *, iostat = errleca ) LisUniGpoImp ( icuenta )
                 if ( errleca .ne. 0 ) go to 1002
                 write ( 1, 400 ) LisUniGpoImp ( icuenta )  , nombunirc  ( LisUniGpoImp ( icuenta )  )
                 icuenta = icuenta + 1
              endif
          enddo
       enddo
       ApuUniGpoImp (NumGpoImp + 1 ) = icuenta
    else
       bmensaje = 'ERROR DE LECTURA ARCHIVO GPOUNIIMP.csv'
       call ParaProceso ( bmensaje )
    endif

    CLOSE ( UNIT = 117 )
    
endif

1002 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
   write ( letr, "(i5)" ) g
   call Elimina_blancos ( letr, 5)
   bmensaje = 'ERROR DE LECTURA ARCHIVO GPOUNIIMP.csv'
   call EnviaMensajeError ( bmensaje )
   Bmensaje = 'GRUPO: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif

PotMaxGpoImp = PotMaxGpoImp/BASE

100 format ( a )
200 format ( a12, 3x, I2, 168 ( f9.2 ) )
300 format ("GRUPO IMPORTACION:" , 2x, a12 )
400 format (  i3, 2x, a12 )
    
return
end Subroutine LeeGposImportacion

! ---------------------------------------------------------------------
! Se lee informacion de grupos de cargass de exportacion              *
!                            .                                        *
!                                                                     *
! Instituto Nacional de Electricidad y Energias Limpias               *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Agosto de 2018                                                      *
! ---------------------------------------------------------------------   
SUBROUTINE LeeGposExportacion

use ParAUHE

Implicit none   

integer i, k, g,  d, ierror, bloque, icuenta, errleca 

CHARACTER letr*5

character*3000 letaux

! -----------------------------
! * Se leen datos de GPOEXP *
! -----------------------------
OPEN (UNIT = 116, FILE = rut_dat_1( 1 : long_ruta )//'GPOEXP.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0
g = 0
if ( ierror .eq. 0 ) then
    read ( 116, 100, iostat = ierror ) letaux
    do while ( ierror .eq. 0 .and.  len_trim(letaux) .ne. 0 ) 
        g = g + 1
        read ( letaux, *, iostat = errleca ) NomGpoExp(g), NumCarGpoExp(g), ( PotMaxGpoExp (g, i),  i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1001
        read ( 116, 100, iostat = ierror ) letaux
    enddo
    
else
   bmensaje = 'ERROR DE LECTURA ARCHIVO GPOEXP.csv'
   call ParaProceso ( bmensaje )
endif

1001 continue
     
! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
   write ( letr, "(i5)" ) g
   call Elimina_blancos ( letr, 5)
   bmensaje = 'ERROR DE LECTURA ARCHIVO GPOEXP.csv'
   call EnviaMensajeError ( bmensaje )
   Bmensaje = 'GRUPO: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 116 )

NumGpoExp = g
ApuCarGpoExp = 0
LisCarGpoExp = 0
icuenta = 1

write ( 1, * ) " "
write ( 1, * ) "---------------------------------"
write ( 1, * ) "LIMITES DE GRUPOS DE EXPORTACION "
write ( 1, * ) "---------------------------------"

errleca = 0
if ( NumGpoExp .gt. 0 ) then
    do g = 1, NumGpoImp
        bloque = ntintr / 24
        do k = 1, bloque
          write ( 1, 200 ) NomGpoExp ( g ), NumCarGpoExp ( g ), ( PotMaxGpoExp (g, i),  i = 24 * k -23, 24*k  )
        enddo
    enddo

!   Lectura de cargas de gupos de exportación
    OPEN (UNIT = 117, FILE = rut_dat_1( 1 : long_ruta )//'GPOCAREXP.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
    if ( ierror .eq. 0 ) then
       do g = 1, NumGpoImp
          write ( 1, * ) " "
          write ( 1, 300 ) NomGpoExp ( g )
          ApuCarGpoExp (g) = icuenta
          do d = 1, NumCarGpoExp ( g )
              read ( 117, 100, iostat = ierror ) letaux
              if ( ierror .eq. 0 ) then
                 read ( letaux, *, iostat = errleca ) LisCarGpoExp ( icuenta ) 
                 if ( errleca .ne. 0 ) go to 1002
                 write ( 1, 400 ) LisCarGpoExp ( icuenta )  , nombcar  ( LisCarGpoExp ( icuenta )  )
                 icuenta = icuenta + 1
              endif
          enddo
       enddo
       ApuCarGpoExp (NumGpoExp + 1 ) = icuenta
    else
       bmensaje = 'ERROR DE LECTURA ARCHIVO GPOCAREXP.csv'
       call ParaProceso ( bmensaje )
    endif

    CLOSE ( UNIT = 117 )
    
endif

1002 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
   write ( letr, "(i5)" ) g
   call Elimina_blancos ( letr, 5)
   bmensaje = 'ERROR DE LECTURA ARCHIVO GPOCAREXP.csv'
   call EnviaMensajeError ( bmensaje )
   Bmensaje = 'GRUPO: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif

PotMaxGpoExp = PotMaxGpoExp/BASE


100 format ( a )
200 format ( a12, 3x, I2, 168 ( f9.2 ) )
300 format ("GRUPO EXPORTACION:" , 2x, a12 )
400 format (  i3, 2x, a12 )
    
return
end Subroutine LeeGposExportacion


Subroutine LeeAsigRegMDA
! ---------------------------------------------------------------------
! Se leen las asignaciones manuales de reserva de regulacion para el  *
! MDA.                                                                *
!                                                                     *
! Instituto Nacional de Electricidad y Energias Limpias               *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre de 2019                                                   *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE

implicit none

INTEGER   i, u, ierror, modo, tipo, ro, errleca

real*8    reserva ( maxint ), res, epsilon, min

character*12 nombre
character*5 letr

character*3000 letaux


letaux = 'letaux'
ierror = 0
epsilon = 0.005
reserva = 0.0
errleca = 0

! Abre archivos csv de resultados de reserva de regulacion asignada por unidad y por intervalo
OPEN ( 179, FILE = trim(rut_dat_1)//'D_RESRERESEU.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 6000 )

! para las unidades de rango continuo
do u = 1 , NumUniRC
    read ( 179, 100, iostat = ierror ) letaux
    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
!       se lee asignacion de reservas de regulacion
        read ( letaux, *, iostat = errleca ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1001
        do i = 1,NTINTR
            if ( reserva ( i ) .gt. epsilon ) then
                res = (reserva ( i ) - epsilon) / Base
            else
                res = 0.0
            endif
            if ( res .gt. 0.0 ) then
                if ( DISPOURC (u, i ) .eq. 1 ) then
                    min = CalOferResRegRC (u, i)
                    do ro = 1, NoRaOpRC ( u )*SiBandProh
                        if ( min .gt. CalOferRegRORC ( u, ro, i ) .and. RaRegSupRC ( u, ro, i ) .gt. RaRegInfRC ( u, ro, i )) then
                            min = CalOferRegRORC ( u, ro, i )
                        endif
                    enddo
                    if ( res .gt. min ) then
                        write(234,*) 'regulacion, unidad:',nombunirc(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                        res = min
                    endif
                    lbMILP ( IRRERC + u + (i-1)*NumUniRC - 1 ) =  res
                else
                    min = 0.0
                    write(234,*) 'regulacion, unidad:',nombunirc(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                    exit
                endif
            endif
        enddo
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRERESEU.csv'
       call ParaProceso ( bmensaje )
    endif
enddo
! para las unidades de rango discontinuo
do u = 1 , NumUniRD
    read ( 179, 100, iostat = ierror ) letaux
    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
!       se lee asignacion de reservas de regulacion
        read ( letaux, *, iostat = errleca ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1001
        do i = 1,NTINTR
            modo = 2 !!! pendiente definir modo
            if ( reserva ( i ) .gt. epsilon ) then
                res = (reserva ( i ) - epsilon) / Base
            else
                res = 0.0
            endif
            if ( res .gt. 0.0 ) then
                if ( res .gt. CalOferResRegRD (u, modo, i) ) then
                    write(234,*) 'regulacion, unidad:',nombunird(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                    res = CalOferResRegRD (u, modo, i)
                endif
                lbMILP ( IRRERD + INIURDI ( u, i ) + modo - 1  ) =  res
            endif
        enddo
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRERESEU.csv'
       call ParaProceso ( bmensaje )
    endif
enddo
! para las unidades hidro
do u = 1 , NumUniHid
    read ( 179, 100, iostat = ierror ) letaux
    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
!       se lee asignacion de reservas de regulacion
        read ( letaux, *, iostat = errleca ) nombre, tipo, ( reserva ( i ), i = 1, NTINTR )
        if ( errleca .ne. 0 ) go to 1001
        do i = 1,NTINTR
            if ( reserva ( i ) .gt. epsilon ) then
                res = (reserva ( i ) - epsilon) / Base
            else
                res = 0.0
            endif
            if ( res .gt. 0.0 ) then
                min = CalOferResRegH (u, i)
                do ro = 1, NoRaOpH ( u )*SiBandProh
                    if ( min .gt. CalOferRegROH ( u, ro, i ) .and. RaRegSupH ( u, ro, i ) .gt. RaRegInfH ( u, ro, i ) ) then
                        min = CalOferRegROH ( u, ro, i )
                    endif
                enddo
                if ( res .gt. min ) then
                    write(234,*) 'regulacion, unidad:',nombunih(u),' intervalo: ',i,' asignado: ',res, ' posible: ',min
                    res = min
                endif
                lbMILP ( IRREH + u + (i-1)*NumUniHid - 1 ) =  res
            endif
        enddo
    else	
       bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRERESEU.csv'
       call ParaProceso ( bmensaje )
    endif
enddo

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
   write ( letr, "(i5)" ) u
   call Elimina_blancos ( letr, 5)
   bmensaje = 'ERROR DE LECTURA ARCHIVO D_RESRERESEU.csv'
   call EnviaMensajeError ( bmensaje )
   Bmensaje = 'UNIDAD: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif


close (179)

100 format ( a )

return
end


!**************************************************************************
!                                                                         *
!     Propósito:                                                          *
!         Se leen datos de unidades de propiedad conjunta                 *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!                                Febrero 2020                           *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************

Subroutine LeeUPC

Use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, NumUPC, indiceup, &
                   NTINTR, ofcapupc, base

Implicit none

integer ierror, i, ierr, u

character*20 let

character*6000 letaux

ierror = 0
u = 0
letaux = ""
ierr = 0

! Abre archivos de datos de unidades de propiedad conjunta
OPEN ( UNIT = 4, FILE = rut_dat_1( 1 : long_ruta )//'UPCS.csv', IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 6000 )

! Lee información de division hasta encontrar fin de informacion
read ( 4, 100, iostat = ierror ) letaux
do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
    u = u + 1
    read ( letaux, *, iostat = ierr ) let, indiceup(u), ( ofcapupc ( u, i ), i = 1, NTINTR )
    if ( ierr .ne. 0 ) go to 1000
    read ( 4, 100, iostat = ierror ) letaux
enddo

1000 continue
     
NumUPC = u

if ( ierr .ne. 0 ) then
   bmensaje = '!!! ERROR EN LECTURA DE ARCHIVO UPCS.CSV'
   call EnviaMensajeError ( bmensaje )
   bmensaje = '!!! REGISTRO '// trim (LETAUX(1:30))
   call EnviaMensajeError ( bmensaje )
endif

ofcapupc = ofcapupc/base

100 format ( a )

close ( unit = 4 )

end subroutine LeeUPC    