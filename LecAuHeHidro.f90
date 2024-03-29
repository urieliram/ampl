!
!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!      P R O G R A M A S   D E   A P L I C A C I O N   A V A N Z A D A    *
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
!     Prop�sito:                                                          *
!         Realiza las lecturas de los datos necesarios para la asignacion *
!         de unidades hidroelectricas; preparacion de datos               *
!                                                                         *
!     Nombre y fecha de implementaci�n:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Junio 2013                            *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                Septiembre 2016                          *
!**************************************************************************
SUBROUTINE Lecturas_Hidro_1 

USE ParAUHE
Use ParAuHeHidro
use ParGloRed, only: BasMva

!Los volumenes de los embalses y las aportaciones estan en millones de metros cubicos (MMC)
!Las unidades dentro del modelo son miles de metros cubicos (mMC)
MValor = 3.6

!Lee datos de vias divergentes (min y max del gasto turbinado sobre ella)
call Lectura_132_Viadiv_1

!Lee modelos de las unidades hidro
call COEFMH_104_COEFMH

!Lee datos de condicones iniciales del embalse, de aqui sale tambien el namo,
!politicas de operacion en embalses
call vasoen_1

!Lee datos de dise�o de las unidades
call MODLOH_109_MODLOH

!Calcula el gasto maximo de la unidad por modelo
call Gasto_max_unidad_mod

!Lee datos de las vias convergentes
call VIACON_131

!Lee datos de aportacion natural y otros usos del agua del embalse
call VASOHE_130 

!Lee agua que viene en transito en las vias (son condiciones iniciales)
call VASOCI_126VAVICO

!Lee datos sobre vertidos programados sobre las vias
call LVIAHE_134VIAHE

!Calcula las aportaciones netas a los embalses
call Cal_Aporta_Embalse

!Imprime red hidraulica
call imprime_red_hidro

!Validar politicas en embalses
call valida_politica

EfijEmb = EfijEmb / BasMva

END SUBROUTINE Lecturas_Hidro_1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE Abre_Archivos_Generales_1
!
!***********************************************************************
!  Proposito:
!             
!       Apertura de archivos de debugger y bitacora, nunca se deben de cerrar
!
!  Registro de revisiones:
!       Fecha             Programador          Descripcion de cambios
!    ============    ===================       =======================
!    19 Septiembre 2009    Isaias Guillen Moya       Codigo original
!    29 Mayo 2013          Juan Alvarez Lopez        Adecuacion Hidro MIP
!    27 Septiembre 2016    Jose Luis Ceciliano       Adecuacion Hidro AUHE
!
!***********************************************************************
use ParAUHE
use ParAuHeHidro, only: unlg12_1
!
IMPLICIT NONE
      
integer error_h
!
!--------------------------------------------
!* Abre archivos resultados generales Hidro *
!--------------------------------------------
OPEN ( UNIT = unlg12_1, FILE = rut_res//'Data_Hidro.res', IOSTAT = error_h, STATUS='UNKNOWN', RECORDSIZE = 3024 )

!--------------------------------------------
!* Abre archivos violaciones Hidro *
!--------------------------------------------
OPEN ( UNIT = 559, FILE = rut_res//'Infactibilidad_Hidro.res', IOSTAT = error_h, STATUS='UNKNOWN', RECORDSIZE = 3024 )

END SUBROUTINE Abre_Archivos_Generales_1

SUBROUTINE Lectura_Dimension_Hidro_1
!***********************************************************************
!  Proposito:
!       
!    Lectura del archivo 24. DIMENS (Dimensiones para el caso) 
!
!  Registro de revisiones:
!       Fecha             Programador          Descripcion de cambios
!    ============    ===================       =======================
!    30 Octubre 2009    Isaias Guillen Moya       Codigo original
!    29 Mayo 2013       Juan Alvarez Lopez        Adecuacion Hidro MIP
!    27 Septiembre 2016 Jose Luis Ceciliano       Adecuacion Hidro AUHE
!***********************************************************************
USE ParAuHeHidro
use ParAUHE, only: rut_dat_1, bmensaje
  
IMPLICIT NONE
!----------------------------
!* Declaracion de variables *
!----------------------------
CHARACTER fecha_Ej*19, cerror*3
INTEGER error_h
INTEGER ibanbit
INTEGER E1,E2

!----------------------------------------
!* 24. DIMENS (Dimensiones para el caso) *
!----------------------------------------
OPEN (UNIT = 24, DEFAULTFILE = rut_dat_1, FILE='DIMENS.csv', STATUS='OLD', IOSTAT = error_h )
!-------------------------------------------------
!* Lee la informaci�n del archivo del Dimension  *
!-------------------------------------------------
READ ( 24, *, iostat = error_h  ) E1,E2
!----------------
!* No hay error *
!----------------
IF ( error_h .eq. 0 ) THEN
    !---------------------
    !* N�mero de cuencas.*
    !---------------------
    nmcuen = E1
    !-------------------------------------
    !* N�mero de plantas hidroel�ctricas *
    !-------------------------------------
    nplhid = E2
    if ( nplhid .eq. 0 ) then
        nmcuen = 0
    endif
ELSE
    ibanbit = 1
    !-------------------------------------------
    !* Escribe mensaje a bitacora y a pantalla *
    !-------------------------------------------
    WRITE ( cerror , 1350 ) error_h
    CALL FechaEjecucion (fecha_Ej)
    !
    BMensaje = fecha_Ej//' Error en SUBROUTINE Lectura_Dimension_Hidro: Tabla DIMENS.csv  '
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    !
    BMensaje = '                       Codigo de error '//cerror
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    BMensaje = fecha_Ej//' TERMINACION ANORMAL '
    !
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    WRITE(6,*)'1'                  
    ! 
    BMensaje = fecha_Ej//'1'
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    !
    CALL EXIT
END IF
CLOSE ( 24 )

WRITE (unlg12_1, *)' SUBROUTINE Lectura_Dimension_Hidro '
WRITE (unlg12_1, * )'  '
WRITE (UNLG12_1, 6100 )nmcuen
WRITE (UNLG12_1, 6200 )nplhid

6100  FORMAT(5X,'Numero total de cuencas           (nmcuen) = ',I4)
6200  FORMAT(5X,'Numero total de plantas hidro     (nplhid) = ',I4)

1350  FORMAT ( I3.3 )
!
END SUBROUTINE Lectura_Dimension_Hidro_1


SUBROUTINE Lectura_Cuenca
!
!***********************************************************************
!  Proposito:
!    Lectura del archivo 124. CUENCA (Cuencas)
!
!  Registro de revisiones:
!       Fecha             Programador          Descripcion de cambios
!    ============    ===================       =======================
!    30 Octubre 2009    Isaias Guillen Moya       Codigo original
!     4 Junio 2013      Juan Alvarez Lopez        Adecuacion Hidro MIP
!
!***********************************************************************
!
USE ParAuHeHidro
use ParAUHE, only: rut_dat_1, bmensaje
      
      
IMPLICIT NONE
!----------------------------
!* Declaracion de variables *
!----------------------------
INTEGER i, ie, error_h, icuenca, iapunregvas, errleca
INTEGER ibanbit
CHARACTER* 5000 letaux
CHARACTER fecha_Ej*19, cerror*3
!---------------------------------------
!* 124. CUENCA (Cuencas) *
!---------------------------------------
OPEN (UNIT = 124, DEFAULTFILE = rut_dat_1, FILE='CUENCA.csv', STATUS='OLD', IOSTAT = error_h )
ibanbit = 1
!------------------------------------------------------------------
!* INICIACION DEL CONTADOR DE VALLES Y DE LA LISTA DE APUNTADORES *
!* DE VALLES A EMBALSES.                                          *
!------------------------------------------------------------------
i = 0; errleca = 0
DNEMVA ( 1 ) = 1
IF ( error_h .EQ. 0 ) THEN
    READ ( 124, 100, iostat = error_h  ) letaux
    DO WHILE ( (i .lt. nmcuen) .and. (error_h .eq. 0) .and. len_trim(letaux) .ne. 0 )
        i = i + 1
        !------------------------------------------------
        !* Lee la informaci�n del 124. CUENCA (Cuencas)
        !------------------------------------------------
        !----------------
        !* No hay error *
        !----------------
        IF ( error_h .eq. 0 ) THEN
            read ( letaux, *, iostat = errleca ) icuenca,  NOMVAL(i), nmembc ( i ) , iapunregvas
            if ( errleca .ne. 0 ) go to 1000
            NOMVAL(i) = trim ( NOMVAL(i) )
            !---------------------------------------------------------
            !* DETERMINAR EL APUNTADOR AL INICIO DE LOS EMBALSES DEL *
            !* SIGUIENTE VALLE.                                      *
            !---------------------------------------------------------
            DNEMVA ( i + 1 ) = DNEMVA ( i ) + nmembc ( i )
            DO  IE = DNEMVA ( i ) , DNEMVA ( i + 1 ) - 1
                CUEAEM ( IE ) = i
            END DO
        END IF
        READ ( 124, 100, iostat = error_h  ) letaux
    END DO
ELSE
    ibanbit = 1
    !-------------------------------------------
    !* Escribe mensaje a bitacora y a pantalla *
    !-------------------------------------------
    WRITE ( cerror , 1350 ) error_h
    CALL FechaEjecucion (fecha_Ej)
    !
    BMensaje = fecha_Ej//' ERROR EN SUBROUTINE Lectura_Cuenca: Tabla CUENCA.csv  '
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    !
    BMensaje = '                   Codigo de error '//cerror
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    BMensaje = fecha_Ej//' TERMINACION ANORMAL '
    !
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    WRITE(6,*)'1'                  
    ! 
    BMensaje = fecha_Ej//'1'
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    !
    CALL EXIT
END IF
CLOSE ( 124 )
!
GO TO 2000

1000CONTINUE
ibanbit = 1
!-------------------------------------------
!* Escribe mensaje a bitacora y a pantalla *
!-------------------------------------------
WRITE ( cerror , 1350 ) error_h
CALL FechaEjecucion (fecha_Ej)
!
BMensaje = fecha_Ej//' ERROR EN SUBROUTINE Lectura_Cuenca: Tabla CUENCA.csv  '
CALL Mensaje_cht ( 0, ibanbit, BMensaje )
!
BMensaje = '                   Codigo de error '//cerror
CALL Mensaje_cht ( 0, ibanbit, BMensaje )
BMensaje = fecha_Ej//' TERMINACION ANORMAL '
!
CALL Mensaje_cht ( 0, ibanbit, BMensaje )
WRITE(6,*)'1'                  
! 
BMensaje = fecha_Ej//'1'
CALL Mensaje_cht ( 0, ibanbit, BMensaje )
!
CALL EXIT
2000CONTINUE
!
100   FORMAT ( a )
300   FORMAT ( i )
1350  FORMAT ( I3.3 )
!
END SUBROUTINE Lectura_Cuenca

    
SUBROUTINE Lectura_Vasos
!
!***********************************************************************
!  Proposito:
!       
!       
!    Lectura del archivo 125. VASOS (Vasos) 
!
!  Registro de revisiones:
!       Fecha             Programador          Descripcion de cambios
!    ============    ===================       =======================
!    30 Octubre 2009    Isaias Guillen Moya       Codigo original
!     4 Junio 2013      Juan Alvarez Lopez        Adecuacion Hidro MIP
!
!***********************************************************************
USE ParAuHeHidro
use ParAUHE, only: rut_dat_1, bmensaje
      
IMPLICIT NONE
!----------------------------
!* Declaracion de variables *
!----------------------------
!
INTEGER i, j, nvasos, reg, error_h
INTEGER ivi
INTEGER ibanbit
INTEGER ivas, iare, icuen, iavdiv, iavcon 

REAL rnamo,  limrazproesc, limrazotrusos

REAL * 8 varrea, ivutil_1, vecrea (9)
!
CHARACTER* 5000 letaux


! -----------------------------------------------------------------
! * INICIAR INDICES, APUNTADORES, Y DETERMINAR EL NO. DE EMBALSES *
! * DEL SISTEMA.                                                  *
! -----------------------------------------------------------------
REG = 0
DNVIOU ( 1 ) = 1
DNVIIN ( 1 ) = 1
NVASOS = DNEMVA ( nmcuen + 1 ) - 1

!-----------------------
!* 125. VASOS (Vasos)  *
!-----------------------
OPEN (UNIT = 125, DEFAULTFILE = rut_dat_1, FILE='VASOS.csv', STATUS='OLD', IOSTAT = error_h )

ibanbit = 1
i = 0
READ ( 125, 100, iostat = error_h  ) letaux
DO WHILE ( (i .lt. nvasos) .and. (error_h .eq. 0) .and. len_trim(letaux) .ne. 0 )
    i = i + 1
    
    !------------------------------------------------
    !* Lee la informaci�n del 125. VASOS (Vasos) 
    !------------------------------------------------
    !----------------
    !* No hay error *
    !----------------
    IF ( error_h .eq. 0 ) THEN
        read ( letaux, *, iostat = error_h ) ivas, NOMEMB ( i ), iare, icuen, rnamo, NAMINO ( i ), nmviasd ( i ), iavdiv, nmviasc ( i ), iavcon, limrazproesc, limrazotrusos, &
                           ( VOUTIL ( j, i ), j = 1, 9 ),  ( ELUTIL ( j, i ), j = 1, 9 ), ( vecrea ( j ), j = 1, 9 )
        if ( error_h .eq. 0 ) THEN
            NOMEMB ( i ) = trim ( NOMEMB ( i ) )
            !-----------------------------
            ! Apuntador a vias divergentes
            DNVIOU ( i + 1 ) = DNVIOU ( i ) +  nmviasd ( i )
                    !-----------------------------
            ! Apuntador a vias convergentes
            DNVIIN ( i + 1 ) = DNVIIN ( i ) + nmviasc ( i )
        
            ! Volumen util
            do j = 1, 9
                VOUTIL ( j, i ) = 1000.0*VOUTIL ( j, i )
        
                ! Elevacion util
               ELUTIL ( j, i ) = ELUTIL ( j, i ) - NAMINO ( i )
            enddo
        
            varrea = 0.0
            VolMnEmb ( i ) = IVUTIL_1 ( i , varrea )
            !Se protege contra volumenes utiles negativos
            if ( VolMnEmb ( i ) .lt. 0.0 ) then
                VolMnEmb ( i ) = 0.0
            end if
                
            DO  IVI = DNVIOU ( i ) , DNVIOU ( i + 1 ) - 1
                CUEAVI ( IVI ) = CUEAEM ( i )
            END DO
        endif
    END IF 
    READ ( 125, 100, iostat = error_h  ) letaux
END DO

CLOSE ( 125 )

100 FORMAT ( a )

    
END SUBROUTINE Lectura_Vasos

      
SUBROUTINE Lectura_Plah
!***********************************************************************
!  Proposito:
!       
!    Lectura del archivo 110. PLAH (Plantas hidroel�ctricas)
!
!  Registro de revisiones:
!       Fecha             Programador          Descripcion de cambios
!    ============    ===================       =======================
!    30 Octubre 2009    Isaias Guillen Moya       Codigo original
!     5 Junio 2013      Juan Alvarez Lopez        Adecuacion Hidro MIP
!
!***********************************************************************
USE ParAuHeHidro      
use ParAUHE, only: rut_dat_1, bmensaje
      
IMPLICIT NONE
!----------------------------
!* Declaracion de variables *
!----------------------------
INTEGER i, error_h, contador, iplanta, inodo, iarea, iregunipla, iprior, errleca
INTEGER nmuni, ivi
INTEGER ipin, iuin, NVIAS
CHARACTER fecha_Ej*19, cerror*3, letare*3 
INTEGER * 4 ibanbit
!
CHARACTER* 5000 letaux
!---------------------------------------
!* . PLAH (Plantas hidroel�ctricas) *
!---------------------------------------
OPEN (UNIT = 110, DEFAULTFILE = rut_dat_1, FILE='PLAH.csv', STATUS='OLD', IOSTAT = error_h )
i = 1; errleca = 0
!---------------------------------------------
!* DETERMINAR EL NUMERO DE VIAS DEL SISTEMA. *
!---------------------------------------------
NVIAS = DNVIOU ( DNEMVA ( nmcuen + 1 ) ) - 1
nmuni = 0
DNUNPH ( 1 ) = 1
write ( 1, 100 ) '--------------' 
write ( 1, 100 ) 'PLANTAS HIDRO ' 
write ( 1, 100 ) '--------------' 
write ( 1, 100 ) 'Planta    Nombre       Embalse  Unidades x Planta' 
IF ( error_h .EQ. 0 ) THEN
    READ ( 110, 100, iostat = error_h  ) letaux
    DO WHILE ( (i .le. nplhid) .and. (error_h .eq. 0)  .and. len_trim(letaux) .ne. 0 )
        !------------------------------------------------
        !* Lee la informaci�n del 110. PLAH (Plantas hidroel�ctricas)
        !------------------------------------------------
        !----------------
        !* No hay error *
        !----------------
        IF ( error_h .eq. 0 ) THEN
            read ( letaux, *, iostat = errleca ) iplanta, NOMPLAH ( i ), inodo, IVIA ( i ),  embapl ( i ), iarea, letare, NOUN ( i ), iregunipla, iprior, COEPER ( i ) 
            if ( errleca .ne. 0 ) go to 1000
             NOMPLAH ( i ) = trim (  NOMPLAH ( i ) )
!           --------------------------------------------------------------
!           * SE INCREMENTA EL NUMERO DE UNIDADES, SE ANOTA EN UNA LISTA *
!           * EL NUMERO EXTERNO DE LA PLANTA,  EN OTRA LA VIA EN LA QUE  *
!           * DESCARGA, Y EN OTRA EL NUMERO NUMERO DE UNIDADES ASOCIADAS *
!           --------------------------------------------------------------
            NMUNI = NMUNI +NOUN ( i )
            !Coeficiente de perdidas por planta
            !OJO: era cero en el codigo original!!!
            COEPER ( i ) = 0.0
            !COEPER ( i ) = varrea
            !Cuenca a la que pertenece la planta
            CUEAPL ( i ) = CUEAVI ( IVIA ( i ) )
            !Numero de plantas que desfogan sobre la via
            NPHVI ( IVIA ( i ) ) = NPHVI ( IVIA ( i ) ) + 1
        END IF      
        write ( 1, 400 ) i, NOMPLAH ( i ), NOMEMB ( embapl ( i ) ), NOUN ( i )
        i = i + 1
        READ ( 110, 100, iostat = error_h  ) letaux
    END DO
ELSE
    ibanbit = 1
    !-------------------------------------------
    !* Escribe mensaje a bitacora y a pantalla *
    !-------------------------------------------
    WRITE ( cerror , 1350 ) error_h
    CALL FechaEjecucion (fecha_Ej)
    !
    BMensaje = fecha_Ej//' ERROR EN SUBROUTINE Lectura_Plah_1: Tabla PLAH.csv  '
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    !
    BMensaje = '                   Codigo de error '//cerror
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    BMensaje = fecha_Ej//' TERMINACION ANORMAL '
    !
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    WRITE(6,*)'1'                  
    ! 
    BMensaje = fecha_Ej//'1'
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    !
    CALL EXIT
END IF
CLOSE ( 110 )  

!-----------------------------------------------------------------------
!* FORMAR LISTA DE APUNTADORES DE PLANTAS INTERNAS A UNIDADES INTERNAS *
!-----------------------------------------------------------------------
DO  IPIN = 1 , NPLHID 
    DNUNPH ( IPIN + 1 ) = DNUNPH ( IPIN ) + NOUN( IPIN )
    DO  IUIN = DNUNPH ( IPIN ) , DNUNPH ( IPIN + 1 ) - 1
        !Cuenca a la que pertenece la unidad
        CUEAUN ( IUIN ) = CUEAPL ( IPIN )
    END DO
END DO
!Apuntador planta - via
contador = 0
dnphvi = 0
DO  IVI = 1 , NVIAS
    DO  IPIN = 1 , NPLHID 
        if ( ivia ( ipin ) .eq. ivi ) then
            contador = contador + 1 
            if ( dnphvi ( ivi ) .eq. 0 ) then
                dnphvi ( ivi ) = contador
            end if
            plantas_x_via ( contador ) = ipin
        end if
    end do
END DO

GO TO 2000

1000  CONTINUE
ibanbit = 1
!-------------------------------------------
!* Escribe mensaje a bitacora y a pantalla *
!-------------------------------------------
WRITE ( cerror , 1350 ) error_h
CALL FechaEjecucion (fecha_Ej)
!
BMensaje = fecha_Ej//' ERROR EN SUBROUTINE Lectura_Plah_1: Tabla PLAH.csv  '
CALL Mensaje_cht ( 0, ibanbit, BMensaje )
!
BMensaje = '                   Codigo de error '//cerror
CALL Mensaje_cht ( 0, ibanbit, BMensaje )
BMensaje = fecha_Ej//' TERMINACION ANORMAL '
!
CALL Mensaje_cht ( 0, ibanbit, BMensaje )
WRITE(6,*)'1'                  
! 
BMensaje = fecha_Ej//'1'
CALL Mensaje_cht ( 0, ibanbit, BMensaje )
!
CALL EXIT
2000  CONTINUE
!
100   FORMAT ( a )
300   FORMAT ( i )
400   FORMAT ( i3, 2 ( 7x, a6 ), 10x, i3 )    
1350  FORMAT ( I3.3 )
!
END SUBROUTINE Lectura_Plah
    
    
SUBROUTINE Lectura_132_Viadiv_1
!******************************************************************************
!  Objetivo:                                                                  *
!  Lee el archivo 132.VIADIV (V�as divergentes)                               *
!                                                                             *
!  Registro de revisiones:                                                    *
!                                                                             *
!      Fecha         Programador       Descripcion de codigo                  *
!      ====      =================     =====================                  *
!    20/04/11   Isa�as Guill�n Moya    Codigo original                        *
!     7/06/13   Juan Alvarez Lopez     Adecuacion Hidro MIP                   *
!                                                                             *
!******************************************************************************
USE ParAuHeHidro
use ParAUHE, only: rut_dat_1, bmensaje
     
IMPLICIT NONE
integer ierror

!Declaraci�n de variables locales		  
CHARACTER(10), DIMENSION(nmxvia):: arrgcha1		! (2) Nombre de la v�a
CHARACTER(10), DIMENSION(nmxvia):: arrgcha2		! (4) Nombre del vaso origen
CHARACTER(10), DIMENSION(nmxvia):: arrgcha3		! (6) Nombre del vaso destino  

INTEGER, DIMENSION (nmxvia):: arrgent1			! (1) N�mero de la v�a
!INTEGER, DIMENSION (nmxvia):: arrgent2			! (3) N�mero del vaso origen
!INTEGER, DIMENSION (nmxvia):: arrgent3			! (5) N�mero del vaso destino
INTEGER, DIMENSION (nmxvia):: arrgent4			! (9) Tiempo medio de viaje del agua en horas. (ver comentario)
	 
REAL * 8, DIMENSION (nmxvia):: arrgrea1				! (7) Gasto m�nimo MCS
REAL * 8, DIMENSION (nmxvia):: arrgrea2				! (8) Gasto m�ximo MCS
REAL * 8, DIMENSION (nmxvia):: arrgrea3				!(10) Nivel Medio de desfogue
REAL * 8, DIMENSION (nmxvia):: arrgrea4				!(11) L�mite razonable del derrame programado por usuario
	  
REAL * 8, DIMENSION (nmxvia,9):: arrgrea5		!(12) Gasto en v�a MCS (9 puntos).
REAL * 8, DIMENSION (nmxvia,9):: arrgrea6		!(13) Elevaci�n desfogue en metros (9 puntos)
	  
INTEGER::i,j, ivi, ipu, nvasos, nvias, iv, errleca
character*5 letr
!---------------------------------------------------
! * Abre el archivo 132.VIADIV (V�as divergentes) *
!---------------------------------------------------
OPEN (UNIT=132, DEFAULTFILE=rut_dat_1, FILE='VIADIV.csv', STATUS='OLD', iostat = ierror)
!----------------------------------------
!* Lee los datos del archivo 132.VIADIV *
!---------------------------------------- 
errleca = 0
NVASOS = DNEMVA ( nmcuen + 1 ) - 1
NVIAS = DNVIOU ( DNEMVA ( nmcuen + 1 ) ) - 1
DO i=1,NVIAS
    READ (132,*, iostat = errleca ) arrgent1 (i), arrgcha1 (i), arrgent2 (i), arrgcha2 (i), arrgent3 (i), &
                 arrgcha3 (i), arrgrea1 (i), arrgrea2 (i), arrgent4 (i), arrgrea3 (i), &
                 arrgrea4 (i), (arrgrea5 (i,j), j=1,9), (arrgrea6 (i,j), j=1,9)
    if ( errleca .ne. 0 ) go to 1001
END DO
CLOSE (132)

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS VIADIV.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'VIA: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif     
    
DO ivi = 1, NVIAS
    IV = CUEAVI ( ivi )
    !---------------------
    !* Nombre de la v�a. *
    !---------------------
    NOMVIA ( ivi ) = arrgcha1(ivi)
    !------------------------------------------------
    !* ALMACENAR EL INDICE DEL EMBALSE AGUAS ABAJO  *
    !------------------------------------------------
    !EMABVI ( ivi ) = arrgent3(ivi)
    !-----------------------------
    !* Gasto m�nimo MCS.*
    !-----------------------------    
    GAMNVI ( ivi ) = MValor * arrgrea1(ivi)
    !-----------------------------
    !* Gasto m�ximo MCS.*
    !-----------------------------
    GAMXVI ( ivi ) = MValor * arrgrea2(ivi)
    !-----------------------------
    !* Tiempo medio de viaje del agua en horas. (ver comentario)* 
    !-----------------------------
    TiViAgu ( ivi ) = arrgent4(ivi) 
    !-----------------------------
    !* Nivel medio de desfogue.*
    !-----------------------------
    NMDVI ( ivi ) = arrgrea3 (ivi)

    !Gasto sobre la via de desfogue 9 puntos en millones de metros cubicos
    GSDES1 (1,ivi) = arrgrea5 (ivi,1)
    		 
    !Elevacion en la via de desfogue 9 puntos en m
    nivdes (1,ivi) = arrgrea6 (ivi,1)
    
    DO IPU = 2 , nmxpmd
        !Gasto sobre la via de desfogue 9 puntos en millones de metros cubicos
    	GSDES1 (ipu, ivi) = arrgrea5 (ivi,ipu)
    	!Elevacion en la via de desfogue 9 puntos en m
    	nivdes (ipu, ivi) = arrgrea6 (ivi,ipu)               
    END DO

END DO

END SUBROUTINE Lectura_132_Viadiv_1 
    
SUBROUTINE COEFMH_104_COEFMH  
!
!***********************************************************************
!  Proposito:
!       
!         LECTURA DE LOS DATOS DE DISENO DE LAS UNIDADES
!       
!                104. COEFMH (Coeficientes de modelos hidroel�ctricos)
!  Registro de revisiones:
!       Fecha             Programador          Descripcion de cambios
!    ============    ===================       =======================
!    30 Octubre 2009    Isaias Guillen Moya       Codigo original
!     7 Junio 2013      Juan Alvarez Lopez        Adecuacion Hidro MIP
!
!***********************************************************************
USE ParAuHeHidro
use ParAUHE, only: rut_dat_1, bmensaje     
IMPLICIT NONE
!----------------------------
!* Declaracion de variables *
!----------------------------
INTEGER error_h, i, j, errleca
INTEGER tip
INTEGER reg, NREG
INTEGER ibanbit
REAL * 8 auxxrea(5,NMXPMU*NMXUNI)
!
CHARACTER* 5000 letaux
character*5 letr
DATA NREG   / 9 /
!
!---------------------------------------------------------
!* 104. COEFMH (Coeficientes de modelos hidroel�ctricos) *
!---------------------------------------------------------
OPEN (UNIT = 104, DEFAULTFILE = rut_dat_1, FILE='COEFMH.csv', STATUS='OLD', IOSTAT = error_h )
ibanbit = 1
i = 0
errleca = 0
READ ( 104, 100, iostat = error_h  ) letaux
DO WHILE ( (i .lt. nuti*9) .and. (error_h .eq. 0) .and. len_trim ( letaux ) .ne. 0)
    i = i + 1
    !------------------------------------------------
    !* Lee la informaci�n del 104. COEFMH (Coeficientes de modelos hidroel�ctricos)
    !------------------------------------------------
    !----------------
    !* No hay error *
    !----------------
    IF ( error_h .eq. 0 ) THEN
        read ( letaux, *, iostat = errleca ) auxxrea(1,i), auxxrea(2,i), auxxrea(3,i), auxxrea(4,i), auxxrea(5,i)
        if ( errleca .ne. 0 ) go to 1001
    END IF 
    READ ( 104, 100, iostat = error_h  ) letaux
END DO
CLOSE ( 104 )

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS COEFMH.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'MODELO: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif  

!
!--------------------------------------------------------------
!* INICIAR EL INDICE DEL PRIMER REGISTRO A LEER Y DEL TIPO DE *
!* UNIDAD.                                                    *
!--------------------------------------------------------------
REG = 1 - NREG
TIP = 0
!---------------------------------------------------------------
!* MIENTAS NO SE HAYAN CONSIDERADO TODOS LOS TIPOS DE UNIDADES *
!* Y NO SE HAYA PRESENTADO ERROR EN LA LECTURA:                *
!---------------------------------------------------------------
DO WHILE (  TIP .LT. NUTI  )
    REG = REG + NREG
    TIP = TIP + 1
    J = 0
    DO I = 1 , NMXPMU
        J = I + reg - 1
        !Coeficientes a multiplicar por la altura de la funcion de generacion contra gasto y altura               
        GQC ( I , TIP ) = auxxrea(1,j)
        !
        CARGA ( I , TIP ) = auxxrea(3,j)
        !              
        QMINUN ( I , TIP ) = auxxrea(4,j)
        !              
        QMAXUN ( I , TIP ) = auxxrea(5,j)
    END DO
END DO


100   FORMAT ( a )

END SUBROUTINE COEFMH_104_COEFMH       
     
SUBROUTINE vasoen_1 
!
!***********************************************************************
!  Proposito:
!       
!       
!         LECTURA DE LOS NIVELES INICIALES EN LOS EMBALSES, LAS        *
!         POLITICAS Y LOS VALORES SOLICITADOS.                         *
!                129. VASOEN (Pol�tica de operaci�n en vasos)
!  Registro de revisiones:
!       Fecha             Programador          Descripcion de cambios
!    ============    ===================       =======================
!    30 Octubre 2009    Isaias Guillen Moya       Codigo original
!    11 Junio   2013    Juan Alvarez Lopez        Modificacion Hidro MIP
!
!***********************************************************************
USE ParAuHeHidro
use ParAUHE, only: rut_dat_1, bmensaje, MaxIntervalo, NtIntr, CostoCorte
                   
use ParGloRed, only: BasMva

IMPLICIT NONE
!----------------------------
!* Declaracion de variables *
!----------------------------
INTEGER i, varent, nvasos, error_h
INTEGER ini, ifin, ipos, idir
INTEGER INICIA_CHAR_IGM
INTEGER ibanbit
INTEGER POLITI
REAL * 8 varrea
REAL * 8 COTFIN , COTINI , ENERGI , VOLUME 
REAL * 8 TCOTINI, IVUTIL_1
!
CHARACTER* 5000 letaux
CHARACTER*1   let
CHARACTER*12   varch
WEmbFin = 0.0

!---------------------------------------
!* 129. VASOEN (Pol�tica de operaci�n en vasos) *
!---------------------------------------
OPEN (UNIT = 129, DEFAULTFILE = rut_dat_1, FILE='VASOEN.csv', STATUS='OLD', IOSTAT = error_h )
!
NVASOS = DNEMVA ( nmcuen + 1 ) - 1
ibanbit = 1
i = 0
DO WHILE ( (i .lt. nvasos) .and. (error_h .eq. 0) )
    i = i + 1
    !------------------------------------------------
    !* Lee la informaci�n del 129. VASOEN (Pol�tica de operaci�n en vasos)
    !------------------------------------------------
    READ ( 129, 100, iostat = error_h  ) letaux
    idir = 1
    !----------------
    !* No hay error *
    !----------------
    IF ( error_h .eq. 0 ) THEN
        ini = 1
        ifin = 5000
        let = ','
        !-------------------------------
        !* Nombre del vaso. *
        !-------------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        READ ( letaux(ini:ipos-1), 100 ) varch
        ini = ipos + 1
        !--------------------------
        !* Nombre de la cuenca. *
        !--------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        READ ( letaux(ini:ipos-1), 100 ) varch
        ini = ipos + 1
        !-----------------------------
        !* Pol�tica de operaci�n*
        !-----------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        READ ( letaux(ini:ipos-1), 300 ) POLITI
        PoliEmb ( i ) = POLITI
        !1    ! POLITICA DE MAXIMA EXTRACCION
        !     ! Costo en la f.o.
        EMxExt = 0.0
        !2    ! POLITICA DE MINIMA EXTRACCION.
        !     ! Costo en la f.o.
        EMnExt = 0.5 * CostoCorte
        !3    ! POLITICA DE COTA FIJA.
        !4    ! POLITICA DE ENERGIA FIJA.
        !5    ! POLITICA DE VOLUMEN FIJO.
        ini = ipos + 1
        !-------------------------------
        !* Nombre del �rea..*
        !-------------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        READ ( letaux(ini:ipos-1), 100 ) varch
        ini = ipos + 1
        !-------------------------------
        !* Prioridad en pol�tica. *
        !-------------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        READ ( letaux(ini:ipos-1), 300 ) varent
        ini = ipos + 1
        !--------------------------
        !* Nivel inicial.. *
        !--------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        CALL LEEREAl_IGM ( ini, ipos-1,  letaux, error_h, COTINI )
        IF ( error_h .ne. 0 ) GO TO 1000
        NIVINI ( i) = COTINI
        ini = ipos + 1
        !-----------------------------
        !* Nivel final en metros.*
        !-----------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        CALL LEEREAl_IGM ( ini, ipos-1,  letaux, error_h, COTFIN )     
        IF ( error_h .ne. 0 ) GO TO 1000
        !Politica cota final fija
        !Pasar la cota final fija del embalse de m. a volumen util m^3
        WEmbFin ( i ) = IVUTIL_1 ( i , COTFIN - NAMINO ( i ) )
        
        ini = ipos + 1
        !-------------------------------
        !* Tolerancia..*
        !-------------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        CALL LEEREAl_IGM ( ini, ipos-1,  letaux, error_h, varrea )
        IF ( error_h .ne. 0 ) GO TO 1000
        ini = ipos + 1
        !-------------------------------
        !* Energ�a a producir GWH.. *
        !-------------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        CALL LEEREAl_IGM ( ini, ipos-1,  letaux, error_h, ENERGI )
        IF ( error_h .ne. 0 ) GO TO 1000
        !Politica de enegia fija
        !Pasar de GWH a MWH
        ENERGI = ENERGI * 1000
        !Dividir entre la base
        EfijEmb ( i ) = ENERGI
        ini = ipos + 1
        !--------------------------
        !* Volumen a turbinar Mm3. *
        !--------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        CALL LEEREAl_IGM ( ini, ipos-1,  letaux, error_h, VOLUME )
        IF ( error_h .ne. 0 ) GO TO 1000
        !Politica de volumen a turbinar fijo
        !Dato en MMC multiplicar x 1000
        TFijEmb ( i ) = VOLUME * 1000
        ini = ipos + 1
        !-----------------------------
        !* Fecha �ltima medici�n*
        !-----------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        READ ( letaux(ini:ipos-1), 100 ) varch
        ini = ipos + 1
        !-------------------------------
        !* Nivel medido en metros.*
        !-------------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        CALL LEEREAl_IGM ( ini, ipos-1,  letaux, error_h, varrea )
        IF ( error_h .ne. 0 ) GO TO 1000
        !De aqui se toma ahora el namo
        NAMO ( i ) = varrea
        
        !Se calcula el volumen UTIL! maximo del embalse dada la altura UTIL! maxima (namo-namino)
        VolMxEmb ( i ) = IVUTIL_1 ( i , NAMO ( i ) - NAMINO ( i ) )
        
        !
        TCOTINI = COTINI
        !Volumen UTIL! inicial
        VMINI ( i ) = IVUTIL_1 ( i , TCOTINI - NAMINO ( i ) )
    END IF      
END DO
CLOSE ( 129 )

GO TO 2000

1000  CONTINUE
bmensaje = '!!! CHT '// &
'Error en lectura de Lectura_Vasos registro "'// letaux(ini:ipos-1)// '"'
CALL Mensaje_cht ( error_h, ibanbit, BMensaje )
2000  CONTINUE


100   FORMAT ( a )
300   FORMAT ( i )

!
END SUBROUTINE vasoen_1 

FUNCTION IVUTIL_1 ( IE , ALTURA )

!***************************** CHAU ***********************************
!                                                                     *
!     PROPOSITO calcula el volumen del embalse dada la altura         *
!                                                                     *
!     NOMBRE Y FECHA DE IMPLEMENTACION                                *
!        MANUEL RUIZ CASILLAS           85 08 20                      *
!                                                                     *
!     NOMBRE Y FECHA DE REVISION(ES)                                  *
!        MANUEL RUIZ CASILLAS           95 03 02                      *
!                                                                     *
!     NOMBRE Y FECHA DE REVISION(ES)                                  *
!        Juan Alvarez Lopez             13 06 11                      *
!        Adecuacion Hidro MIP                                         *
!                                                                     *
!**********************************************************************
use ParAuHeHidro
IMPLICIT NONE
!-----------------------------
!* DECLARACION DE VARIABLES. *
!-----------------------------
REAL * 8 ALTURA , DDAL   , DEAL   , DEVO   , VOL, IVUTIL_1    
INTEGER * 4 IE     , ipunto 
!-------------------------------------------------------------------
!* SE BUSCARA EL SEGMENTO LINEAL DENTRO DEL CUAL QUEDA COMPRENDIDO *
!* EL VALOR CONOCIDO DE LA ALTURA. SI ESTE VALOR QUEDA FUERA DE LA *
!* TABLA, SE EXTRAPOLARA SUPONIENDO QUE FUERA DE LA TABLA EL COM-  *
!* PORTAMIENTO ES COMO LA DEL ULTIMO SEGMENTO MAS PROXIMO.         *
!-------------------------------------------------------------------
ipunto = 1
DO WHILE ( ( ALTURA .GT. ELUTIL ( ipunto , IE ) ) .AND.( ipunto .LT. 9 ) )
    ipunto = ipunto + 1
ENDDO
IF ( ipunto .NE. 1 ) THEN
    DEAL = ELUTIL ( ipunto , IE ) - ELUTIL ( ipunto - 1 , IE )
    DEVO = VOUTIL ( ipunto , IE ) - VOUTIL ( ipunto - 1 , IE )
    DDAL = ALTURA - ELUTIL ( ipunto , IE )
    VOL = VOUTIL ( ipunto , IE ) + DDAL * DEVO / DEAL
ELSE
    DEAL = ELUTIL ( 2 , IE ) - ELUTIL ( 1 , IE )
    DEVO = VOUTIL ( 2 , IE ) - VOUTIL ( 1 , IE )
    DDAL = ALTURA - ELUTIL ( 1 , IE )
    VOL = VOUTIL ( 1 , IE ) + DDAL * DEVO / DEAL
END IF
IVUTIL_1 = VOL
END FUNCTION IVUTIL_1



!******************************************************************************
!  Objetivo:                                                                  *
!      		Subrutina que envia mensaje de error del programa                 *
!                                                                             *
!  Registro de revisiones:                                                    *
!                                                                             *
!      Fecha         Programador       Descripcion de codigo                  *
!      ====      =================     =====================                  *
!    30/11/11   Isa�as Guill�n Moya       Codigo original                    *
!******************************************************************************

SUBROUTINE Mensaje_cht ( error_h, ibanbit, BMensaje )

implicit none

integer error_h, ibanbit

integer ini, ifin, imax, idesp

integer ipos, ich, ibl, idir, msj

integer Inicia_Noblanco_IGM, Inicia_blanco_IGM

character BMensaje*255

character*75 Letaux

character*1 blanco

logical sigue

!  Inicializa blanco
Blanco = ' '

ini = 1
idesp = 5 ! despazamiento del mensaje a partir del segundo renglon
ifin = 255 ! tama�o de la cadena
imax = 75  ! magnitud del renglon

!  ...................................................
!  Escribe en bitacora si el tipo de error lo permite
!  :::::::::::::::::::::::::::::::::::::::::::::::::::
if ( ibanbit .eq. 1 ) then
    Call Mensaje_Bitacora_IGM ( error_h, BMensaje )
    call gebita2(BMensaje)
endif

!  ..............................................................
!  Escribe en pantalla en uno o varios renglones de acuerdo a la
!  magnitud del mensaje
!  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sigue = .true.
msj = 0
do while ( sigue  )
    msj = msj + 1
    ipos = min ( (ini + imax - 1 ) , ifin )
    idir = 1
    ich = Inicia_Noblanco_IGM ( ini, ipos, idir, BMensaje )
    if ( ich .ne. 0 ) then
        if ( BMensaje(ipos:ipos) .eq. blanco ) then
            call AcomMensaje_IGM ( msj, idesp, ini, ipos, BMensaje, Letaux ) 
            call EscriPanta_IGM ( Letaux )

            ini = ipos + 1
        else
            if ( BMensaje(ipos+1:ipos+1) .eq. blanco ) then
                call AcomMensaje_IGM ( msj, idesp, ini, ipos, BMensaje, Letaux ) 
                call EscriPanta_IGM ( Letaux )

                ini = ipos + 2
            else
                !Encuentra el primer blanco atras de ipos
                idir = - 1
                ibl = Inicia_blanco_IGM ( ini, ipos, idir, BMensaje ) 
                call AcomMensaje_IGM ( msj, idesp, ini, ibl, BMensaje, Letaux ) 
                call EscriPanta_IGM ( Letaux  )

                ini = ibl + 1
            endif
        endif
    else
        sigue = .false.
    endif
    if ( msj .eq. 1 ) then
        imax = imax - idesp
    endif
enddo

!  .................................
!  Verifica si el error es de falla
!  :::::::::::::::::::::::::::::::::
if ( error_h .ge. 50 ) then
    BMensaje = '             '
    Call EscriPanta_IGM ( BMensaje )

    BMensaje = '              Oprima <Return> para detener el proceso.'
    Call EscriPanta_IGM ( BMensaje )
    Read ( *, * )
!   algoritmo no termina bien
    call SalidaError
    STOP
endif

Return
end

!******************************************************************************
!  Objetivo:                                                                  *
!      		Encuentra el primer "char" en la direccion incremental ( idir = 1)*
!           direccion decreciente ( idir ne 1 )                               *
!                                                                             *
!  Registro de revisiones:                                                    *
!                                                                             *
!      Fecha         Programador       Descripcion de codigo                  *
!      ====      =================     =====================                  *
!    30/11/11   Isa�as Guill�n Moya       Codigo original                     *
!******************************************************************************
INTEGER FUNCTION INICIA_CHAR_IGM ( ini, ifin, idir, Char, Letrero )
IMPLICIT NONE
INTEGER, INTENT(in) :: ini, ifin, idir
CHARACTER, INTENT(in) :: Letrero*(*), Char*1
INTEGER :: j, k, ind
CHARACTER :: varch*1, comillas*1, nada*1
LOGICAL :: bandera
!
comillas = '"'
nada = ''
ind = 0
!Busca el primer char en la direccion incremental
IF ( idir .eq. 1 ) THEN
    j = ini 
    k = ini 
    bandera = .false.
    varch = Letrero(j:j)
    IF ( varch .eq. comillas ) THEN
        k = k + 1
        DO WHILE ( (k .le. ifin) .AND. (.NOT. bandera) )
            IF ( Letrero(k:k) .eq. comillas ) THEN
                bandera = .true.
                ind = k + 1
            END IF
            k = k + 1
        END DO
    ELSE
        DO WHILE ( j .le. ifin .and. ind .eq. 0 )
            IF ( Letrero(j:j) .eq. char ) THEN
                ind = j
            END IF
            j = j + 1
        END DO
    END IF
END IF
!
IF ( idir .NE. 1 ) THEN
    !Busca el primer char en la direccion decreciente
    j = ifin
    DO WHILE ( j .ge. ini .and. ind .eq. 0 )
        varch = Letrero(j:j)
        IF ( Letrero(j:j) .NE. nada ) THEN
            ind = j
        END IF
        j = j - 1
    END DO
END IF
INICIA_CHAR_IGM = ind
RETURN
END FUNCTION

!******************************************************************************
!  Objetivo:                                                                  *
!      		                                                                  *
!                                                                             *
!  Registro de revisiones:                                                    *
!                                                                             *
!      Fecha         Programador       Descripcion de codigo                  *
!      ====      =================     =====================                  *
!    30/11/11   Isa�as Guill�n Moya       Codigo original                     *
!******************************************************************************

SUBROUTINE LEEREAL_IGM168 ( ini, ifin,  letaux, error_h, valor )
!
IMPLICIT NONE

INTEGER ini, ifin, error_h
INTEGER idir, ipos, INICIA_CHAR_IGM
INTEGER ivalor

REAL*8 valor

CHARACTER*3024 letaux
CHARACTER*1 let

idir = 1

let = '.'
ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
IF ( ipos .ne. 0 ) THEN
    READ ( letaux(ini:ifin), 200, iostat = error_h ) valor
ELSE
    READ ( letaux(ini:ifin), 100, iostat = error_h ) ivalor
    valor = ivalor
END IF

100   FORMAT ( i )
200   FORMAT ( f )
RETURN
END

!******************************************************************************
!  Objetivo:                                                                  *
!      		                                                                  *
!                                                                             *
!  Registro de revisiones:                                                    *
!                                                                             *
!      Fecha         Programador       Descripcion de codigo                  *
!      ====      =================     =====================                  *
!    30/11/11   Isa�as Guill�n Moya       Codigo original                     *
!******************************************************************************
SUBROUTINE LEEREAL_IGM ( ini, ifin,  letaux, error_h, valor )

!
IMPLICIT NONE

INTEGER ini, ifin, error_h
INTEGER idir, ipos, INICIA_CHAR_IGM
INTEGER ivalor

REAL*8 valor

CHARACTER*3024 letaux
CHARACTER*1 let

idir = 1

let = '.'
ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
IF ( ipos .ne. 0 ) THEN
    READ ( letaux(ini:ifin), 200, iostat = error_h ) valor
ELSE
    READ ( letaux(ini:ifin), 100, iostat = error_h ) ivalor
    valor = ivalor
END IF

100   FORMAT ( i )
200   FORMAT ( f )
RETURN
END

!******************************************************************************
!  Objetivo: Subrutina que envia mensaje de error del programa                *
!            a bitacora                                                       *
!      		                                                                  *
!                                                                             *
!  Registro de revisiones:                                                    *
!                                                                             *
!      Fecha         Programador       Descripcion de codigo                  *
!      ====      =================     =====================                  *
!    30/11/11   Isa�as Guill�n Moya       Codigo original                     *
!******************************************************************************


SUBROUTINE Mensaje_Bitacora_IGM ( error_h, Bmensaje )

implicit none

integer error_h

integer ini, ifin, imax, idesp

integer ipos, ich, ibl, idir, msj

integer Inicia_Noblanco_IGM, Inicia_blanco_IGM

character BMensaje*255


character*85 Letaux

character*1 blanco

logical sigue

!  Inicializa blanco
Blanco = ' '

!   NomtablaB = 'Bitacora Flujos Optimos de Potencia Activa'
ini = 1
if ( error_h .ge. 50 ) then
    idesp = 4 ! despazamiento del mensaje a partir del segundo renglon
else
    idesp = 4 ! despazamiento del mensaje a partir del segundo renglon
endif
ifin = 255 ! tama�o de la cadena
imax = 75  ! magnitud del renglon

!  ..............................................................
!  Escribe en bitacora en uno o varios renglones de acuerdo a la
!  magnitud del mensaje
!  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sigue = .true.
msj = 0
do while ( sigue  )
    msj = msj + 1
    ipos = min ( (ini + imax - 1 ) , ifin )
    idir = 1
    ich = Inicia_Noblanco_IGM ( ini, ipos, idir, BMensaje )
    if ( ich .ne. 0 ) then
        if ( BMensaje(ipos:ipos) .eq. blanco ) then
            call AcomMensaje_IGM ( msj, idesp, ini, ipos, BMensaje, Letaux ) 
            call EscribeBitacora_IGM ( Letaux )

            ini = ipos + 1
        else
            if ( BMensaje(ipos+1:ipos+1) .eq. blanco ) then
                call AcomMensaje_IGM ( msj, idesp, ini, ipos, BMensaje, Letaux ) 
                call EscribeBitacora_IGM ( Letaux )

                ini = ipos + 2
            else
                !Encuentra el primer blanco atras de ipos
                idir = - 1
                ibl = Inicia_blanco_IGM ( ini, ipos, idir, BMensaje ) 
                call AcomMensaje_IGM ( msj, idesp, ini, ibl, BMensaje, Letaux ) 
                call EscribeBitacora_IGM ( Letaux )

                ini = ibl + 1
            endif
        endif
    else
        sigue = .false.
    endif
    if ( msj .eq. 1 ) then
        imax = imax - idesp
    endif
enddo
Return
end

!******************************************************************************
!  Objetivo: Encuentra el primer Noblanco en la direccion incremental         *
!            ( ndir = 1) direccion decreciente ( ndir ne 1 )                  *
!      		                                                                  *
!                                                                             *
!  Registro de revisiones:                                                    *
!                                                                             *
!      Fecha         Programador       Descripcion de codigo                  *
!      ====      =================     =====================                  *
!    30/11/11   Isa�as Guill�n Moya       Codigo original                     *
!******************************************************************************

Integer Function Inicia_Noblanco_IGM ( ini, ifin, idir, Letrero )

Implicit none

integer ini, ifin, idir

integer j, ind

character Letrero*(*), blanco*1

blanco = ' '

ind = 0
if ( idir .eq. 1 ) then
    j = ini 
    do while ( j .le. ifin .and. ind .eq. 0 )
        if ( Letrero(j:j) .ne. blanco ) then
            ind = j
        endif
        j = j + 1
    enddo
else
    j = ifin 
    do while ( j .ge. ini .and. ind .eq. 0 )
        if ( Letrero(j:j) .ne. blanco ) then
            ind = j
        endif
    j = j - 1
    enddo
endif

Inicia_Noblanco_IGM = ind

Return

End Function

!******************************************************************************
!  Objetivo: Acomoda mensaje de error dependiendo si es el primer renglon     * 
!            o uno de los siguientes renglones                                *
!      		                                                                  *
!                                                                             *
!  Registro de revisiones:                                                    *
!                                                                             *
!      Fecha         Programador       Descripcion de codigo                  *
!      ====      =================     =====================                  *
!    30/11/11   Isa�as Guill�n Moya       Codigo original                     *
!******************************************************************************

SUBROUTINE AcomMensaje_IGM ( msj, idesp, ini, ifin, BMensaje, Letaux )

implicit none

integer i, j

integer msj, idesp, ini, ifin

Character BMensaje*255

Character*(*) Letaux


! Llena de blancos el letrero final
Letaux = ' '

! Si es el primer mensaje, no hay despalzamiento de inicio de mensaje
if ( msj .eq. 1 ) then
    j = 0
    do i = ini, ifin
        j = j + 1
        letaux(j:j) = BMensaje(i:i)
    enddo
else
    ! Si no es el primer mensaje, hay un despalzamiento de inicio de mensaje
    j = idesp
    do i = ini, ifin
        j = j + 1
        letaux(j:j) = BMensaje(i:i)
    enddo
endif
Return
end

!******************************************************************************
!  Objetivo: Escribe mensaje a pantalla                                       *
!      		                                                                  *
!                                                                             *
!  Registro de revisiones:                                                    *
!                                                                             *
!      Fecha         Programador       Descripcion de codigo                  *
!      ====      =================     =====================                  *
!    30/11/11   Isa�as Guill�n Moya       Codigo original                     *
!******************************************************************************

SUBROUTINE EscriPanta_IGM ( BMensaje )
IMPLICIT NONE 
Character*(*) BMensaje
write ( *, * ) Trim ( BMensaje )
Return
End

!******************************************************************************
!  Objetivo: Encuentra el primer "blanco" en la direccion incremental         *
!            ( ndir = 1) direccion decreciente ( ndir ne 1 )                  *
!      		                                                                  *
!                                                                             *
!  Registro de revisiones:                                                    *
!                                                                             *
!      Fecha         Programador       Descripcion de codigo                  *
!      ====      =================     =====================                  *
!    30/11/11   Isa�as Guill�n Moya       Codigo original                     *
!******************************************************************************
Integer Function Inicia_blanco_IGM ( ini, ifin, idir, Letrero )

Implicit none

integer ini, ifin, idir

integer j, ind

character Letrero*(*), blanco*1

blanco = ' '

ind = 0
!  Busca el primer blanco en la direccion incremental
if ( idir .eq. 1 ) then
    j = ini 
    do while ( j .le. ifin .and. ind .eq. 0 )
        if ( Letrero(j:j) .eq. blanco ) then
            ind = j
        endif
        j = j + 1
    enddo
else
    !Busca el primer blanco en la direccion decreciente
    j = ifin
    do while ( j .ge. ini .and. ind .eq. 0 )
        if ( Letrero(j:j) .eq. blanco ) then
            ind = j
        endif
        j = j - 1
    enddo
endif

Inicia_blanco_IGM = ind

Return

End Function

!******************************************************************************
!  Objetivo: Escribe mensaje a bitacora                                       *
!      		                                                                  *
!                                                                             *
!  Registro de revisiones:                                                    *
!                                                                             *
!      Fecha         Programador       Descripcion de codigo                  *
!      ====      =================     =====================                  *
!    30/11/11   Isa�as Guill�n Moya       Codigo original                     *
!******************************************************************************

SUBROUTINE EscribeBitacora_IGM ( BMensaje )
IMPLICIT NONE
CHARACTER*(*) BMensaje
WRITE ( 444, * ) TRIM ( BMensaje )
END SUBROUTINE EscribeBitacora_IGM

SUBROUTINE MODLOH_109_MODLOH 
!
!***********************************************************************
!  Proposito:
!       
!         LECTURA DE LOS DATOS DE DISENO DE LAS UNIDADES
!       
!                109. MODLOH (Modelo hidroel�ctrico)
!  Registro de revisiones:
!       Fecha             Programador          Descripcion de cambios
!    ============    ===================       =======================
!    30 Octubre 2009    Isaias Guillen Moya       Codigo original
!    13 Junio 2013      Juan Alvarez Lopez        Adecuacion Hidro MIP
!
!***********************************************************************
USE ParAuHeHidro
use ParAUHE, only: rut_dat_1, bmensaje
IMPLICIT NONE

!----------------------------
!* Declaracion de variables *
!----------------------------
INTEGER error_h, i
INTEGER ini, ifin, ipos, idir
INTEGER INICIA_CHAR_IGM
INTEGER ibanbit
REAL * 8 varrea
!
CHARACTER* 5000 letaux
CHARACTER*1   let
CHARACTER*12   varch
!---------------------------------------
!* 109. MODLOH (Modelo hidroel�ctrico) *
!---------------------------------------
OPEN (UNIT = 109, DEFAULTFILE = rut_dat_1, FILE='MODLOH.csv', STATUS='OLD', IOSTAT = error_h )
!
ibanbit = 1
i = 0
DO WHILE ( (i .lt. nuti) .and. (error_h .eq. 0) )
    i = i + 1
    !------------------------------------------------
    !* Lee la informaci�n del 109. MODLOH (Modelo hidroel�ctrico)
    !------------------------------------------------
    READ ( 109, 100, iostat = error_h  ) letaux
    idir = 1
    !----------------
    !* No hay error *
    !----------------
    IF ( error_h .eq. 0 ) THEN
        ini = 1
        ifin = 5000
        let = ','
        !-------------------
        !* Tipo de unidad. *
        !-------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        READ ( letaux(ini:ipos-1), 100 ) varch
        ini = ipos + 1
        !--------------------------
        !* REM. *
        !--------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        CALL LEEREAl_IGM ( ini, ipos-1,  letaux, error_h, varrea )
        IF ( error_h .ne. 0 ) GO TO 1000
        ini = ipos + 1
        !-----------------------------
        !* Carga de dise�o metros *
        !-----------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        CALL LEEREAl_IGM ( ini, ipos-1,  letaux, error_h, varrea )
        IF ( error_h .ne. 0 ) GO TO 1000
        !----------------------------------------------------------
        !* ALMACENAR LA CARGA HIDRAULICA DE DISENO DE LA UNIDAD *
        !----------------------------------------------------------
        CARDIS ( i ) = varrea
        ini = ipos + 1
        !-------------------------------
        !* Potencia de dise�o en MW. *
        !-------------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        CALL LEEREAl_IGM ( ini, ipos-1,  letaux, error_h, varrea )
        IF ( error_h .ne. 0 ) GO TO 1000
        !--------------------------------------
        !* ALMACENAR LA GENERACION DE DISENO. *
        !---------------------------------------
        !GENDIS ( i ) = varrea
        ini = ipos + 1
        !-------------------------------
        !* Gasto de dise�o CV. *
        !-------------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        CALL LEEREAl_IGM ( ini, ipos-1,  letaux, error_h, varrea )
        IF ( error_h .ne. 0 ) GO TO 1000
        ini = ipos + 1
        !--------------------------
        !* Velocidad esp. . *
        !--------------------------
        ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
        CALL LEEREAl_IGM ( ini, ipos-1,  letaux, error_h, varrea )
        IF ( error_h .ne. 0 ) GO TO 1000
    END IF      
END DO
CLOSE ( 109 )

GO TO 2000

1000  CONTINUE
bmensaje = '!!! CHT '// &
'Error en lectura de MODLOH_109_MODLOH registro "'// letaux(ini:ipos-1)// '"'
CALL Mensaje_cht ( error_h, ibanbit, BMensaje )
2000  CONTINUE
!
100   FORMAT ( a )
200   FORMAT ( f )
300   FORMAT ( i )
5000  FORMAT(2x,i4,x,i4)
1350  FORMAT ( I2.2 )
!
END SUBROUTINE MODLOH_109_MODLOH

FUNCTION QMXEF ( IP )
!***************************** CHAU ***********************************
!                                                                     *
!     PROPOSITO: CALCULA EL GASTO A MAXIMA EFICIENCIA DE LA UNIDAD    *
!                                                                     *
!     NOMBRE Y FECHA DE IMPLEMENTACION                                *
!        MANUEL RUIZ CASILLAS           85 08 20                      *
!                                                                     *
!     NOMBRE Y FECHA DE REVISION(ES)                                  *
!        MANUEL RUIZ CASILLAS           95 03 02                      *
!                                                                     *
!     NOMBRE Y FECHA DE REVISION(ES)                                  *
!        Juan Alvarez Lopez             13 06 14                      *
!        Adecuacion Hidro MIP                                         * 
!                                                                     *
!**********************************************************************
USE ParAuHeHidro
IMPLICIT NONE
!-----------------------------
!* DECLARACION DE VARIABLES. *
!-----------------------------
REAL * 8 ALT    , ALTCUA ,  QMXEF  , XDEN   , XNUM
INTEGER * 4 IP     , ITI    , IU 
!
!----------------------------------------------------------------
!* SE DETERMINA EL INDICE DE UNA DE LAS UNIDADES DE LA PLANTA Y *
!* EL MODELO ASOCIADO A ESA UNIDAD.                             *
!----------------------------------------------------------------
IU = DNUNPH ( IP )
!ITI = ITIPOU ( IU )
ITI = MOUNHI ( IU )
!-------------------------------------------------------------------
!* SE CALCULA EL GASTO A MAXIMA EFICIENCIA DE LA UNIDAD A LA CARGA *
!* DE ALTURA DE DISENO.                                            *
!-------------------------------------------------------------------
ALT = CARDIS ( ITI )
ALTCUA = ALT * ALT
XNUM = GQC( 1 , ITI ) + GQC( 2 , ITI ) * ALT +   GQC( 3 , ITI ) * ALTCUA
XDEN = GQC( 7 , ITI ) + GQC( 8 , ITI ) * ALT +   GQC( 9 , ITI ) * ALTCUA
QMXEF = DSQRT ( XNUM / XDEN )      
END FUNCTION QMXEF
     
FUNCTION RELTAH ( IP, QUN )
!nnnn
!***************************** CHAU ***********************************
!                                                                     *
!     PROPOSITO: CALCULA LA PERDIDA DE ALTURA EN CONDUCTOS            *
!                                                                     *
!     NOMBRE Y FECHA DE IMPLEMENTACION                                *
!        MANUEL RUIZ CASILLAS           85 08 20                      *
!                                                                     *
!     NOMBRE Y FECHA DE REVISION(ES)                                  *
!        MANUEL RUIZ CASILLAS           95 03 02                      *
!                                                                     *
!     NOMBRE Y FECHA DE REVISION(ES)                                  *
!        Juan Alvarez Lopez             13 06 14                      *
!        Adecuacion Hidro MIP                                         *
!                                                                     *
!**********************************************************************
USE ParAuHeHidro

IMPLICIT NONE
!-----------------------------
!* DECLARACION DE VARIABLES. *
!-----------------------------
REAL * 8 GASTO  , QUN    , RELTAH
INTEGER * 4 IP  
!
GASTO = QUN 
!-------------------------------------------------
!* SE CALCULA LA PERDIDA DE ALTURA EN CONDUCTOS. *
!-------------------------------------------------
RELTAH = COEPER ( IP ) * GASTO * GASTO
END FUNCTION RELTAH

!*******************************************************/******************
!  Proposito:
!       
!    Forma relaci�n de unidades pertenecientes a los embalses
!    Dice cuantos embalses hay en el sistema
!
!  Registro de revisiones:
!       Fecha               Programador            Descripcion de cambios
!    ============       ===================       =======================
!     14 Junio   2013    Juan Alvarez Lopez           Codigo original
!
!**************************************************************************      
subroutine unidad_embalse

use ParAUHE, only: NumEmbalses
USE ParAuHeHidro

implicit none

integer planta, contador, embalse, localidad_1, unidad, consecutivo


local_uni_emb ( 1 ) = 1
consecutivo = 0
!hacer para todos los embalses
do embalse = 1, NumEmbalses
    !hacer para todas las plantas
    contador = 0
    do planta = 1, nplhid
        if ( embapl ( planta ) .eq. embalse ) then
            !hacer para todas las unidades 
            !contador = 0
            do localidad_1 = 0, NOUN ( planta ) - 1
                unidad = DNUNPH ( planta ) + localidad_1 
                contador = contador + 1
                consecutivo = consecutivo + 1 
                unidad_x_embalse ( consecutivo ) = unidad
            end do
            numuni_x_embalse ( embalse ) = contador  
            if ( embalse .gt. 1 ) then
                local_uni_emb ( embalse ) = local_uni_emb ( embalse - 1 ) + numuni_x_embalse ( embalse - 1 )
            end if
        end if
    end do

end do 

end subroutine unidad_embalse


FUNCTION GASMN ( IU , ALTNET )

!***************************** CHAU ***********************************
!                                                                     *
!     PROPOSITO: Calcula gasto minimo de la unidad hidro con una      *
!                altura neta dada                                     *
!                                                                     *
!     NOMBRE Y FECHA DE IMPLEMENTACION                                *
!        MANUEL RUIZ CASILLAS           85 08 20                      *
!                                                                     *
!     NOMBRE Y FECHA DE REVISION(ES)                                  *
!        MANUEL RUIZ CASILLAS           95 03 02                      *
!                                                                     *
!     NOMBRE Y FECHA DE REVISION(ES)                                  *
!        Juan Alvarez Lopez             13 06 17                      *
!        Adecuacion Hidro MIP                                         *
!                                                                     *
!**********************************************************************
USE ParAuHeHidro
IMPLICIT NONE
!-----------------------------
!* DECLARACION DE VARIABLES. *
!-----------------------------
REAL * 8 ALTNET , DECA   , DEGA   , GASMN  
INTEGER * 4 ipunto   , ITI    , IU   , iv
!
!------------------------------------------------
!* SE DETERMINA EL MODELO ASOCIADO A LA UNIDAD. *
!------------------------------------------------
ITI = MOUNHI ( IU )
!--------------------------------------------------------------------
!* SE BUSCA EN LA TABLA EL SEGMENTO EN EL QUE ESTE COMPRENDIDA      *
!* LA ALTURA DADA. SI LA ALTURA DADA SALE DE LA TABLA, SE EXTRAPOLA *
!* EMPLEANDO EL ULTIMO SEGMENTO MAS PROXIMO.                        *
!--------------------------------------------------------------------
ipunto = 1
DO WHILE ( ( ALTNET .GE. CARGA ( ipunto , ITI ) ) .AND. ( ipunto .LT. 9 ) )
    ipunto = ipunto + 1
ENDDO
IF ( ipunto .EQ. 1 ) THEN
    DECA = CARGA ( 2 , ITI ) - CARGA ( 1 , ITI )
    DEGA = QMINUN ( 2 , ITI ) - QMINUN ( 1 , ITI )
    GASMN = QMINUN ( 1 , ITI ) + ( ALTNET - CARGA ( 1 , ITI ) )* DEGA / DECA
ELSE
    DECA = CARGA ( ipunto , ITI ) - CARGA ( ipunto - 1 , ITI )
    DEGA = QMINUN ( ipunto , ITI ) - QMINUN ( ipunto - 1 , ITI )
    GASMN = QMINUN ( ipunto , ITI ) + ( ALTNET - CARGA ( ipunto , ITI ) ) * DEGA / DECA
END IF
IV = CUEAUN ( IU )
END FUNCTION GASMN

FUNCTION RGASMX ( IU , ALTNET )
      
!***************************** CHAU ***********************************
!                                                                     *
!     PROPOSITO: Calcula gasto maximo de la unidad hidro con una      *
!                altura neta dada                                     *
!                                                                     *
!     NOMBRE Y FECHA DE IMPLEMENTACION                                *
!        MANUEL RUIZ CASILLAS           85 08 20                      *
!                                                                     *
!     NOMBRE Y FECHA DE REVISION(ES)                                  *
!        MANUEL RUIZ CASILLAS           95 03 02                      *
!                                                                     *
!     NOMBRE Y FECHA DE REVISION(ES)                                  *
!        Juan Alvarez Lopez             13 06 17                      *
!        Adecuacion Hidro MIP                                         *
!                                                                     *
!**********************************************************************
USE ParAuHeHidro
IMPLICIT NONE
!-----------------------------
!* DECLARACION DE VARIABLES. *
!-----------------------------
REAL * 8 ALTNET , DECA   , DEGA   , RGASMX
INTEGER * 4 ipunto   , ITI     , IU , iv    
!------------------------------------------------
!* SE DETERMINA EL MODELO ASOCIADO A LA UNIDAD. *
!------------------------------------------------
ITI = MOUNHI ( IU )
!--------------------------------------------------------------------
!* SE BUSCA EN LA TABLA EL SEGMENTO EN EL QUE ESTE COMPRENDIDA      *
!* LA ALTURA DADA. SI LA ALTURA DADA SALE DE LA TABLA, SE EXTRAPOLA *
!* EMPLEANDO EL ULTIMO SEGMENTO MAS PROXIMO.                        *
!--------------------------------------------------------------------
ipunto = 1
DO WHILE ( ( ALTNET .GE. CARGA ( ipunto , ITI ) ) .AND. ( ipunto .LT. 9 ) )
    ipunto = ipunto + 1
ENDDO
IF ( ipunto .EQ. 1 ) THEN
    DECA = CARGA ( 2 , ITI ) - CARGA ( 1 , ITI )
    DEGA = QMAXUN ( 2 , ITI ) - QMAXUN ( 1 , ITI )
    RGASMX = QMAXUN ( 1 , ITI ) + ( ALTNET - CARGA ( 1 , ITI ) ) * DEGA / DECA
ELSE
    DECA = CARGA ( ipunto , ITI ) - CARGA ( ipunto - 1 , ITI )
    DEGA = QMAXUN ( ipunto , ITI ) - QMAXUN ( ipunto - 1 , ITI )
    RGASMX = QMAXUN ( ipunto , ITI ) + ( ALTNET - CARGA ( ipunto , ITI ) ) * DEGA / DECA
END IF
IV = CUEAUN ( IU )
END FUNCTION RGASMX

!**************************************************************************
!  Proposito:
!       
!    Calcula arreglo que indica en que via divergente desfoga la unidad y
!    el gasto maximo de la unidad dependiendo del modelo y la altura del modelo
!
!  Registro de revisiones:
!       Fecha               Programador            Descripcion de cambios
!    ============       ===================       =======================
!     17 Junio   2013    Juan Alvarez Lopez           Codigo original
!
!**************************************************************************      
subroutine Gasto_max_unidad_mod

USE ParAuHeHidro

implicit none

integer cuenca, embalse, via, planta, localidad_1, unidad, localidad, &
        localidad_2, localidad_3, contador, modelo

!Hacer para todas las cuencas
do cuenca = 1,nmcuen    
    !Hacer para todos los emblases de la cuenca
    do localidad_2 = 0, nmembc ( cuenca ) - 1
        embalse = dnemva ( cuenca ) + localidad_2
        !Hacer para todas las vias divergentes
        do localidad_3 = 0, nmviasd ( embalse ) - 1 
            via = DNVIOU ( embalse ) + localidad_3 
            !altura = Alt_net_asig ( via )
            !Hacer para todas las plantas que descargan sobre la v�a
            do localidad = dnphvi ( via ), dnphvi ( via ) + nphvi ( via ) - 1
                planta = plantas_x_via ( localidad )
                !Hacer para todas las unidades de la planta
                do localidad_1 = 0, NOUN ( planta ) - 1
                    unidad = DNUNPH ( planta ) + localidad_1
                    !arreglo que indica en que via divergente desfoga la unidad
                    viadaun ( unidad ) = via  
                    modelo = mounhi ( unidad )                   
                end do
                !Calcual gasto maximo de la unidad dependiendo del modelo y la altura del modelo
                !Encuentra el gasto maximo
                qmxmod ( planta ) = 0.0
                do contador = 1, 9
                    if ( qmaxun ( contador, modelo ) .gt. qmxmod ( planta ) ) then
                        qmxmod ( planta ) = qmaxun ( contador, modelo )
                    end if
                end do
            end do
        end do
    end do
end do

end subroutine Gasto_max_unidad_mod

!**************************************************************************
!  Proposito:
!       
!    Calcula los coeficientes de la linealizacion de la funcion de
!    generacion hidro contra gasto y altura
!
!  Registro de revisiones:
!       Fecha               Programador            Descripcion de cambios
!    ============       ===================       =======================
!     17 Junio   2013    Juan Alvarez Lopez           Codigo original
!
!**************************************************************************      
subroutine Coef_gen_hidro_lineal ( unidad, intervalo, via, planta , altura, volumen )

USE ParAuHeHidro
use ParAUHE, only: bmensaje
use ParGloRed, only: BasMva

implicit none

real*8 A_hat, B_hat, C_hat, altura, gasto, QMXEF, volumen, eval, &
       A_1, B_1, C_1, A_2, B_2, C_2, A_3, B_3, C_3

integer unidad, planta, via, modelo, ibanbit, ierror, intervalo

CHARACTER fecha_Ej*19

if ( unidad == 14 .and. intervalo == 3 ) then
    continue
endif

!Aqui via se utiliza SOLAMENTE como bandera para recalcular los parametros hidro
!1: Recalcular, Segunda vuelta
!2: Primera vuelta
if ( via .eq. 1 ) then
    gasto = qgastores ( unidad, intervalo )
    if ( gasto .eq. 0.0 ) then
        !Calcular gasto a maxima eficiencia
        gasto = QMXEF ( planta )
        if ( gasto .lt. qmxmod ( planta ) ) then
            gasto = qmxmod ( planta )
        end if
    end if
else
    !Calcular gasto a maxima eficiencia
    gasto = QMXEF ( planta )
    if ( gasto .lt. qmxmod ( planta ) ) then
!    if ( gasto .gt. qmxmod ( planta ) ) then
        gasto = qmxmod ( planta )
    end if    
end if

if ( gasto .eq. 0.0 ) then
    A_hatd ( unidad, intervalo ) = 0.0
    B_hatd ( unidad, intervalo ) = 0.0
    C_hatd ( unidad, intervalo ) = 0.0
    CIndGLH ( unidad, intervalo ) = 0.0
    CLinGLH ( unidad, intervalo ) = 0.0
    CLinGLHQ ( unidad, intervalo ) = 0.0
    CLinGLHW ( unidad, intervalo ) = 0.0
    CIndGLHWQ ( unidad, intervalo ) = 0.0
    goto 100
end if

if ( volumen .le. 0.0 ) then
    volumen = 1.0d-2
end if
            
modelo = MOUNHI ( unidad )
A_hatd ( unidad, intervalo ) = gqc ( 1, modelo ) + gqc ( 2, modelo ) * altura + gqc ( 3, modelo ) * altura * altura
B_hatd ( unidad, intervalo ) = gqc ( 4, modelo ) + gqc ( 5, modelo ) * altura + gqc ( 6, modelo ) * altura * altura
C_hatd ( unidad, intervalo ) = gqc ( 7, modelo ) + gqc ( 8, modelo ) * altura + gqc ( 9, modelo ) * altura * altura
if ( C_hatd ( unidad, intervalo ) .gt. 0 ) then
    ibanbit = 1
    ierror = 0
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' CHTAULEC ERROR MODELO GENERACION HIDRO'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   algoritmo no termina bien
    call SalidaError
    stop
end if
if ( intervalo == 24 ) then
    continue
end if
!Linealizacion wrt a gasto
CIndGLH ( unidad, intervalo ) = ( A_hatd ( unidad, intervalo ) - C_hatd ( unidad, intervalo ) * gasto * gasto ) / BasMva 
CLinGLH ( unidad, intervalo ) = ( B_hatd ( unidad, intervalo ) + 2 * C_hatd ( unidad, intervalo ) * gasto ) / BasMva   

!Linealizacion wrt gasto y volumen
!Calcular nuevos coeficientes wrt volumen en vez de altura
A_1 = gqc ( 1, modelo )
B_1 = ( gqc ( 2, modelo ) * altura ) / volumen
C_1 = ( gqc ( 3, modelo ) * altura * altura ) / ( volumen * volumen )

A_2 = gqc ( 4, modelo )
B_2 = ( gqc ( 5, modelo ) * altura ) / volumen
C_2 = ( gqc ( 6, modelo ) * altura * altura ) / ( volumen * volumen )

A_3 = gqc ( 7, modelo )
B_3 = ( gqc ( 8, modelo ) * altura ) / volumen
C_3 = ( gqc ( 9, modelo ) * altura * altura ) / ( volumen * volumen )

!Evaluar funcion bi-cuadratica
A_hat = A_1 + B_1 * volumen + C_1 * volumen * volumen
B_hat = A_2 + B_2 * volumen + C_2 * volumen * volumen
C_hat = A_3 + B_3 * volumen + C_3 * volumen * volumen

eval = A_hat + B_hat * gasto + C_hat * gasto * gasto

CLinGLHQ ( unidad, intervalo ) = A_2 + B_2 * volumen + C_2 * volumen * volumen + 2 * ( A_3 + B_3 * volumen + C_3 * volumen * volumen ) * gasto 

CLinGLHW ( unidad, intervalo ) = B_1 + B_2 * gasto + B_3 * gasto * gasto + 2 * ( C_1 + C_2 * gasto + C_3 * gasto * gasto ) * volumen

CIndGLHWQ ( unidad, intervalo )= eval - CLinGLHQ ( unidad, intervalo ) * gasto - CLinGLHW ( unidad, intervalo ) * volumen 

CLinGLHQ ( unidad, intervalo ) = CLinGLHQ ( unidad, intervalo ) / BasMva 

CLinGLHW ( unidad, intervalo ) = CLinGLHW ( unidad, intervalo ) / BasMva 

CIndGLHWQ ( unidad, intervalo ) = CIndGLHWQ ( unidad, intervalo ) / BasMva 

100 continue

end subroutine Coef_gen_hidro_lineal

SUBROUTINE VASOCI_126VAVICO 
!
!***********************************************************************
!  Proposito:
!       
!         LECTURA DE LAS CONDICIONES INICIALES DEL VOLUMEN EN          
!         TRANSITO.                                                    
!       
!          126. VAVICO (Vaso v�a convergente)
!  Registro de revisiones:
!       Fecha             Programador          Descripcion de cambios
!    ============    ===================       =======================
!    30 Octubre 2009    Isaias Guillen Moya       Codigo original
!    20 Junio 2013      Juan Alvarez Lopez        Adecuacion Hidro MIP
!
!***********************************************************************
USE ParAuHeHidro
use ParAUHE, only: rut_dat_1, bmensaje
IMPLICIT NONE
!----------------------------
!* Declaracion de variables *
!----------------------------
CHARACTER fecha_Ej*19, cerror*3
!
INTEGER  i, ie, nvasos, NVCOVC, error_h
INTEGER ini, ifin, ipos, idir, iv, IEM, IPVC, IUVC
INTEGER INICIA_CHAR_IGM, ivi, in, INMX
INTEGER ibanbit
REAL * 8 varrea, aux
REAL * 8    AUXREA (NMXINR)
!
CHARACTER* 5000 letaux
CHARACTER*1   let
CHARACTER*12   varch
!
!----------------------------------------------------------
!* DETERMINAR EL NUMERO DE EMBALSES E INICIAR EL REGISTRO *
!* QUE SE LEE.                                            *
!----------------------------------------------------------
NVASOS = DNEMVA ( nmcuen + 1 ) - 1
!-----------------------------------------------------------
!* DETERMINAR EL NUMERO DE EMBALSES CON VIAS CONVERGENTES. *
!-----------------------------------------------------------
NVCOVC = 0
DO  IE = 1 , NVASOS
    IF ( DNVIIN ( IE + 1 ) .GT. DNVIIN ( IE ) ) THEN
        NVCOVC = NVCOVC + 1
    END IF
    !DO  IN = 1 , NMXINR
    !    ESCTRA ( IE , IN ) = 0.0
    !END DO
END DO
!---------------------------------------
!* 126. VAVICO (Vaso v�a convergente) *
!---------------------------------------
OPEN ( UNIT = 126, DEFAULTFILE = rut_dat_1, FILE='VAVICO.csv', STATUS='OLD', IOSTAT = error_h )
i = 0
IF ( error_h .EQ. 0 ) THEN
    DO WHILE ( (i .lt. NVCOVC) .and. (error_h .eq. 0) )
        i = i + 1
        !------------------------------------------------
        !* Lee la informaci�n del 126. VAVICO (Vaso v�a convergente)
        !------------------------------------------------
        READ ( 126, 100, iostat = error_h  ) letaux
        idir = 1
        !----------------
        !* No hay error *
        !----------------
        IF ( error_h .eq. 0 ) THEN
            ini = 1
            ifin = 5000
            let = ','
            !-------------------------------
            !* N�mero vaso. *
            !-------------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            READ ( letaux(ini:ipos-1), 300 ) IEM
            ini = ipos + 1
            !--------------------------
            !* Nombre del vaso. *
            !--------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            READ ( letaux(ini:ipos-1), 100 ) varch
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 1.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(1) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 2.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(2) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 3.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(3) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 4.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(4) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 5.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(5) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 6.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(6) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 7.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(7) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 8.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(8) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 9.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(9) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 10.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(10) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 11.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(11) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 12.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(12) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 13.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(13) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 14.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(14) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 15.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(15) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 16.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(16) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 17.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(17) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 18.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(18) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 19.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(19) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 20.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(20) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 21.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(21) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 22.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(22) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 23.*
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(23) = varrea
            ini = ipos + 1
            !-----------------------------
            !* Descarga m�s derrames 24 *
            !-----------------------------
            ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
            CALL LEEREAl_IGM168 ( ini, ipos-1,  letaux, error_h, varrea )
            IF ( error_h .ne. 0 ) GO TO 1000
            AUXREA(24) = varrea

            IF ( ( IEM .GT. 0 ) .AND. ( IEM .LE. NVASOS ) ) THEN
                IV = CUEAEM ( IEM )
                IPVC = DNVIIN ( IEM )
                IUVC = DNVIIN ( IEM + 1 )
                INMX = 0
                DO WHILE ( IPVC .LT. IUVC )
                    IVI = VIAINC ( IPVC )
                    INMX = MAX0 ( INMX , TiViAgu ( IVI ) )
                    IPVC = IPVC + 1
                ENDDO
                INMX = MIN0 ( INMX , NMXINR )
                IN = 1
                DO WHILE ( IN .LE. INMX )
                    !------------------------------------------------------
                    !* SE CONSIDERA COMO APORTACION AL EMBALSE EL VOLUMEN *
                    !* EN TRANSITO AL INICIO DEL PERIODO DE PLANEACION.   *
                    !------------------------------------------------------
                    AUX = AUXREA ( IN ) * 1000.0
                    APOEMB ( IEM , IN ) = APOEMB ( IEM , IN ) + AUX
                    AguaEnViaje ( IEM , IN ) = AUX
                    
                    IN = IN + 1
                ENDDO
            END IF      
        END IF      
    END DO

ELSE
    ibanbit = 1
    !-------------------------------------------
    !* Escribe mensaje a bitacora y a pantalla *
    !-------------------------------------------
    WRITE ( cerror , 1350 ) error_h
    CALL FechaEjecucion (fecha_Ej)
    !
    BMensaje = fecha_Ej//' Error en SUBROUTINE VASOCI_126VAVICO : Tabla VAVICO.csv  '
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    !
    BMensaje = '                       Codigo de error '//cerror
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    BMensaje = fecha_Ej//' TERMINACION ANORMAL '
    !
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    WRITE(6,*)'1'                  
    ! 
    BMensaje = fecha_Ej//'1'
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    !
    CALL EXIT
END IF
CLOSE ( 126 )
!

1100  FORMAT ( / )
2300  FORMAT ( 1X , I4 , 6 ( 1X , I10 ) )
3400  FORMAT ( 6X , A6 , 4X , I3 , A3 , I3 )
!
GO TO 2000

1000  CONTINUE
ibanbit = 1
!-------------------------------------------
!* Escribe mensaje a bitacora y a pantalla *
!-------------------------------------------
WRITE ( cerror , 1350 ) error_h
CALL FechaEjecucion (fecha_Ej)
!
BMensaje = fecha_Ej//' Error en SUBROUTINE VASOCI_126VAVICO : Tabla VAVICO.csv  '
CALL Mensaje_cht ( 0, ibanbit, BMensaje )
!
BMensaje = '                       Codigo de error '//cerror
CALL Mensaje_cht ( 0, ibanbit, BMensaje )
BMensaje = fecha_Ej//' TERMINACION ANORMAL '
!
CALL Mensaje_cht ( 0, ibanbit, BMensaje )
WRITE(6,*)'1'                  
! 
BMensaje = fecha_Ej//'1'
CALL Mensaje_cht ( 0, ibanbit, BMensaje )
!
CALL EXIT
2000  CONTINUE
100   FORMAT ( a )
200   FORMAT ( f )
300   FORMAT ( i )
5000  FORMAT(2x,i4,x,i4)
1350  FORMAT ( I3.3 )
!
END SUBROUTINE VASOCI_126VAVICO

SUBROUTINE VIACON_131
!
!***********************************************************************
!  Proposito:
!       
!       
!         LECTURA DE VIAS CONVERGENTES   
!          131. VIACON
!  Registro de revisiones:
!       Fecha             Programador             Descripcion de cambios
!    ============    �  ===================       =======================
!    30 Octubre 2009    Isaias Guillen Moya       Codigo original
!    21 Junio 2013      Juan Alvarez Lopez        Adecuacion Hidro MIP
!
!***********************************************************************
USE ParAuHeHidro
use ParAUHE, only: rut_dat_1, bmensaje
     
IMPLICIT NONE
!----------------------------
!* Declaracion de variables *
!----------------------------
CHARACTER fecha_Ej*19, cerror*3
!
INTEGER i, varent,  nvasos, error_h
INTEGER ini, ifin, ipos, idir
INTEGER INICIA_CHAR_IGM
INTEGER ibanbit, nvin
!
CHARACTER* 5000 letaux
CHARACTER*1   let
!---------------------------------------
!* 131. VIACON (V�as convergentes) *
!---------------------------------------
OPEN ( UNIT = 131, DEFAULTFILE = rut_dat_1, FILE='VIACON.csv', STATUS='OLD', IOSTAT = error_h )
!---------------------------------------------------
!* DETERMINAR EL NUMERO DE EMBALSES EN EL SISTEMA. *
!---------------------------------------------------
!numero de embalses
NVASOS = DNEMVA ( nmcuen + 1 ) - 1
!numero de vias convergentes
NVIN = DNVIIN ( DNEMVA ( nmcuen + 1 ) ) - 1
!    
i = 0
IF ( error_h .EQ. 0 ) THEN
    DO WHILE ( (i .lt. NVIN) .and. (error_h .eq. 0) )
    i = i + 1
    !------------------------------------------------
    !* Lee la informaci�n del 131. VIACON (V�as convergentes)
    !------------------------------------------------
    READ ( 131, 100, iostat = error_h  ) letaux
    idir = 1
    !----------------
    !* No hay error *
    !----------------
    IF ( error_h .eq. 0 ) THEN
    ini = 1
    ifin = 5000
    let = ','
    !-------------------------------
    !* N�mero de la v�a.. *
    !-------------------------------
    ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
    READ ( letaux(ini:ipos-1), 300 ) varent
    !--------------------------------------------
    !* ALMACENAR EL INDICE DE LA VIA INCIDENTE. *
    !--------------------------------------------
    VIAINC ( i ) = varent
    ini = ipos + 1
    !--------------------------
    !* N�mero del vaso origen.. *
    !--------------------------
    ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
    READ ( letaux(ini:ipos-1), 300 ) varent
    ini = ipos + 1
    !-----------------------------
    !* N�mero del vaso destino..*
    !-----------------------------
    ipos = INICIA_CHAR_IGM ( ini, ifin, idir, let, letaux )
    READ ( letaux(ini:ipos-1), 300 ) varent
    END IF      
    END DO
ELSE
    ibanbit = 1
    !-------------------------------------------
    !* Escribe mensaje a bitacora y a pantalla *
    !-------------------------------------------
    WRITE ( cerror , 1350 ) error_h
    CALL FechaEjecucion (fecha_Ej)
    !
    BMensaje = fecha_Ej//' Error en SUBROUTINE VIACON_131 : Tabla VIACON.csv  '
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    !
    BMensaje = '                       Codigo de error '//cerror
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    BMensaje = fecha_Ej//' TERMINACION ANORMAL '
    !
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    WRITE(6,*)'1'                  
    ! 
    BMensaje = fecha_Ej//'1'
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    !
    CALL EXIT
END IF
CLOSE ( 131 )
!

1100  FORMAT ( / )
2100  FORMAT ( 5X , 6 ( 1X , A10 ) )
2300  FORMAT ( 1X , I4 , 6 ( 1X , I10 ) )

!
100   FORMAT ( a )
200   FORMAT ( f )
300   FORMAT ( i )
5000  FORMAT(2x,i4,x,i4)
1350  FORMAT ( I3.3 )
!
END SUBROUTINE VIACON_131  

    
SUBROUTINE VASOHE_130
!***********************************************************************
!  Proposito:
!       
!       
!         LECTURA DE APORTACIONES A LOS EMBALSES.   
!          VASOHE (Vasos horario)
!  Registro de revisiones:
!       Fecha             Programador          Descripcion de cambios
!    ============    ===================       =======================
!    30 Octubre 2009    Isaias Guillen Moya       Codigo original
!    21 Junio 2013      Juan Alvarez Lopez        Adecuacion Hidro MIP
!    07 Septiembre 2017 Jose Luis Ceciliano       Adecuacion Hidro AUHE
!
!***********************************************************************
USE ParAuHeHidro
use ParAUHE, only: rut_dat_1, durdia, bmensaje, intdia, NumEmbalses, NomEjecu, TipoEjecu

IMPLICIT NONE
!----------------------------
!* Declaracion de variables *
!----------------------------
CHARACTER fecha_Ej*19
!
INTEGER d, e, i, j, ierror, ierr
INTEGER ibanbit
real*8  let, aponat, extraccion

CHARACTER* 5000 letaux

!---------------------------------------
!* VASOHE (Vasos horario)*
!---------------------------------------
OPEN ( UNIT = 130, DEFAULTFILE = rut_dat_1, FILE='VASOHE.csv', STATUS='OLD', IOSTAT = ierror, RECORDSIZE = 250 )
if ( ierror .eq. 0 ) then
!   Para todos los embalses
    do e = 1, NumEmbalses
        j = 0
!       Para todos los dias
        do d = 1, durdia
!           Para todos los intervalos del dia
            do i = 1, intdia ( d )
                j = j + 1
	            read ( 130, 100, iostat = ierror ) letaux
                if ( ierror .eq. 0 ) then
			        read ( letaux, *, iostat = ierr ) let, aponat, extraccion, let, let, let, let
                    APOEMB ( e, j ) = aponat - extraccion
                else	
	                ibanbit = 1
                    ierror = 0
                    Call FechaEjecucion (fecha_Ej)
                    bmensaje = fecha_Ej//' '//NomEjecu//'LEC ERROR DE LECTURA ARCHIVO VASOHE.csv'
                    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
                    write(*,*) '1'
!                   Se ecribe resultado de semaforos
                    call EscSemaforosError
!                   algoritmo no termina bien
                    call SalidaError
                    stop
                endif
            enddo
        enddo
        if ( TipoEjecu .eq. 5 .and. durdia .lt. 7 ) then
!           Para todos los intervalos restantes
            do i = j+1, 168
	            read ( 130, 100, iostat = ierror ) letaux
                if ( ierror .ne. 0 ) then
	                ibanbit = 1
                    ierror = 0
                    Call FechaEjecucion (fecha_Ej)
                    bmensaje = fecha_Ej//' '//NomEjecu//'LEC ERROR DE LECTURA ARCHIVO VASOHE.csv'
                    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
                    write(*,*) '1'
!                   Se ecribe resultado de semaforos
                    call EscSemaforosError
!                   algoritmo no termina bien
                    call SalidaError
                    stop
                endif
            enddo
        endif
    enddo
else	
	ibanbit = 1
    ierror = 0
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'LEC ERROR DE LECTURA ARCHIVO VASOHE.csv'
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
endif

close ( 130 )

! se cambia a miles de m3
APOEMB = APOEMB*1000.0

100   FORMAT ( a )

END SUBROUTINE VASOHE_130 

    
SUBROUTINE LVIAHE_134VIAHE  
!
!***********************************************************************
!  Proposito:
!       
!         LECTURA DE EL VOLUMEN PROGRAMADO PARA SER VERTIDO SOBRE      
!         LAS VIAS.
!       
!          213. VIAHE (V�a horaria)
!  Registro de revisiones:
!       Fecha             Programador          Descripcion de cambios
!    ============    ===================       =======================
!    30 Octubre 2009    Isaias Guillen Moya       Codigo original
!    21 Junio 2013      Juan Alvarez Lopez        Adecuacion Hidro MIP
!    10 Octubre 2017    Jose Luis Ceciliano       Cambio para AUHE
!***********************************************************************
USE ParAuHeHidro
use ParAUHE, only: rut_dat_1, bmensaje, ntintr, durdia, intdia, NumEmbalses, NomEjecu

IMPLICIT NONE

integer d, e, i, j, ierror, ierr, ibanbit
CHARACTER fecha_Ej*19
CHARACTER* 500 letaux

!---------------------------------------
!* VIAHE (Via horaria)*
!---------------------------------------
OPEN ( UNIT = 130, DEFAULTFILE = rut_dat_1, FILE='VIAHE.csv', STATUS='OLD', IOSTAT = ierror, RECORDSIZE = 250 )
if ( ierror .eq. 0 ) then
!   Para todos los embalses
    do e = 1, NumEmbalses
        j = 0
!       Para todos los dias
        do d = 1, durdia
!           Para todos los intervalos del dia
            do i = 1, intdia ( d )
                j = j + 1
	            read ( 130, 100, iostat = ierror ) letaux
                if ( ierror .eq. 0 ) then
			        read ( letaux, *, iostat = ierr ) VERTDO ( e , j )
                else	
	                ibanbit = 1
                    ierror = 0
                    Call FechaEjecucion (fecha_Ej)
                    bmensaje = fecha_Ej//' '//NomEjecu//'LEC ERROR DE LECTURA ARCHIVO VIAHE.csv'
                    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
                    write(*,*) '1'
!                   Se ecribe resultado de semaforos
                    call EscSemaforosError
!                   algoritmo no termina bien
                    call SalidaError
                    stop
                endif
            enddo
        enddo
    enddo
    VERTDO = VERTDO*1000.0
else	
	ibanbit = 1
    ierror = 0
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'LEC ERROR DE LECTURA ARCHIVO VIAHE.csv'
    CALL Mensaje_cht ( 0, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
endif

close ( 130 )

100 format ( a )

END SUBROUTINE LVIAHE_134VIAHE

SUBROUTINE Cal_Aporta_Embalse 
!
!***********************************************************************
!  Proposito:
!       
!         CALCULA LAS APORTACIONES NETAS A LOS EMBALSES:
!         APOEMB  + VERTIDO EN VIAS: DONDE APOEMB ES IGUAL A EL AGUA
!         EN TRANSITO MAS APORTACION NATURAL - OTROS USOS DEL AGUA.                                                    
!       
!  Registro de revisiones:
!       Fecha             Programador          Descripcion de cambios
!    ============    ===================       =======================
!    20 Junio 2013      Juan Alvarez Lopez        Adecuacion Hidro MIP
!
!***********************************************************************
USE ParAuHeHidro
use ParAUHE, only: ntintr
IMPLICIT NONE
!----------------------------
!* Declaracion de variables *
!----------------------------
integer nvasos, ie, viac, consecutivo, intervalo, viad
!
!----------------------------------------------------------
!* DETERMINAR EL NUMERO DE EMBALSES E INICIAR EL REGISTRO *
!* QUE SE LEE.                                            *
!----------------------------------------------------------
NVASOS = DNEMVA ( nmcuen + 1 ) - 1

!hacer para todos los intervalos
do intervalo = 1, ntintr
    !Hacer para todos los embalses 
    consecutivo = 0
    do ie = 1, NVASOS 
        IF ( DNVIIN ( IE + 1 ) .GT. DNVIIN ( IE ) ) THEN
            !embalse tiene via convergente
            consecutivo = consecutivo + 1
            viac = VIAINC ( consecutivo )
            viad = DNVIOU ( ie )
            embviaconv ( ie ) = ie
            if ( intervalo - TiViAgu ( viac ) .gt. 0 ) then
                ApoNetEmb ( ie, intervalo ) = apoemb ( ie, intervalo ) + vertdo ( viac, intervalo ) - vertdo ( viad, intervalo )
            else
                ApoNetEmb ( ie, intervalo ) = apoemb ( ie, intervalo ) - vertdo ( viad, intervalo )
            end if     
        else
            viad = DNVIOU ( ie )
            ApoNetEmb ( ie, intervalo ) = apoemb ( ie, intervalo ) - vertdo ( viad, intervalo )
            embviaconv ( ie ) = 0
        end if
    end do  
end do
!a_tra_min ( intervalo + TiViAgu ( viad ), viad ) = turb_min + VERTDO ( viad, intervalo )
!vol_util_max ( embalse, intervalo ) = VMINI ( embalse ) + Apomax - turb_min - VERTDO ( viad, intervalo )

END SUBROUTINE Cal_Aporta_Embalse

!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!      P R O G R A M A S   D E   A P L I C A C I O N   A V A N Z A D A    *
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
!      Proposito: Imprime los datos del sistema hidrol�gico               *
!                                                                         *
!                                                                         *
!                                                                         *
!     Nombre y fecha de implementaci�n:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Junio 2013                            *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
!
!
SUBROUTINE imprime_red_hidro

use ParAUHE, only: NumEmbalses
USE ParAuHeHidro
!use ParAuHeHidro, only: NOMEMB

Implicit none
integer * 4 cuenca, embalse, localidad, localidad_2, localidad_3, &
            error_h, consecutivo, viad
            
logical entro
            
OPEN ( UNIT = 519, FILE = RUT_RES//'Red_Hidro.res', IOSTAT = error_h, STATUS='UNKNOWN', RECORDSIZE = 3024 )

WRITE ( 519 , * ) '************************************************************************'     
WRITE ( 519 , * ) 'RED HIDROLOGICA'   
WRITE ( 519 , * ) '************************************************************************'       

!Hacer para todas las cuencas
consecutivo = 0
!Vector que contiene el numero de la via convergente al embalse cuando dicho numero es diferente de cero
DNVICO = 0
WRITE ( 519 , * ) '************************************************************************'    
write ( 519, * ) 'POLITICAS DE OPERACION'
WRITE ( 519 , * ) '************************************************************************'    
write ( 519, * ) 'Maxima Extraccion : 1'
write ( 519, * ) 'Minima Extraccion : 2'
write ( 519, * ) 'Cota Fija Final   : 3'
write ( 519, * ) 'Energia Economica : 4'
write ( 519, * ) 'Vol. Maximo a Turb: 5'
write ( 519, * ) ''
WRITE ( 519 , * ) '************************************************************************'    
write ( 519, * ) 'Aportacion = Agua en Transito + Aportacion Natural - Otros Usos del Agua' 
WRITE ( 519 , * ) '************************************************************************'    
WRITE ( 519 , * ) ''    
do cuenca = 1,nmcuen        
    !Imprimir nombre y numero de la cuenca
    WRITE ( 519 , 112 ) '************************************************************************'
    WRITE ( 519 , 100 ) ' CUENCA NO:', cuenca, ' NOMBRE:', nomval ( cuenca )  
    WRITE ( 519 , 112 ) '************************************************************************' 
    WRITE ( 519 , * ) ' '       
    !Hacer para todos los emblases de la cuenca     
    localidad_3 = 0
    entro = .false. 
    do while ( localidad_3 .le. nmembc ( cuenca ) - 1 )
        do localidad_2 = 0, nmembc ( cuenca ) - 1
            embalse = dnemva ( cuenca ) + localidad_2
            !Se revisa si tiene via convergente
            if ( embviaconv ( embalse ) .eq. 0 .and. entro .eq. .false. ) then
                !Primer embalse de la cuenca
                WRITE ( 519, 101 ) '                         |   |'
                write ( 519, 102 ) 'Politica:', PoliEmb ( embalse ), '              |   |   Aportacion:', apoemb ( embalse, 1 ) / 1000.00, 'MMC' 
                write ( 519, 103 ) 'NAMINO:', namino ( embalse ), 'm','     /\   |'
                write ( 519, 103 ) 'NIMXOP:', namo ( embalse ), 'm','    /  \  /'
                write ( 519, 104 ) 'V.U.CI:', VMINI ( embalse ) / 1000.0, 'MMC',' /    \/'
                write ( 519, 105 ) '                     /', nomemb ( embalse ), embalse, '\'
                write ( 519, 106 ) 'V.Mx:', VolMxEmb ( embalse ) / 1000.00, 'MMC', ' /        \'
                write ( 519, 107 ) '                   ---------- ) Vertido programado:', vertdo ( DNVIOU ( embalse ), 1 ) / 1000.00, 'MMC'
                write ( 519, 108 ) '                        |    /'
                !Imprime via divergente
                viad = DNVIOU ( embalse ) 
                write ( 519, 109 ) NOMVIA ( embalse ), '                    |'
                write ( 519, 110 ) 'NMD:', NMDVI ( viad ), 'm', '          |'
                write ( 519, 111 ) 'Limit Min:', GAMNVI ( viad ) / 1000.00, 'MMC', '  |'	
                write ( 519, 111 ) 'Limit Max:', GAMXVI ( viad ) / 1000.00, 'MMC', '  |'	
                write ( 519, 113 ) 'T.V.A:', TiViAgu ( viad ), 'hr', '      |'	
                localidad_3 = localidad_3 + 1
                entro = .true.
            else
                if ( entro .eq. .true. ) then
                    !Buscar que embalse tiene la via divergente del embalse aguas arriba como via convergente 
                    !Hacer para todos los embalses de la cuenca
                    do localidad = 0, NumEmbalses - 1   
                        consecutivo = consecutivo + 1                         
!                         if ( arrgent3(viad) .eq. viainc ( consecutivo ) ) then
                         if ( viad .eq. viainc ( consecutivo ) ) then
                           DNVICO ( embalse ) = viainc ( consecutivo )                            
                            WRITE ( 519, 101 ) '                         |   |'
                            write ( 519, 102 ) 'Politica:', PoliEmb ( embalse ), '             |   |   Aportacion:', apoemb ( embalse, 1 ) / 1000.00, 'MMC' 
                            write ( 519, 103 ) 'NAMINO:', namino ( embalse ), 'm','     /\   |'
                            write ( 519, 103 ) 'NIMXOP:', namo ( embalse ), 'm','    /  \  /'
                            write ( 519, 104 ) 'V.U.CI:', VMINI ( embalse ) / 1000.0, 'MMC',' /    \/'
                            write ( 519, 105 ) '                    /', nomemb ( embalse ), embalse, '\'
                            write ( 519, 106 ) 'V.Mx:', VolMxEmb ( embalse ) / 1000.00, 'MMC', '  /        \'
                            write ( 519, 107 ) '                    ---------- ) Vertido programado:', vertdo ( DNVIOU ( embalse ), 1 ) / 1000.00, 'MMC'
                            write ( 519, 108 ) '                         |    /'
                            !Imprime via divergente
                            viad = DNVIOU ( embalse ) 
                            write ( 519, 109 ) NOMVIA ( embalse ), '                   |'
                            write ( 519, 110 ) 'NMD:', NMDVI ( viad ), 'm', '          |'
                            write ( 519, 111 ) 'Limit Min:', GAMNVI ( viad ) / 1000.00, 'MMC', ' |'	
                            write ( 519, 111 ) 'Limit Max:', GAMXVI ( viad ) / 1000.00, 'MMC', ' |'
                            write ( 519, 113 ) 'T.V.A:', TiViAgu ( viad ), 'hr', '     |'
                            localidad_3 = localidad_3 + 1 
                            exit                    
                        end if
                    end do
                end if
            end if 
        end do
    
    end do
end do

close ( unit = 519 )
100 format ( a13, x, i3, 2( a11, x ) )
101 format ( a30 )
102 format ( a9, x, i1, a33, x, F7.2, x, a3 )
103 format (  a7, x, f8.2, x, a1, x, a11) 
104 format (  a7, x, f8.2, x, a3, x, a8)
105 format (  a22, a3, x, i2, a1 ) 
106 format (  a5, x, f8.2, x, a3, a12 )
107 format (  a52, x, f9.5, x, a3 )
108 format (  a31 )
109 format (  a5, a21 )
110 format (  a4, x, f8.2, x, a1,  a11 )
111 format (  a10, x, f8.2, x, a3,  a3 )
112 format ( a72 )
113 format ( a6, x, i2, x, a2, a14)
END SUBROUTINE imprime_red_hidro

FUNCTION ALUTIL ( IE , VOLUME )
!***************************** CHAU ***********************************
!                                                                     *
!     PROPOSITO calcula la altura del embalse dado un volumen util    *
!                                                                     *
!     NOMBRE Y FECHA DE IMPLEMENTACION                                *
!        MANUEL RUIZ CASILLAS           85 08 20                      *
!                                                                     *
!     NOMBRE Y FECHA DE REVISION(ES)                                  *
!        MANUEL RUIZ CASILLAS           95 03 02                      *
!                                                                     *
!     Adeucacion Hidro MIP                                            *
!        Juan Alvarez Lopez             13 07 01                      *
!                                                                     *
!**********************************************************************
USE ParAuHeHidro
IMPLICIT NONE
!-----------------------------
!* DECLARACION DE VARIABLES. *
!-----------------------------
REAL * 8 DELTAL , DELTAV , DELVV  , VOLU   , alutil, VOLUME
INTEGER * 4 IE     , ipunto   , iv
!-----------------------
!* VARIABLES GLOBALES. *
!-----------------------
IV = CUEAEM ( IE )
VOLU = VOLUME
ipunto = 1
!-------------------------------------------------------------------
!* SE BUSCARA EL SEGMENTO LINEAL DENTRO DEL CUAL QUEDA COMPRENDIDO *
!* EL VALOR CONOCIDO DEL VOLUMEN. SI ESTE VALOR QUEDA FUERA DE LA  *
!* TABLA, SE EXTRAPOLARA SUPONIENDO QUE FUERA DE LA TABLA EL COM-  *
!* PORTAMIENTO ES COMO LA DEL ULTIMO SEGMENTO MAS PROXIMO.         *
!-------------------------------------------------------------------
DO WHILE ( ( VOLU .GT. VOUTIL ( ipunto , IE ) ) .AND. ( ipunto .NE. 9 ) )
    ipunto = ipunto + 1
ENDDO
IF ( ipunto .NE. 1 ) THEN
    DELTAV = VOUTIL ( ipunto , IE ) - VOUTIL ( ipunto - 1 , IE )
    DELTAL = ELUTIL ( ipunto , IE ) - ELUTIL ( ipunto - 1 , IE )
    DELVV  = VOUTIL ( ipunto , IE ) - VOLU
    ALUTIL = ELUTIL ( ipunto , IE ) - DELVV * DELTAL / DELTAV
ELSE
    DELTAV = VOUTIL ( 2 , IE ) - VOUTIL ( 1 , IE )
    DELTAL = ELUTIL ( 2 , IE ) - ELUTIL ( 1 , IE )
    DELVV  = VOUTIL ( 1 , IE ) - VOLU
    ALUTIL = ELUTIL ( 1 , IE ) - DELVV * DELTAL / DELTAV
END IF
END FUNCTION ALUTIL

FUNCTION RIVDES ( IE, IVI , QPL , IVERTI , ALEMAB )
!nnnn
!***************************** CHAU ***********************************
!                                                                     *
!     PROPOSITO: Calcula la altura de la via de desfogue              *
!                                                                     *
!     NOMBRE Y FECHA DE IMPLEMENTACION                                *
!        MANUEL RUIZ CASILLAS           85 08 20                      *
!                                                                     *
!     NOMBRE Y FECHA DE REVISION(ES)                                  *
!        MANUEL RUIZ CASILLAS           95 03 02                      *
!                                                                     *
!     NOMBRE Y FECHA DE REVISION(ES)                                  *
!        Juan Alvarez Lopez             13 07 01                      *
!**********************************************************************
USE ParAuHeHidro
IMPLICIT NONE
!-----------------------------
!* DECLARACION DE VARIABLES. *
!-----------------------------
REAL * 8 ALEMAB , AUX    , GASTO  , QPL    , RIVDES , XX     , YY  , ZZ, IVERTI
INTEGER * 4 IE     ,  IVI, IPU
!


GASTO = IVERTI + QPL

!GASTO = GASTO /  3.6 
IPU = 2
DO WHILE ( ( GASTO .GT. GSDES1 ( IPU , IVI ) ) .AND. ( IPU .LT. NMXPMD ) )
    IPU = IPU + 1
ENDDO
YY = NIVDES ( IPU , IVI ) - NIVDES ( IPU - 1 , IVI )
XX = GSDES1 ( IPU , IVI ) - GSDES1 ( IPU - 1 , IVI )
ZZ = GASTO - GSDES1 ( IPU - 1 , IVI )
AUX = NIVDES ( IPU - 1 , IVI ) + ZZ * YY / XX
RIVDES = DMAX1 ( AUX , ALEMAB )
END FUNCTION RIVDES

!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!      P R O G R A M A S   D E   A P L I C A C I O N   A V A N Z A D A    *
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
!      Proposito: Validar politicas en los embalses por hora considerando *
!                 las condiciones iniciales, las restricciones            *
!                 hidraulicas, DAC, y generacion fija                     *                                             
!                                                                         *
!                                                                         *
!                                                                         *
!     Nombre y fecha de implementaci�n:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Julio 2013                            *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
!
!
SUBROUTINE valida_politica

USE ParAuHeHidro
use ParAUHE, only: ntintr, durdia, BMensaje, intdia, nombunih, &
                   NumEmbalses, PotMaxUniH, PotMinUniH, DispoUH, AsignUH, CoordUH
use ProblemaAUHE, only: Sembandera

Implicit none

integer * 4 cuenca, embalse, via, planta, localidad, unidad, localidad_1, &
            localidad_2, localidad_3, intervalo, dia, error_h, consecutivo, &
            viad, viac, ibanbit, ierror, intervaloo, downstream, contador, localidad_4, &
            localidad_5, unitcount, localidad_7, PrevUnit ( maxuh ), cuentah, &
            PrevEmb, PrevInt
            
logical entro, pantallazo ( nmxemb ) 
            
real*8 q_max_embv ( nmxemb, maxint ), q_min_embv ( nmxemb, maxint ), gasto, q_max_pla, q_min_pla, q_max_uni, q_min_uni, &
       e_max_embv ( nmxemb, maxint ), e_min_embv ( nmxemb, maxint ), e_max_pla, e_min_pla, e_max_uni, e_min_uni, &
       q_max_emb, q_min_emb, e_max_emb, e_min_emb, apomax, apomin, turb_max, turb_min, ALUTIL, relajado, solicitado, &
       ener_max, ener_min, vol_util_max ( nmxemb, maxint ), vol_util_min ( nmxemb, maxint ), a_tra_min ( maxint + 25, nmxvia ), &
       a_tra_max ( maxint + 25, nmxvia ), turb_maxT ( nmxemb ), turb_minT ( nmxemb ), ener_maxT ( nmxemb ), ener_minT ( nmxemb ), &
       qmxef, nivdownstream, RIVDES, gastou, reltah, RGASMX, GASMN, g_min, g_max, nivdownstreamax, nivdownstreamin, &
       ndvimin ( nmxvia, maxint ), ndvimax ( nmxvia, maxint ), gastomax, gastomin, perdidasmax, perdidasmin, &
       qmxdac ( maxuh, MAXINT ), qmndac ( maxuh, MAXINT ), emxdac ( maxuh, MAXINT ), emndac ( maxuh, MAXINT ), &
       altura_neta, volumen_util
       
character*2 dia_text
character*1 aaux1

CHARACTER fecha_Ej*19

character*7 aaux, aaux_3, aaux_4
character*3 aaux_2
character*12  aaux_1, aaux_5
integer     ninter

!Calcula la validacion de las politicas de operacion por intervalo
OPEN ( UNIT = 520, FILE = RUT_RES//'Bal_Hid_Hor_Validado.res', IOSTAT = error_h, STATUS='UNKNOWN', RECORDSIZE = 3024 )

WRITE ( 520 , * ) '************************************************************************'     
WRITE ( 520 , * ) 'BALANCE DEL AGUA HORARIO (VALIDACION)'   
WRITE ( 520 , * ) '************************************************************************'       

!Vector que contiene el numero de la via convergente al embalse cuando dicho numero es diferente de cero
WRITE ( 520 , * ) '************************************************************************'    
write ( 520, * ) 'POLITICAS DE OPERACION'
WRITE ( 520 , * ) '************************************************************************'    
write ( 520, * ) 'Maxima Extraccion     : 1'
write ( 520, * ) 'Minima Extraccion     : 2'
write ( 520, * ) 'Cota Fija Final      m: 3'
write ( 520, * ) 'Energia Economica  GWH: 4'
write ( 520, * ) 'Vol. Maximo a Turb MMC: 5'
write ( 520, * ) ''
WRITE ( 520 , * ) '************************************************************************'    
write ( 520, * ) 'Aportacion = Agua en Transito Escenario Anterior + Aportacion Natural' 
write ( 520, * ) '           - Otros Usos del Agua + Turbinado Embalse Aguas Arriba'
write ( 520, * ) '             Considerando Tiempo de Viaje del Agua' 
WRITE ( 520 , * ) '************************************************************************'    
WRITE ( 520 , * ) '' 

!Hacer para todas las cuencas   
turb_maxT = 0.0
turb_minT = 0.0
ener_maxT = 0.0
ener_minT = 0.0
PrevUnit = 0
PrevInt = 0
pantallazo = .false.
PrevEmb = 0
cuentah = 1
do cuenca = 1,nmcuen        
    WRITE ( UNLG12_1 , * ) ' ' 
    WRITE ( UNLG12_1 , * ) '***********************************************************************************'
    !Imprimir nombre y numero de la cuenca
    WRITE ( UNLG12_1 , 100 ) ' CUENCA NO:', cuenca, ' NOMBRE:', nomval ( cuenca )    
    WRITE ( UNLG12_1 , * ) '***********************************************************************************'
    !Imprimir nombre y numero de la cuenca
    WRITE ( 520 , 117 ) '************************************************************************'
    WRITE ( 520 , 100 ) ' CUENCA NO:', cuenca, ' NOMBRE:', nomval ( cuenca )  
    WRITE ( 520 , 117 ) '************************************************************************' 
    WRITE ( 520 , * ) ' '   
    ninter = 1
    !Hacer para todos los dias
    do dia = 1, durdia
        write ( aaux1, 5103 ) dia
        dia_text = 'D'//aaux1
        write ( 520, 119 ) '--------------------------------','Dia', dia_text, '--------------------------------'
        !Hacer para todas las horas
        do intervalo = ninter, intdia (dia ) + ninter - 1
            WRITE ( UNLG12_1 , * ) ' '
            WRITE ( UNLG12_1 , * ) '------------------'
            WRITE ( UNLG12_1 , 123 ) ' INTERVALO:', intervalo 
            WRITE ( UNLG12_1 , * ) '------------------'
            consecutivo = 0
            write ( 520, * ) ' '
            write ( 520, 120 ) '--------------------------------','Hr', intervalo, '--------------------------------'
            write ( 520, * ) ' '
    
            !Hacer para todos los emblases de la cuenca     
            localidad_3 = 0
            entro = .false. 
            do while ( localidad_3 .le. nmembc ( cuenca ) - 1 )
                  do localidad_2 = 0, nmembc ( cuenca ) - 1
                    embalse = dnemva ( cuenca ) + localidad_2
                    q_max_embv ( embalse, intervalo ) = 0.0
                    q_min_embv ( embalse, intervalo ) = 0.0
                    e_max_embv ( embalse, intervalo ) = 0.0
                    e_min_embv ( embalse, intervalo ) = 0.0    
                    !Se revisa si tiene via convergente
                    if ( embviaconv ( embalse ) .eq. 0 .and. entro .eq. .false. ) then
                        !Imprimir nombre y numero del embalse
                        WRITE ( UNLG12_1 , * ) ' '     
                        WRITE ( UNLG12_1 , * ) '--------------------------------------------------------------------------'
                        WRITE ( UNLG12_1 , 100 ) ' EMBALSE NO:', embalse, ' NOMBRE:', NOMEMB ( embalse ) 
                        WRITE ( UNLG12_1 , * ) '--------------------------------------------------------------------------'
                        
                        !Primer embalse de la cuenca
                        !via divergente
                        viad = DNVIOU ( embalse ) 
                        !Encontrar el embalse aguas abajo
                        !Hacer para todos los embalses
                        downstream = 0
                        do contador = 1, NumEmbalses
                            if ( dnvico ( contador ) .eq. viad ) then
                                downstream = embviaconv ( contador )  
                                exit                        
                            end if
                        end do                      
                        if ( intervalo .eq. 1 ) then
                            !Hacer para todas las plantas que descargan sobre la v�a
                            !Inicializa gasto (acumulado) de todas las plantas que pertenecen al embalse
                            gastomin = 0.0
                            gastomax = 0.0                                    
                            !Inicializa perdidas (acumulado) de todas las plantas que pertenecen al embalse
                            perdidasmax = 0.0
                            perdidasmin = 0.0                            
                            do localidad = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                 planta = plantas_x_via ( localidad )
                                !Hacer para todas las unidades de la planta
                                do unitcount = 0, NOUN ( planta ) - 1                                
                                    unidad = DNUNPH ( planta ) + unitcount
                                    gastou = 0.0 
                                    if ( DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                        !Escoger el gasto que sea mayor: gasto maximo o gasto a maxima eficiencia
                                        gastou = QMXEF ( planta )
                                        if ( gastou .lt. qmxmod ( planta ) ) then
                                            gastou = qmxmod ( planta )                                                    
                                        end if
                                    end if
                                    if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 1 ) then
                                         gastomax = gastomax + gastou
                                         perdidasmax = perdidasmax + RELTAH ( planta , gastou )
                                    end if
                                    if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 0 ) then
                                        gastomax = gastomax + gastou
                                        gastomin = gastomin + gastou
                                        perdidasmax = perdidasmax + RELTAH ( planta , gastou )
                                        perdidasmin = perdidasmin + RELTAH ( planta , gastou )
                                    end if
                                end do
                             end do           
                            !Aportacion al embalse
                            Apomax = apoemb ( embalse, intervalo )
                            Apomin = apoemb ( embalse, intervalo )                            
                            if (downstream .ne. 0 ) then
                                !nivel en el embalse aguas abajo (condiciones iniciales)
                                nivdownstream = nivini ( downstream )                                    
                            else
                                nivdownstream = 0.0
                            end if                            
                            !Nivel en la via de desfogue minimo   
                            ndvimin ( viad, intervalo ) = RIVDES ( embalse, viad, gastomin, vertdo ( viad, intervalo ), nivdownstream )
                            !Nivel en la via de desfogue maximo   
                            ndvimax ( viad, intervalo ) = RIVDES ( embalse, viad, gastomax, vertdo ( viad, intervalo ), nivdownstream )
                            !Altura neta para las unidades del embalse    
                            Alt_net_desmin ( viad, intervalo ) = nivini ( embalse ) - ndvimax ( viad, intervalo ) - perdidasmax
                            !Altura neta maxima para las unidades del embalse    
                            Alt_net_desmax ( viad, intervalo ) = nivini ( embalse ) - ndvimin ( viad, intervalo ) - perdidasmin
                            !Hacer para todas las plantas que descargan sobre la v�a
                            do localidad = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                planta = plantas_x_via ( localidad )
                                WRITE ( UNLG12_1 , * ) 'Unidad/Planta  ---- Generacion (MW) -----  ----- Gasto (mMC/hr) -----  --------- Altura ---------'
                                !imprimir el nombre de la planta
                                WRITE ( UNLG12_1 , 122 ) planta, NOMPLAH ( planta ), '           Maximo        Minimo        Maximo        Minimo        Maximo        Minimo'
                                !Hacer para todas las unidades de la planta
                                do localidad_1 = 0, NOUN ( planta ) - 1
                                    unidad = DNUNPH ( planta ) + localidad_1
                                    !Calcular gastos minimos y maximos para las unidades del embalse
                                    !usando un promedio entre la altura minima y maxima
                                    !altura_neta = ( Alt_net_desmin ( viad, intervalo ) + Alt_net_desmax ( viad, intervalo ) ) / 2.0
                                    !altura neta fija 
                                    if ( nivini ( embalse ) .le. namo ( embalse ) ) then
                                        altura_neta =  nivini ( embalse ) - NMDVI ( viad )
                                        !volumen util
                                        volumen_util = vmini ( embalse )
                                    else
                                        altura_neta =  namo ( embalse ) - NMDVI ( viad )
                                        !volumen util
                                        volumen_util = VolMxEmb ( embalse )
                                    end if
                                    !gasto maximo calculado altura neta aproximimada para AU
                                    qmxgastodes ( unidad, intervalo ) = RGASMX ( unidad , altura_neta)
                                    !gasto minimo calculado altura neta aproximimada para AU
                                    qmngastodes ( unidad, intervalo ) = GASMN ( unidad , altura_neta )
                                    !Calcular coeficientes de la funcion de generacion para asignacion
                                    call Coef_gen_hidro_lineal ( unidad, intervalo, 2, planta, altura_neta, volumen_util ) 
                                    !Revisar si los limites minimos y maximos de generacion definidos estan dentro de los 
                                    !limites de los turbinados minimos y maximos
                                    !Evaluar generacion minima y maxima dependiendo del gasto minimo, maximo CUADRATICO                                    
                                    g_min = C_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) + &
                                                                A_hatd ( unidad, intervalo )
                                    g_max = C_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) + &
                                                                A_hatd ( unidad, intervalo )                                    
                                    !Revisar si hay violacion de limites de generacion definidos comparados con los turbinados min. y max. obtenidos con la altura
                                    !Ambos limites por debajo
                                    if ( PotMinUniH ( unidad, intervalo ) .lt. g_min .and. &
                                         PotMaxUniH ( unidad, intervalo ) .lt. g_min .and. &
                                         DispoUh ( unidad, intervalo ) .eq. 1 ) then
                                        PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                        if ( PrevUnit ( unidad ) .eq. 1 ) then
                                            ibanbit = 1
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_min                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )        
                                                                 
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_min   
                                            PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )  

                                            SemBandera ( 8 ) = 1

                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                
                                            ibanbit = 0
                                            ierror = 0
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )                       
                                        else
                                            ibanbit = 2
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_min                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )        
                                                                 
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_min   
                                            PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )  

                                            SemBandera ( 8 ) = 1
                                        
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                        end if
                                    end if
                                    
                                    !Ambos limites por arriba
                                    if ( PotMinUniH ( unidad, intervalo ) .gt. g_max .and. &
                                         PotMaxUniH ( unidad, intervalo ) .gt. g_max  .and. &
                                        DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                        PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                        if ( PrevUnit ( unidad ) .eq. 1  ) then
                                            ibanbit = 1
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_max                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                 
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                            PotMaxUniH ( unidad, intervalo ) = g_max    

                                            SemBandera ( 8 ) = 1
                                        
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                
                                            ibanbit = 0
                                            ierror = 0
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                        else
                                            ibanbit = 2
                                            ierror = 0
                                                
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_max                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                 
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                            PotMaxUniH ( unidad, intervalo ) = g_max    

                                            SemBandera ( 8 ) = 1

                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                        end if                                        
                                    end if
                                    
                                    !Potencia minima por debajo
                                    if ( PotMinUniH ( unidad, intervalo ) .lt. g_min ) then
                                        !Redefinir potencia maxima
                                        PotMinUniH ( unidad, intervalo ) = g_min
                                        SemBandera ( 8 ) = 1
                                    end if
                                    
                                    !Potencia maxima por arriba
                                    if ( PotMaxUniH ( unidad, intervalo ) .gt. g_max ) then
                                        !Redefinir potencia maxima
                                        PotMaxUniH ( unidad, intervalo ) = g_max
                                        SemBandera ( 8 ) = 1
                                    end if
                                    !Redefinir los gastos minimos y maximos dependiendo de las generaciones definidas por el usuario ya validadas lineal
                                    if ( PotMaxUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) .gt. 0.0 ) then
                                        qmxgastodes ( unidad, intervalo ) = ( PotMaxUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                    end if
                                    if ( PotMinUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) .gt. 0.0 ) then
                                        qmngastodes ( unidad, intervalo ) = ( PotMinUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                    end if
                                        !imprimir el nombre de la unidad
                                    !WRITE ( UNLG12_1 , 124 ) unidad, nombunih ( unidad ), PotMaxUniH ( unidad, intervalo ), PotMinUniH ( unidad, intervalo ), qmxgastodes ( unidad, intervalo ) * MValor, qmngastodes ( unidad, intervalo ) * MValor, Alt_net_desmax ( viad, intervalo ), Alt_net_desmin ( viad, intervalo )
                                    WRITE ( UNLG12_1 , 124 ) unidad, nombunih ( unidad ), PotMaxUniH ( unidad, intervalo ), PotMinUniH ( unidad, intervalo ), qmxgastodes ( unidad, intervalo ) * MValor, qmngastodes ( unidad, intervalo ) * MValor, altura_neta, altura_neta
                                    !si la unidad es disponible
                                    if ( DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                        !si la unidad es asignable coordinable
                                        if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 1 .and. CoordUH ( unidad, intervalo ) .eq. 1 ) then
                                            !Calcular gastos y energia respentando gastos minimos y maximos con
                                            !generaciones minimas y maximas definidas ya validadas
                                            q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + qmxgastodes ( unidad, intervalo ) * MValor
                                            !q_min_embv ( embalse, intervalo ) = q_min_embv ( embalse, intervalo ) + ( qmxgastodes ( unidad, intervalo ) * MValor ) / 2.0
                                            qmxdac ( unidad, intervalo ) = qmxgastodes ( unidad, intervalo )
                                            qmndac ( unidad, intervalo ) = 0.0                                            
                                            e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                            emxdac ( unidad, intervalo ) = Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                            emndac ( unidad, intervalo ) = 0.0
                                        end if      
                                        !si la unidad es no asignable coordinable
                                        if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 0 .and. CoordUH ( unidad, intervalo ) .eq. 1 ) then
                                            q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + qmxgastodes ( unidad, intervalo ) * MValor
                                            q_min_embv ( embalse, intervalo ) = q_min_embv ( embalse, intervalo ) + qmngastodes ( unidad, intervalo ) * MValor
                                            qmxdac ( unidad, intervalo ) = qmxgastodes ( unidad, intervalo )
                                            qmndac ( unidad, intervalo ) = qmngastodes ( unidad, intervalo )
                                            e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                            e_min_embv ( embalse, intervalo ) = e_min_embv ( embalse, intervalo ) + Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) )
                                            emxdac ( unidad, intervalo ) = Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                            emndac ( unidad, intervalo ) = Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) )
                                        end if         
                                        !si la unidad es asignable no coordinable
                                        if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 1 .and. CoordUH ( unidad, intervalo ) .eq. 0 ) then
                                            !calcular el gasto a la generacion solicitada LINEALIZADA
                                            gasto = ( potfijah ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                            q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + gasto * MValor
                                            qmxdac ( unidad, intervalo ) = gasto
                                            qmndac ( unidad, intervalo ) = 0
                                            e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + potfijah ( unidad, intervalo )
                                            emxdac ( unidad, intervalo ) = potfijah ( unidad, intervalo )
                                            emndac ( unidad, intervalo ) = 0.0
                                        end if      
                                        !si la unidad es no asignable no coordinable
                                        if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 0 .and. CoordUH ( unidad, intervalo ) .eq. 0 ) then                                
                                            !calcular el gasto a la generacion solicitada LINEALIZADA
                                            gasto = ( potfijah ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                            q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + gasto * MValor
                                            q_min_embv ( embalse, intervalo ) = q_min_embv ( embalse, intervalo ) + gasto * MValor
                                            qmxdac ( unidad, intervalo ) = gasto
                                            qmndac ( unidad, intervalo ) = gasto
                                            e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + potfijah ( unidad, intervalo )
                                            e_min_embv ( embalse, intervalo ) = e_min_embv ( embalse, intervalo ) + potfijah ( unidad, intervalo )
                                            emxdac ( unidad, intervalo ) = potfijah ( unidad, intervalo )
                                            emndac ( unidad, intervalo ) = potfijah ( unidad, intervalo )
                                        end if                                  
                                    else
                                        !Gasto maximo de la unidad hidro por DAC
                                        qmxdac ( unidad, intervalo ) = 0.0
                                        !Gasto maximo de la unidad hidro por DAC
                                        qmndac ( unidad, intervalo ) = 0.0
                                        emxdac ( unidad, intervalo ) = 0.0
                                        emndac ( unidad, intervalo ) = 0.0
                                    end if
                                end do   
                            end do                    
                            !Turbinado maximo y minimo del embalse por intervalo
                            turb_max = q_max_embv ( embalse, intervalo )
                            turb_min = q_min_embv ( embalse, intervalo )
                            
                            vol_util_max ( embalse, intervalo ) = VMINI ( embalse ) + Apomax - turb_min - VERTDO ( viad, intervalo )
                            vol_util_min ( embalse, intervalo ) = VMINI ( embalse ) + Apomin - turb_max - VERTDO ( viad, intervalo )
                            
                            if ( vol_util_min ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                
                                vol_util_min ( embalse, intervalo ) = VMINI ( embalse ) + Apomax - turb_min - VERTDO ( viad, intervalo )
                                
                                !turb_max = q_min_embv ( embalse, intervalo )
                                !e_max_embv ( embalse, intervalo ) = e_min_embv ( embalse, intervalo )
                               
                                if ( vol_util_min ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                    !if ( PrevEmb .ne. embalse ) then
                                    if ( pantallazo ( embalse ) .ne. .true. ) then
                                        !PrevEmb = embalse
                                        pantallazo ( embalse ) = .true.
                                        PrevInt = intervalo
                                        !Infactibilidad hidraulica violacion vol min.                                    
                                        ibanbit = 1
                                        ierror = 0
                                    
                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                    
                                        Call FechaEjecucion (fecha_Ej)
                                        bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                
                                        write ( aaux, 5100 ) embalse
                                        write ( aaux_1, 5101 ) nomemb ( embalse )
                                        write ( aaux_2, 5100 ) intervaloo 
                                        !write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_min ( embalse, intervalo ) ) / 1000.0
                                        write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - VolEmb ( embalse, intervalo ) ) / 1000.0
                                        write ( aaux_5, 5101 ) dia_text
                                                    
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                               
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                        
                                        ibanbit = 0
                                        ierror = 0
                                        Call FechaEjecucion (fecha_Ej)
                                        bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )               
                                    else
                                        if ( PrevInt .ne. intervalo ) then
                                            PrevInt = intervalo
                                            !Infactibilidad hidraulica violacion vol min.                                    
                                            ibanbit = 2
                                            ierror = 0
                                    
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                    
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                
                                            write ( aaux, 5100 ) embalse
                                            write ( aaux_1, 5101 ) nomemb ( embalse )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_min ( embalse, intervalo ) ) / 1000.0
                                            write ( aaux_5, 5101 ) dia_text
                                                    
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                               
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                        end if
                                    end if
                                end if
                            end if
                            
                            if ( vol_util_max ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                !if ( PrevEmb .ne. embalse ) then
                                if ( pantallazo ( embalse ) .ne. .true. ) then
                                    !PrevEmb = embalse
                                    pantallazo ( embalse ) = .true.
                                    PrevInt = intervalo
                                    !Infactibilidad hidraulica violacion vol min.                                    
                                    ibanbit = 1
                                    ierror = 0
                                        
                                    call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                    Call FechaEjecucion (fecha_Ej)
                                    bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                    write ( aaux, 5100 ) embalse
                                    write ( aaux_1, 5101 ) nomemb ( embalse )
                                    write ( aaux_2, 5100 ) intervaloo 
                                    write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_max ( embalse, intervalo ) ) / 1000.0
                                    write ( aaux_5, 5101 ) dia_text
                                                        
                                    Call FechaEjecucion (fecha_Ej)
                                    BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                               
                                    Call FechaEjecucion (fecha_Ej)
                                    BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                    
                                    ibanbit = 0
                                    ierror = 0
                                    Call FechaEjecucion (fecha_Ej)
                                    bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )              
                                else
                                    if ( PrevInt .ne. intervalo ) then
                                        PrevInt = intervalo
                                        !Infactibilidad hidraulica violacion vol min.                                    
                                        ibanbit = 2
                                        ierror = 0
                                        
                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                        Call FechaEjecucion (fecha_Ej)
                                        bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                        write ( aaux, 5100 ) embalse
                                        write ( aaux_1, 5101 ) nomemb ( embalse )
                                        write ( aaux_2, 5100 ) intervaloo 
                                        write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_max ( embalse, intervalo ) ) / 1000.0
                                        write ( aaux_5, 5101 ) dia_text
                                                        
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                               
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                    end if                                    
                                end if
                            end if
                            
                            if ( vol_util_max ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                
                                vol_util_max ( embalse, intervalo ) = VMINI ( embalse ) + Apomin - turb_max - VERTDO ( viad, intervalo )
                                
                                !turb_min = q_max_embv ( embalse, intervalo )
                                !e_min_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo )
                                
                                if ( vol_util_max ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                    !if ( PrevEmb .ne. embalse ) then
                                    if ( pantallazo ( embalse ) .ne. .true. ) then
                                        !PrevEmb = embalse
                                        pantallazo ( embalse ) = .true.
                                        PrevInt = intervalo
                                        !Infactibilidad hidraulica violacion vol max.                                    
                                        ibanbit = 1
                                        ierror = 0
                                        
                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                        Call FechaEjecucion (fecha_Ej)
                                        bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                        write ( aaux, 5100 ) embalse
                                        write ( aaux_1, 5101 ) nomemb ( embalse )
                                        write ( aaux_2, 5100 ) intervaloo 
                                        write ( aaux_3, 5102 ) ( vol_util_max ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                        write ( aaux_5, 5101 ) dia_text
                                                        
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                  
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                        
                                        ibanbit = 0
                                        ierror = 0
                                        Call FechaEjecucion (fecha_Ej)
                                        bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )       
                                    else
                                        if ( PrevInt .ne. intervalo ) then
                                            PrevInt = intervalo
                                            !Infactibilidad hidraulica violacion vol max.                                    
                                            ibanbit = 2
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) embalse
                                            write ( aaux_1, 5101 ) nomemb ( embalse )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) ( vol_util_max ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                  
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                        end if                                        
                                    end if
                                end if
                            end if
                            if ( vol_util_min ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                !if ( PrevEmb .ne. embalse ) then
                                if ( pantallazo ( embalse ) .ne. .true. ) then
                                    !PrevEmb = embalse
                                    pantallazo ( embalse ) = .true.
                                    PrevInt = intervalo
                            
                                    !Infactibilidad hidraulica violacion vol max.                                    
                                    ibanbit = 1
                                    ierror = 0
                                        
                                    call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                    Call FechaEjecucion (fecha_Ej)
                                    bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                    write ( aaux, 5100 ) embalse
                                    write ( aaux_1, 5101 ) nomemb ( embalse )
                                    write ( aaux_2, 5100 ) intervaloo 
                                    write ( aaux_3, 5102 ) ( vol_util_min ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                    write ( aaux_5, 5101 ) dia_text
                                                        
                                    Call FechaEjecucion (fecha_Ej)
                                    BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                  
                                    Call FechaEjecucion (fecha_Ej)
                                    BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                    
                                    ibanbit = 0
                                    ierror = 0
                                    Call FechaEjecucion (fecha_Ej)
                                    bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                else
                                    if ( PrevInt .ne. intervalo ) then
                                        PrevInt = intervalo
                                        !Infactibilidad hidraulica violacion vol max.                                    
                                        ibanbit = 2
                                        ierror = 0
                                        
                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                        Call FechaEjecucion (fecha_Ej)
                                        bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                        write ( aaux, 5100 ) embalse
                                        write ( aaux_1, 5101 ) nomemb ( embalse )
                                        write ( aaux_2, 5100 ) intervaloo 
                                        write ( aaux_3, 5102 ) ( vol_util_min ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                        write ( aaux_5, 5101 ) dia_text
                                                        
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                  
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                    end if                                    
                                end if
                            end if         
                                                         
                            turb_maxT ( embalse ) = turb_maxT ( embalse ) + turb_max
                            turb_minT ( embalse ) = turb_minT ( embalse ) + turb_min
                            
                            a_tra_max ( intervalo + TiViAgu ( viad ), viad ) = turb_max + VERTDO ( viad, intervalo )
                            a_tra_min ( intervalo + TiViAgu ( viad ), viad ) = turb_min + VERTDO ( viad, intervalo )
                            
                            ener_max = e_max_embv ( embalse, intervalo )
                            ener_min = e_min_embv ( embalse, intervalo )   
                            
                            ener_maxT ( embalse ) = ener_maxT ( embalse ) + ener_max
                            ener_minT ( embalse ) = ener_minT ( embalse ) + ener_min
                        
                        else
                            !Hacer para todas las plantas que descargan sobre la v�a
                            !Inicializa gasto (acumulado) de todas las plantas que pertenecen al embalse
                            gastomin = 0.0
                            gastomax = 0.0                                    
                            !Inicializa perdidas (acumulado) de todas las plantas que pertenecen al embalse
                            perdidasmax = 0.0
                            perdidasmin = 0.0                   
                            do localidad = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                planta = plantas_x_via ( localidad )
                                !Hacer para todas las unidades de la planta
                                do unitcount = 0, NOUN ( planta ) - 1                                
                                    unidad = DNUNPH ( planta ) + unitcount                                    
                                    gastou = 0.0 
                                    if ( DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                        gastou = QMXEF ( planta )
                                        if ( gastou .lt. qmxmod ( planta ) ) then
                                            gastou = qmxmod ( planta )
                                        end if
                                    end if
                                    if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 1  ) then
                                        gastomax = gastomax + gastou
                                        perdidasmax = perdidasmax + RELTAH ( planta , gastou )
                                    end if
                                    if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 0 ) then
                                        gastomax = gastomax + gastou
                                        gastomin = gastomin + gastou
                                        perdidasmax = perdidasmax + RELTAH ( planta , gastou )
                                        perdidasmin = perdidasmin + RELTAH ( planta , gastou )
                                    end if
                                end do
                            end do                            
                            !Aportacion al embalse
                            Apomax = apoemb ( embalse, intervalo )
                            Apomin = apoemb ( embalse, intervalo )                            
                            if (downstream .ne. 0 ) then
                                !nivel en el embalse aguas abajo (calculado con volumenes en embalse dejados al final de intervalo anterior)
                                !Nivel minimo del embalse aguas abajo calculado con turbinado minimo
                                nivdownstreamin = namino ( downstream ) + ALUTIL ( downstream , vol_util_min ( downstream, intervalo - 1 ) )
                                !Nivel maximo del embalse aguas abajo calculado con turbinado maximo
                                nivdownstreamax = namino ( downstream ) + ALUTIL ( downstream , vol_util_max ( downstream, intervalo - 1 ) )
                            else
                                nivdownstreamin = 0.0
                                nivdownstreamax = 0.0
                            end if
                            !Nivel en la via de desfogue minimo   
                            ndvimin ( viad, intervalo ) = RIVDES ( embalse, viad, gastomin, vertdo ( viad, intervalo ), nivdownstreamin )
                            !Nivel en la via de desfogue maximo   
                            ndvimax ( viad, intervalo ) = RIVDES ( embalse, viad, gastomax, vertdo ( viad, intervalo ), nivdownstreamax )
                            !Altura neta minima para las unidades del embalse    
                            Alt_net_desmin ( viad, intervalo ) = namino ( embalse ) + ALUTIL ( embalse , vol_util_min ( embalse, intervalo - 1 ) ) &
                                                                 - ndvimax ( viad, intervalo ) - perdidasmax
                            !Altura neta maxima para las unidades del embalse    
                            Alt_net_desmax ( viad, intervalo ) = namino ( embalse ) + ALUTIL ( embalse , vol_util_max ( embalse, intervalo - 1 ) ) &
                                                                 - ndvimin ( viad, intervalo ) - perdidasmin
                            !Hacer para todas las plantas que descargan sobre la v�a
                            do localidad = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                planta = plantas_x_via ( localidad )
                                WRITE ( UNLG12_1 , * ) 'Unidad/Planta  ---- Generacion (MW) -----  ----- Gasto (mMC/hr) -----  --------- Altura ---------'
                                !imprimir el nombre de la planta
                                WRITE ( UNLG12_1 , 122 ) planta, NOMPLAH ( planta ), '           Maximo        Minimo        Maximo        Minimo        Maximo        Minimo'
                                !Hacer para todas las unidades de la planta
                                do localidad_1 = 0, NOUN ( planta ) - 1
                                    unidad = DNUNPH ( planta ) + localidad_1
                                    !Calcular gastos minimos y maximos para las unidades del embalse
                                    !usando un promedio entre la altura minima y maxima
                                    !altura_neta = ( Alt_net_desmin ( viad, intervalo ) + Alt_net_desmax ( viad, intervalo ) ) /2.0                                    
                                    !altura neta fija 
                                    if ( nivini ( embalse ) .le. namo ( embalse ) ) then
                                        altura_neta =  nivini ( embalse ) - NMDVI ( viad )
                                        !volumen util
                                        volumen_util = vmini ( embalse )
                                    else
                                        altura_neta =  namo ( embalse ) - NMDVI ( viad )
                                        !volumen util
                                        volumen_util = VolMxEmb ( embalse )
                                    end if
                                    !volumen util variable
                                    !volumen_util = ( vol_util_min ( embalse, intervalo - 1 ) + vol_util_max ( embalse, intervalo - 1 ) ) / 2.0
                                    !volumen util fijo
                                    !gasto maximo calculado altura neta aproximimada para AU
                                    qmxgastodes ( unidad, intervalo ) = RGASMX ( unidad , altura_neta )
                                    !gasto minimo calculado altura neta aproximimada para AU
                                    qmngastodes ( unidad, intervalo ) = GASMN ( unidad , altura_neta )
                                    !Calcular coeficientes de la funcion de generacion para despacho usando altura promedio
                                    call Coef_gen_hidro_lineal ( unidad, intervalo, 2, planta, altura_neta, volumen_util ) 
                                    !Revisar si los limites minimos y maximos de generacion definidos estan dentro de los 
                                    !limites de los turbinados minimos y maximos
                                    !Evaluar generacion minima y maxima dependiendo del gasto minimo, maximo y altura CUADRATICO                                    
                                    g_min = C_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) + &
                                                                A_hatd ( unidad, intervalo )
                                    g_max = C_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) + &
                                                                A_hatd ( unidad, intervalo )                 
                                    !Revisar si hay violacion de limites de generacion definidos comparados con los turbinados min. y max. obtenidos con la altura
                                    !Ambos limites por debajo
                                    if ( PotMinUniH ( unidad, intervalo ) .lt. g_min .and. &
                                         PotMaxUniH ( unidad, intervalo ) .lt. g_min  .and. &
                                         DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                        PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                        if ( PrevUnit ( unidad ) .eq. 1 ) then
                                            ibanbit = 1
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_min                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                                         
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_min   
                                            PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )

                                            SemBandera ( 8 ) = 1
                                        
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                
                                            ibanbit = 0
                                            ierror = 0
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )                       
                                        else
                                            ibanbit = 2
                                            ierror = 0
                                                
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_min                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                                         
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_min   
                                            PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )

                                            SemBandera ( 8 ) = 1
                                        
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                        end if
                                    end if
                                    
                                    !Ambos limites por arriba
                                    if ( PotMinUniH ( unidad, intervalo ) .gt. g_max .and. &
                                         PotMaxUniH ( unidad, intervalo ) .gt. g_max  .and. &
                                         DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                        PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                        if ( PrevUnit ( unidad ) .eq. 1 ) then
                                        
                                            ibanbit = 1
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_max                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                        
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                            PotMaxUniH ( unidad, intervalo ) = g_max    

                                            SemBandera ( 8 ) = 1

                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                
                                            ibanbit = 0
                                            ierror = 0
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                        else
                                            ibanbit = 2
                                            ierror = 0
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_min                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )        
                                                                 
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_min   
                                            PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )

                                            SemBandera ( 8 ) = 1
                                        
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                        end if
                                    end if
                                    
                                    !Potencia minima por debajo
                                    if ( PotMinUniH ( unidad, intervalo ) .lt. g_min ) then
                                        !Redefinir potencia maxima
                                        PotMinUniH ( unidad, intervalo ) = g_min
                                        SemBandera ( 8 ) = 1
                                    end if
                                    
                                    !Potencia maxima por arriba
                                    if ( PotMaxUniH ( unidad, intervalo ) .gt. g_max ) then                                    
                                        !Redefinir potencia maxima
                                        PotMaxUniH ( unidad, intervalo ) = g_max
                                        SemBandera ( 8 ) = 1
                                    end if
                                    !Redefinir los gastos minimos y maximos dependiendo de las generaciones definidas por el usuario ya validadas
                                    if ( PotMaxUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) .gt. 0.0 ) then
                                        qmxgastodes ( unidad, intervalo ) = ( PotMaxUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                    end if
                                    if ( PotMinUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) .gt. 0.0 ) then
                                        qmngastodes ( unidad, intervalo ) = ( PotMinUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                    end if
                                   
                                        !WRITE ( UNLG12_1 , 124 ) unidad, nombunih ( unidad ), PotMaxUniH ( unidad, intervalo ), PotMinUniH ( unidad, intervalo ), qmxgastodes ( unidad, intervalo ) * MValor, qmngastodes ( unidad, intervalo ) * MValor, Alt_net_desmax ( viad, intervalo ), Alt_net_desmin ( viad, intervalo )
                                    WRITE ( UNLG12_1 , 124 ) unidad, nombunih ( unidad ), PotMaxUniH ( unidad, intervalo ), PotMinUniH ( unidad, intervalo ), qmxgastodes ( unidad, intervalo ) * MValor, qmngastodes ( unidad, intervalo ) * MValor, altura_neta, altura_neta                                    
                                    !si la unidad es disponible
                                    if ( DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                        !si la unidad es asignable coordinable
                                        if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 1 .and. CoordUH ( unidad, intervalo ) .eq. 1 ) then
                                            !Calcular gastos y energia respentando gastos minimos y maximos con
                                            !generaciones minimas y maximas definidas ya validadas
                                            q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + qmxgastodes ( unidad, intervalo ) * MValor
                                            !q_min_embv ( embalse, intervalo ) = q_min_embv ( embalse, intervalo ) + ( qmxgastodes ( unidad, intervalo ) * MValor ) / 2.0
                                            !Gasto maximo de la unidad hidro por DAC
                                            qmxdac ( unidad, intervalo ) = qmxgastodes ( unidad, intervalo )
                                            !Gasto maximo de la unidad hidro por DAC
                                            qmndac ( unidad, intervalo ) = 0.0
                                            e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                            emxdac ( unidad, intervalo ) = Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                            emndac ( unidad, intervalo ) = 0.0
                                        end if      
                                        !si la unidad es no asignable coordinable
                                        if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 0 .and. CoordUH ( unidad, intervalo ) .eq. 1 ) then
                                            q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + qmxgastodes ( unidad, intervalo ) * MValor
                                            q_min_embv ( embalse, intervalo ) = q_min_embv ( embalse, intervalo ) + qmngastodes ( unidad, intervalo ) * MValor
                                            !Gasto maximo de la unidad hidro por DAC
                                            qmxdac ( unidad, intervalo ) = qmxgastodes ( unidad, intervalo )
                                            !Gasto maximo de la unidad hidro por DAC
                                            qmndac ( unidad, intervalo ) = qmngastodes ( unidad, intervalo )
                                            e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                            e_min_embv ( embalse, intervalo ) = e_min_embv ( embalse, intervalo ) + Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) )
                                            emxdac ( unidad, intervalo ) = Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                            emndac ( unidad, intervalo ) = Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) )
                                        end if         
                                        !si la unidad es asignable no coordinable
                                        if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 1 .and. CoordUH ( unidad, intervalo ) .eq. 0 ) then
                                            !calcular el gasto a la generacion solicitada LINEALIZADA
                                            gasto = ( potfijah ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                            q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + gasto * MValor
                                            !Gasto maximo de la unidad hidro por DAC
                                            qmxdac ( unidad, intervalo ) = gasto
                                            !Gasto maximo de la unidad hidro por DAC
                                            qmndac ( unidad, intervalo ) = 0.0
                                            e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + potfijah ( unidad, intervalo )
                                            emxdac ( unidad, intervalo ) = potfijah ( unidad, intervalo )
                                            emndac ( unidad, intervalo ) = 0.0
                                        end if      
                                        !si la unidad es no asignable no coordinable
                                        if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 0 .and. CoordUH ( unidad, intervalo ) .eq. 0 ) then                                
                                            !calcular el gasto a la generacion solicitada LINEALIZADA
                                            gasto = ( potfijah ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                            q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + gasto * MValor
                                            q_min_embv ( embalse, intervalo ) = q_min_embv ( embalse, intervalo ) + gasto * MValor
                                            !Gasto maximo de la unidad hidro por DAC
                                            qmxdac ( unidad, intervalo ) = gasto
                                            !Gasto maximo de la unidad hidro por DAC
                                            qmndac ( unidad, intervalo ) = gasto
                                            e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + potfijah ( unidad, intervalo )
                                            e_min_embv ( embalse, intervalo ) = e_min_embv ( embalse, intervalo ) + potfijah ( unidad, intervalo )
                                            emxdac ( unidad, intervalo ) = potfijah ( unidad, intervalo )
                                            emndac ( unidad, intervalo ) = potfijah ( unidad, intervalo )
                                        end if                                  
                                    else
                                        !Gasto maximo de la unidad hidro por DAC
                                        qmxdac ( unidad, intervalo ) = 0.0
                                        !Gasto maximo de la unidad hidro por DAC
                                        qmndac ( unidad, intervalo ) = 0.0
                                        emxdac ( unidad, intervalo ) = 0.0
                                        emndac ( unidad, intervalo ) = 0.0
                                    end if
                                end do                                
                            end do                            
                            !Turbinado maximo y minimo del embalse por intervalo
                            turb_max = q_max_embv ( embalse, intervalo )
                            turb_min = q_min_embv ( embalse, intervalo )
                            
                            vol_util_max ( embalse, intervalo ) = vol_util_max ( embalse, intervalo - 1 ) + Apomax - turb_min - VERTDO ( viad, intervalo )
                            vol_util_min ( embalse, intervalo ) = vol_util_min ( embalse, intervalo - 1 ) + Apomin - turb_max - VERTDO ( viad, intervalo )
                            
                            if ( vol_util_min ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                
                                vol_util_min ( embalse, intervalo ) = vol_util_min ( embalse, intervalo - 1 ) + Apomax - turb_min - VERTDO ( viad, intervalo )
                                
                                !turb_max = q_min_embv ( embalse, intervalo )
                                !e_max_embv ( embalse, intervalo ) = e_min_embv ( embalse, intervalo )
                                
                                if ( vol_util_min ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                    !if ( PrevEmb .ne. embalse ) then
                                    if ( pantallazo ( embalse ) .ne. .true. ) then
                                        !PrevEmb = embalse
                                        pantallazo ( embalse ) = .true.
                                        PrevInt = intervalo
                                        !Infactibilidad hidraulica violacion vol min.                                    
                                        ibanbit = 1
                                        ierror = 0
                                            
                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                            
                                        Call FechaEjecucion (fecha_Ej)
                                        bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                        
                                        write ( aaux, 5100 ) embalse
                                        write ( aaux_1, 5101 ) nomemb ( embalse )
                                        write ( aaux_2, 5100 ) intervaloo 
                                        write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_min ( embalse, intervalo ) ) / 1000.0
                                        write ( aaux_5, 5101 ) dia_text
                                                            
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                      
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                        
                                        ibanbit = 0
                                        ierror = 0
                                        Call FechaEjecucion (fecha_Ej)
                                        bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )               
                                    else
                                        if ( PrevInt .ne. intervalo ) then
                                            PrevInt = intervalo
                                            !Infactibilidad hidraulica violacion vol min.                                    
                                            ibanbit = 2
                                            ierror = 0
                                            
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                            
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                        
                                            write ( aaux, 5100 ) embalse
                                            write ( aaux_1, 5101 ) nomemb ( embalse )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_min ( embalse, intervalo ) ) / 1000.0
                                            write ( aaux_5, 5101 ) dia_text
                                                            
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                      
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                        end if                                        
                                    end if
                                end if
                            end if
                            
                            if ( vol_util_max ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                !if ( PrevEmb .ne. embalse ) then
                                if ( pantallazo ( embalse ) .ne. .true. ) then
                                    !PrevEmb = embalse
                                    pantallazo ( embalse ) = .true.
                                    PrevInt = intervalo
                                    !Infactibilidad hidraulica violacion vol min.                                    
                                    ibanbit = 1
                                    ierror = 0
                                            
                                    call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                            
                                    Call FechaEjecucion (fecha_Ej)
                                    bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                        
                                    write ( aaux, 5100 ) embalse
                                    write ( aaux_1, 5101 ) nomemb ( embalse )
                                    write ( aaux_2, 5100 ) intervaloo 
                                    write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_max ( embalse, intervalo ) ) / 1000.0
                                    write ( aaux_5, 5101 ) dia_text
                                                            
                                    Call FechaEjecucion (fecha_Ej)
                                    BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                      
                                    Call FechaEjecucion (fecha_Ej)
                                    BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                    
                                    ibanbit = 0
                                    ierror = 0
                                    Call FechaEjecucion (fecha_Ej)
                                    bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                else
                                    if ( PrevInt .ne. intervalo ) then
                                        PrevInt = intervalo
                                        !Infactibilidad hidraulica violacion vol min.                                    
                                        ibanbit = 2
                                        ierror = 0
                                            
                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                            
                                        Call FechaEjecucion (fecha_Ej)
                                        bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                        
                                        write ( aaux, 5100 ) embalse
                                        write ( aaux_1, 5101 ) nomemb ( embalse )
                                        write ( aaux_2, 5100 ) intervaloo 
                                        write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_max ( embalse, intervalo ) ) / 1000.0
                                        write ( aaux_5, 5101 ) dia_text
                                                            
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                      
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                    end if                                    
                                end if
                            end if
                            
                            if ( vol_util_max ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                vol_util_max ( embalse, intervalo ) = vol_util_max ( embalse, intervalo - 1 ) + Apomin - turb_max - VERTDO ( viad, intervalo )
                                
                                !turb_min = q_max_embv ( embalse, intervalo )
                                !e_min_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) 
                                
                                if ( vol_util_max ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                    !if ( PrevEmb .ne. embalse ) then
                                    if ( pantallazo ( embalse ) .ne. .true. ) then
                                        !PrevEmb = embalse
                                        pantallazo ( embalse ) = .true.
                                        PrevInt = intervalo
                                        !Infactibilidad hidraulica violacion vol max.                                    
                                        ibanbit = 1
                                        ierror = 0
                                            
                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                            
                                        Call FechaEjecucion (fecha_Ej)
                                        bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                        
                                        write ( aaux, 5100 ) embalse
                                        write ( aaux_1, 5101 ) nomemb ( embalse )
                                        write ( aaux_2, 5100 ) intervaloo 
                                        write ( aaux_3, 5102 ) ( vol_util_max ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                        write ( aaux_5, 5101 ) dia_text
                                                            
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                      
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                        
                                        ibanbit = 0
                                        ierror = 0
                                        Call FechaEjecucion (fecha_Ej)
                                        bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )               
                                    else
                                        if ( PrevInt .ne. intervalo ) then
                                            PrevInt = intervalo
                                            !Infactibilidad hidraulica violacion vol max.                                    
                                            ibanbit = 2
                                            ierror = 0
                                            
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                            
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                        
                                            write ( aaux, 5100 ) embalse
                                            write ( aaux_1, 5101 ) nomemb ( embalse )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) ( vol_util_max ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                            write ( aaux_5, 5101 ) dia_text
                                                            
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                      
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )          
                                        end if                                        
                                    end if
                                end if
                            end if
                            
                            if ( vol_util_min ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                !if ( PrevEmb .ne. embalse ) then
                                if ( pantallazo ( embalse ) .ne. .true. ) then
                                    !PrevEmb = embalse
                                    pantallazo ( embalse ) = .true.
                                    PrevInt = intervalo
                                    !Infactibilidad hidraulica violacion vol max.                                    
                                    ibanbit = 1
                                    ierror = 0
                                            
                                    call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                            
                                    Call FechaEjecucion (fecha_Ej)
                                    bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                      
                                    write ( aaux, 5100 ) embalse
                                    write ( aaux_1, 5101 ) nomemb ( embalse )
                                    write ( aaux_2, 5100 ) intervaloo 
                                    write ( aaux_3, 5102 ) ( vol_util_min ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                    write ( aaux_5, 5101 ) dia_text
                                                            
                                    Call FechaEjecucion (fecha_Ej)
                                    BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                      
                                    Call FechaEjecucion (fecha_Ej)
                                    BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                    
                                    ibanbit = 0
                                    ierror = 0
                                    Call FechaEjecucion (fecha_Ej)
                                    bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )     
                                else
                                    if ( PrevInt .ne. intervalo ) then
                                        PrevInt = intervalo
                                        !Infactibilidad hidraulica violacion vol max.                                    
                                        ibanbit = 2
                                        ierror = 0
                                            
                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                            
                                        Call FechaEjecucion (fecha_Ej)
                                        bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                      
                                        write ( aaux, 5100 ) embalse
                                        write ( aaux_1, 5101 ) nomemb ( embalse )
                                        write ( aaux_2, 5100 ) intervaloo 
                                        write ( aaux_3, 5102 ) ( vol_util_min ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                        write ( aaux_5, 5101 ) dia_text
                                                            
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                      
                                        Call FechaEjecucion (fecha_Ej)
                                        BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                    end if                                    
                                end if
                            end if                            
                            
                            turb_maxT ( embalse ) = turb_maxT ( embalse ) + turb_max
                            turb_minT ( embalse ) = turb_minT ( embalse ) + turb_min
                            
                            a_tra_max ( intervalo + TiViAgu ( viad ), viad ) = turb_max + VERTDO ( viad, intervalo )
                            a_tra_min ( intervalo + TiViAgu ( viad ), viad ) = turb_min + VERTDO ( viad, intervalo )
                            ener_max = e_max_embv ( embalse, intervalo )
                            ener_min = e_min_embv ( embalse, intervalo )
                            
                            ener_maxT ( embalse ) = ener_maxT ( embalse ) + ener_max
                            ener_minT ( embalse ) = ener_minT ( embalse ) + ener_min
                            
                        end if                            
                        !Primer embalse de la cuenca
                        
                        WRITE ( 520, 101 ) '                            |   |   Aportacion Min:', Apomin / 1000.00, 'MMC'                         
                        WRITE ( 520, 101 ) '                            |   |   Aportacion Max:', Apomax / 1000.00, 'MMC'                         
                        write ( 520, 103 ) 'Vol.U.Ma:', vol_util_max ( embalse, intervalo ) / 1000.00, 'MMC','     /\   |'
                        write ( 520, 103 ) 'Vol.U.Mi:', vol_util_min ( embalse, intervalo ) / 1000.00, 'MMC','    /  \  /'                        
                        write ( 520, 103 ) 'Turb Max:', turb_max / 1000.0, 'MMC','   /    \/ '
                        write ( 520, 104 ) 'Turb Min:', turb_min / 1000.0, 'MMC','  /', nomemb ( embalse ), embalse, '\'   
                        write ( 520, 105 ) 'Ener Max:', ener_max / 1000.00, 'GWH', ' /        \'
                        write ( 520, 106 ) 'Ener Min:', ener_min / 1000.00, 'GWH', '---------- )', 'Vertido programado:', vertdo ( DNVIOU ( embalse ), intervalo ) / 1000.00, 'MMC' 
                        write ( 520, 107 ) '                            |    /'
                        !Imprime via divergente
                        write ( 520, 109 ) NOMVIA ( embalse ), '                       |'
                        write ( 520, 108 ) '                            |'
                        write ( 520, 116 ) 'Gasto Min:', ( turb_min + VERTDO ( viad, intervalo ) ) / 1000.00, 'MMC', '     |'	
                        write ( 520, 116 ) 'Gasto Max:', ( turb_max + VERTDO ( viad, intervalo ) ) / 1000.00, 'MMC', '     |'	
                        write ( 520, 118 ) 'T.V.A:', TiViAgu ( viad ), 'hr', '         |'	
                        localidad_3 = localidad_3 + 1
                        entro = .true.
                    else
                        if ( entro .eq. .true. ) then                            
                            !Buscar que embalse tiene la via divergente del embalse aguas arriba como via convergente                             
                            !Hacer para todos los embalses
                            do localidad = 0, NumEmbalses - 1   
                                consecutivo = consecutivo + 1                         
                                if ( viad .eq. viainc ( consecutivo ) ) then 
                                    !Imprimir nombre y numero del embalse
                                    WRITE ( UNLG12_1 , * ) ' '     
                                    WRITE ( UNLG12_1 , * ) '--------------------------------------------------------------------------'
                                    WRITE ( UNLG12_1 , 100 ) ' EMBALSE NO:', embalse, ' NOMBRE:', NOMEMB ( embalse ) 
                                    WRITE ( UNLG12_1 , * ) '--------------------------------------------------------------------------'
                                    !Via divergente
                                    viad = DNVIOU ( embalse ) 
                                    !Via convergente
                                    viac = viainc ( consecutivo )
                                    !Encontrar el embalse aguas abajo
                                    !Hacer para todos los embalses
                                    downstream = 0
                                    do contador = 1, NumEmbalses
                                        if ( dnvico ( contador ) .eq. viad ) then
                                            downstream = embviaconv ( contador )  
                                            exit                        
                                        end if
                                    end do
                                    if ( intervalo .eq. 1 ) then    
                                        !Hacer para todas las plantas que descargan sobre la v�a
                                        !Inicializa gasto (acumulado) de todas las plantas que pertenecen al embalse
                                        gastomin = 0.0
                                        gastomax = 0.0                                    
                                        !Inicializa perdidas (acumulado) de todas las plantas que pertenecen al embalse
                                        perdidasmax = 0.0
                                        perdidasmin = 0.0                                
                                        do localidad_5 = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                            planta = plantas_x_via ( localidad_5 )                                            
                                            !Hacer para todas las unidades de la planta
                                            do unitcount = 0, NOUN ( planta ) - 1                                
                                                unidad = DNUNPH ( planta ) + unitcount
                                                gastou = 0.0 
                                                if ( DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                                    gastou = QMXEF ( planta )
                                                    if ( gastou .lt. qmxmod ( planta ) ) then
                                                        gastou = qmxmod ( planta )                                                    
                                                    end if
                                                end if
                                                if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 1 ) then
                                                    gastomax = gastomax + gastou
                                                    perdidasmax = perdidasmax + RELTAH ( planta , gastou )
                                                end if
                                                if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 0 ) then
                                                    gastomax = gastomax + gastou
                                                    gastomin = gastomin + gastou
                                                    perdidasmax = perdidasmax + RELTAH ( planta , gastou )
                                                    perdidasmin = perdidasmin + RELTAH ( planta , gastou )
                                                end if
                                            end do
                                         end do                                    
                                        !Aportacion al embalse
                                        Apomax = apoemb ( embalse, intervalo ) + a_tra_max ( intervalo, viac )
                                        Apomin = apoemb ( embalse, intervalo ) + a_tra_min ( intervalo, viac )   
                                        if (downstream .ne. 0 ) then
                                            !nivel en el embalse aguas abajo (condiciones iniciales)
                                            nivdownstream = nivini ( downstream )                                    
                                        else
                                            nivdownstream = 0.0
                                        end if
                                        !Nivel en la via de desfogue minimo   
                                        ndvimin ( viad, intervalo ) = RIVDES ( embalse, viad, gastomin, vertdo ( viad, intervalo ), nivdownstream )
                                        !Nivel en la via de desfogue maximo   
                                        ndvimax ( viad, intervalo ) = RIVDES ( embalse, viad, gastomax, vertdo ( viad, intervalo ), nivdownstream )
                                        !Altura neta para las unidades del embalse    
                                        Alt_net_desmin ( viad, intervalo ) = nivini ( embalse ) - ndvimax ( viad, intervalo ) - perdidasmax
                                        !Altura neta maxima para las unidades del embalse    
                                        Alt_net_desmax ( viad, intervalo ) = nivini ( embalse ) - ndvimin ( viad, intervalo ) - perdidasmin
                                        !Hacer para todas las plantas que descargan sobre la v�a
                                        do localidad_4 = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                            planta = plantas_x_via ( localidad_4 )
                                            WRITE ( UNLG12_1 , * ) 'Unidad/Planta  ---- Generacion (MW) -----  ----- Gasto (mMC/hr) -----  --------- Altura ---------'
                                            !imprimir el nombre de la planta
                                            WRITE ( UNLG12_1 , 122 ) planta, NOMPLAH ( planta ), '           Maximo        Minimo        Maximo        Minimo        Maximo        Minimo'
                                            !Hacer para todas las unidades de la planta
                                            do localidad_1 = 0, NOUN ( planta ) - 1
                                               unidad = DNUNPH ( planta ) + localidad_1
                                                !Calcular gastos minimos y maximos para las unidades del embalse
                                                !usando un promedio entre la altura minima y maxima
                                                !altura_neta = ( Alt_net_desmin ( viad, intervalo ) + Alt_net_desmax ( viad, intervalo ) ) /2.0
                                                !altura neta fija 
                                                if ( nivini ( embalse ) .le. namo ( embalse ) ) then
                                                    altura_neta =  nivini ( embalse ) - NMDVI ( viad )
                                                    !volumen util
                                                    volumen_util = vmini ( embalse )
                                                else
                                                    altura_neta =  namo ( embalse ) - NMDVI ( viad )
                                                    !volumen util
                                                    volumen_util = VolMxEmb ( embalse )
                                                end if                                
                                                !gasto maximo calculado altura neta aproximimada para AU
                                                qmxgastodes ( unidad, intervalo ) = RGASMX ( unidad , altura_neta )
                                                !gasto minimo calculado altura neta aproximimada para AU
                                                qmngastodes ( unidad, intervalo ) = GASMN ( unidad , altura_neta )
                                                !Calcular coeficientes de la funcion de generacion para despacho
                                                call Coef_gen_hidro_lineal ( unidad, intervalo, 2, planta, altura_neta, volumen_util ) 
                                                !Revisar si los limites minimos y maximos de generacion definidos estan dentro de los 
                                                !limites de los turbinados minimos y maximos
                                                !Evaluar generacion minima y maxima dependiendo del gasto minimo, maximo y altura CUADRATICO                                    
                                                g_min = C_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) ** 2 + &
                                                                            B_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) + &
                                                                            A_hatd ( unidad, intervalo )
                                                g_max = C_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) ** 2 + &
                                                                            B_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) + &
                                                                            A_hatd ( unidad, intervalo )                 
                                                !Revisar si hay violacion de limites de generacion por turbinados min. y max. y definidos
                                                !Ambos limites por debajo
                                                if ( PotMinUniH ( unidad, intervalo ) .lt. g_min .and. &
                                                     PotMaxUniH ( unidad, intervalo ) .lt. g_min  .and. &
                                                    DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                                    PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                                    if ( PrevUnit ( unidad ) .eq. 1 ) then
                                                        ibanbit = 1
                                                        ierror = 0
                                                    
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_min                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_min   
                                                        PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )

                                                        SemBandera ( 8 ) = 1
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                            
                                                        ibanbit = 0
                                                        ierror = 0
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )     
                                                    else
                                                        ibanbit = 2
                                                        ierror = 0
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_min                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_min   
                                                        PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )

                                                        SemBandera ( 8 ) = 1
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                    end if
                                                end if
                                                
                                                !Ambos limites por arriba
                                                if ( PotMinUniH ( unidad, intervalo ) .gt. g_max .and. &
                                                     PotMaxUniH ( unidad, intervalo ) .gt. g_max  .and. &
                                                    DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                                    PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                                    if ( PrevUnit ( unidad ) .eq. 1 ) then
                                                   
                                                        ibanbit = 1
                                                        ierror = 0
                                                    
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_max                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                                        PotMaxUniH ( unidad, intervalo ) = g_max

                                                        SemBandera ( 8 ) = 1
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                        ibanbit = 0
                                                        ierror = 0
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )                       
                                                    else
                                                        ibanbit = 2
                                                        ierror = 0
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_max                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                                        PotMaxUniH ( unidad, intervalo ) = g_max

                                                        SemBandera ( 8 ) = 1
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    end if
                                                end if
                                                
                                                !Potencia minima por debajo
                                                if ( PotMinUniH ( unidad, intervalo ) .lt. g_min .and. DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                                    !Redefinir potencia maxima
                                                    PotMinUniH ( unidad, intervalo ) = g_min
                                                    SemBandera ( 8 ) = 1
                                                end if
                                                
                                                !Potencia maxima por arriba
                                                if ( PotMaxUniH ( unidad, intervalo ) .gt. g_max .and. DispoUH ( unidad, intervalo ) .eq. 1 ) then                                          
                                                    !Redefinir potencia maxima
                                                    PotMaxUniH ( unidad, intervalo ) = g_max
                                                    SemBandera ( 8 ) = 1
                                                end if
                                                !Redefinir los gastos minimos y maximos dependiendo de las generaciones definidas por el usuario ya validadas
                                                if ( PotMaxUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) .gt. 0.0 ) then
                                                    qmxgastodes ( unidad, intervalo ) = ( PotMaxUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                                end if
                                                if ( PotMinUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) .gt. 0.0 ) then
                                                    qmngastodes ( unidad, intervalo ) = ( PotMinUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                                end if                                                
                                                !WRITE ( UNLG12_1 , 124 ) unidad, nombunih ( unidad ), PotMaxUniH ( unidad, intervalo ), PotMinUniH ( unidad, intervalo ), qmxgastodes ( unidad, intervalo ) * MValor, qmngastodes ( unidad, intervalo ) * MValor, Alt_net_desmax ( viad, intervalo ), Alt_net_desmin ( viad, intervalo )
                                                WRITE ( UNLG12_1 , 124 ) unidad, nombunih ( unidad ), PotMaxUniH ( unidad, intervalo ), PotMinUniH ( unidad, intervalo ), qmxgastodes ( unidad, intervalo ) * MValor, qmngastodes ( unidad, intervalo ) * MValor, altura_neta, altura_neta                                                
                                                !si la unidad es disponible
                                                if ( DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                                    !si la unidad es asignable coordinable
                                                    if ( AsignUH ( unidad, intervalo ) .eq. 1 .and. CoordUH ( unidad, intervalo ) .eq. 1 ) then
                                                        !Calcular gastos y energia respentando gastos minimos y maximos con
                                                        !generaciones minimas y maximas definidas ya validadas
                                                        q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + qmxgastodes ( unidad, intervalo ) * MValor
                                                        !q_min_embv ( embalse, intervalo ) = q_min_embv ( embalse, intervalo ) + ( qmxgastodes ( unidad, intervalo ) * MValor ) / 2.0
                                                        !Gasto maximo de la unidad hidro por DAC
                                                        qmxdac ( unidad, intervalo ) = qmxgastodes ( unidad, intervalo )
                                                        !Gasto minimo de la unidad hidro por DAC
                                                        qmndac ( unidad, intervalo ) = 0.0
                                                        e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                                        emxdac ( unidad, intervalo ) = Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                                        emndac ( unidad, intervalo ) = 0.0
                                                    end if      
                                                    !si la unidad es no asignable coordinable
                                                    if ( AsignUH ( unidad, intervalo ) .eq. 0 .and. CoordUH ( unidad, intervalo ) .eq. 1 ) then
                                                        q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + qmxgastodes ( unidad, intervalo ) * MValor
                                                        q_min_embv ( embalse, intervalo ) = q_min_embv ( embalse, intervalo ) + qmngastodes ( unidad, intervalo ) * MValor
                                                        !Gasto maximo de la unidad hidro por DAC
                                                        qmxdac ( unidad, intervalo ) = qmxgastodes ( unidad, intervalo )
                                                        !Gasto minimo de la unidad hidro por DAC
                                                        qmndac ( unidad, intervalo ) = qmngastodes ( unidad, intervalo )
                                                        e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                                        e_min_embv ( embalse, intervalo ) = e_min_embv ( embalse, intervalo ) + Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) )
                                                        emxdac ( unidad, intervalo ) = Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                                        emndac ( unidad, intervalo ) = Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) )
                                                    end if         
                                                    !si la unidad es asignable no coordinable
                                                    if ( AsignUH ( unidad, intervalo ) .eq. 1 .and. CoordUH ( unidad, intervalo ) .eq. 0 ) then
                                                        !calcular el gasto a la generacion solicitada LINEALIZADA
                                                        gasto = ( potfijah ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                                        q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + gasto * MValor
                                                        !Gasto maximo de la unidad hidro por DAC
                                                        qmxdac ( unidad, intervalo ) = gasto
                                                        !Gasto minimo de la unidad hidro por DAC
                                                        qmndac ( unidad, intervalo ) = 0.0
                                                        e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + potfijah ( unidad, intervalo )
                                                        emxdac ( unidad, intervalo ) = potfijah ( unidad, intervalo )
                                                        emndac ( unidad, intervalo ) = 0.0
                                                    end if      
                                                    !si la unidad es no asignable no coordinable
                                                    if ( AsignUH ( unidad, intervalo ) .eq. 0 .and. CoordUH ( unidad, intervalo ) .eq. 0 ) then                                
                                                        !calcular el gasto a la generacion solicitada LINEALIZADA
                                                        gasto = ( potfijah ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                                        q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + gasto * MValor
                                                        q_min_embv ( embalse, intervalo ) = q_min_embv ( embalse, intervalo ) + gasto * MValor
                                                        !Gasto maximo de la unidad hidro por DAC
                                                        qmxdac ( unidad, intervalo ) = gasto
                                                        !Gasto minimo de la unidad hidro por DAC
                                                        qmndac ( unidad, intervalo ) = gasto
                                                        e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + potfijah ( unidad, intervalo )
                                                        e_min_embv ( embalse, intervalo ) = e_min_embv ( embalse, intervalo ) + potfijah ( unidad, intervalo )
                                                        emxdac ( unidad, intervalo ) = potfijah ( unidad, intervalo )
                                                        emndac ( unidad, intervalo ) = potfijah ( unidad, intervalo )
                                                    end if                                  
                                                else
                                                    !Gasto maximo de la unidad hidro por DAC
                                                    qmxdac ( unidad, intervalo ) = 0.0
                                                    !Gasto minimo de la unidad hidro por DAC
                                                    qmndac ( unidad, intervalo ) = 0.0
                                                    emxdac ( unidad, intervalo ) = 0.0
                                                    emndac ( unidad, intervalo ) = 0.0
                                                end if
                                            end do                                
                                        end do
                                        !Turbinado maximo y minimo del embalse por intervalo
                                        turb_max = q_max_embv ( embalse, intervalo ) 
                                        turb_min = q_min_embv ( embalse, intervalo ) 
                                        
                                        vol_util_max ( embalse, intervalo ) = VMINI ( embalse ) + Apomax - turb_min - VERTDO ( viad, intervalo )
                                        vol_util_min ( embalse, intervalo ) = VMINI ( embalse ) + Apomin - turb_max - VERTDO ( viad, intervalo )
                                                                                
                                        if ( vol_util_min ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                            vol_util_min ( embalse, intervalo ) = VMINI ( embalse ) + Apomax - turb_min - VERTDO ( viad, intervalo )
                                            
                                            !turb_max = q_min_embv ( embalse, intervalo )
                                            !e_max_embv ( embalse, intervalo ) = e_min_embv ( embalse, intervalo )
                                            
                                            if ( vol_util_min ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                                !if ( PrevEmb .ne. embalse ) then
                                                if ( pantallazo ( embalse ) .ne. .true. ) then
                                                    !PrevEmb = embalse
                                                    pantallazo ( embalse ) = .true.
                                                    PrevInt = intervalo
                                                    !Infactibilidad hidraulica violacion vol min.                                    
                                                    ibanbit = 1
                                                    ierror = 0
                                                        
                                                    call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                        
                                                    Call FechaEjecucion (fecha_Ej)
                                                    bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                    
                                                    write ( aaux, 5100 ) embalse
                                                    write ( aaux_1, 5101 ) nomemb ( embalse )
                                                    write ( aaux_2, 5100 ) intervaloo 
                                                    write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_min ( embalse, intervalo ) ) / 1000.0
                                                    write ( aaux_5, 5101 ) dia_text
                                                                        
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                  
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                    ibanbit = 0
                                                    ierror = 0
                                                    Call FechaEjecucion (fecha_Ej)
                                                    bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                else
                                                    if ( PrevInt .ne. intervalo ) then
                                                        PrevInt = intervalo
                                                        !Infactibilidad hidraulica violacion vol min.                                    
                                                        ibanbit = 2
                                                        ierror = 0
                                                        
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                        
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                    
                                                        write ( aaux, 5100 ) embalse
                                                        write ( aaux_1, 5101 ) nomemb ( embalse )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_min ( embalse, intervalo ) ) / 1000.0
                                                        write ( aaux_5, 5101 ) dia_text
                                                                        
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                  
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    end if                                                    
                                                end if
                                            end if
                                        end if
                                        
                                        if ( vol_util_max ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                            !if ( PrevEmb .ne. embalse ) then
                                            if ( pantallazo ( embalse ) .ne. .true. ) then
                                                !PrevEmb = embalse
                                                pantallazo ( embalse ) = .true.
                                                PrevInt = intervalo
                                                !Infactibilidad hidraulica violacion vol min.                                    
                                                ibanbit = 1
                                                ierror = 0
                                                        
                                                call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                        
                                                Call FechaEjecucion (fecha_Ej)
                                                bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                    
                                                write ( aaux, 5100 ) embalse
                                                write ( aaux_1, 5101 ) nomemb ( embalse )
                                                write ( aaux_2, 5100 ) intervaloo 
                                                write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_max ( embalse, intervalo ) ) / 1000.0
                                                write ( aaux_5, 5101 ) dia_text
                                                                        
                                                Call FechaEjecucion (fecha_Ej)
                                                BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                  
                                                Call FechaEjecucion (fecha_Ej)
                                                BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                
                                                ibanbit = 0
                                                ierror = 0
                                                Call FechaEjecucion (fecha_Ej)
                                                bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )              
                                            else
                                                if ( PrevInt .ne. intervalo ) then
                                                    PrevInt = intervalo
                                                    !Infactibilidad hidraulica violacion vol min.                                    
                                                    ibanbit = 2
                                                    ierror = 0
                                                        
                                                    call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                        
                                                    Call FechaEjecucion (fecha_Ej)
                                                    bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                    
                                                    write ( aaux, 5100 ) embalse
                                                    write ( aaux_1, 5101 ) nomemb ( embalse )
                                                    write ( aaux_2, 5100 ) intervaloo 
                                                    write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_max ( embalse, intervalo ) ) / 1000.0
                                                    write ( aaux_5, 5101 ) dia_text
                                                                        
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                  
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                end if                                                
                                            end if
                                        end if
                                        
                                        if ( vol_util_max ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                            vol_util_max ( embalse, intervalo ) = VMINI ( embalse ) + Apomin - turb_max - VERTDO ( viad, intervalo )
                                            
                                            !turb_min = q_max_embv ( embalse, intervalo ) 
                                            !e_min_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo )
                                            
                                            if ( vol_util_max ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                                !if ( PrevEmb .ne. embalse ) then
                                                if ( pantallazo ( embalse ) .ne. .true. ) then
                                                    !PrevEmb = embalse
                                                    pantallazo ( embalse ) = .true.
                                                    PrevInt = intervalo
                                                    !Infactibilidad hidraulica violacion vol max.                                    
                                                    ibanbit = 1
                                                    ierror = 0
                                                            
                                                    call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                            
                                                    Call FechaEjecucion (fecha_Ej)
                                                    bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                        
                                                    write ( aaux, 5100 ) embalse
                                                    write ( aaux_1, 5101 ) nomemb ( embalse )
                                                    write ( aaux_2, 5100 ) intervaloo 
                                                    write ( aaux_3, 5102 ) ( vol_util_max ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                                    write ( aaux_5, 5101 ) dia_text
                                                                            
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                      
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                    ibanbit = 0
                                                    ierror = 0
                                                    Call FechaEjecucion (fecha_Ej)
                                                    bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )           
                                                else
                                                    if ( PrevInt .ne. intervalo ) then
                                                        PrevInt = intervalo
                                                        !Infactibilidad hidraulica violacion vol max.                                    
                                                        ibanbit = 2
                                                        ierror = 0
                                                            
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                            
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                        
                                                        write ( aaux, 5100 ) embalse
                                                        write ( aaux_1, 5101 ) nomemb ( embalse )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) ( vol_util_max ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                                        write ( aaux_5, 5101 ) dia_text
                                                                            
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                      
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    end if                                                    
                                                end if
                                            end if
                                        end if
                                        
                                        if ( vol_util_min ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                            !if ( PrevEmb .ne. embalse ) then
                                            if ( pantallazo ( embalse ) .ne. .true. ) then
                                                !PrevEmb = embalse
                                                pantallazo ( embalse ) = .true.
                                                PrevInt = intervalo
                                                !Infactibilidad hidraulica violacion vol max.                                    
                                                ibanbit = 1
                                                ierror = 0
                                                            
                                                call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                            
                                                Call FechaEjecucion (fecha_Ej)
                                                bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                        
                                                write ( aaux, 5100 ) embalse
                                                write ( aaux_1, 5101 ) nomemb ( embalse )
                                                write ( aaux_2, 5100 ) intervaloo 
                                                write ( aaux_3, 5102 ) ( vol_util_min ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                                write ( aaux_5, 5101 ) dia_text
                                                                            
                                                Call FechaEjecucion (fecha_Ej)
                                                BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                      
                                                Call FechaEjecucion (fecha_Ej)
                                                BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                
                                                ibanbit = 0
                                                ierror = 0
                                                Call FechaEjecucion (fecha_Ej)
                                                bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                            else
                                                if ( PrevInt .ne. intervalo ) then
                                                    PrevInt = intervalo
                                                    !Infactibilidad hidraulica violacion vol max.                                    
                                                    ibanbit = 2
                                                    ierror = 0
                                                            
                                                    call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                            
                                                    Call FechaEjecucion (fecha_Ej)
                                                    bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                        
                                                    write ( aaux, 5100 ) embalse
                                                    write ( aaux_1, 5101 ) nomemb ( embalse )
                                                    write ( aaux_2, 5100 ) intervaloo 
                                                    write ( aaux_3, 5102 ) ( vol_util_min ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                                    write ( aaux_5, 5101 ) dia_text
                                                                            
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                      
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                end if                                                
                                            end if
                                        end if                                        
                                        
                                        turb_maxT ( embalse ) = turb_maxT ( embalse ) + turb_max
                                        turb_minT ( embalse ) = turb_minT ( embalse ) + turb_min
                                        
                                        a_tra_max ( intervalo + TiViAgu ( viad ), viad ) = turb_max + VERTDO ( viad, intervalo )
                                        a_tra_min ( intervalo + TiViAgu ( viad ), viad ) = turb_min + VERTDO ( viad, intervalo )
                                        
                                        ener_max = e_max_embv ( embalse, intervalo )
                                        ener_min = e_min_embv ( embalse, intervalo )    
                                        
                                        ener_maxT ( embalse ) = ener_maxT ( embalse ) + ener_max
                                        ener_minT ( embalse ) = ener_minT ( embalse ) + ener_min      
                                        
                                    else  ! Termina intervalo 1, evalua para los siguientes intervalos                                      
                                        
                                        !Hacer para todas las plantas que descargan sobre la v�a
                                        !Inicializa gasto (acumulado) de todas las plantas que pertenecen al embalse
                                        gastomin = 0.0
                                        gastomax = 0.0                                    
                                        !Inicializa perdidas (acumulado) de todas las plantas que pertenecen al embalse
                                        perdidasmax = 0.0
                                        perdidasmin = 0.0                   
                                        do localidad_7 = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                            planta = plantas_x_via ( localidad_7 )
                                            !Hacer para todas las unidades de la planta
                                            do unitcount = 0, NOUN ( planta ) - 1                                
                                                unidad = DNUNPH ( planta ) + unitcount
                                                gastou = 0.0 
                                                if ( DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                                    gastou = QMXEF ( planta )
                                                    if ( gastou .lt. qmxmod ( planta ) ) then
                                                        gastou = qmxmod ( planta )
                                                    end if
                                                end if
                                                if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 1 ) then
                                                    gastomax = gastomax + gastou
                                                    perdidasmax = perdidasmax + RELTAH ( planta , gastou )
                                                end if
                                                if ( DispoUH ( unidad, intervalo ) .eq. 1 .and. AsignUH ( unidad, intervalo ) .eq. 0 ) then
                                                    gastomax = gastomax + gastou
                                                    gastomin = gastomin + gastou
                                                    perdidasmax = perdidasmax + RELTAH ( planta , gastou )
                                                    perdidasmin = perdidasmin + RELTAH ( planta , gastou )
                                                end if
                                            end do
                                        end do      
                                        !Aportacion al embalse
                                        Apomax = apoemb ( embalse, intervalo ) + a_tra_max ( intervalo, viac )
                                        Apomin = apoemb ( embalse, intervalo ) + a_tra_min ( intervalo, viac )   
                                        if (downstream .ne. 0 ) then
                                            !nivel en el embalse aguas abajo (calculado con volumenes en embalse dejados al final de intervalo anterior)
                                            !Nivel minimo del embalse aguas abajo calculado con turbinado minimo
                                            nivdownstreamin = namino ( downstream ) + ALUTIL ( downstream , vol_util_min ( downstream, intervalo - 1 ) )
                                            !Nivel maximo del embalse aguas abajo calculado con turbinado maximo
                                            nivdownstreamax = namino ( downstream ) + ALUTIL ( downstream , vol_util_max ( downstream, intervalo - 1 ) )
                                        else
                                            nivdownstreamin = 0.0
                                            nivdownstreamax = 0.0
                                        end if
                                        !Nivel en la via de desfogue minimo   
                                        ndvimin ( viad, intervalo ) = RIVDES ( embalse, viad, gastomin, vertdo ( viad, intervalo ), nivdownstreamin )
                                        !Nivel en la via de desfogue maximo   
                                        ndvimax ( viad, intervalo ) = RIVDES ( embalse, viad, gastomax, vertdo ( viad, intervalo ), nivdownstreamax )
                                        !Altura neta minima para las unidades del embalse    
                                        Alt_net_desmin ( viad, intervalo ) = namino ( embalse ) + ALUTIL ( embalse , vol_util_min ( embalse, intervalo - 1 ) ) &
                                                                             - ndvimax ( viad, intervalo ) - perdidasmax
                                        !Altura neta maxima para las unidades del embalse    
                                        Alt_net_desmax ( viad, intervalo ) = namino ( embalse ) + ALUTIL ( embalse , vol_util_max ( embalse, intervalo - 1 ) ) &
                                                                         - ndvimin ( viad, intervalo ) - perdidasmin
                                        !Hacer para todas las plantas que descargan sobre la v�a
                                        do localidad_4 = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                            planta = plantas_x_via ( localidad_4 )
                                            WRITE ( UNLG12_1 , * ) 'Unidad/Planta  ---- Generacion (MW) -----  ----- Gasto (mMC/hr) -----  --------- Altura ---------'
                                            !imprimir el nombre de la planta
                                            WRITE ( UNLG12_1 , 122 ) planta, NOMPLAH ( planta ), '           Maximo        Minimo        Maximo        Minimo        Maximo        Minimo'
                                            !Hacer para todas las unidades de la planta
                                            do localidad_1 = 0, NOUN ( planta ) - 1
                                                unidad = DNUNPH ( planta ) + localidad_1
                                                !Calcular gastos minimos y maximos para las unidades del embalse
                                                !usando un promedio entre la altura minima y maxima
                                                !altura_neta = ( Alt_net_desmin ( viad, intervalo ) + Alt_net_desmax ( viad, intervalo ) ) /2.0
                                                !altura neta fija 
                                                if ( nivini ( embalse ) .le. namo ( embalse ) ) then
                                                    altura_neta =  nivini ( embalse ) - NMDVI ( viad )
                                                    !volumen util
                                                    volumen_util = vmini ( embalse )
                                                else
                                                    altura_neta =  namo ( embalse ) - NMDVI ( viad )
                                                    !volumen util
                                                    volumen_util = VolMxEmb ( embalse )
                                                end if
                                                !volumen util variable
                                                !volumen_util = ( vol_util_min ( embalse, intervalo - 1 ) + vol_util_max ( embalse, intervalo - 1 ) ) / 2.0
                                                !volumen util fijo
                                                !gasto maximo calculado altura neta aproximimada para AU
                                                qmxgastodes ( unidad, intervalo ) = RGASMX ( unidad , altura_neta )
                                                !gasto minimo calculado altura neta aproximimada para AU
                                                qmngastodes ( unidad, intervalo ) = GASMN ( unidad , altura_neta )
                                                !Calcular coeficientes de la funcion de generacion para despacho usando altura promedio
                                                call Coef_gen_hidro_lineal ( unidad, intervalo, 2, planta, altura_neta, volumen_util )  
                                                !Revisar si los limites minimos y maximos de generacion definidos estan dentro de los 
                                                !limites de los turbinados minimos y maximos
                                                !Evaluar generacion minima y maxima dependiendo del gasto minimo, maximo y altura CUADRATICO                                    
                                                g_min = C_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) ** 2 + &
                                                                            B_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) + &
                                                                            A_hatd ( unidad, intervalo )
                                                g_max = C_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) ** 2 + &
                                                                            B_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) + &
                                                                            A_hatd ( unidad, intervalo ) 
                                                !Revisar si hay violacion de limites de generacion por turbinados min. y max. y definidos
                                                !Ambos limites por debajo
                                                if ( PotMinUniH ( unidad, intervalo ) .lt. g_min .and. &
                                                     PotMaxUniH ( unidad, intervalo ) .lt. g_min  .and. &
                                                    DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                                    PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                                    if ( PrevUnit ( unidad ) .eq. 1 ) then
                                                        
                                                        ibanbit = 1
                                                        ierror = 0
                                                    
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_min                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_min   
                                                        PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )
                                            
                                                        SemBandera ( 8 ) = 1
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                        ibanbit = 0
                                                        ierror = 0
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )   
                                                    else
                                                        ibanbit = 2
                                                        ierror = 0
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_min                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_min   
                                                        PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )
                                            
                                                        SemBandera ( 8 ) = 1
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    end if 
                                                end if
                                                
                                                !Ambos limites por arriba
                                                if ( PotMinUniH ( unidad, intervalo ) .gt. g_max .and. &
                                                    PotMaxUniH ( unidad, intervalo ) .gt. g_max  .and. &
                                                    DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                                    PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                                    if ( PrevUnit ( unidad ) .eq. 1 ) then
                                                        ibanbit = 1
                                                        ierror = 0
                                                    
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_max                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                                        PotMaxUniH ( unidad, intervalo ) = g_max
                                            
                                                        SemBandera ( 8 ) = 1
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                        ibanbit = 0
                                                        ierror = 0
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )                       
                                                    else
                                                        ibanbit = 2
                                                        ierror = 0
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_max                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                                        PotMaxUniH ( unidad, intervalo ) = g_max
                                            
                                                        SemBandera ( 8 ) = 1
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                    end if
                                                end if
                                                
                                                !Potencia minima por debajo
                                                if ( PotMinUniH ( unidad, intervalo ) .lt. g_min .and. DispoUH ( unidad, intervalo ) .eq. 1) then
                                                    !Redefinir potencia maxima
                                                    PotMinUniH ( unidad, intervalo ) = g_min
                                                    SemBandera ( 8 ) = 1
                                                end if
                                                
                                                !Potencia maxima por arriba
                                                if ( PotMaxUniH ( unidad, intervalo ) .gt. g_max.and. DispoUH ( unidad, intervalo ) .eq. 1 ) then                                                
                                                    !Redefinir potencia maxima
                                                    PotMaxUniH ( unidad, intervalo ) = g_max
                                                    SemBandera ( 8 ) = 1
                                                end if
                                                !Redefinir los gastos minimos y maximos dependiendo de las generaciones definidas por el usuario ya validadas
                                                if ( PotMaxUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) .gt. 0.0 ) then
                                                    qmxgastodes ( unidad, intervalo ) = ( PotMaxUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                                end if
                                                if ( PotMinUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) .gt. 0.0 ) then
                                                    qmngastodes ( unidad, intervalo ) = ( PotMinUniH ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                                end if
                                                !imprimir el nombre de la unidad
                                                !WRITE ( UNLG12_1 , 124 ) unidad, nombunih ( unidad ), PotMaxUniH ( unidad, intervalo ), PotMinUniH ( unidad, intervalo ), qmxgastodes ( unidad, intervalo ) * MValor, qmngastodes ( unidad, intervalo ) * MValor, Alt_net_desmax ( viad, intervalo ), Alt_net_desmin ( viad, intervalo )
                                                WRITE ( UNLG12_1 , 124 ) unidad, nombunih ( unidad ), PotMaxUniH ( unidad, intervalo ), PotMinUniH ( unidad, intervalo ), qmxgastodes ( unidad, intervalo ) * MValor, qmngastodes ( unidad, intervalo ) * MValor, altura_neta, altura_neta                                                
                                                !Si la unidad es disponible calcular gastos y energia respentando gastos minimos y maximos
                                                !con las generaciones minimas y maximas definidas ya validadas
                                                if ( DispoUH ( unidad, intervalo ) .eq. 1 ) then
                                                    !si la unidad es asignable coordinable
                                                    if ( AsignUH ( unidad, intervalo ) .eq. 1 .and. CoordUH ( unidad, intervalo ) .eq. 1 ) then
                                                        !Calcular gastos y energia respentando gastos minimos y maximos con
                                                        !generaciones minimas y maximas definidas ya validadas
                                                        q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + qmxgastodes ( unidad, intervalo ) * MValor
                                                        !q_min_embv ( embalse, intervalo ) = q_min_embv ( embalse, intervalo ) + ( qmxgastodes ( unidad, intervalo ) * MValor ) / 2.0
                                                        !Gasto maximo de la unidad hidro por DAC
                                                        qmxdac ( unidad, intervalo ) = qmxgastodes ( unidad, intervalo )
                                                        !Gasto minimo de la unidad hidro por DAC
                                                        qmndac ( unidad, intervalo ) = 0.0
                                                        e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                                        emxdac ( unidad, intervalo ) = Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                                        emndac ( unidad, intervalo ) = 0.0
                                                    end if      
                                                    !si la unidad es no asignable coordinable
                                                    if ( AsignUH ( unidad, intervalo ) .eq. 0 .and. CoordUH ( unidad, intervalo ) .eq. 1 ) then
                                                        q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + qmxgastodes ( unidad, intervalo ) * MValor
                                                        q_min_embv ( embalse, intervalo ) = q_min_embv ( embalse, intervalo ) + qmngastodes ( unidad, intervalo ) * MValor
                                                        !Gasto maximo de la unidad hidro por DAC
                                                        qmxdac ( unidad, intervalo ) = qmxgastodes ( unidad, intervalo )
                                                        !Gasto minimo de la unidad hidro por DAC
                                                        qmndac ( unidad, intervalo ) = qmngastodes ( unidad, intervalo )
                                                        e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                                        e_min_embv ( embalse, intervalo ) = e_min_embv ( embalse, intervalo ) + Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) )
                                                        emxdac ( unidad, intervalo ) = Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) )
                                                        emndac ( unidad, intervalo ) = Base * ( CIndGLH ( unidad, intervalo ) + CLinGLH ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) )
                                                    end if         
                                                    !si la unidad es asignable no coordinable
                                                    if ( AsignUH ( unidad, intervalo ) .eq. 1 .and. CoordUH ( unidad, intervalo ) .eq. 0 ) then
                                                        !calcular el gasto a la generacion solicitada LINEALIZADA
                                                        gasto = ( potfijah ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                                        q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + gasto * MValor
                                                        !Gasto maximo de la unidad hidro por DAC
                                                        qmxdac ( unidad, intervalo ) = gasto
                                                        !Gasto minimo de la unidad hidro por DAC
                                                        qmndac ( unidad, intervalo ) = 0.0
                                                        e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + potfijah ( unidad, intervalo )
                                                        emxdac ( unidad, intervalo ) = potfijah ( unidad, intervalo )
                                                        emndac ( unidad, intervalo ) = 0.0
                                                    end if      
                                                    !si la unidad es no asignable no coordinable
                                                    if ( AsignUH ( unidad, intervalo ) .eq. 0 .and. CoordUH ( unidad, intervalo ) .eq. 0 ) then                                
                                                        !calcular el gasto a la generacion solicitada LINEALIZADA
                                                        gasto = ( potfijah ( unidad, intervalo ) / Base - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                                        q_max_embv ( embalse, intervalo ) = q_max_embv ( embalse, intervalo ) + gasto * MValor
                                                        q_min_embv ( embalse, intervalo ) = q_min_embv ( embalse, intervalo ) + gasto * MValor
                                                        !Gasto maximo de la unidad hidro por DAC
                                                        qmxdac ( unidad, intervalo ) = gasto
                                                        !Gasto minimo de la unidad hidro por DAC
                                                        qmndac ( unidad, intervalo ) = gasto
                                                        e_max_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo ) + potfijah ( unidad, intervalo )
                                                        e_min_embv ( embalse, intervalo ) = e_min_embv ( embalse, intervalo ) + potfijah ( unidad, intervalo )
                                                        emxdac ( unidad, intervalo ) = potfijah ( unidad, intervalo )
                                                        emndac ( unidad, intervalo ) = potfijah ( unidad, intervalo )
                                                    end if                                  
                                                else
                                                    !Gasto maximo de la unidad hidro por DAC
                                                    qmxdac ( unidad, intervalo ) = 0.0
                                                    !Gasto minimo de la unidad hidro por DAC
                                                    qmndac ( unidad, intervalo ) = 0.0
                                                    emxdac ( unidad, intervalo ) = 0.0
                                                    emndac ( unidad, intervalo ) = 0.0
                                                end if
                                            end do                                
                                        end do
                                        !Turbinado maximo y minimo del embalse por intervalo
                                        turb_max = q_max_embv ( embalse, intervalo )
                                        turb_min = q_min_embv ( embalse, intervalo )
                                        
                                        vol_util_max ( embalse, intervalo ) = vol_util_max ( embalse, intervalo - 1 ) + Apomax - turb_min - VERTDO ( viad, intervalo )
                                        vol_util_min ( embalse, intervalo ) = vol_util_min ( embalse, intervalo - 1 ) + Apomin - turb_max - VERTDO ( viad, intervalo )
                                        
                                        if ( vol_util_min ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                            vol_util_min ( embalse, intervalo ) = vol_util_min ( embalse, intervalo - 1 ) + Apomax - turb_min - VERTDO ( viad, intervalo )
                                                                                        
                                            !turb_max = q_min_embv ( embalse, intervalo )
                                            !e_max_embv ( embalse, intervalo ) = e_min_embv ( embalse, intervalo )
                                            
                                            if ( vol_util_min ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                                !if ( PrevEmb .ne. embalse ) then
                                                if ( pantallazo ( embalse ) .ne. .true. ) then
                                                    !PrevEmb = embalse
                                                    pantallazo ( embalse ) = .true.
                                                    PrevInt = intervalo
                                                    !Infactibilidad hidraulica violacion vol min.                                    
                                                    ibanbit = 1
                                                    ierror = 0
                                                                
                                                    call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                                
                                                    Call FechaEjecucion (fecha_Ej)
                                                    bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                            
                                                    write ( aaux, 5100 ) embalse
                                                    write ( aaux_1, 5101 ) nomemb ( embalse )
                                                    write ( aaux_2, 5100 ) intervaloo 
                                                    write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_min ( embalse, intervalo ) ) / 1000.0
                                                    write ( aaux_5, 5101 ) dia_text
                                                                                
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                          
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                    ibanbit = 0
                                                    ierror = 0
                                                    Call FechaEjecucion (fecha_Ej)
                                                    bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )            
                                                else
                                                    if ( PrevInt .ne. intervalo ) then
                                                        PrevInt = intervalo
                                                        !Infactibilidad hidraulica violacion vol min.                                    
                                                        ibanbit = 2
                                                        ierror = 0
                                                                
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                                
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                            
                                                        write ( aaux, 5100 ) embalse
                                                        write ( aaux_1, 5101 ) nomemb ( embalse )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_min ( embalse, intervalo ) ) / 1000.0
                                                        write ( aaux_5, 5101 ) dia_text
                                                                                
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                          
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    end if                                                    
                                                end if
                                            end if
                                        end if
                                        
                                        if ( vol_util_max ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                            !if ( PrevEmb .ne. embalse ) then
                                            if ( pantallazo ( embalse ) .ne. .true. ) then
                                                !PrevEmb = embalse
                                                pantallazo ( embalse ) = .true.
                                                PrevInt = intervalo
                                                !Infactibilidad hidraulica violacion vol min.                                    
                                                ibanbit = 1
                                                ierror = 0
                                                                
                                                call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                                
                                                Call FechaEjecucion (fecha_Ej)
                                                bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                            
                                                write ( aaux, 5100 ) embalse
                                                write ( aaux_1, 5101 ) nomemb ( embalse )
                                                write ( aaux_2, 5100 ) intervaloo 
                                                write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_max ( embalse, intervalo ) ) / 1000.0
                                                write ( aaux_5, 5101 ) dia_text
                                                                                
                                                Call FechaEjecucion (fecha_Ej)
                                                BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                          
                                                Call FechaEjecucion (fecha_Ej)
                                                BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                
                                                ibanbit = 0
                                                ierror = 0
                                                Call FechaEjecucion (fecha_Ej)
                                                bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )           
                                            else
                                                if ( PrevInt .ne. intervalo ) then
                                                    PrevInt = intervalo
                                                    !Infactibilidad hidraulica violacion vol min.                                    
                                                    ibanbit = 2
                                                    ierror = 0
                                                                
                                                    call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                                
                                                    Call FechaEjecucion (fecha_Ej)
                                                    bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE DEFICIT'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                            
                                                    write ( aaux, 5100 ) embalse
                                                    write ( aaux_1, 5101 ) nomemb ( embalse )
                                                    write ( aaux_2, 5100 ) intervaloo 
                                                    write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_max ( embalse, intervalo ) ) / 1000.0
                                                    write ( aaux_5, 5101 ) dia_text
                                                                                
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                          
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                end if                                                
                                            end if
                                        end if
                                        
                                        if ( vol_util_max ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                            vol_util_max ( embalse, intervalo ) = vol_util_max ( embalse, intervalo - 1 ) + Apomin - turb_max - VERTDO ( viad, intervalo )
                                            
                                            !turb_min = q_max_embv ( embalse, intervalo )
                                            !e_min_embv ( embalse, intervalo ) = e_max_embv ( embalse, intervalo )
                                            
                                            if ( vol_util_max ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                                !if ( PrevEmb .ne. embalse ) then
                                                if ( pantallazo ( embalse ) .ne. .true. ) then
                                                    !PrevEmb = embalse
                                                    pantallazo ( embalse ) = .true.
                                                    PrevInt = intervalo
                                                    !Infactibilidad hidraulica violacion vol max.                                    
                                                    ibanbit = 1
                                                    ierror = 0
                                                                    
                                                    call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                                    
                                                    Call FechaEjecucion (fecha_Ej)
                                                    bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                                
                                                    write ( aaux, 5100 ) embalse
                                                    write ( aaux_1, 5101 ) nomemb ( embalse )
                                                    write ( aaux_2, 5100 ) intervaloo 
                                                    write ( aaux_3, 5102 ) ( vol_util_max ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                                    write ( aaux_5, 5101 ) dia_text
                                                                                    
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                              
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                    ibanbit = 0
                                                    ierror = 0
                                                    Call FechaEjecucion (fecha_Ej)
                                                    bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )            
                                                else
                                                    if ( PrevInt .ne. intervalo ) then
                                                        PrevInt = intervalo
                                                        !Infactibilidad hidraulica violacion vol max.                                    
                                                        ibanbit = 2
                                                        ierror = 0
                                                                    
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                                
                                                        write ( aaux, 5100 ) embalse
                                                        write ( aaux_1, 5101 ) nomemb ( embalse )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) ( vol_util_max ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                                        write ( aaux_5, 5101 ) dia_text
                                                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                              
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    end if                                                    
                                                end if
                                            end if
                                        end if
                                        
                                        if ( vol_util_min ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                            !if ( PrevEmb .ne. embalse ) then
                                            if ( pantallazo ( embalse ) .ne. .true. ) then
                                                !PrevEmb = embalse
                                                pantallazo ( embalse ) = .true.
                                                PrevInt = intervalo
                                                !Infactibilidad hidraulica violacion vol max.                                    
                                                ibanbit = 1
                                                ierror = 0
                                                                   
                                                call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                                    
                                                Call FechaEjecucion (fecha_Ej)
                                                bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                                
                                                write ( aaux, 5100 ) embalse
                                                write ( aaux_1, 5101 ) nomemb ( embalse )
                                                write ( aaux_2, 5100 ) intervaloo 
                                                write ( aaux_3, 5102 ) ( vol_util_min ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                                write ( aaux_5, 5101 ) dia_text
                                                                                    
                                                Call FechaEjecucion (fecha_Ej)
                                                BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                             
                                                Call FechaEjecucion (fecha_Ej)
                                                BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                
                                                ibanbit = 0
                                                ierror = 0
                                                Call FechaEjecucion (fecha_Ej)
                                                bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )           
                                            else
                                                if ( PrevInt .ne. intervalo ) then
                                                    PrevInt = intervalo
                                                    !Infactibilidad hidraulica violacion vol max.                                    
                                                    ibanbit = 2
                                                    ierror = 0
                                                                   
                                                    call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                                    
                                                    Call FechaEjecucion (fecha_Ej)
                                                    bmensaje = fecha_Ej//' HIDRO102 INFACTIBILIDAD HIDRAULICA: POSIBLE EXCEDENTE'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                                
                                                    write ( aaux, 5100 ) embalse
                                                    write ( aaux_1, 5101 ) nomemb ( embalse )
                                                    write ( aaux_2, 5100 ) intervaloo 
                                                    write ( aaux_3, 5102 ) ( vol_util_min ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                                    write ( aaux_5, 5101 ) dia_text
                                                                                    
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                             
                                                    Call FechaEjecucion (fecha_Ej)
                                                    BMensaje = fecha_Ej//' HIDRO102 NO. VIOLACION: '//aaux_3//' MMC'
                                                    write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                end if                                                
                                            end if
                                        end if            
                                                                               
                                        turb_maxT ( embalse ) = turb_maxT ( embalse ) + turb_max
                                        turb_minT ( embalse ) = turb_minT ( embalse ) + turb_min
                                        
                                        a_tra_max ( intervalo + TiViAgu ( viad ), viad ) = turb_max + VERTDO ( viad, intervalo )
                                        a_tra_min ( intervalo + TiViAgu ( viad ), viad ) = turb_min + VERTDO ( viad, intervalo )
                                        
                                        ener_max = e_max_embv ( embalse, intervalo )
                                        ener_min = e_min_embv ( embalse, intervalo )
                                        
                                        ener_maxT ( embalse ) = ener_maxT ( embalse ) + ener_max
                                        ener_minT ( embalse ) = ener_minT ( embalse ) + ener_min   
                                        
                                    end if
                                    
                                    WRITE ( 520, 101 ) '                           |   |   Aportacion Min:', Apomin / 1000.00, 'MMC'                         
                                    WRITE ( 520, 101 ) '                           |   |   Aportacion Max:', Apomax / 1000.00, 'MMC'
                                    write ( 520, 103 ) 'Vol.U.MA:', vol_util_max ( embalse, intervalo ) / 1000.00, 'MMC','     /\   |'
                                    write ( 520, 103 ) 'Vol.U.Mi:', vol_util_min ( embalse, intervalo ) / 1000.00, 'MMC','    /  \  /'
                                    write ( 520, 103 ) 'Turb Max:', turb_max / 1000.0, 'MMC','   /    \/ '
                                    write ( 520, 104 ) 'Turb Min:', turb_min / 1000.0, 'MMC','  /', nomemb ( embalse ), embalse, '\'                                    
                                    write ( 520, 105 ) 'Ener Max:', ener_max / 1000.00, 'GWH', ' /        \'
                                    write ( 520, 106 ) 'Ener Min:', ener_min / 1000.00, 'GWH', '---------- )', 'Vertido programado:', vertdo ( DNVIOU ( embalse ), intervalo ) / 1000.00, 'MMC' 
                                    write ( 520, 107 ) '                           |    /'
                                    !Imprime via divergente
                                    write ( 520, 109 ) NOMVIA ( embalse ), '                      |'
                                    write ( 520, 108 ) '                           |'
                                    write ( 520, 116 ) 'Gasto Min:', ( turb_min + VERTDO ( viad, intervalo ) ) / 1000.00, 'MMC', ' |'	
                                    write ( 520, 116 ) 'Gasto Max:', ( turb_max + VERTDO ( viad, intervalo ) ) / 1000.00, 'MMC', ' |'
                                    write ( 520, 118 ) 'T.V.A:', TiViAgu ( viad ), 'hr', '     |'
                                    localidad_3 = localidad_3 + 1 
                                    exit                    
                                end if
                            end do
                        end if                    
                    end if 
                end do
            end do
        end do
        ninter = ninter + intdia ( dia )
    end do
end do

WRITE ( 520 , * ) ' '
WRITE ( 520 , * ) '************************************************************************'    
write ( 520, * ) 'MINIMOS Y MAXIMOS DE POLITICAS DE OPERACION'
WRITE ( 520 , * ) '************************************************************************'
WRITE ( 520 , * ) ' '   
!Hacer para todos los embalses
do embalse = 1, NumEmbalses
    !Revisar pol�ticas de operaci�n
    !Cota final fija
    if ( PoliEmb ( embalse ) .eq. 3 ) then
        if ( WEmbFin ( embalse ) .ge. vol_util_min ( embalse, ntintr ) .and. &
             WEmbFin ( embalse ) .le. vol_util_max ( embalse, ntintr ) ) then
            continue
        else
            !Violacion Politica Cota Final Fija
            if ( WEmbFin ( embalse ) .lt. vol_util_min ( embalse, ntintr ) ) then
                relajado = namino ( embalse ) + ALUTIL ( embalse , vol_util_min ( embalse, ntintr ) ) 
                solicitado = WEmbFin ( embalse )
                !WEmbFin ( embalse ) = vol_util_min ( embalse, ntintr )
            end if
            if ( WEmbFin ( embalse ) .gt. vol_util_max ( embalse, ntintr ) ) then
                relajado = namino ( embalse ) + ALUTIL ( embalse , vol_util_max ( embalse, ntintr ) ) 
                solicitado = WEmbFin ( embalse )
                !WEmbFin ( embalse ) = vol_util_max ( embalse, ntintr )
            end if
                        
            ibanbit = 1
            ierror = 0
                            
            !call DiaHoraEP ( intervalo, dia_text, intervaloo )
                            
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' HIDRO103 POSIBLE VIOLACION POLITICA 3 CFF'
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                        
            write ( aaux, 5100 ) embalse
            write ( aaux_1, 5101 ) nomemb ( embalse )
            write ( aaux_3, 5102 ) namino ( embalse ) + ALUTIL ( embalse , solicitado ) 
            write ( aaux_4, 5102 ) relajado
                                            
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO103 No. '//aaux//' NOMBRE: '//aaux_1 
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                       
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO103 SOLICITADO: '//aaux_3//'m.'//' CALCULADO: '//aaux_4//'m.'
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
        end if
    end if
    !Generacion fija
    if ( PoliEmb ( embalse ) .eq. 8 ) then
        if ( EfijEmb ( embalse ) .ge. ener_minT ( embalse ) .and. &
             EfijEmb ( embalse ) .le. ener_maxT ( embalse ) ) then
            continue
        else
            !Violacion Politica Energia fija
            if ( EfijEmb ( embalse ) .lt. ener_minT ( embalse ) ) then
                relajado = ener_minT ( embalse )
                solicitado = EfijEmb ( embalse )
                !EfijEmb ( embalse ) = ener_minT ( embalse )
            end if
            if ( EfijEmb ( embalse ) .gt. ener_maxT ( embalse ) ) then
                relajado = ener_maxT ( embalse )
                solicitado = EfijEmb ( embalse )
                !EfijEmb ( embalse ) = ener_maxT ( embalse )
            end if
                        
            ibanbit = 1
            ierror = 0
                            
            !call DiaHoraEP ( intervalo, dia_text, intervaloo )
                            
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' HIDRO103 POSIBLE VIOLACION POLITICA 4 EF'
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                        
            write ( aaux, 5100 ) embalse
            write ( aaux_1, 5101 ) nomemb ( embalse )
            write ( aaux_3, 5102 ) solicitado / 1000.0
            write ( aaux_4, 5102 ) relajado / 1000.0
                                            
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO103 No. '//aaux//' NOMBRE: '//aaux_1 
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                       
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO103 SOLICITADO: '//aaux_3//'GWH'//' CALCULADO: '//aaux_4//'GWH'
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
        end if
    end if
    !Volumen a turbinar fijo
    if ( PoliEmb ( embalse ) .eq. 5 ) then
        if ( TFijEmb ( embalse ) .ge. turb_minT ( embalse ) .and. &
             TFijEmb ( embalse ) .le. turb_maxT ( embalse ) ) then
            continue
        else
            !Violacion Politica Volumen a turbinar fijo
            if ( TFijEmb ( embalse ).lt. turb_minT ( embalse ) ) then
                relajado = turb_minT ( embalse )
                solicitado = TFijEmb ( embalse )
                !TFijEmb ( embalse ) = turb_minT ( embalse )
            end if
            if ( TFijEmb ( embalse ) .gt. turb_maxT ( embalse ) ) then
                relajado = turb_maxT ( embalse )
                solicitado = TFijEmb ( embalse )
                !TFijEmb ( embalse ) = turb_maxT ( embalse )
            end if
                        
            ibanbit = 1
            ierror = 0
                            
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' HIDRO103 POSIBLE VIOLACION POLITICA 5 VTF'
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                        
            write ( aaux, 5100 ) embalse
            write ( aaux_1, 5101 ) nomemb ( embalse )
            write ( aaux_3, 5102 ) solicitado  / 1000.0
            write ( aaux_4, 5102 ) relajado / 1000.0
                                            
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO103 No. '//aaux//' NOMBRE: '//aaux_1 
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                       
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO103 SOLICITADO: '//aaux_3//'MMC'//' CALCULADO: '//aaux_4//'MMC'
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        end if
    end if
!Escribe validaciones de politicas por embalse
    WRITE ( 520 , * ) '--------------------------------------------------------------------------'
    WRITE ( 520 , 100 ) ' EMBALSE NO:', embalse, ' NOMBRE:', NOMEMB ( embalse ) 
    WRITE ( 520 , * ) '--------------------------------------------------------------------------'
    WRITE ( 520 , * ) '      Politica                Min.    Max.'
    write ( 520, 121 ) 'Maxima Extraccion  (MMC): 1', vol_util_min ( embalse, ntintr ) / 1000.0
    write ( 520, 121 ) 'Minima Extraccion  (MMC): 2', vol_util_max ( embalse, ntintr ) / 1000.0
    write ( 520, 121 ) 'Cota Fija Final      (m): 3', namino ( embalse ) + ALUTIL ( embalse , vol_util_min ( embalse, ntintr ) ), namino ( embalse ) + ALUTIL ( embalse , vol_util_max ( embalse, ntintr ) )
    write ( 520, 121 ) 'Generacion Fija    (GWH): 4', ener_minT ( embalse ) / 1000.0, ener_maxT ( embalse ) / 1000.0 
    write ( 520, 121 ) 'Vol. Turbinar Fijo (MMC): 5', turb_minT ( embalse ) / 1000.0, turb_maxT ( embalse ) / 1000.0
    write ( 520, 121 ) ''
end do


close ( unit = 520 )

OPEN ( UNIT = 521, FILE = RUT_RES//'Turb_Ener_Min_Max_Emb.res', IOSTAT = error_h, STATUS='UNKNOWN', RECORDSIZE = 3024 )

!Calcula gastos y energias minimas y maximas respetando DAC y generaciones fijas
       
WRITE ( 521 , * ) ' '     
WRITE ( 521 , * ) '///////////////////////////////////////////////////////////////////////////////////'     
WRITE ( 521 , * ) 'TURBINADOS Y ENERGIA MINIMOS Y MAXIMOS DE LOS EMBALSES POR DAC'   
WRITE ( 521 , * ) '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'              

!Hacer para todos los dias del escenario de planeacion
do dia = 1, durdia

    write ( aaux1, 5103 ) dia
    dia_text = 'D'//aaux1

    !Hacer para todas las cuencas
    do cuenca = 1,nmcuen  
        WRITE ( 521 , * ) ' ' 
        WRITE ( 521 , * ) '***********************************************************************************'
        !Imprimir nombre y numero de la cuenca
        WRITE ( 521 , 100 ) ' CUENCA NO:', cuenca, ' NOMBRE:', nomval ( cuenca )    
        WRITE ( 521 , * ) '***********************************************************************************'
        !Hacer para todos los emblases de la cuenca
        do localidad_2 = 0, nmembc ( cuenca ) - 1
            
            q_max_emb = 0.0
            q_min_emb = 0.0
                    
            e_max_emb = 0.0
            e_min_emb = 0.0
            
            embalse = dnemva ( cuenca ) + localidad_2
            !Hacer par a todos los intervalos
            do intervalo = dia * 24 - 23 , dia * 24 
                q_max_emb = q_max_emb + q_max_embv ( embalse, intervalo )
                q_min_emb = q_min_emb + q_min_embv ( embalse, intervalo )
                e_max_emb = e_max_emb + e_max_embv ( embalse, intervalo )
                e_min_emb = e_min_emb + e_min_embv ( embalse, intervalo )
            end do
            
            !Imprimir nombre y numero del embalse
            WRITE ( 521 , * ) ' '     
            WRITE ( 521 , * ) '--------------------------------------------------------------------------'
            WRITE ( 521 , 100 ) ' EMBALSE NO:', embalse, ' NOMBRE:', NOMEMB ( embalse ) 
            WRITE ( 521 , * ) '--------------------------------------------------------------------------'
            
            write ( 521, * ) ''
            write ( 521, 114 ) 'Unidad/Planta', '----------------------- Dia', dia_text, '------------------------'
            write ( 521, 111 ) '              ', '-------- Vol. mMC --------  ------- Energia MWh ------'
            write ( 521, 110) '             ', '      Maximo        Minimo', '      Maximo        Minimo' 
            write ( 521, * ) ''           
            
            !Hacer para todas las vias divergentes
            do localidad_3 = 0, nmviasd ( embalse ) - 1 
                via = DNVIOU ( embalse ) + localidad_3 
                !Hacer para todas las plantas que descargan sobre la v�a
                do localidad = dnphvi ( via ), dnphvi ( via ) + nphvi ( via ) - 1
                    q_max_pla = 0.0
                    q_min_pla = 0.0
                    
                    e_max_pla = 0.0
                    e_min_pla = 0.0
                    
                    planta = plantas_x_via ( localidad )                
                    !Hacer para todas las unidades de la planta
                    do localidad_1 = 0, NOUN ( planta ) - 1
                        q_max_uni = 0.0
                        q_min_uni = 0.0
                        
                        e_max_uni = 0.0
                        e_min_uni = 0.0
                        
                        unidad = DNUNPH ( planta ) + localidad_1
                        
                        !Hacer par a todos los intervalos
                        do intervalo = dia * 24 - 23 , dia * 24 
                            q_max_uni = q_max_uni + qmxdac ( unidad, intervalo ) * MValor
                            q_min_uni = q_min_uni + qmndac ( unidad, intervalo ) * MValor
                            e_max_uni = e_max_uni + emxdac ( unidad, intervalo )
                            e_min_uni = e_min_uni + emndac ( unidad, intervalo )
                        end do
                        q_max_pla = q_max_pla + q_max_uni
                        q_min_pla = q_min_pla + q_min_uni
                        e_max_pla = e_max_pla + e_max_uni
                        e_min_pla = e_min_pla + e_min_uni
                        write ( 521, 113 ) unidad, nombunih ( unidad ), q_max_uni, q_min_uni, e_max_uni, e_min_uni
                    end do
                    write ( 521, 112) '              ', '------------  ------------', '------------  ------------'
                    write ( 521, 113 ) planta, NOMPLAH ( planta ), q_max_pla, q_min_pla, e_max_pla, e_min_pla
                    write ( 521, 112) ' '
                end do
                write ( 521, 112 ) 'TOT. EMBALSE:', '------------  ------------', '------------  ------------'  
                write ( 521, 113 ) embalse, '     ', q_max_emb, q_min_emb, e_max_emb, e_min_emb
            end do
        end do
    end do       


end do

close ( unit = 521 )

!PotMinUniH = PotMinUniH / Base
!PotMaxUniH = PotMaxUniH / Base


100 format ( a12, x, i3, 2( a11, x ) )
110 format ( a13, 2x, a26, 2x, a26, 2x )
111 format ( a14, x, a54, 2x ) 
112 format ( a14, x,  a26, 2x, a26, 2x )  
113 format ( x, i3, a12, f12.2, 2x, f12.2, 2x, f12.2, 2x, f12.2, 2x )
114 format ( a13, 2x, a27, x, a2, x, a24, 2x )   

101 format ( a51, x, F7.2, x, a3  )
102 format ( 2x, i2, x, a12, 9x, 5(f9.2, 5x) ) 
122 format ( 2x, i2, x, a12, x, a87 ) 
123 format ( a11, x, i3 )
124 format ( 2x, i2, x, a12, 2x, 6(f9.2, 5x) ) 

103 format ( a9, x, f8.2, x, a3, a11 ) 
104 format ( a9, x, f8.2, x, a3, a3, a3, x, i2, a1)
105 format ( a9, x, f8.2, x, a3, a11)
106 format ( a9, x, f8.2, x, a3, x, a12, x, a19, x, f9.5, x, a3)
107 format (  a34 )
108 format (  a29 )
109 format (  a5, a24 )
115 format (  a4, x, f8.2, x, a1,  a11 )
116 format (  a10, x, f8.2, x, a3,  a6 )
117 format ( a72 )
118 format ( a6, x, i2, x, a2, a17)
119 format ( a32, x, a3, x, a2, x, a32)   
120 format ( a32, x, a2, x, i3, x, a32)  
121 format ( a27, x, f8.2, x, f8.2 )

5100 FORMAT (I3)
5101 format (a12)
5102 format (F7.2)
5103 FORMAT (I1)

end subroutine valida_politica

!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!      P R O G R A M A S   D E   A P L I C A C I O N   A V A N Z A D A    *
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
!      Proposito: Imprime los datos del sistema hidrol�gico               *
!                                                                         *
!                                                                         *
!                                                                         *
!     Nombre y fecha de implementaci�n:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Junio 2013                            *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!        Jose Luis Ceciliano Meza   Octubre 2016                          *
!**************************************************************************
!
!
SUBROUTINE DiaHoraEP ( intervalo, dia_text, intervaloo )

use ParAUHE, only: intdia, durdia

Implicit none

integer dia, ninter, intervalo, intervaloo
character*2 dia_text
character*1 aaux1

ninter = 0
                                            
do dia = 1, durdia
    intervaloo = intervalo - ninter
    ninter = ninter + intdia ( dia )
    if ( intervalo .le. ninter ) then
        write ( aaux1, 5100 ) dia
        dia_text = 'D'//aaux1
        exit
    end if
enddo

5100 FORMAT (I1)
return

end subroutine DiaHoraEP
    
    

!**************************************************************************
!******************************* CHAU *************************************
!**************************************************************************
!                                                                         *
!      P R O G R A M A S   D E   A P L I C A C I O N   A V A N Z A D A    *
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
!      Proposito: Reporta los resultados en los embalses por hora         *
!                 considerando la funcion de                              *
!                 de generacion hidro cuadratica                          *                                             
!                                                                         *
!                                                                         *
!                                                                         *
!     Nombre y fecha de implementaci�n:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Agosto 2013                           *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
!
!
SUBROUTINE res_agua_quad ( volumhR, cargahR, UniOnHid, a_tra_res, gplah, qplah, aa4, LinVSCuad )

USE ParAuHeHidro
use ParAUHE, only: ntintr, durdia, BMensaje, Numembalses, &
                   NumUniHid, intdia, nombunih
                   
Use parGloRed, only: basmva

Implicit none

integer * 4 cuenca, embalse, via, planta, localidad, unidad, localidad_1, &
            localidad_2, localidad_3, intervalo, dia, error_h, consecutivo, &
            viad, viac, ierror, intervaloo, downstream, contador, localidad_4, &
            localidad_5, unitcount, localidad_7, i, intini, UniOnHid ( nmxpla, maxint ), & 
            int_hexa, LinVSCuad

integer ibanbit
            
logical entro 
            
real*8 gasto, q_max_pla, q_min_pla, q_max_uni, q_min_uni, &
       e_max_pla, e_min_pla, e_max_uni, e_min_uni, &
       q_max_emb, q_min_emb, e_max_emb, e_min_emb, ALUTIL, relajado, solicitado, &
       turb_maxT ( nmxemb ), turb_minT ( nmxemb ), ener_maxT ( nmxemb ), ener_minT ( nmxemb ), &
       nivdownstream, RIVDES, perdidas, reltah, RGASMX, GASMN,  &
       g_res ( maxuh, MAXINT ), gasto_1, gasto_2, q_res_embv ( nmxemb, maxint ), e_res_embv ( nmxemb, maxint ), &
       turb_res, turb_resT ( nmxemb ), a_tra_res ( maxint + 25, nmxvia ), Aporta, ener_res, &
       ener_resT ( nmxemb ), radicando, e_embv ( nmxemb, maxint ), ener_T ( nmxemb ), hexa, dummy, altura ( nmxemb, maxint ), &
       volumen ( nmxemb, maxint ), gplah ( nmxpla, maxint ), qplah ( nmxpla, maxint ), aa4 ( nmxemb, maxdia, 7 ), &
       A_hatd_copy ( maxuh, MAXINT ), B_hatd_copy( maxuh, MAXINT ), C_hatd_copy( maxuh, MAXINT ), turb_T ( nmxemb ), &  
       q_res_embvl ( nmxemb, maxint ), turb_resl, ener_resl, e_res_embvl ( nmxemb, maxint )
       
character*2 dia_text

CHARACTER fecha_Ej*19

character*6 aaux, aaux_1, aaux_2, aaux_5
character*8 aaux_3, aaux_4

real*8   volumhR ( maxuh, MAXINT ), cargahR ( maxuh, MAXINT )
character*1 aaux1
integer     ninter


!Calcula la validacion de las politicas de operacion por intervalo
OPEN ( UNIT = 522, FILE = RUT_RES//'Result_Embalse_quad.res', IOSTAT = error_h, STATUS='UNKNOWN', RECORDSIZE = 3024 )
OPEN ( UNIT = 524, FILE = RUT_RES//'LinealVsCuadratico.res', IOSTAT = error_h, STATUS='UNKNOWN', RECORDSIZE = 3024 )

WRITE ( 522 , * ) '************************************************************************'     
WRITE ( 522 , * ) 'BALANCE DEL AGUA HORARIO (RESULTADOS CUADRATICOS)'   
WRITE ( 522 , * ) '************************************************************************'       

!Vector que contiene el numero de la via convergente al embalse cuando dicho numero es diferente de cero
WRITE ( 522 , * ) '************************************************************************'    
write ( 522, * ) 'POLITICAS DE OPERACION'
WRITE ( 522 , * ) '************************************************************************'    
write ( 522, * ) 'Maxima Extraccion     : 1'
write ( 522, * ) 'Minima Extraccion     : 2'
write ( 522, * ) 'Cota Fija Final      m: 3'
write ( 522, * ) 'Energia Economica  GWH: 4'
write ( 522, * ) 'Vol. Maximo a Turb MMC: 5'
write ( 522, * ) ''
WRITE ( 522 , * ) '************************************************************************'    
write ( 522, * ) 'Aportacion = Agua en Transito Escenario Anterior + Aportacion Natural' 
write ( 522, * ) '           - Otros Usos del Agua + Turbinado Embalse Aguas Arriba'
write ( 522, * ) '             Considerando Tiempo de Viaje del Agua' 
WRITE ( 522 , * ) '************************************************************************'    
WRITE ( 522 , * ) '' 

if ( LinVSCuad .eq. 1 ) then
    !Evaluacion del problema de despacho   
    !ierror = 0
    !ibanbit = 2
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//'          EVAL CUADRATICA PROBLEMA DESPACHO'
    !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( 524, * ) bmensaje
else
    !Evaluacion del problema de asignacion   
    !ierror = 0
    !ibanbit = 2
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//'          EVAL CUADRATICA PROBLEMA ASIGNACION'
    !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write ( 524, * ) bmensaje
end if

!Hacer para todas las cuencas   
turb_maxT = 0.0
turb_minT = 0.0
ener_maxT = 0.0
ener_minT = 0.0
UniOnHid = 0
ApoVaUp = 0.0
gplah = 0.0
qplah = 0.0 
ener_resT = 0.0
Vol_util_res = 0.0
turb_resT = 0.0
turb_T = 0.0
ener_T = 0.0
ener_res = 0.0
ener_resl = 0.0
qmxgastodes = 0.0
qmngastodes = 0.0 
Alt_net_des = 0.0
qgastores = 0.0
g_res = 0.0
A_hatd_copy = A_hatd
B_hatd_copy = B_hatd
C_hatd_copy = C_hatd
A_hatd = 0.0
B_hatd = 0.0 
C_hatd = 0.0
aa4 ( :, :, 7 ) = 0.0
do cuenca = 1,nmcuen        
    WRITE ( UNLG12_1 , * ) ' ' 
    WRITE ( UNLG12_1 , * ) ' R E S U L T A D O S ' 
    WRITE ( UNLG12_1 , * ) '***********************************************************************************'
    !Imprimir nombre y numero de la cuenca
    WRITE ( UNLG12_1 , 100 ) ' CUENCA NO:', cuenca, ' NOMBRE:', nomval ( cuenca )    
    WRITE ( UNLG12_1 , * ) '***********************************************************************************'
    !Imprimir nombre y numero de la cuenca
    WRITE ( 522 , 117 ) '************************************************************************'
    WRITE ( 522 , 100 ) ' CUENCA NO:', cuenca, ' NOMBRE:', nomval ( cuenca )  
    WRITE ( 522 , 117 ) '************************************************************************' 
    WRITE ( 522 , * ) ' '   
    ninter = 1   
    !Hacer para todos los dias
    do dia = 1, durdia
        write ( aaux1, 5103 ) dia
        dia_text = 'D'//aaux1
        write ( 522, 119 ) '--------------------------------','Dia', dia_text, '--------------------------------'
        !Hacer para todas las horas
        do intervalo = ninter, intdia (dia ) + ninter - 1 
            WRITE ( UNLG12_1 , * ) ' '
            WRITE ( UNLG12_1 , * ) '------------------'
            WRITE ( UNLG12_1 , 123 ) ' INTERVALO:', intervalo 
            WRITE ( UNLG12_1 , * ) '------------------'
            consecutivo = 0
            write ( 522, * ) ' '
            write ( 522, 120 ) '--------------------------------','Hr', intervalo, '--------------------------------'
            write ( 522, * ) ' '
    
            !Hacer para todos los emblases de la cuenca     
            localidad_3 = 0
            entro = .false. 
            do while ( localidad_3 .le. nmembc ( cuenca ) - 1 )
                do localidad_2 = 0, nmembc ( cuenca ) - 1
                    embalse = dnemva ( cuenca ) + localidad_2
!                    aa4 ( embalse, dia, 7 ) = 0.0
                    q_res_embv ( embalse, intervalo ) = 0.0
                    q_res_embvl ( embalse, intervalo ) = 0.0
                    e_res_embv ( embalse, intervalo ) = 0.0
                    e_res_embvl ( embalse, intervalo ) = 0.0
                    e_embv ( embalse, intervalo ) = 0.0
                    !Se revisa si tiene via convergente
                    if ( embviaconv ( embalse ) .eq. 0 .and. entro .eq. .false. ) then
                        !Imprimir nombre y numero del embalse
                        WRITE ( UNLG12_1 , * ) ' '     
                        WRITE ( UNLG12_1 , * ) '--------------------------------------------------------------------------'
                        WRITE ( UNLG12_1 , 100 ) ' EMBALSE NO:', embalse, ' NOMBRE:', NOMEMB ( embalse ) 
                        WRITE ( UNLG12_1 , * ) '--------------------------------------------------------------------------'
                        
                        !Primer embalse de la cuenca
                        !via divergente
                        viad = DNVIOU ( embalse ) 
                        !Encontrar el embalse aguas abajo
                        !Hacer para todos los embalses
                        downstream = 0
                        do contador = 1, NumEmbalses
                            if ( dnvico ( contador ) .eq. viad ) then
                                downstream = embviaconv ( contador )  
                                exit                        
                            end if
                        end do                        
                        if ( intervalo .eq. 1 ) then
                            !Hacer para todas las plantas que descargan sobre la v�a
                            !Inicializa gasto (acumulado) de todas las plantas que pertenecen al embalse
                            gasto = 0.0
                            !Inicializa perdidas (acumulado) de todas las plantas que pertenecen al embalse
                            perdidas = 0.0                            
                            do localidad = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                 planta = plantas_x_via ( localidad )
                                !Hacer para todas las unidades de la planta
                                do unitcount = 0, NOUN ( planta ) - 1                                
                                    unidad = DNUNPH ( planta ) + unitcount
                                    !obtener gasto
                                    !gasto = gasto + qgastores ( unidad, intervalo )
                                    gasto = gasto + qgasto ( unidad, intervalo )
                                    !Calcula las perdidas maxima por intervalo para todas las plantas del embalse
                                    !perdidas = perdidas + RELTAH ( planta , qgastores ( unidad, intervalo ) )
                                    perdidas = perdidas + RELTAH ( planta , qgasto ( unidad, intervalo ) )
                                end do
                            end do           
                            !Aportacion al embalse
                            Aporta = apoemb ( embalse, intervalo )
                            if (downstream .ne. 0 ) then
                                !nivel en el embalse aguas abajo (condiciones iniciales)
                                nivdownstream = nivini ( downstream )                                    
                            else
                                nivdownstream = 0.0
                            end if
                            !Nivel en la via de desfogue   
                            ndvi ( viad, intervalo ) = RIVDES ( embalse, viad, gasto, vertdo ( viad, intervalo ), nivdownstream )
                            !Altura neta para las unidades del embalse    
                            Alt_net_des ( viad, intervalo ) = nivini ( embalse ) - ndvi ( viad, intervalo ) - perdidas
                            !Hacer para todas las plantas que descargan sobre la v�a
                            do localidad = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                planta = plantas_x_via ( localidad )
                                WRITE ( UNLG12_1 , * ) 'Unidad/Planta  ---- Generacion (MW) -----  ----- Gasto (mMC/hr) -----  --------- Altura ---------'
                                !imprimir el nombre de la planta
                                WRITE ( UNLG12_1 , 122 ) planta, NOMPLAH ( planta ), '           Maximo        Minimo        Maximo        Minimo        Maximo        Minimo'
                                !Hacer para todas las unidades de la planta
                                do localidad_1 = 0, NOUN ( planta ) - 1
                                    unidad = DNUNPH ( planta ) + localidad_1
                                    !Calcular coeficientes de la funcion de generacion para resultado
                                    call Coef_gen_hidro_quad_res ( unidad, intervalo, viad ) 
                                    !Obtener el gasto dependiendo de la generacion hidro usando funcion cuadratica
                                    g_res ( unidad, intervalo ) = genunh ( unidad, intervalo ) * basmva
                                    gplah ( planta, intervalo ) = gplah ( planta, intervalo ) + g_res ( unidad, intervalo )
                                    if ( g_res ( unidad, intervalo ) .gt. 0.0 ) then
                                        UniOnHid ( planta, intervalo ) = UniOnHid ( planta, intervalo ) + 1 
                                        radicando = B_hatd ( unidad, intervalo ) ** 2 - 4 * C_hatd ( unidad, intervalo ) * ( A_hatd ( unidad, intervalo ) - g_res ( unidad, intervalo ) )
                                        if ( radicando .gt. 0.0 ) then
                                            gasto_1 = ( - B_hatd ( unidad, intervalo ) + sqrt ( radicando ) )/ ( 2 * C_hatd ( unidad, intervalo ) )
                                            gasto_2 = ( - B_hatd ( unidad, intervalo ) - sqrt ( radicando ) )/ ( 2 * C_hatd ( unidad, intervalo ) )
                                        else
                                            gasto_1 = qgasto ( unidad, intervalo )
                                            gasto_2 = 10000.0
                                            g_res ( unidad, intervalo ) = C_hatd ( unidad, intervalo ) * qgasto ( unidad, intervalo ) ** 2 + &
                                                    B_hatd ( unidad, intervalo ) * qgasto ( unidad, intervalo ) + &
                                                    A_hatd ( unidad, intervalo )
                                        end if
                                        if ( abs ( qgasto ( unidad, intervalo ) - gasto_1 ) .lt. &
                                             abs ( qgasto ( unidad, intervalo ) - gasto_2 ) ) then
                                            qgastores ( unidad, intervalo ) = gasto_1
                                        else
                                            qgastores ( unidad, intervalo ) = gasto_2
                                        end if
                                        !gasto maximo calculado altura neta aproximimada para AU
                                        qmxgastodes ( unidad, intervalo ) = RGASMX ( unidad , Alt_net_des ( viad, intervalo ) )
                                        !gasto minimo calculado altura neta aproximimada para AU
                                        qmngastodes ( unidad, intervalo ) = GASMN ( unidad , Alt_net_des ( viad, intervalo ) )                                        
                                        if ( qgastores ( unidad, intervalo ) .gt. qmxgastodes ( unidad, intervalo ) ) then
                                            qgastores ( unidad, intervalo ) = qmxgastodes ( unidad, intervalo )                                            
                                            g_res ( unidad, intervalo ) = C_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) ** 2 + &
                                                                          B_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) + &
                                                                          A_hatd ( unidad, intervalo )    
                                        end if
                                        if ( qgastores ( unidad, intervalo ) .lt. qmngastodes ( unidad, intervalo ) ) then
                                            qgastores ( unidad, intervalo ) = qmngastodes ( unidad, intervalo )
                                            g_res ( unidad, intervalo ) = C_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) ** 2 + &
                                                                          B_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) + &
                                                                          A_hatd ( unidad, intervalo )    
                                        end if
                                    else
                                        qgastores ( unidad, intervalo ) = 0.0
                                    end if
                                    !imprimir el nombre de la unidad
                                    WRITE ( UNLG12_1 , 102 ) unidad, nombunih ( unidad ), g_res ( unidad, intervalo ), g_res ( unidad, intervalo ), qgastores ( unidad, intervalo ) * MValor, qgastores ( unidad, intervalo ) * MValor, Alt_net_des ( viad, intervalo )
                                    qplah ( planta, intervalo ) =  qplah ( planta, intervalo ) + qgastores ( unidad, intervalo ) * MValor 
                                    q_res_embv ( embalse, intervalo ) = q_res_embv ( embalse, intervalo ) + qgastores ( unidad, intervalo ) * MValor
                                    q_res_embvl ( embalse, intervalo ) = q_res_embvl ( embalse, intervalo ) + qgasto ( unidad, intervalo ) * MValor
                                    turb_T ( embalse ) = turb_T ( embalse ) + qgasto ( unidad, intervalo ) * MValor
                                    e_res_embv ( embalse, intervalo ) = e_res_embv ( embalse, intervalo ) + g_res ( unidad, intervalo ) / basmva
                                    e_res_embvl ( embalse, intervalo ) = e_res_embvl ( embalse, intervalo ) + genunh ( unidad, intervalo )
                                    e_embv ( embalse, intervalo ) = e_embv ( embalse, intervalo ) + genunh ( unidad, intervalo )
!                                   volumen de la unidad
                                    volumhR ( unidad, intervalo ) = qgastores ( unidad, intervalo ) * MValor
!                                    volumhR ( unidad, intervalo ) = qgasto ( unidad, intervalo ) * MValor
!                                   carga de la unidad
                                    cargahR ( unidad, intervalo ) = Alt_net_des ( viad, intervalo )
                               end do   
                            end do                    
                            !Turbinado del embalse por intervalo
                            turb_res = q_res_embv ( embalse, intervalo )
                            
                            turb_resl = q_res_embvl ( embalse, intervalo )
                            
                            turb_resT ( embalse ) = turb_resT ( embalse ) + turb_resl
                            
                            a_tra_res ( intervalo + TiViAgu ( viad ), viad ) = turb_resl + VERTDO ( viad, intervalo )
                            vol_util_res ( embalse, intervalo ) = VMINI ( embalse ) + Aporta - turb_resl - VERTDO ( viad, intervalo )
                            aa4 ( embalse, dia, 7 ) = aa4 ( embalse, dia, 7 ) + VERTDO ( viad, intervalo )
                            ener_res = e_res_embv ( embalse, intervalo )
                            
                            ener_resl = e_res_embvl ( embalse, intervalo )
                            
                            ener_resT ( embalse ) = ener_resT ( embalse ) + ener_res
                            
                            ener_T ( embalse ) = ener_T ( embalse ) + e_embv ( embalse, intervalo )
                            
                            if ( vol_util_res ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then  
                                vol_util_res ( embalse, intervalo ) = VolMnEmb ( embalse )
                            end if                            
                            if ( DefiAguEmb ( embalse, intervalo ) .gt. 0.0 ) then
                                !Infactibilidad hidraulica violacion vol min.                      
                                if ( LinVSCuad .eq. 1 ) then
                                    ibanbit = 1
                                else
                                    ibanbit = 2
                                end if
                                ierror = 0
                                    
                                call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                    
                                Call FechaEjecucion (fecha_Ej)
                                bmensaje = fecha_Ej//' HIDRO105 INFACTIBILIDAD HIDRAULICA: DEFICIT'
                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                
                                write ( aaux, 5100 ) embalse
                                write ( aaux_1, 5101 ) nomemb ( embalse )
                                write ( aaux_2, 5100 ) intervaloo 
                                !write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_res ( embalse, intervalo ) ) / 1000.0
                                write ( aaux_3, 5102 ) DefiAguEmb ( embalse, intervalo )
                                write ( aaux_5, 5101 ) dia_text
                                                    
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//' HIDRO105 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                               
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//' HIDRO105 NO. VIOLACION: '//aaux_3//' MMC'
                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                
                                !vol_util_res ( embalse, intervalo ) = VolMnEmb ( embalse )
                            end if
                            if ( vol_util_res ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                vol_util_res ( embalse, intervalo ) = VolMxEmb ( embalse )
                            end if                            
                            if ( ExceAguEmb ( embalse, intervalo ) .gt. 0.0 ) then
                                !Infactibilidad hidraulica violacion vol max.                                    
                                if ( LinVSCuad .eq. 1 ) then
                                    ibanbit = 1
                                else
                                    ibanbit = 2
                                end if
                                ierror = 0
                                        
                                call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                       
                                Call FechaEjecucion (fecha_Ej)
                                bmensaje = fecha_Ej//' HIDRO105 INFACTIBILIDAD HIDRAULICA: EXCEDENTE'
                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                write ( aaux, 5100 ) embalse
                                write ( aaux_1, 5101 ) nomemb ( embalse )
                                write ( aaux_2, 5100 ) intervaloo 
                                !write ( aaux_3, 5102 ) ( vol_util_res ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                write ( aaux_3, 5102 ) ExceAguEmb ( embalse, intervalo )
                                write ( aaux_5, 5101 ) dia_text
                                                        
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//' HIDRO105 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                  
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//' HIDRO105 NO. VIOLACION: '//aaux_3//' MMC'
                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                
                                !vol_util_res ( embalse, intervalo ) = VolMxEmb ( embalse )
                            end if
                        else
                            !Hacer para todas las plantas que descargan sobre la v�a
                            !Inicializa gasto (acumulado) de todas las plantas que pertenecen al embalse
                            gasto = 0.0
                            !Inicializa perdidas (acumulado) de todas las plantas que pertenecen al embalse
                            perdidas = 0.0  
                            do localidad = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                planta = plantas_x_via ( localidad )
                                !Hacer para todas las unidades de la planta
                                do unitcount = 0, NOUN ( planta ) - 1                                
                                    unidad = DNUNPH ( planta ) + unitcount
                                    !obtener gasto
                                    !gasto = gasto + qgastores ( unidad, intervalo )
                                    gasto = gasto + qgasto ( unidad, intervalo )
                                    !Calcula las perdidas maxima por intervalo para todas las plantas del embalse
                                    !perdidas = perdidas + RELTAH ( planta , qgastores ( unidad, intervalo ) )
                                    perdidas = perdidas + RELTAH ( planta , qgasto ( unidad, intervalo ) )
                                end do
                            end do                            
                            !Aportacion al embalse
                            Aporta = apoemb ( embalse, intervalo )
                            if (downstream .ne. 0 ) then
                                !Nivel  del embalse aguas abajo calculado con turbinado 
                                nivdownstream = namino ( downstream ) + ALUTIL ( downstream , vol_util_res ( downstream, intervalo - 1 ) )
                            else
                                nivdownstream = 0.0
                            end if
                            !Nivel en la via de desfogue    
                            ndvi ( viad, intervalo ) = RIVDES ( embalse, viad, gasto, vertdo ( viad, intervalo ), nivdownstream )
                            !Altura neta para las unidades del embalse    
                            Alt_net_des ( viad, intervalo ) = namino ( embalse ) + ALUTIL ( embalse , vol_util_res ( embalse, intervalo - 1 ) ) &
                                                             - ndvi ( viad, intervalo ) - perdidas
                            !Hacer para todas las plantas que descargan sobre la v�a
                            do localidad = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                planta = plantas_x_via ( localidad )
                                WRITE ( UNLG12_1 , * ) 'Unidad/Planta  ---- Generacion (MW) -----  ----- Gasto (mMC/hr) -----  --------- Altura ---------'
                                !imprimir el nombre de la planta
                                WRITE ( UNLG12_1 , 122 ) planta, NOMPLAH ( planta ), '           Maximo        Minimo        Maximo        Minimo        Maximo        Minimo'
                                !Hacer para todas las unidades de la planta
                                do localidad_1 = 0, NOUN ( planta ) - 1
                                    unidad = DNUNPH ( planta ) + localidad_1
                                    !Calcular coeficientes de la funcion de generacion para resultado
                                    call Coef_gen_hidro_quad_res ( unidad, intervalo, viad ) 
                                    !Obtener el gasto dependiendo de la generacion hidro usando funcion cuadratica
                                    g_res ( unidad, intervalo ) = genunh ( unidad, intervalo ) * basmva
                                    gplah ( planta, intervalo ) = gplah ( planta, intervalo ) + g_res ( unidad, intervalo )
                                    if ( g_res ( unidad, intervalo ) .gt. 0.0 ) then
                                        UniOnHid ( planta, intervalo ) = UniOnHid ( planta, intervalo ) + 1 
                                        radicando = B_hatd ( unidad, intervalo ) ** 2 - 4 * C_hatd ( unidad, intervalo ) * ( A_hatd ( unidad, intervalo ) - g_res ( unidad, intervalo ) )
                                        if ( radicando .gt. 0.0 ) then
                                            gasto_1 = ( - B_hatd ( unidad, intervalo ) + sqrt ( radicando ) )/ ( 2 * C_hatd ( unidad, intervalo ) )
                                            gasto_2 = ( - B_hatd ( unidad, intervalo ) - sqrt ( radicando ) )/ ( 2 * C_hatd ( unidad, intervalo ) )
                                        else
                                            gasto_1 = qgasto ( unidad, intervalo )
                                            gasto_2 = 10000.0
                                            g_res ( unidad, intervalo ) = C_hatd ( unidad, intervalo ) * qgasto ( unidad, intervalo ) ** 2 + &
                                                    B_hatd ( unidad, intervalo ) * qgasto ( unidad, intervalo ) + &
                                                    A_hatd ( unidad, intervalo )
                                        end if
                                        if ( abs ( qgasto ( unidad, intervalo ) - gasto_1 ) .lt. &
                                                 abs ( qgasto ( unidad, intervalo ) - gasto_2 ) ) then
                                                qgastores ( unidad, intervalo ) = gasto_1
                                            else
                                                qgastores ( unidad, intervalo ) = gasto_2
                                        end if
                                        !gasto maximo calculado altura neta aproximimada para AU
                                        qmxgastodes ( unidad, intervalo ) = RGASMX ( unidad , Alt_net_des ( viad, intervalo ) )
                                        !gasto minimo calculado altura neta aproximimada para AU
                                        qmngastodes ( unidad, intervalo ) = GASMN ( unidad , Alt_net_des ( viad, intervalo ) )
                                        if ( qgastores ( unidad, intervalo ) .gt. qmxgastodes ( unidad, intervalo ) ) then
                                            qgastores ( unidad, intervalo ) = qmxgastodes ( unidad, intervalo )
                                            g_res ( unidad, intervalo ) = C_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) ** 2 + &
                                                                          B_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) + &
                                                                          A_hatd ( unidad, intervalo )    
                                        end if
                                        if ( qgastores ( unidad, intervalo ) .lt. qmngastodes ( unidad, intervalo ) ) then
                                            qgastores ( unidad, intervalo ) = qmngastodes ( unidad, intervalo )
                                            g_res ( unidad, intervalo ) = C_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) ** 2 + &
                                                                          B_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) + &
                                                                          A_hatd ( unidad, intervalo )    
                                        end if
                                    else
                                        qgastores ( unidad, intervalo ) = 0.0
                                    end if
                                    !imprimir el nombre de la unidad
                                    WRITE ( UNLG12_1 , 102 ) unidad, nombunih ( unidad ), g_res ( unidad, intervalo ), g_res ( unidad, intervalo ), qgastores ( unidad, intervalo ) * MValor, qgastores ( unidad, intervalo ) * MValor, Alt_net_des ( viad, intervalo )
                                    qplah ( planta, intervalo ) =  qplah ( planta, intervalo ) + qgastores ( unidad, intervalo ) * MValor
                                    q_res_embv ( embalse, intervalo ) = q_res_embv ( embalse, intervalo ) + qgastores ( unidad, intervalo ) * MValor
                                    q_res_embvl ( embalse, intervalo ) = q_res_embvl ( embalse, intervalo ) + qgasto ( unidad, intervalo ) * MValor
                                    turb_T ( embalse ) = turb_T ( embalse ) + qgasto ( unidad, intervalo ) * MValor
                                    e_res_embv ( embalse, intervalo ) = e_res_embv ( embalse, intervalo ) + g_res ( unidad, intervalo ) / basmva
                                    e_res_embvl ( embalse, intervalo ) = e_res_embvl ( embalse, intervalo ) + genunh ( unidad, intervalo )
                                    e_embv ( embalse, intervalo ) = e_embv ( embalse, intervalo ) + genunh ( unidad, intervalo )
!                                   volumen de la unidad
                                    volumhR ( unidad, intervalo ) = qgastores ( unidad, intervalo ) * MValor
!                                    volumhR ( unidad, intervalo ) = qgasto ( unidad, intervalo ) * MValor
!                                   carga de la unidad
                                    cargahR ( unidad, intervalo ) = Alt_net_des ( viad, intervalo )
                                end do                                
                            end do                            
                            !Turbinado del embalse por intervalo
                            turb_res = q_res_embv ( embalse, intervalo )
                            
                            turb_resl = q_res_embvl ( embalse, intervalo )
                            
                            turb_resT ( embalse ) = turb_resT ( embalse ) + turb_resl
                            
                            a_tra_res ( intervalo + TiViAgu ( viad ), viad ) = turb_resl + VERTDO ( viad, intervalo )
                            aa4 ( embalse, dia, 7 ) = aa4 ( embalse, dia, 7 ) + VERTDO ( viad, intervalo )
                            vol_util_res ( embalse, intervalo ) = vol_util_res ( embalse, intervalo - 1 ) + Aporta - turb_resl - VERTDO ( viad, intervalo )
                            ener_res = e_res_embv ( embalse, intervalo )
                            
                            ener_resl = e_res_embvl ( embalse, intervalo )
                            
                            ener_resT ( embalse ) = ener_resT ( embalse ) + ener_res
                            
                            ener_T ( embalse ) = ener_T ( embalse ) + e_embv ( embalse, intervalo )
                            
                            if ( vol_util_res ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                vol_util_res ( embalse, intervalo ) = VolMnEmb ( embalse )
                            end if                            
                            if ( DefiAguEmb ( embalse, intervalo ) .gt. VolMnEmb ( embalse ) ) then
                                !Infactibilidad hidraulica violacion vol min.                                    
                                if ( LinVSCuad .eq. 1 ) then
                                    ibanbit = 1
                                else
                                    ibanbit = 2
                                end if
                                ierror = 0
                                            
                                call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                           
                                Call FechaEjecucion (fecha_Ej)
                                bmensaje = fecha_Ej//' HIDRO105 INFACTIBILIDAD HIDRAULICA: DEFICIT'
                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                        
                                write ( aaux, 5100 ) embalse
                                write ( aaux_1, 5101 ) nomemb ( embalse )
                                write ( aaux_2, 5100 ) intervaloo 
                                !write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_res ( embalse, intervalo ) ) / 1000.0
                                write ( aaux_3, 5102 ) DefiAguEmb ( embalse, intervalo )
                                
                                write ( aaux_5, 5101 ) dia_text
                                                            
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//' HIDRO105 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                      
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//' HIDRO105 NO. VIOLACION: '//aaux_3//' MMC'
                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                
                                !vol_util_res ( embalse, intervalo ) = VolMnEmb ( embalse )
                            end if
                            if ( vol_util_res ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                vol_util_res ( embalse, intervalo ) = VolMxEmb ( embalse )
                            end if                            
                            if ( ExceAguEmb ( embalse, intervalo ) .gt. 0.0 ) then
                                !Infactibilidad hidraulica violacion vol max.                                    
                                if ( LinVSCuad .eq. 1 ) then
                                    ibanbit = 1
                                else
                                    ibanbit = 2
                                end if
                                ierror = 0
                                            
                                call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                            
                                Call FechaEjecucion (fecha_Ej)
                                bmensaje = fecha_Ej//' HIDRO105 INFACTIBILIDAD HIDRAULICA: EXCEDENTE'
                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                        
                                write ( aaux, 5100 ) embalse
                                write ( aaux_1, 5101 ) nomemb ( embalse )
                                write ( aaux_2, 5100 ) intervaloo 
                                !write ( aaux_3, 5102 ) ( vol_util_res ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                write ( aaux_3, 5102 ) ExceAguEmb ( embalse, intervalo )
                                write ( aaux_5, 5101 ) dia_text
                                                            
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//' HIDRO105 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                      
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//' HIDRO105 NO. VIOLACION: '//aaux_3//' MMC'
                                write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                
                                !vol_util_res ( embalse, intervalo ) = VolMxEmb ( embalse ) 
                            end if
                        end if                            
                        !Primer embalse de la cuenca
                        WRITE ( 522, 101 ) '                            |   |   Aportacion:    ', Aporta / 1000.00, 'MMC'                         
                        WRITE ( 522, 101 ) '                            |   |                  '                   
                        write ( 522, 103 ) 'Vol.Util:', VolEmb ( embalse, intervalo ) / 1000.00, 'MMC','     /\   |' !vol_util_res
                        write ( 522, 103 ) '         ', 0.0 , '   ','    /  \  /'
                        write ( 522, 103 ) 'Turbinad:', turb_resl / 1000.0, 'MMC','   /    \/ '!turb_res
                        write ( 522, 104 ) '         ', 0.0, '   ','  /', nomemb ( embalse ), embalse, '\'   
                        write ( 522, 105 ) 'Energia :', ener_resl / 10.00, 'GWH', ' /        \'!ener_res
                        write ( 522, 106 ) '         ', 0.0, '   ', '---------- )', 'Vertido programado:', vertdo ( DNVIOU ( embalse ), intervalo ) / 1000.00, 'MMC' 
                        write ( 522, 107 ) '                            |    /'
                        !Imprime via divergente
                        write ( 522, 109 ) NOMVIA ( embalse ), '                       |'
                        write ( 522, 108 ) '                            |'
                        write ( 522, 116 ) 'Gasto     :', ( turb_resl + VERTDO ( viad, intervalo ) ) / 1000.00, 'MMC', '    |'!turb_res	
                        write ( 522, 116 ) '           ', 0.0, '   ', '    |'	
                        write ( 522, 118 ) 'T.V.A:', TiViAgu ( viad ), 'hr', '         |'	
                        localidad_3 = localidad_3 + 1
                        entro = .true.
                        hexa = intervalo / 6.0
                        dummy = abs ( hexa - int ( hexa ) )
                        if ( dummy .gt. 0.0 ) then
                            int_hexa = int ( hexa ) + 1
                        else 
                            int_hexa = int ( hexa ) 
                        end if
                    else
                        if ( entro .eq. .true. ) then                            
                            !Buscar que embalse tiene la via divergente del embalse aguas arriba como via convergente                             
                            !Hacer para todos los embalses
                            do localidad = 0, NumEmbalses - 1   
                                consecutivo = consecutivo + 1                         
                                if ( viad .eq. viainc ( consecutivo ) ) then 
                                    !Imprimir nombre y numero del embalse
                                    WRITE ( UNLG12_1 , * ) ' '     
                                    WRITE ( UNLG12_1 , * ) '--------------------------------------------------------------------------'
                                    WRITE ( UNLG12_1 , 100 ) ' EMBALSE NO:', embalse, ' NOMBRE:', NOMEMB ( embalse ) 
                                    WRITE ( UNLG12_1 , * ) '--------------------------------------------------------------------------'
                                    
                                    !Via divergente
                                    viad = DNVIOU ( embalse ) 
                                    !Via convergente
                                    viac = viainc ( consecutivo )
                                    !Encontrar el embalse aguas abajo
                                    !Hacer para todos los embalses
                                    downstream = 0
                                    do contador = 1, NumEmbalses
                                        if ( dnvico ( contador ) .eq. viad ) then
                                            downstream = embviaconv ( contador )  
                                            exit                        
                                        end if
                                    end do
                                    if ( intervalo .eq. 1 ) then    
                                        !Hacer para todas las plantas que descargan sobre la v�a
                                        !Inicializa gasto (acumulado) de todas las plantas que pertenecen al embalse
                                        gasto = 0.0
                                        !Inicializa perdidas (acumulado) de todas las plantas que pertenecen al embalse
                                        perdidas = 0.0
                                        do localidad_5 = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                            planta = plantas_x_via ( localidad_5 )
                                            !WRITE ( UNLG12_1 , * ) 'Unidad/Planta  ---- Generacion (MW) -----  ----- Gasto (mMC/hr) -----  --------- Altura ---------'
                                            !imprimir el nombre de la planta
                                            !WRITE ( UNLG12_1 , 122 ) planta, NOMPLAH ( planta ), '           Maximo        Minimo        Maximo        Minimo        Maximo        Minimo'
                                            !Hacer para todas las unidades de la planta
                                            do unitcount = 0, NOUN ( planta ) - 1                                
                                                unidad = DNUNPH ( planta ) + unitcount
                                                !obtener gasto
                                                !gasto = gasto + qgastores ( unidad, intervalo )
                                                gasto = gasto + qgasto ( unidad, intervalo )
                                                !Calcula las perdidas maxima por intervalo para todas las plantas del embalse
                                                !perdidas = perdidas + RELTAH ( planta , qgastores ( unidad, intervalo ) )
                                                perdidas = perdidas + RELTAH ( planta , qgasto ( unidad, intervalo ) )
                                            end do
                                        end do                                    
                                        !Aportacion al embalse
                                        Aporta = apoemb ( embalse, intervalo ) + a_tra_res ( intervalo, viac )
                                        if (downstream .ne. 0 ) then
                                            !nivel en el embalse aguas abajo (condiciones iniciales)
                                            nivdownstream = nivini ( downstream )                                    
                                        else
                                            nivdownstream = 0.0
                                        end if
                                        !Nivel en la via de desfogue   
                                        ndvi ( viad, intervalo ) = RIVDES ( embalse, viad, gasto, vertdo ( viad, intervalo ), nivdownstream )
                                        !Altura neta para las unidades del embalse    
                                        Alt_net_des ( viad, intervalo ) = nivini ( embalse ) - ndvi ( viad, intervalo ) - perdidas    
                                        !Hacer para todas las plantas que descargan sobre la v�a
                                        do localidad_4 = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                            planta = plantas_x_via ( localidad_4 )
                                            WRITE ( UNLG12_1 , * ) 'Unidad/Planta  ---- Generacion (MW) -----  ----- Gasto (mMC/hr) -----  --------- Altura ---------'
                                            !imprimir el nombre de la planta
                                            WRITE ( UNLG12_1 , 122 ) planta, NOMPLAH ( planta ), '           Maximo        Minimo        Maximo        Minimo        Maximo        Minimo'
                                            !Hacer para todas las unidades de la planta
                                            do localidad_1 = 0, NOUN ( planta ) - 1
                                               unidad = DNUNPH ( planta ) + localidad_1
                                               !Calcular coeficientes de la funcion de generacion para resultado
                                                call Coef_gen_hidro_quad_res ( unidad, intervalo, viad ) 
                                               !Obtener el gasto dependiendo de la generacion hidro usando funcion cuadratica
                                                g_res ( unidad, intervalo ) = genunh ( unidad, intervalo ) * basmva
                                                gplah ( planta, intervalo ) = gplah ( planta, intervalo ) + g_res ( unidad, intervalo )
                                                if ( g_res ( unidad, intervalo ) .gt. 0.0 ) then
                                                    UniOnHid ( planta, intervalo ) = UniOnHid ( planta, intervalo ) + 1 
                                                    radicando = B_hatd ( unidad, intervalo ) ** 2 - 4 * C_hatd ( unidad, intervalo ) *  ( A_hatd ( unidad, intervalo ) - g_res ( unidad, intervalo ) )
                                                    if ( radicando .gt. 0.0 ) then
                                                        gasto_1 = ( - B_hatd ( unidad, intervalo ) + sqrt ( radicando ) )/ ( 2 * C_hatd ( unidad, intervalo ) )
                                                        gasto_2 = ( - B_hatd ( unidad, intervalo ) - sqrt ( radicando ) )/ ( 2 * C_hatd ( unidad, intervalo ) )
                                                    else
                                                        gasto_1 = qgasto ( unidad, intervalo )
                                                        gasto_2 = 10000.0                                                        
                                                        g_res ( unidad, intervalo ) = C_hatd ( unidad, intervalo ) * gasto_1 ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * gasto_1 + &
                                                                A_hatd ( unidad, intervalo ) 
                                                    end if
                                                    if ( abs ( qgasto ( unidad, intervalo ) - gasto_1 ) .lt. &
                                                         abs ( qgasto ( unidad, intervalo ) - gasto_2 ) ) then
                                                        qgastores ( unidad, intervalo ) = gasto_1
                                                    else
                                                        qgastores ( unidad, intervalo ) = gasto_2
                                                    end if
                                                    !gasto maximo calculado altura neta aproximimada para AU
                                                    qmxgastodes ( unidad, intervalo ) = RGASMX ( unidad , Alt_net_des ( viad, intervalo ) )
                                                    !gasto minimo calculado altura neta aproximimada para AU
                                                    qmngastodes ( unidad, intervalo ) = GASMN ( unidad , Alt_net_des ( viad, intervalo ) )
                                                    if ( qgastores ( unidad, intervalo ) .gt. qmxgastodes ( unidad, intervalo ) ) then
                                                        qgastores ( unidad, intervalo ) = qmxgastodes ( unidad, intervalo )
                                                        g_res ( unidad, intervalo ) = C_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) + &
                                                                A_hatd ( unidad, intervalo )   
                                                    end if
                                                    if ( qgastores ( unidad, intervalo ) .lt. qmngastodes ( unidad, intervalo ) ) then
                                                        qgastores ( unidad, intervalo ) = qmngastodes ( unidad, intervalo )
                                                        g_res ( unidad, intervalo ) = C_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) + &
                                                                A_hatd ( unidad, intervalo )
                                                    end if
                                                else
                                                    qgastores ( unidad, intervalo ) = 0.0
                                                end if
                                                !imprimir el nombre de la unidad
                                                WRITE ( UNLG12_1 , 102 ) unidad, nombunih ( unidad ), g_res ( unidad, intervalo ), g_res ( unidad, intervalo ), qgastores ( unidad, intervalo ) * MValor, qgastores ( unidad, intervalo ) * MValor, Alt_net_des ( viad, intervalo )
                                                qplah ( planta, intervalo ) =  qplah ( planta, intervalo ) + qgastores ( unidad, intervalo ) * MValor
                                                q_res_embv ( embalse, intervalo ) = q_res_embv ( embalse, intervalo ) + qgastores ( unidad, intervalo ) * MValor
                                                q_res_embvl ( embalse, intervalo ) = q_res_embvl ( embalse, intervalo ) + qgasto ( unidad, intervalo ) * MValor
                                                turb_T ( embalse ) = turb_T ( embalse ) + qgasto ( unidad, intervalo ) * MValor
                                                e_res_embv ( embalse, intervalo ) = e_res_embv ( embalse, intervalo ) + g_res ( unidad, intervalo ) / basmva
                                                e_res_embvl ( embalse, intervalo ) = e_res_embvl ( embalse, intervalo ) + genunh ( unidad, intervalo )
                                                e_embv ( embalse, intervalo ) = e_embv ( embalse, intervalo ) + genunh ( unidad, intervalo )
!                                               volumen de la unidad
                                                volumhR ( unidad, intervalo ) = qgastores ( unidad, intervalo ) * MValor
!                                                volumhR ( unidad, intervalo ) = qgasto ( unidad, intervalo ) * MValor
!                                               carga de la unidad
                                                cargahR ( unidad, intervalo ) = Alt_net_des ( viad, intervalo )
                                            end do                                
                                        end do
                                        !Turbinado del embalse por intervalo
                                        turb_res = q_res_embv ( embalse, intervalo )
                                        
                                        turb_resl = q_res_embvl ( embalse, intervalo )
                                        
                                        turb_resT ( embalse ) = turb_resT ( embalse ) + turb_resl
                                        
                                        a_tra_res ( intervalo + TiViAgu ( viad ), viad ) = turb_resl + VERTDO ( viad, intervalo )
                                        vol_util_res ( embalse, intervalo ) = VMINI ( embalse ) + Aporta - turb_resl - VERTDO ( viad, intervalo )
                                        aa4 ( embalse, dia, 7 ) = aa4 ( embalse, dia, 7 ) + VERTDO ( viad, intervalo )
                                        ener_res = e_res_embv ( embalse, intervalo )
                                        
                                        ener_resl = e_res_embvl ( embalse, intervalo )
                                        
                                        ener_resT ( embalse ) = ener_resT ( embalse ) + ener_res  
                                        
                                        ener_T ( embalse ) = ener_T ( embalse ) + e_embv ( embalse, intervalo )
                                        if ( vol_util_res ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                            vol_util_res ( embalse, intervalo ) = VolMnEmb ( embalse )
                                        end if                                        
                                        if ( DefiAguEmb ( embalse, intervalo ) .gt. 0.0 ) then
                                            !Infactibilidad hidraulica violacion vol min.                                    
                                            if ( LinVSCuad .eq. 1 ) then
                                                ibanbit = 1
                                            else
                                                ibanbit = 2
                                            end if
                                            ierror = 0
                                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO105 INFACTIBILIDAD HIDRAULICA: DEFICIT'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                    
                                            write ( aaux, 5100 ) embalse
                                            write ( aaux_1, 5101 ) nomemb ( embalse )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            !write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_res ( embalse, intervalo ) ) / 1000.0
                                            write ( aaux_3, 5102 ) DefiAguEmb ( embalse, intervalo )
                                            write ( aaux_5, 5101 ) dia_text
                                                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO105 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                  
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO105 NO. VIOLACION: '//aaux_3//' MMC'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                            
                                            !vol_util_res ( embalse, intervalo ) = VolMnEmb ( embalse )
                                        end if
                                        if ( vol_util_res ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                            vol_util_res ( embalse, intervalo ) = VolMxEmb ( embalse )
                                        end if                                        
                                        if ( ExceAguEmb ( embalse, intervalo ) .gt. 0.0 ) then
                                            !Infactibilidad hidraulica violacion vol max.                                    
                                            if ( LinVSCuad .eq. 1 ) then
                                                ibanbit = 1
                                            else
                                                ibanbit = 2
                                            end if
                                            ierror = 0
                                                            
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                            
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO105 INFACTIBILIDAD HIDRAULICA: EXCEDENTE'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                        
                                            write ( aaux, 5100 ) embalse
                                            write ( aaux_1, 5101 ) nomemb ( embalse )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            !write ( aaux_3, 5102 ) ( vol_util_res ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                            write ( aaux_3, 5102 ) ExceAguEmb ( embalse, intervalo )
                                            write ( aaux_5, 5101 ) dia_text
                                                                            
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO105 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                  
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO105 NO. VIOLACION: '//aaux_3//' MMC'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                            
                                            !vol_util_res ( embalse, intervalo ) = VolMxEmb ( embalse )
                                        end if              
                                    else                                        
                                        !Hacer para todas las plantas que descargan sobre la v�a
                                        !Inicializa gasto (acumulado) de todas las plantas que pertenecen al embalse
                                        gasto = 0.0
                                        !Inicializa perdidas (acumulado) de todas las plantas que pertenecen al embalse
                                        perdidas = 0.0  
                                        do localidad_7 = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                            planta = plantas_x_via ( localidad_7 )
                                            !Hacer para todas las unidades de la planta
                                            do unitcount = 0, NOUN ( planta ) - 1                                
                                                unidad = DNUNPH ( planta ) + unitcount
                                                !obtener gasto 
                                                !gasto = gasto + qgastores ( unidad, intervalo )
                                                gasto = gasto + qgasto ( unidad, intervalo )
                                                !Calcula las perdidas por intervalo para todas las plantas del embalse
                                                !perdidas = perdidas + RELTAH ( planta , qgastores ( unidad, intervalo ) )
                                                perdidas = perdidas + RELTAH ( planta , qgasto ( unidad, intervalo ) )
                                            end do
                                        end do      
                                        !Aportacion al embalse
                                        Aporta = apoemb ( embalse, intervalo ) + a_tra_res ( intervalo, viac )                                        
                                        if (downstream .ne. 0 ) then
                                            !Nivel del embalse aguas abajo calculado con turbinado
                                            nivdownstream = namino ( downstream ) + ALUTIL ( downstream , vol_util_res ( downstream, intervalo - 1 ) )
                                        else
                                            nivdownstream = 0.0
                                        end if
                                        !Nivel en la via de desfogue   
                                        ndvi ( viad, intervalo ) = RIVDES ( embalse, viad, gasto, vertdo ( viad, intervalo ), nivdownstream )
                                        !Altura neta minima para las unidades del embalse    
                                        Alt_net_des ( viad, intervalo ) = namino ( embalse ) + ALUTIL ( embalse , vol_util_res ( embalse, intervalo - 1 ) ) &
                                                                         - ndvi ( viad, intervalo ) - perdidas
                                        !Hacer para todas las plantas que descargan sobre la v�a
                                        do localidad_4 = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                            planta = plantas_x_via ( localidad_4 )
                                            WRITE ( UNLG12_1 , * ) 'Unidad/Planta  ---- Generacion (MW) -----  ----- Gasto (mMC/hr) -----  --------- Altura ---------'
                                            !imprimir el nombre de la planta
                                            WRITE ( UNLG12_1 , 122 ) planta, NOMPLAH ( planta ), '           Maximo        Minimo        Maximo        Minimo        Maximo        Minimo'
                                            !Hacer para todas las unidades de la planta
                                            do localidad_1 = 0, NOUN ( planta ) - 1
                                                unidad = DNUNPH ( planta ) + localidad_1
                                                !Calcular coeficientes de la funcion de generacion para resultado
                                                call Coef_gen_hidro_quad_res ( unidad, intervalo, viad ) 
                                                !Obtener el gasto dependiendo de la generacion hidro usando funcion cuadratica
                                                g_res ( unidad, intervalo ) = genunh ( unidad, intervalo ) * basmva
                                                gplah ( planta, intervalo ) = gplah ( planta, intervalo ) + g_res ( unidad, intervalo )
                                                if ( g_res ( unidad, intervalo ) .gt. 0.0 ) then
                                                    UniOnHid ( planta, intervalo ) = UniOnHid ( planta, intervalo ) + 1 
                                                    radicando = B_hatd ( unidad, intervalo ) ** 2 - 4 * C_hatd ( unidad, intervalo ) * ( A_hatd ( unidad, intervalo ) - g_res ( unidad, intervalo ) )
                                                    if ( radicando .gt. 0.0 ) then
                                                        gasto_1 = ( - B_hatd ( unidad, intervalo ) + sqrt ( radicando ) )/ ( 2 * C_hatd ( unidad, intervalo ) )
                                                        gasto_2 = ( - B_hatd ( unidad, intervalo ) - sqrt ( radicando ) )/ ( 2 * C_hatd ( unidad, intervalo ) )
                                                    else
                                                        gasto_1 = qgasto ( unidad, intervalo )
                                                        gasto_2 = 10000.0
                                                        g_res ( unidad, intervalo ) = C_hatd ( unidad, intervalo ) * qgasto ( unidad, intervalo ) ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * qgasto ( unidad, intervalo ) + &
                                                                A_hatd ( unidad, intervalo )
                                                    end if
                                                    if ( abs ( qgasto ( unidad, intervalo ) - gasto_1 ) .lt. &
                                                         abs ( qgasto ( unidad, intervalo ) - gasto_2 ) ) then
                                                        qgastores ( unidad, intervalo ) = gasto_1
                                                    else
                                                        qgastores ( unidad, intervalo ) = gasto_2
                                                    end if
                                                    !gasto maximo calculado altura neta aproximimada para AU
                                                    qmxgastodes ( unidad, intervalo ) = RGASMX ( unidad , Alt_net_des ( viad, intervalo ) )
                                                    !gasto minimo calculado altura neta aproximimada para AU
                                                    qmngastodes ( unidad, intervalo ) = GASMN ( unidad , Alt_net_des ( viad, intervalo ) )
                                                    if ( qgastores ( unidad, intervalo ) .gt. qmxgastodes ( unidad, intervalo ) ) then
                                                        qgastores ( unidad, intervalo ) = qmxgastodes ( unidad, intervalo )
                                                        g_res ( unidad, intervalo ) = C_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) + &
                                                                A_hatd ( unidad, intervalo )
                                                    end if
                                                    if ( qgastores ( unidad, intervalo ) .lt. qmngastodes ( unidad, intervalo ) ) then
                                                        qgastores ( unidad, intervalo ) = qmngastodes ( unidad, intervalo )
                                                        g_res ( unidad, intervalo ) = C_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * qgastores ( unidad, intervalo ) + &
                                                                A_hatd ( unidad, intervalo )  
                                                    end if
                                                else
                                                    qgastores ( unidad, intervalo ) = 0.0
                                                end if
                                                !imprimir el nombre de la unidad
                                                WRITE ( UNLG12_1 , 102 ) unidad, nombunih ( unidad ), g_res ( unidad, intervalo ), g_res ( unidad, intervalo ), qgastores ( unidad, intervalo ) * MValor, qgastores ( unidad, intervalo ) * MValor, Alt_net_des ( viad, intervalo )
                                                qplah ( planta, intervalo ) =  qplah ( planta, intervalo ) + qgastores ( unidad, intervalo ) * MValor
                                                q_res_embv ( embalse, intervalo ) = q_res_embv ( embalse, intervalo ) + qgastores ( unidad, intervalo ) * MValor
                                                q_res_embvl ( embalse, intervalo ) = q_res_embvl ( embalse, intervalo ) + qgasto ( unidad, intervalo ) * MValor
                                                turb_T ( embalse ) = turb_T ( embalse ) + qgasto ( unidad, intervalo ) * MValor
                                                e_res_embv ( embalse, intervalo ) = e_res_embv ( embalse, intervalo ) + g_res ( unidad, intervalo ) / basmva
                                                e_res_embvl ( embalse, intervalo ) = e_res_embvl ( embalse, intervalo ) + genunh ( unidad, intervalo )
                                                e_embv ( embalse, intervalo ) = e_embv ( embalse, intervalo ) + genunh ( unidad, intervalo )
!                                               volumen de la unidad
                                                volumhR ( unidad, intervalo ) = qgastores ( unidad, intervalo ) * MValor
!                                                volumhR ( unidad, intervalo ) = qgasto ( unidad, intervalo ) * MValor
!                                               carga de la unidad
                                                cargahR ( unidad, intervalo ) = Alt_net_des ( viad, intervalo )
                                            end do                                
                                        end do
                                                                                
                                        turb_res = q_res_embv ( embalse, intervalo )
                                        
                                        turb_resl = q_res_embvl ( embalse, intervalo )
                                        
                                        turb_resT ( embalse ) = turb_resT ( embalse ) + turb_resl
                                        
                                        a_tra_res ( intervalo + TiViAgu ( viad ), viad ) = turb_resl + VERTDO ( viad, intervalo )
                                        aa4 ( embalse, dia, 7 ) = aa4 ( embalse, dia, 7 ) + VERTDO ( viad, intervalo )
                                        vol_util_res ( embalse, intervalo ) = vol_util_res ( embalse, intervalo - 1 ) + Aporta - turb_resl - VERTDO ( viad, intervalo )
                                        ener_res = e_res_embv ( embalse, intervalo )
                                        
                                        ener_resl = e_res_embvl ( embalse, intervalo )
                                        
                                        ener_resT ( embalse ) = ener_resT ( embalse ) + ener_res
                                        
                                        ener_T ( embalse ) = ener_T ( embalse ) + e_embv ( embalse, intervalo )

                                        if ( vol_util_res ( embalse, intervalo ) .lt. VolMnEmb ( embalse ) ) then
                                            vol_util_res ( embalse, intervalo ) = VolMnEmb ( embalse )
                                        end if                                        
                                        if ( DefiAguEmb ( embalse, intervalo ) .gt. 0.0 ) then
                                            !Infactibilidad hidraulica violacion vol min.                                    
                                            if ( LinVSCuad .eq. 1 ) then
                                                ibanbit = 1
                                            else
                                                ibanbit = 2
                                            end if
                                            ierror = 0
                                                                
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                                
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO105 INFACTIBILIDAD HIDRAULICA: DEFICIT'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                            
                                            write ( aaux, 5100 ) embalse
                                            write ( aaux_1, 5101 ) nomemb ( embalse )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            !write ( aaux_3, 5102 ) ( VolMnEmb ( embalse ) - vol_util_res ( embalse, intervalo ) ) / 1000.0
                                            write ( aaux_3, 5102 ) DefiAguEmb ( embalse, intervalo )
                                            write ( aaux_5, 5101 ) dia_text
                                                                                
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO105 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                          
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO105 NO. VIOLACION: '//aaux_3//' MMC'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                            
                                            !vol_util_res ( embalse, intervalo ) = VolMnEmb ( embalse )
                                        end if
                                        if ( vol_util_res ( embalse, intervalo ) .gt. VolMxEmb ( embalse ) ) then
                                            vol_util_res ( embalse, intervalo ) = VolMxEmb ( embalse )
                                        end if                                        
                                        if ( ExceAguEmb ( embalse, intervalo ) .gt. 0.0 ) then
                                            !Infactibilidad hidraulica violacion vol max.                                    
                                            if ( LinVSCuad .eq. 1 ) then
                                                ibanbit = 1
                                            else
                                                ibanbit = 2
                                            end if
                                            ierror = 0
                                                                    
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO105 INFACTIBILIDAD HIDRAULICA: EXCEDENTE'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                                
                                            write ( aaux, 5100 ) embalse
                                            write ( aaux_1, 5101 ) nomemb ( embalse )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            !write ( aaux_3, 5102 ) ( vol_util_res ( embalse, intervalo ) - VolMxEmb ( embalse ) ) / 1000.0
                                            write ( aaux_3, 5102 ) ExceAguEmb ( embalse, intervalo )
                                            write ( aaux_5, 5101 ) dia_text
                                                                                    
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO105 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                                                              
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO105 NO. VIOLACION: '//aaux_3//' MMC'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                            
                                            !vol_util_res ( embalse, intervalo ) = VolMxEmb ( embalse )
                                        end if                                                    
                                    end if
                                    
                                    WRITE ( 522, 101 ) '                           |   |   Aportacion    :', Aporta / 1000.00, 'MMC'                         
                                    WRITE ( 522, 101 ) '                           |   |                  '
                                    write ( 522, 103 ) 'Vol.Util:', VolEmb ( embalse, intervalo ) / 1000.00, 'MMC','     /\   |'!vol_util_res
                                    write ( 522, 103 ) '         ', 0.0, '   ','    /  \  /'
                                    write ( 522, 103 ) 'Turbinad:', turb_resl / 1000.00, 'MMC','   /    \/ '!turb_res
                                    write ( 522, 104 ) '         ', 0.0, '   ','  /', nomemb ( embalse ), embalse, '\'                                    
                                    write ( 522, 105 ) 'Energia :', ener_resl / 10.00, 'GWH', ' /        \'!ener_res
                                    write ( 522, 106 ) '         ', 0.0, '   ', '---------- )', 'Vertido programado:', vertdo ( DNVIOU ( embalse ), intervalo ) / 1000.00, 'MMC' 
                                    write ( 522, 107 ) '                           |    /'
                                    !Imprime via divergente
                                    write ( 522, 109 ) NOMVIA ( embalse ), '                      |'
                                    write ( 522, 108 ) '                           |'
                                    write ( 522, 116 ) 'Gasto    :', ( turb_resl + VERTDO ( viad, intervalo ) ) / 1000.00, 'MMC', ' |'!turb_res	
                                    write ( 522, 116 ) '          ', 0.0, '   ', ' |'
                                    write ( 522, 118 ) 'T.V.A:', TiViAgu ( viad ), 'hr', '     |'
                                    localidad_3 = localidad_3 + 1 
                                    hexa = intervalo / 6.0
                                    dummy = abs ( hexa - int ( hexa ) )
                                    if ( dummy .gt. 0.0 ) then
                                        int_hexa = int ( hexa ) + 1
                                    else 
                                        int_hexa = int ( hexa ) 
                                    end if
                                    !Aportacion de embalses arriba Considera el tiempo de viaje del agua
                                    !ApoVaUp ( embalse, intervalo ) =  ApoVaUp ( embalse, intervalo ) + &
                                    !                                                   a_tra_res ( intervalo, viac ) - vertdo ( viac, intervalo ) 
                                    !turbinado + vertido 
                                    ApoVaUp ( embalse, intervalo ) =  ApoVaUp ( embalse, intervalo ) + &
                                                                                       a_tra_res ( intervalo, viac ) - vertdo ( viac, intervalo ) + &
                                                                                       AguaEnViaje ( embalse, intervalo ) 
                                    continue
                                    exit                    
                                end if
                            end do
                        end if                    
                    end if 
                end do
            end do
        end do
        ninter = ninter + intdia ( dia )            
    end do
end do

WRITE ( 522 , * ) ' '
WRITE ( 522 , * ) '************************************************************************'    
write ( 522, * ) 'MINIMOS Y MAXIMOS DE POLITICAS DE OPERACION'
WRITE ( 522 , * ) '************************************************************************'
WRITE ( 522 , * ) ' '   

!Hacer para todos los embalses
do embalse = 1, NumEmbalses
    !Revisa desviaciones entre linealizacion y evaluacion cuadratica
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
    !Revisa, sin importar la politica si hay desbalance de energia
    if ( abs ( ener_T ( embalse ) - ener_resT ( embalse ) ) .gt. 1e-1 ) then 
        !Ignorar hasta 1 MWH en todo el escenario de planeacion
        !Violacion Politica Energia fija
        solicitado = ener_T ( embalse ) !CALCULADO
        relajado = ener_resT ( embalse ) !EVALUADO
        
        !ibanbit = 2
        !ierror = 0
                            
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' HIDRO106 DIF. LINEAL VS CUADRATICO ENERGIA'
        !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write ( 524, * ) bmensaje
        
        write ( aaux, 5100 ) embalse
        write ( aaux_1, 5101 ) nomemb ( embalse )
        write ( aaux_3, 5102 ) solicitado / 10.0
        write ( aaux_4, 5102 ) relajado / 10.0
                                            
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' HIDRO106 No. '//aaux//' NOMBRE: '//aaux_1 
        !call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
        write ( 524, * ) bmensaje
                                                       
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' HIDRO106 LINEAL: '//aaux_3//'GWH'//' CUADRATICO: '//aaux_4//'GWH'
        !call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )   
        write ( 524, * ) bmensaje
    end if
    
        !Cota final fija
    if ( PoliEmb ( embalse ) .eq. 3 ) then
        if ( abs ( WEmbFin ( embalse ) - vol_util_res ( embalse, ntintr ) ) .gt. 1e-3 ) then 
            !Violacion Politica Cota Final Fija
            if ( WEmbFin ( embalse ) .lt. vol_util_res ( embalse, ntintr ) ) then
                relajado = namino ( embalse ) + ALUTIL ( embalse , vol_util_res ( embalse, ntintr ) ) 
                solicitado = WEmbFin ( embalse )
                !WEmbFin ( embalse ) = vol_util_res ( embalse, ntintr )
            end if
            if ( WEmbFin ( embalse ) .gt. vol_util_res ( embalse, ntintr ) ) then
                relajado = namino ( embalse ) + ALUTIL ( embalse , vol_util_res ( embalse, ntintr ) ) 
                solicitado = WEmbFin ( embalse )
                !WEmbFin ( embalse ) = vol_util_res ( embalse, ntintr )
            end if
                        
            !ibanbit = 2
            !ierror = 0
                            
            !call DiaHoraEP ( intervalo, dia_text, intervaloo )
                            
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' HIDRO106 DIF. LINEAL VS CUADRATICO ALTURA'
            !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( 524, * ) bmensaje
                                                        
            write ( aaux, 5100 ) embalse
            write ( aaux_1, 5101 ) nomemb ( embalse )
            write ( aaux_3, 5102 ) namino ( embalse ) + ALUTIL ( embalse , solicitado ) 
            write ( aaux_4, 5102 ) relajado
                                            
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO106 No. '//aaux//' NOMBRE: '//aaux_1 
            !call Mensaje_AuSeg ( ierror, ibanbit, BMensaje ) 
            write ( 524, * ) bmensaje
                                                       
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO106 LINEAL: '//aaux_3//'m.'//' CUADRATICO: '//aaux_4//'m.'
            !call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
            write ( 524, * ) bmensaje
        end if
    end if

        !Volumen a turbinar fijo
    if ( PoliEmb ( embalse ) .eq. 5 ) then
        if ( abs ( TFijEmb ( embalse ) - turb_resT ( embalse ) ) .gt. 1e-3 ) then
            !Violacion Politica Volumen a turbinar fijo
            if ( TFijEmb ( embalse ).lt. turb_resT ( embalse ) ) then
                relajado = turb_resT ( embalse )
                solicitado = TFijEmb ( embalse )
                !TFijEmb ( embalse ) = turb_resT ( embalse )
            end if
            if ( TFijEmb ( embalse ) .gt. turb_resT ( embalse ) ) then
                relajado = turb_resT ( embalse )
                solicitado = TFijEmb ( embalse )
                !TFijEmb ( embalse ) = turb_resT ( embalse )
            end if
                        
            !ibanbit = 2
            !ierror = 0
                            
            !call DiaHoraEP ( intervalo, dia_text, intervaloo )
                            
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' HIDRO106 DIF. LINEAL VS CUADRATICO VOLUMEN'
            !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( 524, * ) bmensaje
                                                        
            write ( aaux, 5100 ) embalse
            write ( aaux_1, 5101 ) nomemb ( embalse )
            write ( aaux_3, 5102 ) solicitado  / 1000.0
            write ( aaux_4, 5102 ) relajado / 1000.0
                                            
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO106 No. '//aaux//' NOMBRE: '//aaux_1 
            !call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
            write ( 524, * ) bmensaje
                                                       
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO106 LINEAL: '//aaux_3//'MMC'//' CUADRATICO: '//aaux_4//'MMC'
            !call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
            write ( 524, * ) bmensaje
        end if
    end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    if ( LinVSCuad .eq. 1 ) then
        ibanbit = 1
    else
        ibanbit = 2
    end if
    
    !Revisar pol�ticas de operaci�n
    !Cota final fija
    if ( PoliEmb ( embalse ) .eq. 3 ) then
        if ( abs ( WEmbFin ( embalse ) - VolEmb ( embalse, ntintr ) ) .gt. 1e-3 ) then 
            !Violacion Politica Cota Final Fija
            if ( WEmbFin ( embalse ) .lt. VolEmb ( embalse, ntintr ) ) then
                relajado = namino ( embalse ) + ALUTIL ( embalse , VolEmb ( embalse, ntintr ) ) 
                solicitado = WEmbFin ( embalse )
                !WEmbFin ( embalse ) = vol_util_res ( embalse, ntintr )
            end if
            if ( WEmbFin ( embalse ) .gt. VolEmb ( embalse, ntintr ) ) then
                relajado = namino ( embalse ) + ALUTIL ( embalse , VolEmb ( embalse, ntintr ) ) 
                solicitado = WEmbFin ( embalse )
                !WEmbFin ( embalse ) = vol_util_res ( embalse, ntintr )
            end if
                        
            !ibanbit = 1
            ierror = 0
                            
            !call DiaHoraEP ( intervalo, dia_text, intervaloo )
                            
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' HIDRO103 VIOLACION POLITICA 3 CFF'
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                        
            write ( aaux, 5100 ) embalse
            write ( aaux_1, 5101 ) nomemb ( embalse )
            write ( aaux_3, 5102 ) namino ( embalse ) + ALUTIL ( embalse , solicitado ) 
            write ( aaux_4, 5102 ) relajado
                                            
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO103 No. '//aaux//' NOMBRE: '//aaux_1 
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                       
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO103 SOLICITADO: '//aaux_3//'m.'//' CALCULADO: '//aaux_4//'m.'
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
        end if
    end if
    !Generacion fija
    if ( PoliEmb ( embalse ) .eq. 8 ) then
        if ( abs ( EfijEmb ( embalse ) - ener_T ( embalse ) ) .gt. 1e-1 ) then 
            !Ignorar hasta 1 MWH en todo el escenario de planeacion
            !Violacion Politica Energia fija
            if ( EfijEmb ( embalse ) .lt. ener_resT ( embalse ) ) then
                relajado = ener_T ( embalse ) 
                solicitado = EfijEmb ( embalse )
                !EfijEmb ( embalse ) = ener_resT ( embalse ) 
            end if
            if ( EfijEmb ( embalse ) .gt. ener_resT ( embalse ) ) then
                relajado = ener_T ( embalse ) 
                solicitado = EfijEmb ( embalse )
                !EfijEmb ( embalse ) = ener_resT ( embalse ) 
            end if
                        
            !ibanbit = 1
            ierror = 0
                            
            !call DiaHoraEP ( intervalo, dia_text, intervaloo )
                            
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' HIDRO103 VIOLACION POLITICA 4 EF'
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                        
            write ( aaux, 5100 ) embalse
            write ( aaux_1, 5101 ) nomemb ( embalse )
            write ( aaux_3, 5102 ) solicitado / 10.0
            write ( aaux_4, 5102 ) relajado / 10.0
                                            
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO103 No. '//aaux//' NOMBRE: '//aaux_1 
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                       
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO103 SOLICITADO: '//aaux_3//'GWH'//' CALCULADO: '//aaux_4//'GWH'
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
        end if
    end if
    !Volumen a turbinar fijo
    if ( PoliEmb ( embalse ) .eq. 5 ) then
        if ( abs ( TFijEmb ( embalse ) - turb_T ( embalse ) ) .gt. 1e-3 ) then
            !Violacion Politica Volumen a turbinar fijo
            if ( TFijEmb ( embalse ).lt. turb_T ( embalse ) ) then
                relajado = turb_T ( embalse )
                solicitado = TFijEmb ( embalse )
                !TFijEmb ( embalse ) = turb_resT ( embalse )
            end if
            if ( TFijEmb ( embalse ) .gt. turb_T ( embalse ) ) then
                relajado = turb_T ( embalse )
                solicitado = TFijEmb ( embalse )
                !TFijEmb ( embalse ) = turb_resT ( embalse )
            end if
                        
            !ibanbit = 1
            ierror = 0
                            
            !call DiaHoraEP ( intervalo, dia_text, intervaloo )
                            
            Call FechaEjecucion (fecha_Ej)
            bmensaje = fecha_Ej//' HIDRO103 VIOLACION POLITICA 5 VTF'
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                        
            write ( aaux, 5100 ) embalse
            write ( aaux_1, 5101 ) nomemb ( embalse )
            write ( aaux_3, 5102 ) solicitado  / 1000.0
            write ( aaux_4, 5102 ) relajado / 1000.0
                                            
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO103 No. '//aaux//' NOMBRE: '//aaux_1 
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                       
            Call FechaEjecucion (fecha_Ej)
            BMensaje = fecha_Ej//' HIDRO103 SOLICITADO: '//aaux_3//'MMC'//' CALCULADO: '//aaux_4//'MMC'
            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        end if
    end if
!Escribe validaciones de politicas por embalse
    WRITE ( 522 , * ) '--------------------------------------------------------------------------'
    WRITE ( 522 , 100 ) ' EMBALSE NO:', embalse, ' NOMBRE:', NOMEMB ( embalse ), ' VALIDACION QUAD' 
    WRITE ( 522 , * ) '--------------------------------------------------------------------------'
    WRITE ( 522 , * ) '      Politica                Min.    Max.'
    write ( 522, 121 ) 'Maxima Extraccion  (MMC): 1', vol_util_res ( embalse, ntintr ) / 1000.0
    write ( 522, 121 ) 'Minima Extraccion  (MMC): 2', vol_util_res ( embalse, ntintr ) / 1000.0
    write ( 522, 121 ) 'Cota Fija Final      (m): 3', namino ( embalse ) + ALUTIL ( embalse , vol_util_res ( embalse, ntintr ) ), namino ( embalse ) + ALUTIL ( embalse , vol_util_res ( embalse, ntintr ) )
    write ( 522, 121 ) 'Generacion Fija    (GWH): 4', ener_resT ( embalse ) / 10.0, ener_resT ( embalse ) / 10.0 
    write ( 522, 121 ) 'Vol. Turbinar Fijo (MMC): 5', turb_resT ( embalse ) / 1000.0, turb_resT ( embalse ) / 1000.0
    write ( 522, 121 ) ''
end do


!close ( unit = 522 )

! escribe solucion de volumenes almacenados
write ( 96, * ) 
if ( LinVSCuad .eq. 1 ) then
    !Evaluacion del problema de despacho   
    write ( 96, * ) 'Evaluacion cuadratica del problema de despacho'
else
    !Evaluacion del problema de asignacion   
    write ( 96, * ) 'Evaluacion cuadratica del problema de asignacion'
end if

ninter = 1
do dia = 1 , DURDIA
   write ( 96, * )
   write ( 96, * ) 'Dia :', dia
   write ( 96, * )
!  Intervalo final del dia
   IntIni = ninter + intdia ( dia ) - 1
!  Escribe solucion de volumenes
   do embalse = 1, NumEmbalses
!     para todos los intervalos de planeacion
      do i = ninter, IntIni
        volumen ( embalse, i ) = vol_util_res ( embalse, i ) / 1000.0
      end do
      write ( 96, 600 ) embalse, NOMEMB(embalse), ( vol_util_res ( embalse, i )/1000.0, i=ninter, IntIni )
   enddo
   write ( 96, * )
    ninter = ninter + intdia ( dia )
enddo

! escribe solucion de alturas en metros de los embalses
write ( 118, * ) 
if ( LinVSCuad .eq. 1 ) then
    !Evaluacion del problema de despacho   
    write ( 118, * ) 'Evaluacion cuadratica del problema de despacho'
else
    !Evaluacion del problema de asignacion   
    write ( 118, * ) 'Evaluacion cuadratica del problema de asignacion'
end if

ninter = 1
do dia = 1 , DURDIA
   write ( 118, * )
   write ( 118, * ) 'Dia :', dia
   write ( 118, * )
!  Intervalo final del dia
   IntIni = ninter + intdia ( dia ) - 1
!  Escribe solucion de volumenes
   do embalse = 1, NumEmbalses
!     para todos los intervalos de planeacion      
      do i = ninter, IntIni
        altura ( embalse, i ) = namino ( embalse ) + ALUTIL ( embalse , vol_util_res ( embalse, i ) )
      end do
      write ( 118, 600 ) embalse, NOMEMB(embalse), ( namino ( embalse ) + ALUTIL ( embalse , vol_util_res ( embalse, i ) ), i=ninter, IntIni )      
   enddo
   write ( 118, * )
   ninter = ninter + intdia ( dia )
enddo

ninter = 1
!Se reporta a *.csv los resultados del despacho lineal NO de la evaluacion cuadratica
do dia = 1 , DURDIA
!  Intervalo final del dia
   IntIni = ninter + intdia ( dia ) - 1
!  Escribe solucion de volumenes
   do embalse = 1, NumEmbalses
!     para todos los intervalos de planeacion
      do i = ninter, IntIni
        volumen ( embalse, i ) = VolEmb ( embalse, i ) / 1000.0
        altura ( embalse, i ) = namino ( embalse ) + ALUTIL ( embalse , VolEmb ( embalse, i ) )
      end do
   enddo
    ninter = ninter + intdia ( dia )
enddo

!Se llama subrutina para crear archivo *.VAS
!call REPORTE_COSTO_AGUA_EMBALSE_HORA ( altura )

!Se llama a la escritura de VASOHE.csv
call ResultVASOHE ( volumen, altura, aa4 )

OPEN ( UNIT = 523, FILE = RUT_RES//'Turb_Ener_Res_Emb.res', IOSTAT = error_h, STATUS='UNKNOWN', RECORDSIZE = 3024 )

!Calcula gastos y energias minimas y maximas respetando DAC y generaciones fijas
       
WRITE ( 523 , * ) ' '     
WRITE ( 523 , * ) '///////////////////////////////////////////////////////////////////////////////////'     
WRITE ( 523 , * ) 'TURBINADOS Y ENERGIA DE LOS EMBALSES: RESULTADO'   
WRITE ( 523 , * ) '\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\'              

ninter = 1
!Hacer para todos los dias del escenario de planeacion
do dia = 1, durdia
    write ( aaux1, 5103 ) dia
    dia_text = 'D'//aaux1

    !Hacer para todas las cuencas
    do cuenca = 1,nmcuen  
        WRITE ( 523 , * ) ' ' 
        WRITE ( 523 , * ) '***********************************************************************************'
        !Imprimir nombre y numero de la cuenca
        WRITE ( 523 , 100 ) ' CUENCA NO:', cuenca, ' NOMBRE:', nomval ( cuenca )    
        WRITE ( 523 , * ) '***********************************************************************************'
        !Hacer para todos los emblases de la cuenca
        do localidad_2 = 0, nmembc ( cuenca ) - 1
            
            q_max_emb = 0.0
            q_min_emb = 0.0
                    
            e_max_emb = 0.0
            e_min_emb = 0.0
            
            embalse = dnemva ( cuenca ) + localidad_2
            !Hacer par a todos los intervalos
            do intervalo = ninter, intdia (dia ) + ninter - 1 
                !q_max_emb = q_max_emb + q_res_embv ( embalse, intervalo )
                !q_min_emb = q_min_emb + q_res_embv ( embalse, intervalo )
                q_max_emb = q_max_emb + q_res_embvl ( embalse, intervalo )
                q_min_emb = q_min_emb + q_res_embvl ( embalse, intervalo )
                !e_max_emb = e_max_emb + e_res_embv ( embalse, intervalo )
                !e_min_emb = e_min_emb + e_res_embv ( embalse, intervalo )
                e_max_emb = e_max_emb + e_res_embvl ( embalse, intervalo )
                e_min_emb = e_min_emb + e_res_embvl ( embalse, intervalo )
            end do
            
            !Imprimir nombre y numero del embalse
            WRITE ( 523 , * ) ' '     
            WRITE ( 523 , * ) '--------------------------------------------------------------------------'
            WRITE ( 523 , 100 ) ' EMBALSE NO:', embalse, ' NOMBRE:', NOMEMB ( embalse ) 
            WRITE ( 523 , * ) '--------------------------------------------------------------------------'
            
            write ( 523, * ) ''
            write ( 523, 114 ) 'Unidad/Planta', '----------------------- Dia', dia_text, '------------------------'
            write ( 523, 111 ) '              ', '-------- Vol. mMC --------  ------- Energia MWh ------'
            write ( 523, 110) '             ', '      Maximo        Minimo', '      Maximo        Minimo' 
            write ( 523, * ) ''           
            
            !Hacer para todas las vias divergentes
            do localidad_3 = 0, nmviasd ( embalse ) - 1 
                via = DNVIOU ( embalse ) + localidad_3 
                !Hacer para todas las plantas que descargan sobre la v�a
                do localidad = dnphvi ( via ), dnphvi ( via ) + nphvi ( via ) - 1
                    q_max_pla = 0.0
                    q_min_pla = 0.0
                    
                    e_max_pla = 0.0
                    e_min_pla = 0.0
                    
                    planta = plantas_x_via ( localidad )                
                    !Hacer para todas las unidades de la planta
                    do localidad_1 = 0, NOUN ( planta ) - 1
                        q_max_uni = 0.0
                        q_min_uni = 0.0
                        
                        e_max_uni = 0.0
                        e_min_uni = 0.0
                        
                        unidad = DNUNPH ( planta ) + localidad_1
                        
                        !Hacer par a todos los intervalos
                        do intervalo = ninter, intdia (dia ) + ninter - 1 
                            q_max_uni = q_max_uni + qgastores ( unidad, intervalo ) * MValor
                            q_min_uni = q_min_uni + qgastores ( unidad, intervalo ) * MValor
                            e_max_uni = e_max_uni + g_res ( unidad, intervalo )
                            e_min_uni = e_min_uni + g_res ( unidad, intervalo )
                        end do
                        q_max_pla = q_max_pla + q_max_uni
                        q_min_pla = q_min_pla + q_min_uni
                        e_max_pla = e_max_pla + e_max_uni
                        e_min_pla = e_min_pla + e_min_uni
                        write ( 523, 113 ) unidad, nombunih ( unidad ), q_max_uni, q_min_uni, e_max_uni, e_min_uni
                    end do
                    write ( 523, 112) '              ', '------------  ------------', '------------  ------------'
                    write ( 523, 113 ) planta, NOMPLAH ( planta ), q_max_pla, q_min_pla, e_max_pla, e_min_pla
                    write ( 523, 112) ' '
                end do
                write ( 523, 112 ) 'TOT. EMBALSE:', '------------  ------------', '------------  ------------'  
                write ( 523, 113 ) embalse, '     ', q_max_emb, q_min_emb, e_max_emb  * basmva, e_min_emb * basmva
            end do
        end do
    end do       
    ninter = ninter + intdia ( dia )
end do

close ( unit = 523 )

do unidad = 1, NumUniHid*0
   do intervalo = 1, NTINTR
      write(50214,*) unidad, intervalo, A_hatd_copy(unidad, intervalo), A_hatd(unidad, intervalo)
      write(50214,*) unidad, intervalo, B_hatd_copy(unidad, intervalo), B_hatd(unidad, intervalo)
      write(50214,*) unidad, intervalo, C_hatd_copy(unidad, intervalo), C_hatd(unidad, intervalo)
   enddo
enddo

close ( unit = 524 )

100 format ( a12, x, i3, 2( a11, x ) )
110 format ( a13, 2x, a26, 2x, a26, 2x )
111 format ( a14, x, a54, 2x ) 
112 format ( a14, x,  a26, 2x, a26, 2x )  
113 format ( x, i3, a15, f12.2, 2x, f12.2, 2x, f12.2, 2x, f12.2, 2x )
114 format ( a13, 2x, a27, x, a2, x, a24, 2x )   

101 format ( a51, x, F7.2, x, a3  )
102 format ( 2x, i2, x, a15, 9x, 5(f9.2, 5x) ) 
122 format ( 2x, i2, x, a15, x, a87 ) 
123 format ( a11, x, i3 )
124 format ( 2x, i2, x, a5, 9x, 6(f9.2, 5x) ) 

103 format ( a9, x, f8.2, x, a3, a11 ) 
104 format ( a9, x, f8.2, x, a3, a3, a3, x, i2, a1)
105 format ( a9, x, f8.2, x, a3, a11)
106 format ( a9, x, f8.2, x, a3, x, a12, x, a19, x, f9.5, x, a3)
107 format (  a34 )
108 format (  a29 )
109 format (  a5, a24 )
115 format (  a4, x, f8.2, x, a1,  a11 )
116 format (  a10, x, f8.2, x, a3,  a6 )
117 format ( a72 )
118 format ( a6, x, i2, x, a2, a17)
119 format ( a32, x, a3, x, a2, x, a32)   
120 format ( a32, x, a2, x, i3, x, a32)  
121 format ( a27, x, f8.2, x, f8.2 )

600 format ( i4, x, a5, 24(f9.2) )

5100 FORMAT (I3)
5101 format (a5)
5102 format (F8.4)
5103 format (I1)

end subroutine res_agua_quad
    

!**************************************************************************
!  Proposito:
!       
!    Calcula los coeficientes de la linealizacion de la funcion de
!    generacion hidro contra gasto y altura para reportar resultados
!
!  Registro de revisiones:
!       Fecha               Programador            Descripcion de cambios
!    ============       ===================       =======================
!     17 Junio   2013    Juan Alvarez Lopez           Codigo original
!
!**************************************************************************      
subroutine Coef_gen_hidro_quad_res ( unidad, intervalo, via )

USE ParAuHeHidro
use ParAUHE, only: bmensaje
use ParGloRed, only: BasMva

implicit none

real*8 altura

integer unidad, via, modelo, ibanbit, ierror, intervalo

CHARACTER fecha_Ej*19

altura = alt_net_des ( via, intervalo )

modelo = MOUNHI ( unidad )
if ( unidad .eq. 5 ) then
    continue
end if
A_hatd ( unidad, intervalo ) = gqc ( 1, modelo ) + gqc ( 2, modelo ) * altura + gqc ( 3, modelo ) * altura * altura
B_hatd ( unidad, intervalo ) = gqc ( 4, modelo ) + gqc ( 5, modelo ) * altura + gqc ( 6, modelo ) * altura * altura
C_hatd ( unidad, intervalo ) = gqc ( 7, modelo ) + gqc ( 8, modelo ) * altura + gqc ( 9, modelo ) * altura * altura
!write(50314,*) unidad, intervalo, altura
if ( C_hatd ( unidad, intervalo ) .gt. 0 ) then
    ibanbit = 1
    ierror = 0
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' CHTAULEC ERROR MODELO GENERACION HIDRO'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   algoritmo no termina bien
    call SalidaError
    stop
end if

end subroutine Coef_gen_hidro_quad_res

!**************************************************************************
!  Proposito:
!       
!    Recalcula los gastos minimos y maximos, generaciones minimas y maximas,
!    y los coeficientes de la funcion de generacion hidro utilizando
!    los gastos y altura de la primer solucion
!
!  Registro de revisiones:
!       Fecha                  Programador            Descripcion de cambios
!    ============          ===================       =======================
!    07 Noviembre   2013    Juan Alvarez Lopez           Codigo original
!
!**************************************************************************      
subroutine Recalcula_Hidro ( bandera )

USE ParAuHeHidro
use ParAUHE, only: bmensaje, durdia, Numembalses, PotMaxUniH_orig, &
                   PotMinUniH_orig, PotMaxUniH, PotMinUniH, DispoUH, intdia, &
                   nombunih

use ParGloRed, only:  BasMva

implicit none

real*8 altura_neta, RGASMX, GASMN, &
       g_min, g_max, volumen_util, ninter

integer unidad, ibanbit, ierror, intervalo, cuenca, &
        dia, consecutivo, localidad_3, localidad_2, embalse, viad,  &
        localidad, planta, unitcount, localidad_5, localidad_7, &
        intervaloo, bandera,  PrevUnit ( maxuh ), cuentah
        
logical entro

character*2 dia_text

character*6 aaux, aaux_1, aaux_2, aaux_3, aaux_4, aaux_5

CHARACTER fecha_Ej*19

if ( bandera .eq. 1 ) then
    !segunda pasada hidro   
    ierror = 0
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej
    write(559, *) Bmensaje !Call Mensaje_AuSeg ( ierror, bandera, BMensaje )
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej
    write(559, *) Bmensaje !Call Mensaje_AuSeg ( ierror, bandera, BMensaje )
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//'          AJUSTES SEGUNDA PASADA HIDRO'
    write(559, *) Bmensaje !Call Mensaje_AuSeg ( ierror, bandera, BMensaje )
else
    !Evaluacion del problema de asignacion   
    ierror = 0
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej
    write(559, *) Bmensaje !Call Mensaje_AuSeg ( ierror, bandera, BMensaje )
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej
    write(559, *) Bmensaje !Call Mensaje_AuSeg ( ierror, bandera, BMensaje )
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//'          AJUSTES PARA PROBLEMA DESPACHO'
    write(559, *) Bmensaje !Call Mensaje_AuSeg ( ierror, bandera, BMensaje )
end if

!Restaurar potencias min y max originales definidas por el usuario
PotMinUniH = PotMinUniH_orig 
PotMaxUniH = PotMaxUniH_orig

PrevUnit = 0
cuentah = 1

!vol_util_res = VolEmb

!Hacer para todas las cuencas
do cuenca = 1,nmcuen
    ninter = 1
    !Hacer para todos los dias
    do dia = 1, durdia
        do intervalo = ninter, intdia (dia ) + ninter - 1 
            consecutivo = 0
            !Hacer para todos los emblases de la cuenca     
            localidad_3 = 0
            entro = .false. 
            do while ( localidad_3 .le. nmembc ( cuenca ) - 1 )
                do localidad_2 = 0, nmembc ( cuenca ) - 1
                    embalse = dnemva ( cuenca ) + localidad_2
                    !Se revisa si tiene via convergente
                    if ( embviaconv ( embalse ) .eq. 0 .and. entro .eq. .false. ) then
                        !Primer embalse de la cuenca
                        !via divergente
                        viad = DNVIOU ( embalse ) 
                        if ( intervalo .eq. 1 ) then
                            !Hacer para todas las plantas que descargan sobre la v�a
                            do localidad = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                 planta = plantas_x_via ( localidad )
                                !Hacer para todas las unidades de la planta
                                do unitcount = 0, NOUN ( planta ) - 1                                
                                    unidad = DNUNPH ( planta ) + unitcount
                                    altura_neta = Alt_net_des ( viad, intervalo )
                                    volumen_util = vmini ( embalse )
                                    !gasto maximo calculado altura neta de la solucion evaluada cuadraticamente
                                    qmxgastodes ( unidad, intervalo ) = RGASMX ( unidad , altura_neta )
                                    !gasto minimo calculado altura neta aproximimada para AU
                                    qmngastodes ( unidad, intervalo ) = GASMN ( unidad , altura_neta )
                                    !Calcular coeficientes de la funcion de generacion para asignacion
                                    if ( volumen_util .gt. VolMxEmb ( embalse ) ) then
                                        volumen_util =  VolMxEmb ( embalse )
                                    end if           
                                    if ( volumen_util .lt. VolMnEmb ( embalse ) ) then
                                        volumen_util =  VolMnEmb ( embalse )
                                    end if          
                                    call Coef_gen_hidro_lineal ( unidad, intervalo, 1, planta, altura_neta, volumen_util ) 
                                    !Revisar si los limites minimos y maximos de generacion definidos estan dentro de los 
                                    !limites de los turbinados minimos y maximos
                                    !Evaluar generacion minima y maxima dependiendo del gasto minimo, maximo CUADRATICO                                    
                                    g_min = C_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) + &
                                                                A_hatd ( unidad, intervalo )
                                    g_max = C_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) + &
                                                                A_hatd ( unidad, intervalo )                                    
                                    !Revisar si hay violacion de limites de generacion definidos comparados con los turbinados min. y max. obtenidos con la altura
                                    !Ambos limites por debajo
                                    if ( PotMinUniH ( unidad, intervalo ) .lt. g_min .and. &
                                         PotMaxUniH ( unidad, intervalo ) .lt. g_min .and. &
                                         DispoUH ( unidad, intervalo ) .eq. 1 .and. &
                                         g_min .ne. 0.0 ) then
                                         PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                         if ( PrevUnit ( unidad ) .eq. 1 .and. bandera .eq. 1) then
                                            ibanbit = 1
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_min                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )        
                                                                 
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_min   
                                            PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )  
                                        
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje ) 
                                            
                                            ibanbit = 0
                                            ierror = 0
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )           
                                        else
                                            ibanbit = 2
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_min                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )        
                                                                 
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_min   
                                            PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )  
                                        
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje ) 
                                        end if
                                    end if
                                    
                                    !Ambos limites por arriba
                                    if ( PotMinUniH ( unidad, intervalo ) .gt. g_max .and. &
                                        PotMaxUniH ( unidad, intervalo ) .gt. g_max  .and. &
                                        DispoUH ( unidad, intervalo ) .eq. 1 .and. &
                                        g_max .ne. 0.0 ) then
                                        PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                        if (  PrevUnit ( unidad ) .eq. 1 .and. bandera .eq. 1 ) then
                                            ibanbit = 1
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_max                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                 
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                            PotMaxUniH ( unidad, intervalo ) = g_max    
                                        
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )       
                                            
                                            ibanbit = 0
                                            ierror = 0
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )           
                                        else
                                            ibanbit = 2
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_max                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                 
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                            PotMaxUniH ( unidad, intervalo ) = g_max    
                                        
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )       
                                        end if                            
                                    end if
                                    
                                    !Potencia minima por debajo
                                    if ( PotMinUniH ( unidad, intervalo ) .lt. g_min ) then
                                        !Redefinir potencia maxima
                                        PotMinUniH ( unidad, intervalo ) = g_min
                                    end if
                                    
                                    !Potencia maxima por arriba
                                    if ( PotMaxUniH ( unidad, intervalo ) .gt. g_max ) then
                                        !Redefinir potencia maxima
                                        PotMaxUniH ( unidad, intervalo ) = g_max
                                    end if
                                    !Redefinir los gastos minimos y maximos dependiendo de las generaciones definidas por el usuario ya validadas lineal
                                    !if ( qgastores ( unidad, intervalo ) .eq. 0.0 ) then
                                    !    qmxgastodes ( unidad, intervalo ) = 0.0
                                    !    qmngastodes ( unidad, intervalo ) = 0.0
                                    !    PotMaxUniH ( unidad, intervalo ) = 0.0
                                    !    PotMinUniH ( unidad, intervalo ) = 0.0
                                    !    CLinGLHW ( unidad, intervalo ) = 0.0
                                    !    CIndGLHWQ ( unidad, intervalo ) = 0.0
                                    !    CLInGLHQ ( unidad, intervalo ) = 0.0
                                    !else
                                        qmxgastodes ( unidad, intervalo ) = ( PotMaxUniH ( unidad, intervalo ) / BasMva - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                        qmngastodes ( unidad, intervalo ) = ( PotMinUniH ( unidad, intervalo ) / BasMva - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                    !end if
                                end do
                            end do                       
                        else
                            !Hacer para todas las plantas que descargan sobre la v�a
                            do localidad = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                planta = plantas_x_via ( localidad )
                                !Hacer para todas las unidades de la planta
                                do unitcount = 0, NOUN ( planta ) - 1                                
                                    unidad = DNUNPH ( planta ) + unitcount
                                    altura_neta = Alt_net_des ( viad, intervalo )
                                    volumen_util = vol_util_res ( embalse, intervalo - 1 )
                                    !gasto maximo calculado altura neta aproximimada para AU
                                    qmxgastodes ( unidad, intervalo ) = RGASMX ( unidad , altura_neta )
                                    !gasto minimo calculado altura neta aproximimada para AU
                                    qmngastodes ( unidad, intervalo ) = GASMN ( unidad , altura_neta )
                                    !Calcular coeficientes de la funcion de generacion para despacho usando altura promedio
                                    if ( volumen_util .gt. VolMxEmb ( embalse ) ) then
                                        volumen_util =  VolMxEmb ( embalse )
                                    end if           
                                    if ( volumen_util .lt. VolMnEmb ( embalse ) ) then
                                        volumen_util =  VolMnEmb ( embalse )
                                    end if          
                                    call Coef_gen_hidro_lineal ( unidad, intervalo, 1, planta, altura_neta, volumen_util ) 
                                    !Revisar si los limites minimos y maximos de generacion definidos estan dentro de los 
                                    !limites de los turbinados minimos y maximos
                                    !Evaluar generacion minima y maxima dependiendo del gasto minimo, maximo y altura CUADRATICO                                    
                                    g_min = C_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) + &
                                                                A_hatd ( unidad, intervalo )
                                    g_max = C_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) ** 2 + &
                                                                B_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) + &
                                                                A_hatd ( unidad, intervalo )                 
                                    !Revisar si hay violacion de limites de generacion definidos comparados con los turbinados min. y max. obtenidos con la altura
                                    !Ambos limites por debajo
                                    if ( PotMinUniH ( unidad, intervalo ) .lt. g_min .and. &
                                         PotMaxUniH ( unidad, intervalo ) .lt. g_min  .and. &
                                         DispoUH ( unidad, intervalo ) .eq. 1 .and. &
                                         g_min .ne. 0.0 ) then
                                        PrevUnit ( unidad) = PrevUnit ( unidad)  + 1
                                        if (  PrevUnit ( unidad) .eq. 1 .and. bandera .eq. 1 ) then
                                            ibanbit = 1
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_min                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                                         
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_min   
                                            PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )    
                                        
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                            
                                            ibanbit = 0
                                            ierror = 0
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )           
                                        else
                                            ibanbit = 2
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_min                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                                         
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_min   
                                            PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )    
                                        
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                        end if
                                    end if
                                    
                                    !Ambos limites por arriba
                                    if ( PotMinUniH ( unidad, intervalo ) .gt. g_max .and. &
                                         PotMaxUniH ( unidad, intervalo ) .gt. g_max  .and. &
                                         DispoUH ( unidad, intervalo ) .eq. 1 .and. &
                                         g_max .ne. 0.0 ) then
                                        PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                        if ( PrevUnit ( unidad )  .eq. 1 .and. bandera .eq. 1 ) then
                                            ibanbit = 1
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_max                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                        
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                            PotMaxUniH ( unidad, intervalo ) = g_max    
                                        
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                            
                                            ibanbit = 0
                                            ierror = 0
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )           
                                        else
                                            ibanbit = 2
                                            ierror = 0
                                        
                                            call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                    
                                            write ( aaux, 5100 ) unidad
                                            write ( aaux_1, 5101 ) nombunih ( unidad )
                                            write ( aaux_2, 5100 ) intervaloo 
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) g_max                                        
                                            write ( aaux_5, 5101 ) dia_text
                                                        
                                            Call FechaEjecucion (fecha_Ej) 
                                            BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                   
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                        
                                            !Redefinir potencia minima y maxima
                                            PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                            PotMaxUniH ( unidad, intervalo ) = g_max    
                                        
                                            write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                            write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                        
                                            Call FechaEjecucion (fecha_Ej)
                                            BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                            write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                        end if
                                    end if
                                    
                                    !Potencia minima por debajo
                                    if ( PotMinUniH ( unidad, intervalo ) .lt. g_min ) then
                                        !Redefinir potencia maxima
                                        PotMinUniH ( unidad, intervalo ) = g_min
                                    end if
                                    
                                    !Potencia maxima por arriba
                                    if ( PotMaxUniH ( unidad, intervalo ) .gt. g_max ) then
                                    
                                        !Redefinir potencia maxima
                                        PotMaxUniH ( unidad, intervalo ) = g_max
                                    end if
                                    !Redefinir los gastos minimos y maximos dependiendo de las generaciones definidas por el usuario ya validadas
                                    !if ( qgastores ( unidad, intervalo ) .eq. 0.0 ) then
                                    !    qmxgastodes ( unidad, intervalo ) = 0.0
                                    !    qmngastodes ( unidad, intervalo ) = 0.0
                                    !    PotMaxUniH ( unidad, intervalo ) = 0.0
                                    !    PotMinUniH ( unidad, intervalo ) = 0.0
                                    !    CLinGLHW ( unidad, intervalo ) = 0.0
                                    !    CIndGLHWQ ( unidad, intervalo ) = 0.0
                                    !    CLInGLHQ ( unidad, intervalo ) = 0.0
                                    !else
                                        qmxgastodes ( unidad, intervalo ) = ( PotMaxUniH ( unidad, intervalo ) / BasMva - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                        qmngastodes ( unidad, intervalo ) = ( PotMinUniH ( unidad, intervalo ) / BasMva - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                    !end if
                                end do
                            end do                            
                        end if          
                        localidad_3 = localidad_3 + 1  
                        entro = .true.                
                    else
                        if ( entro .eq. .true. ) then                            
                            !Buscar que embalse tiene la via divergente del embalse aguas arriba como via convergente                             
                            !Hacer para todos los embalses
                            do localidad = 0, NumEmbalses - 1   
                                consecutivo = consecutivo + 1                         
                                if ( viad .eq. viainc ( consecutivo ) ) then 
                                    !Via divergente
                                    viad = DNVIOU ( embalse ) 
                                    if ( intervalo .eq. 1 ) then    
                                        !Hacer para todas las plantas que descargan sobre la v�a
                                        do localidad_5 = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                            planta = plantas_x_via ( localidad_5 )
                                            !Hacer para todas las unidades de la planta
                                            do unitcount = 0, NOUN ( planta ) - 1                                
                                                unidad = DNUNPH ( planta ) + unitcount
                                                altura_neta = Alt_net_des ( viad, intervalo )
                                                volumen_util = vmini ( embalse )
                                                !gasto maximo calculado altura neta aproximimada para AU
                                                qmxgastodes ( unidad, intervalo ) = RGASMX ( unidad , altura_neta )
                                                !gasto minimo calculado altura neta aproximimada para AU
                                                qmngastodes ( unidad, intervalo ) = GASMN ( unidad , altura_neta )
                                                !Calcular coeficientes de la funcion de generacion para despacho
                                                if ( volumen_util .gt. VolMxEmb ( embalse ) ) then
                                                    volumen_util =  VolMxEmb ( embalse )
                                                end if           
                                                if ( volumen_util .lt. VolMnEmb ( embalse ) ) then
                                                    volumen_util =  VolMnEmb ( embalse )
                                                end if          
                                                call Coef_gen_hidro_lineal ( unidad, intervalo, 1, planta, altura_neta, volumen_util ) 
                                                !Revisar si los limites minimos y maximos de generacion definidos estan dentro de los 
                                                !limites de los turbinados minimos y maximos
                                                !Evaluar generacion minima y maxima dependiendo del gasto minimo, maximo y altura CUADRATICO                                    
                                                g_min = C_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) ** 2 + &
                                                                            B_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) + &
                                                                            A_hatd ( unidad, intervalo )
                                                g_max = C_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) ** 2 + &
                                                                            B_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) + &
                                                                            A_hatd ( unidad, intervalo )                 
                                                !Revisar si hay violacion de limites de generacion por turbinados min. y max. y definidos
                                                !Ambos limites por debajo
                                                if ( PotMinUniH ( unidad, intervalo ) .lt. g_min .and. &
                                                     PotMaxUniH ( unidad, intervalo ) .lt. g_min  .and. &
                                                    DispoUH ( unidad, intervalo ) .eq. 1 .and. &
                                                    g_min .ne. 0.0 ) then
                                                    PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                                    if ( PrevUnit ( unidad ) .eq. 1 .and. bandera .eq. 1 ) then
                                                        ibanbit = 1
                                                        ierror = 0
                                                    
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_min                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_min   
                                                        PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )    
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                        
                                                        ibanbit = 0
                                                        ierror = 0
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                    else
                                                        ibanbit = 2
                                                        ierror = 0
                                                    
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_min                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_min   
                                                        PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )    
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                    end if
                                                end if
                                                
                                                !Ambos limites por arriba
                                                if ( PotMinUniH ( unidad, intervalo ) .gt. g_max .and. &
                                                     PotMaxUniH ( unidad, intervalo ) .gt. g_max  .and. &
                                                    DispoUH ( unidad, intervalo ) .eq. 1 .and. &
                                                    g_max .ne. 0.0 ) then
                                                    PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                                    if ( PrevUnit ( unidad ) .eq. 1 .and. bandera .eq. 1 ) then
                                                        ibanbit = 1
                                                        ierror = 0
                                                    
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_max                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                                        PotMaxUniH ( unidad, intervalo ) = g_max    
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                        
                                                        ibanbit = 0
                                                        ierror = 0
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )          
                                                    else
                                                        ibanbit = 2
                                                        ierror = 0
                                                    
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_max                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                                        PotMaxUniH ( unidad, intervalo ) = g_max    
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                    end if
                                                end if
                                                
                                                !Potencia minima por debajo
                                                if ( PotMinUniH ( unidad, intervalo ) .lt. g_min ) then
                                                    !Redefinir potencia maxima
                                                    PotMinUniH ( unidad, intervalo ) = g_min
                                                end if
                                                
                                                !Potencia maxima por arriba
                                                if ( PotMaxUniH ( unidad, intervalo ) .gt. g_max ) then                                                
                                                    !Redefinir potencia maxima
                                                    PotMaxUniH ( unidad, intervalo ) = g_max
                                                end if
                                                !Redefinir los gastos minimos y maximos dependiendo de las generaciones definidas por el usuario ya validadas
                                                !if ( qgastores ( unidad, intervalo ) .eq. 0.0 ) then
                                                !    qmxgastodes ( unidad, intervalo ) = 0.0
                                                !    qmngastodes ( unidad, intervalo ) = 0.0
                                                !    PotMaxUniH ( unidad, intervalo ) = 0.0
                                                !    PotMinUniH ( unidad, intervalo ) = 0.0
                                                !    CLinGLHW ( unidad, intervalo ) = 0.0
                                                !    CIndGLHWQ ( unidad, intervalo ) = 0.0
                                                !    CLInGLHQ ( unidad, intervalo ) = 0.0
                                                !else
                                                    qmxgastodes ( unidad, intervalo ) = ( PotMaxUniH ( unidad, intervalo ) / BasMva - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                                    qmngastodes ( unidad, intervalo ) = ( PotMinUniH ( unidad, intervalo ) / BasMva - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                                !end if
                                            end do
                                         end do                                    
                                    else                                        
                                        !Hacer para todas las plantas que descargan sobre la v�a
                                        do localidad_7 = dnphvi ( viad ), dnphvi ( viad ) + nphvi ( viad ) - 1
                                            planta = plantas_x_via ( localidad_7 )
                                            !Hacer para todas las unidades de la planta
                                            do unitcount = 0, NOUN ( planta ) - 1                                
                                                unidad = DNUNPH ( planta ) + unitcount
                                                altura_neta = Alt_net_des ( viad, intervalo )
                                                volumen_util = vol_util_res ( embalse, intervalo - 1 )
                                                !gasto maximo calculado altura neta aproximimada para AU
                                                qmxgastodes ( unidad, intervalo ) = RGASMX ( unidad , altura_neta )
                                                !gasto minimo calculado altura neta aproximimada para AU
                                                qmngastodes ( unidad, intervalo ) = GASMN ( unidad , altura_neta )
                                                !Calcular coeficientes de la funcion de generacion para despacho usando altura promedio
                                                if ( volumen_util .gt. VolMxEmb ( embalse ) ) then
                                                    volumen_util =  VolMxEmb ( embalse )
                                                end if           
                                                if ( volumen_util .lt. VolMnEmb ( embalse ) ) then
                                                    volumen_util =  VolMnEmb ( embalse )
                                                end if          
                                                call Coef_gen_hidro_lineal ( unidad, intervalo, 1, planta, altura_neta, volumen_util )  
                                                !Revisar si los limites minimos y maximos de generacion definidos estan dentro de los 
                                                !limites de los turbinados minimos y maximos
                                                !Evaluar generacion minima y maxima dependiendo del gasto minimo, maximo y altura CUADRATICO                                    
                                                g_min = C_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) ** 2 + &
                                                                            B_hatd ( unidad, intervalo ) * qmngastodes ( unidad, intervalo ) + &
                                                                            A_hatd ( unidad, intervalo )
                                                g_max = C_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) ** 2 + &
                                                                            B_hatd ( unidad, intervalo ) * qmxgastodes ( unidad, intervalo ) + &
                                                                            A_hatd ( unidad, intervalo )                 
                                                !Revisar si hay violacion de limites de generacion por turbinados min. y max. y definidos
                                                !Ambos limites por debajo
                                                if ( PotMinUniH ( unidad, intervalo ) .lt. g_min .and. &
                                                     PotMaxUniH ( unidad, intervalo ) .lt. g_min  .and. &
                                                    DispoUH ( unidad, intervalo ) .eq. 1 .and. &
                                                    g_min .ne. 0.0 ) then
                                                    PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                                    if ( PrevUnit ( unidad ) .eq. 1 .and. bandera .eq. 1 ) then
                                                        ibanbit = 1
                                                        ierror = 0
                                                    
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_min                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_min   
                                                        PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )   
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                        
                                                        ibanbit = 0
                                                        ierror = 0
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )         
                                                    else
                                                        ibanbit = 2
                                                        ierror = 0
                                                    
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX < GMIN'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMaxUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_min                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MAX.: '//aaux_3//' MINIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_min   
                                                        PotMaxUniH ( unidad, intervalo ) = g_min + 0.1 * ( g_max - g_min )   
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                    end if
                                                end if
                                                
                                                !Ambos limites por arriba
                                                if ( PotMinUniH ( unidad, intervalo ) .gt. g_max .and. &
                                                     PotMaxUniH ( unidad, intervalo ) .gt. g_max  .and. &
                                                    DispoUH ( unidad, intervalo ) .eq. 1 .and. &
                                                    g_max .ne. 0.0 ) then
                                                    PrevUnit ( unidad ) = PrevUnit ( unidad ) + 1
                                                    if ( PrevUnit ( unidad ) .eq. 1 .and. bandera .eq. 1 ) then
                                                        ibanbit = 1
                                                        ierror = 0
                                                    
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_max                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                                        PotMaxUniH ( unidad, intervalo ) = g_max    
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )  
                                                        
                                                        ibanbit = 0
                                                        ierror = 0
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//'          MAS MENSAJES EN BITACORA'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )       
                                                    else
                                                        ibanbit = 2
                                                        ierror = 0
                                                    
                                                        call DiaHoraEP ( intervalo, dia_text, intervaloo )
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        bmensaje = fecha_Ej//' HIDRO101 PMIN Y PMAX > GMAX'
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                                                
                                                        write ( aaux, 5100 ) unidad
                                                        write ( aaux_1, 5101 ) nombunih ( unidad )
                                                        write ( aaux_2, 5100 ) intervaloo 
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) g_max                                        
                                                        write ( aaux_5, 5101 ) dia_text
                                                                    
                                                        Call FechaEjecucion (fecha_Ej) 
                                                        BMensaje = fecha_Ej//' HIDRO101 No. '//aaux//' NOMBRE: '//aaux_1//' DIA '//aaux_5//' HR '//aaux_2
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )      
                                                                               
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 SOLI. MIN.: '//aaux_3//' MAXIMO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    
                                                        !Redefinir potencia minima y maxima
                                                        PotMinUniH ( unidad, intervalo ) = g_max - 0.1 * ( g_max - g_min )   
                                                        PotMaxUniH ( unidad, intervalo ) = g_max    
                                                    
                                                        write ( aaux_3, 5102 ) PotMinUniH ( unidad, intervalo )
                                                        write ( aaux_4, 5102 ) PotMaxUniH ( unidad, intervalo )                       
                                                    
                                                        Call FechaEjecucion (fecha_Ej)
                                                        BMensaje = fecha_Ej//' HIDRO101 MIN. RELAJADO: '//aaux_3//' MAX. RELAJADO: '//aaux_4
                                                        write(559,*) Bmensaje !Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                                    end if
                                                end if
                                                
                                                !Potencia minima por debajo
                                                if ( PotMinUniH ( unidad, intervalo ) .lt. g_min ) then
                                                    !Redefinir potencia maxima
                                                    PotMinUniH ( unidad, intervalo ) = g_min
                                                end if
                                                
                                                !Potencia maxima por arriba
                                                if ( PotMaxUniH ( unidad, intervalo ) .gt. g_max ) then                                                
                                                    !Redefinir potencia maxima
                                                    PotMaxUniH ( unidad, intervalo ) = g_max
                                                end if
                                                !Redefinir los gastos minimos y maximos dependiendo de las generaciones definidas por el usuario ya validadas
                                                !if ( qgastores ( unidad, intervalo ) .eq. 0.0 ) then
                                                !    qmxgastodes ( unidad, intervalo ) = 0.0
                                                !    qmngastodes ( unidad, intervalo ) = 0.0
                                                !    PotMaxUniH ( unidad, intervalo ) = 0.0
                                                !    PotMinUniH ( unidad, intervalo ) = 0.0
                                                !    CLinGLHW ( unidad, intervalo ) = 0.0
                                                !    CIndGLHWQ ( unidad, intervalo ) = 0.0
                                                !    CLInGLHQ ( unidad, intervalo ) = 0.0
                                                !else
                                                    qmxgastodes ( unidad, intervalo ) = ( PotMaxUniH ( unidad, intervalo ) / BasMva - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                                    qmngastodes ( unidad, intervalo ) = ( PotMinUniH ( unidad, intervalo ) / BasMva - CIndGLH ( unidad, intervalo ) ) / CLinGLH ( unidad, intervalo )
                                                !end if
                                            end do
                                        end do      
                                    end if             
                                    localidad_3 = localidad_3 + 1 
                                    exit
                                end if
                            end do
                        end if                    
                    end if 
                end do
            end do             
        end do
        ninter = ninter + intdia ( dia )
    end do
end do

!Escalar
PotMinUniH = PotMinUniH / BasMVA
PotMaxUniH = PotMaxUniH / BasMVA

5100 FORMAT (I3)
5101 format (a5)
5102 format (F6.2)

end subroutine Recalcula_Hidro
  
    

