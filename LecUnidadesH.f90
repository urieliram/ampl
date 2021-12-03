        
        
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
!         Lee datos de generadores y sistema hidro                        * 
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Enero 2015                            *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                   Julio 2017                            *
!**************************************************************************
    
SUBROUTINE data_hidro

use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, NumNodos, nodo_subsis, nomsis, EstadoIsla, NumUniHid, &
                  IslaGenH, nodo_area, unidadh_area, proph, nodoh, corresph, numsis, nombunih, &
!                  NumEmbalses, NumCenHEmb, NOMPLAH, ApunPlantaEmbalse, ListaPlantasH, ApunUnidadPlanta, ListaUnidadesH, &
                  NumEmbalses, NumCenHEmb, ApunPlantaEmbalse, ListaPlantasH, ApunUnidadPlanta, ListaUnidadesH, &
!                  NoUnidades_plantaH, NumPlaH, nmxpla, PotMinUniH, PotMaxUniH, ntintr, base, AsignUH, EstadoCIUH, &
                  nmxpla, PotMinUniH, PotMaxUniH, ntintr, base, AsignUH, EstadoCIUH, &
                  minresup, minresre, RamEmer10H, RamEmerxH, RamRegH, durintr, maxint, CostoOporUH, OferResR10H, PreVenResR10H, &
                  PreVenResRxH, OferResRxH, OferResRegH, PreVenResRegH, OferResNR10H, PreVenResNR10H, OferResNRxH, PreVenResNRxH, &
                  LimIEnerEmb, LimSEnerEmb, RestEnergia, maxgrure, UniGruResH, maxuh, maxint, &
                  NomEjecu, SisUniH, RaOpSupH, RaOpInfH, NoRaOpH, maxzonproh, PotMinUniH_orig, PotMaxUniH_orig, NumMaxParoUH, PotMinRUniH, &
                  PotMaxRUniH, RaRegInfH, RaRegSupH, NoUnidades_plantaH, CompSincH, NoActParH, MrreUH, TipoLec, maxuh
use ProblemaAUHE


use ParAuHeHidro, only: MOUNHI, NOMEMB, NOMPLAH, embapl, NOUN, NPLHID, nuti

IMPLICIT NONE

integer ierror, dummy, tempnodhid ( maxint ), nodo, subsistema, unidad, i, intervalo, modelo, &
        planta, embalse, j, k, ierror_1, ierror_2, bloque, zona, auxiliar ( maxgrure ), contador, temp1, maxpar, &
        iz, sigue,  NuZonProh, errleca, errlecb, errlecc

character*3000 letaux, letaux_1, letaux_2

character*20 nombre, propietario

character*5 letr

real*8 emergencia, regulacion, CostoOporPH ( nmxpla, maxint ), ZonProh ( maxuh, maxzonproh * 2), epsilon,  ZonProhx ( maxuh, maxzonproh * 2)

!Inicializacion de variables
nombre = ''
modelo = 0
propietario = ''
dummy = 0
temp1 = 0
maxpar = 0
tempnodhid = 0
ierror = 0
ierror_1 = 0
i = 0
j = 0
k = 0
unidad = 0
NumUniHid = 0
IslaGenH = 0
unidadh_area = ''
nombunih = ''
proph = ''
corresph = 0
!NOMEMB = ''
NumEmbalses = 0
NumCenHEmb = 0
!NOMPLAH = ''
!NoUnidades_plantaH = 0
PotMinUniH = 0.0
PotMaxUniH = 0.0
CompSincH = 0
bloque = 0
AsignUH = 0
EstadoCIUH = 0
emergencia = 0.0
regulacion = 0.0
RamEmer10H = 0.0
RamEmerxH = 0.0
RamRegH = 0.0
CostoOporPH = 0.0
OferResR10H = 0.0
PreVenResR10H = 0.0
PreVenResRxH = 0.0
OferResRxH = 0.0
OferResRegH = 0.0
PreVenResRegH = 0.0
OferResNR10H = 0.0
PreVenResNR10H = 0.0
LimIEnerEmb = 0.0
LimSEnerEmb = 0.0 
RestEnergia = 0 
auxiliar = 0
UniGruResH = 0
NumMaxParoUH = 0
DispoUH = 0
MrreUH = 0.0
NoActParH = 0


! Se leen datos de unidades hidro
! ---------------------------------
! * Se leen datos de UNIH         *
! * Se leen datos de ZONASRESUH   *
! * Se leen datos de NODOSH       *
! ---------------------------------

OPEN (UNIT = 51, FILE = rut_dat_1( 1 : long_ruta )//'UNIH'//trim(TipoLec)//'.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 71, FILE = rut_dat_1( 1 : long_ruta )//'ZONASRESUH.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 195, FILE = rut_dat_1( 1 : long_ruta )//'NODOSH.csv', IOSTAT = IERROR_2, STATUS='OLD', RECORDSIZE = 250)
!------------------------------------------------------
!* INICIAR EL NUMERO DE modelos distintos existentes  *
!------------------------------------------------------
nuti = 0

write ( 1, 100 ) '-------------------------' 
write ( 1, 100 ) 'UNIDADES HIDROELECTRICAS  ' 
write ( 1, 100 ) '-------------------------' 
errleca = 0; errlecb = 0; errlecc = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 .and. ierror_2 .eq. 0 ) then
    write ( 1, 100 ) 'Unidad   Nombre        Propietario        Nodo     Area     Subsistema   Modelo   Max/Paros' 
!    write ( 1, 100 ) 'Unidad   Nombre        Propietario        Nodo     Area     Subsistema   Modelo   Max/Paros   Prioridad' 
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	    read ( 51, 100, iostat = ierror ) letaux
        read ( 71, 100, iostat = ierror_1 ) letaux_1
        read ( 195, 100, iostat = ierror_2 ) letaux_2
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. len_trim(letaux_1) .ne. 0 .and. len_trim(letaux_2) .ne. 0 ) then
!            read ( letaux, * )  nombre, modelo, propietario !, dummy, tempnodhid
            read ( letaux, *, iostat = errleca )  nombre, modelo, propietario, dummy, temp1, maxpar 
            read ( letaux_1, *, iostat = errlecb )  ( auxiliar ( zona ), zona = 1, maxgrure )
            read ( letaux_2, *, iostat = errlecc )  ( tempnodhid ( intervalo ), intervalo = 1, ntintr )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0 ) go to 1001
            i = i + 1
            !Hacer para todos los nodos
            do nodo = 1, NumNodos
                !se asigna subsistema con el nodo del primer intervalo
                if ( tempnodhid ( 1 ) .eq. nodo ) then
                    !Hacer para todos los subsistemas
                    do subsistema = 1, numsis
                        if ( nodo_subsis ( nodo ) .eq. nomsis ( subsistema ) ) then
                            !Ver si el subsistema esta activo
                            if ( EstadoIsla ( subsistema ) .eq. 1 ) then
                                unidad = unidad + 1
                                if ( i .gt. maxuh ) then
                                      bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
                                      call EnviaMensajeError ( bmensaje )
                                      bmensaje = "DE UNIDADES HIDRO"
                                      call ParaProceso ( bmensaje )
                                endif
                                IslaGenH ( unidad ) = subsistema
                                !se asigna area con el nodo del primer intervalo
                                unidadh_area ( unidad ) = nodo_area ( nodo )
                                nombunih ( unidad ) = nombre
                                !se asigna prioridad de operacion a unidades hidro
!                                PrioridadH ( unidad ) = dummy
                                NumMaxParoUH ( unidad ) = maxpar
                                proph ( unidad ) = propietario                                
                                nodoh ( unidad, : ) = tempnodhid ( : )
                                corresph ( unidad ) = i
                                UniGruResH ( unidad, : ) =  auxiliar
!                               modelo asociado a la unidad
                                MOUNHI ( unidad ) = modelo
                                nuti = MAX0 ( nuti , modelo )
                                NumMaxParoUH ( unidad ) = NumMaxParoUH ( unidad ) !* durdia
                                !se escribe a debugger
                                write ( 1, 200 ) unidad, nombunih ( unidad ), proph ( unidad ), nodoh ( unidad, 1 ), &
                                                 unidadh_area ( unidad ), nomsis ( IslaGenH ( unidad ) ), MOUNHI ( unidad ), NumMaxParoUH ( unidad )
!                                                 unidadh_area ( unidad ), nomsis ( IslaGenH ( unidad ) ), MOUNHI ( unidad ), NumMaxParoUH ( unidad ), PrioridadH ( unidad )
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
     bmensaje = 'ERROR DE LECTURA ARCHIVO UNIH'//trim(TipoLec)//'.csv'
     call EnviaMensajeError ( bmensaje )
   endif 
   if ( ierror_1 .ne. 0 ) then
     bmensaje = 'ERROR DE LECTURA ARCHIVO ZONASRESUH.csv'
     call EnviaMensajeError ( bmensaje )
   endif  
   if ( ierror_2 .ne. 0 ) then
     bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSH.csv'
     call EnviaMensajeError ( bmensaje )
   endif  
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1001 continue

if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0 ) then 
   write ( letr, "(i5)" ) unidad
   call Elimina_blancos ( letr, 5)
   if ( errleca .ne. 0 ) then
     bmensaje = 'ERROR DE LECTURA ARCHIVO UNIH'//trim(TipoLec)//'.csv'
     call EnviaMensajeError ( bmensaje )
   endif 
   if ( errlecb .ne. 0 ) then
     bmensaje = 'ERROR DE LECTURA ARCHIVO ZONASRESUH.csv'
     call EnviaMensajeError ( bmensaje )
   endif  
   if ( errlecc .ne. 0 ) then
     bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSH.csv'
     call EnviaMensajeError ( bmensaje )
   endif  
   Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunih ( unidad ))
   call ParaProceso ( bmensaje )
endif     
     
NumUniHid = unidad

!Numero total de unidades hidro en la base de datos
SisUniH = i

CLOSE ( UNIT = 51 )
close ( UNIT = 195 )



write ( 1, 100 ) ''

write ( 1, 100 ) '------------------------'
write ( 1, 100 ) 'NODOS DE UNIDADES HIDRO'
write ( 1, 100 ) '------------------------'
!Hacer para todoas las unidades HIDRO
do unidad = 1, NumUniHid
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 500 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        write ( 1, 1400 ) unidad, nombunih ( unidad ), ( nodoh ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 500 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        write ( 1, 1400 ) unidad, nombunih ( unidad ), ( nodoh ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
    end if
end do


write ( 1, 100 ) ''

ierror = 0
ierror_1 = 0
i = 0

! Se leen datos de los embalses si es que hay unidades hidro activas
! ---------------------------------
! * Se leen datos de LIMENEREMB     *
! ---------------------------------
errleca = 0
if ( NumUniHid .gt. 0 ) then
    OPEN (UNIT = 70, FILE = rut_dat_1( 1 : long_ruta )//'LIMENEREMB.csv ', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
    write ( 1, 100 ) '---------' 
    write ( 1, 100 ) 'EMBALSES ' 
    write ( 1, 100 ) '---------' 
    if ( ierror .eq. 0 ) then
        write ( 1, 100 ) 'Embalse  Nombre   Ener. Min  Ener. Max    Activa 1(Si)/0(No)' 
        ! Lee información hasta encontrar fin de información
        do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )     
            read ( 70, 100, iostat = ierror ) letaux
	        if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
                i = i + 1
                read ( letaux, *, iostat = errleca )  LimIEnerEmb ( i ), LimSEnerEmb ( i ), RestEnergia ( i )
                if ( errleca .ne. 0 ) go to 1002
                write ( 1, 303 ) i, NOMEMB ( i ), LimIEnerEmb ( i ), LimSEnerEmb ( i ), RestEnergia ( i ) 
	        endif
        enddo
    else	
      bmensaje = 'ERROR DE LECTURA ARCHIVO LIMENEREMB.csv'//trim(TipoLec)//'.csv'
      call EnviaMensajeError ( bmensaje )
      Bmensaje = ''
      call ParaProceso ( bmensaje )
    end if

1002 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 ) then
        write ( letr, "(i5)" ) i
        call Elimina_blancos ( letr, 5)
        bmensaje = 'ERROR DE LECTURA ARCHIVO LIMENEREMB.csv'
        call EnviaMensajeError ( bmensaje )
        Bmensaje = 'EMBALSE: ['//trim(letr)//'] '//trim(NOMEMB ( i ))
       call ParaProceso ( bmensaje )
    endif

    NumEmbalses = i
!    RestEnergia = 1
    !Escalamiento; de GWh a MWh y luego a PU
!    LimIEnerEmb = LimIEnerEmb * ( 1000.0 / base )
!    LimSEnerEmb = LimSEnerEmb * ( 1000.0 / base )


    CLOSE ( UNIT = 52 )


    write ( 1, 100 ) ''

    ierror = 0
    ierror_1 = 0
    i = 0

! Se leen datos de las plantas si es que hay unidades hidro activas
! ---------------------------------
! * Se leen datos de PLAH         *
! * Se leen datos de COSTOPHID    *
! ---------------------------------

!    OPEN (UNIT = 53, FILE = rut_dat_1( 1 : long_ruta )//'PLAH'//trim(TipoLec)//'.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
    OPEN (UNIT = 59, FILE = rut_dat_1( 1 : long_ruta )//'COSTOPHID.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
!    if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
    errleca = 0
    if ( ierror .eq. 0 ) then
!        write ( 1, 100 ) 'Planta    Nombre       Embalse  Unidades x Planta' 
        ! Lee información hasta encontrar fin de información
!        do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim ( letaux_1 ) .ne. 0 )   
        do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )   
!	        read ( 53, 100, iostat = ierror ) letaux
            read ( 59, 100, iostat = ierror ) letaux
!            if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. ierror_1 .eq. 0 .and. len_trim(letaux_1) .ne. 0 ) then
            if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
                i = i + 1
!                read ( letaux, * )  NOMPLAH ( i ), dummy, embalse_planta ( i ), NoUnidades_plantaH ( i )
                read ( letaux, *, iostat = errleca )  ( CostoOporPH ( i, intervalo ), intervalo = 1, ntintr )
                if ( errleca .ne. 0 ) go to 1003
!                write ( 1, 400 ) i, NOMPLAH ( i ), NOMEMB ( embalse_planta ( i ) ), NoUnidades_plantaH ( i )
	        endif
        enddo
    else	
      bmensaje = 'ERROR DE LECTURA ARCHIVO COSTOPHID.csv'
      call EnviaMensajeError ( bmensaje )
      Bmensaje = ''
      call ParaProceso ( bmensaje )
    end if

1003 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 ) then
        write ( letr, "(i5)" ) i
        call Elimina_blancos ( letr, 5)
        if ( errleca .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO COSTOPHID.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        Bmensaje = 'PLANTA: ['//trim(letr)//'] '//trim(NOMPLAH ( i ))
       call ParaProceso ( bmensaje )
    endif

    
    i = 0
    k = 0
    
    !Se calcula el numero de plantas por embalse
    !Se calcula el numero de unidades por planta
    !Hacer para todos los embalses
    ApunPlantaEmbalse ( 1 ) = 1
    ApunUnidadPlanta ( 1 ) = 1
    do embalse = 1, NumEmbalses
        !Hacer para todas las plantas
        do planta = 1, NPLHID
            !Lista de unidades consecutivas de las plantas hidro
            !Hacer para todas las unidades de la planta
            if ( i .lt. NumUniHid ) then
                do unidad = 1, NOUN ( planta )
                    i = i + 1
                    ListaUnidadesH ( i ) = i                
                end do
                ApunUnidadPlanta ( planta + 1 ) = ApunUnidadPlanta ( planta ) + NOUN ( planta )
            end if
            if ( embapl ( planta ) .eq. embalse ) then      
                NumCenHEmb ( embalse ) = NumCenHEmb ( embalse ) + 1
                ApunPlantaEmbalse ( embalse + 1 ) = ApunPlantaEmbalse ( embalse ) + NumCenHEmb ( embalse )
                k = k + 1
                ListaPlantasH ( k ) = planta
            end if
        end do
    end do
        
    CLOSE ( UNIT = 53 )

    write ( 1, 100 ) ''
    
    !Imprimir unidades por planta y por embalse (embalse-->plantas-->unidades-->)
    !Hacer para todos los embalses
    do embalse = 1, NumEmbalses
        epsilon = 1.0d-2
!        epsilon = 0.50
        write ( 1, 100 ) '-------' 
        write ( 1, 100 ) 'EMBALSE'
        write ( 1, 100 ) '-------'
        write ( 1, 300 ) embalse, NOMEMB ( embalse )
        !Hacer para todas la plantas del embalse
        do i = 0, NumCenHEmb ( embalse ) - 1
            write ( 1, 100 ) '   -------' 
            write ( 1, 100 ) '   PLANTA'
            write ( 1, 100 ) '   -------'
            planta = ListaPlantasH ( ApunPlantaEmbalse ( embalse ) + i )
            write ( 1, 301 ) '   ',planta, NOMPLAH ( planta )
            !Hacer para todas las unidades de la panta
            write ( 1, 100 ) '      --------'
            write ( 1, 100 ) '      UNIDADES' 
            write ( 1, 100 ) '      --------'
            do j = 0, NOUN ( planta ) - 1
                unidad = ListaUnidadesH ( ApunUnidadPlanta ( planta ) + j )
                CostoOporUH ( unidad, : ) = CostoOporPH ( planta, : ) + ( j*epsilon ) 
!                CostoOporUH ( unidad, : ) = CostoOporPH ( planta, : ) + (PrioridadH(unidad)-1)*epsilon
                write ( 1, 302 ) '      ', unidad, nombunih ( unidad )
            end do
        end do
    end do
    write ( 1, 100 ) ''
    write ( 1, 100 ) '--------------------------------------'
    write ( 1, 100 ) 'COSTO DE OPORTUNIDAD DE UNIDADES HIDRO'
    write ( 1, 100 ) '--------------------------------------'
    
    !Hacer para todoas las unidades hidro
    do unidad = 1, NumUniHid
        bloque = ntintr / 24
        do k = 1, bloque
            write ( 1, 500 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
            write ( 1, 600 ) unidad, nombunih ( unidad ), '$/MWh', ( CostoOporUH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
            write ( 1, 500 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
            write ( 1, 600 ) unidad, nombunih ( unidad ), '$/MWh', ( CostoOporUH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end if
    end do
    !Escalar por la base
    CostoOporUH = CostoOporUH * base
    write ( 1, 100 ) ''
    ierror = 0
    ierror_1 = 0

    ! ---------------------------
    ! * Se leen datos de LIUNIH *
    ! * Se leen datos de LSUNIH *
    ! -----------------------------
    !Potencia mínima de unidades térmicas, PotMinGRC
    ! -----------------------------------------
    !Potencia máxima de unidades térmicas, PotMaxGRC
    write ( 1, 100 ) '-------------------------------------------'
    write ( 1, 100 ) 'POTENCIAS MINIMA Y MAXIMA DE UNIDADES HIDRO'
    write ( 1, 100 ) '-------------------------------------------'

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 54, FILE = rut_dat_1( 1 : long_ruta )//'LIUNIH.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 55, FILE = rut_dat_1( 1 : long_ruta )//'LSUNIH.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)
    errleca = 0; errlecb = 0; unidad = 0
    if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( unidad .lt. NumUniHid ) 
            unidad = unidad + 1
            letaux = ''; letaux_1 = ''
            read ( 54, 100, iostat = ierror ) letaux
            read ( 55, 100, iostat = ierror_1 ) letaux_1
            read ( letaux, *, iostat = errleca )  ( PotMinUniH ( unidad, intervalo ), intervalo = 1, ntintr )
            read ( letaux_1, *, iostat = errlecb )  ( PotMaxUniH ( unidad, intervalo ), intervalo = 1, ntintr )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1004
            bloque = ntintr / 24
            do k = 1, bloque
               write ( 1, 500 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
               write ( 1, 600 ) unidad, nombunih ( unidad ), 'Min:', ( PotMinUniH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
               write ( 1, 600 ) unidad, nombunih ( unidad ), 'Max:', ( PotMaxUniH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do
            if ( ntintr - 24 * bloque .gt. 0 ) then
              write ( 1, 500 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
              write ( 1, 600 ) unidad, nombunih ( unidad ), 'Min:', ( PotMinUniH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
              write ( 1, 600 ) unidad, nombunih ( unidad ), 'Max:', ( PotMaxUniH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end if
        end do
    else
      if ( ierror .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO LIUNIH.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      if ( ierror_1 .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO LSUNIH.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      
      Bmensaje = ''
      call ParaProceso ( bmensaje )
    end if

1004 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
        write ( letr, "(i5)" ) unidad
        call Elimina_blancos ( letr, 5)
        if ( errleca .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO LIUNIH.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        if ( errlecb .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO LIUNIH.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunih ( unidad ))
       call ParaProceso ( bmensaje )
    endif 
    
    CLOSE ( UNIT = 54 )
    CLOSE ( UNIT = 55 )

    !Escalamiento
!    PotMinUniH = PotMinUniH / Base
!    PotMaxUniH = PotMaxUniH / Base
    PotMinUniH_orig = PotMinUniH
    PotMaxUniH_orig = PotMaxUniH

!   si se desea considerar limites de regulacion
    if ( SiLimReg .eq. 1 ) then

        write ( 1, 100 ) ''
    
        ierror_1 = 0
        ierror = 0
    
        ! ---------------------------
        ! * Se leen datos de LIRUNIH *
        ! * Se leen datos de LSRUNIH *
        ! -----------------------------
        !Potencia mínima de unidades térmicas, PotMinGRC
        ! -----------------------------------------
        !Potencia máxima de unidades térmicas, PotMaxGRC
        write ( 1, 100 ) '----------------------------------------------------------'
        write ( 1, 100 ) 'POTENCIAS MINIMA Y MAXIMA DE REGULACION DE UNIDADES HIDRO'
        write ( 1, 100 ) '----------------------------------------------------------'
        ! Abre archivo de datos de generadores
        OPEN (UNIT = 54, FILE = rut_dat_1( 1 : long_ruta )//'LIRUNIH.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

        ! Abre archivo de datos de generadores
        OPEN (UNIT = 55, FILE = rut_dat_1( 1 : long_ruta )//'LSRUNIH.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
        
    errleca = 0; errlecb = 0; unidad = 0
    if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( unidad .lt. NumUniHid ) 
            unidad = unidad + 1
            !Hacer para todoas las unidades hidro
            letaux = ''; letaux_1 = ''
            read ( 54, 100, iostat = ierror ) letaux
            read ( 55, 100, iostat = ierror_1 ) letaux_1
            read ( letaux, *, iostat = errleca )  ( PotMinRUniH ( unidad, intervalo ), intervalo = 1, ntintr )
            read ( letaux_1, *, iostat = errlecb )  ( PotMaxRUniH ( unidad, intervalo ), intervalo = 1, ntintr )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1005
            bloque = ntintr / 24
            do k = 1, bloque
               write ( 1, 500 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
               write ( 1, 600 ) unidad, nombunih ( unidad ), 'Min:', ( PotMinRUniH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
               write ( 1, 600 ) unidad, nombunih ( unidad ), 'Max:', ( PotMaxRUniH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do
            if ( ntintr - 24 * bloque .gt. 0 ) then
                write ( 1, 500 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
                write ( 1, 600 ) unidad, nombunih ( unidad ), 'Min:', ( PotMinRUniH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                write ( 1, 600 ) unidad, nombunih ( unidad ), 'Max:', ( PotMaxRUniH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end if
         end do
    else	
      if ( ierror .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO LIRUNIH.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      if ( ierror_1 .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO LSRUNIH.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      Bmensaje = ''
      call ParaProceso ( bmensaje )
    end if

1005 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
        write ( letr, "(i5)" ) unidad
        call Elimina_blancos ( letr, 5)
        if ( errleca .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO LIRUNIH.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        if ( errlecb .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO LSRUNIH.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunih ( unidad ))
        call ParaProceso ( bmensaje )
    endif          
        CLOSE ( UNIT = 54 )
        CLOSE ( UNIT = 55 )

        !Escalamiento
!        PotMinRUniH = PotMinRUniH / Base
!        PotMaxRUniH = PotMaxRUniH / Base
    else
        PotMinRUniH = PotMinUniH
        PotMaxRUniH = PotMaxUniH
    endif

    ierror = 0

    ! ---------------------------
    ! * Se leen datos de COMPSH *
    ! -----------------------------
    !Bandera de condensador sincrono, CompSincH
    ! -----------------------------------------
    write ( 1, 100 ) '--------------------------------------------------'
    write ( 1, 100 ) 'BANDERA DE COMPENSADOR SINCRONO DE UNIDADES HIDRO'
    write ( 1, 100 ) '--------------------------------------------------'

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 54, FILE = rut_dat_1( 1 : long_ruta )//'COMPSH.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

    errleca = 0; unidad = 0
    if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( unidad .lt. NumUniHid ) 
            unidad = unidad + 1
            !Hacer para todoas las unidades hidro
            letaux = ''
            read ( 54, 100, iostat = ierror ) letaux
            read ( letaux, *, iostat = errleca )  ( CompSincH ( unidad, intervalo ), intervalo = 1, ntintr )
            if ( errleca .ne. 0 ) go to 1006
            bloque = ntintr / 24
            do k = 1, bloque
               write ( 1, 500 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
               write ( 1, 601 ) unidad, nombunih ( unidad ), 'Ban:', ( CompSincH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
             end do
             if ( ntintr - 24 * bloque .gt. 0 ) then
                write ( 1, 500 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
                write ( 1, 601 ) unidad, nombunih ( unidad ), 'Ban:', ( CompSincH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
             end if
        end do
    else	
         bmensaje = 'ERROR DE LECTURA ARCHIVO COMPSH.csv'
         call EnviaMensajeError ( bmensaje )
         call ParaProceso ( bmensaje )
    end if

1006 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 ) then
        write ( letr, "(i5)" ) unidad
        call Elimina_blancos ( letr, 5)
        if ( errleca .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO COMPSH.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunih ( unidad ))
       call ParaProceso ( bmensaje )
    endif 

    
601 format ( i3, x, a12, x, a12, x, 24 (I3, 2x) )
        
    CLOSE ( UNIT = 54 )
1111 continue

    write ( 1, 100 ) ''
    
    ierror_1 = 0
    ierror = 0

    write ( 1, 100 ) '----------------------------'
    write ( 1, 100 ) 'ASIGNABILIDAD UNIDADES HIDRO'
    write ( 1, 100 ) '----------------------------'
    ! ----------------------------
    ! * Se leen datos de ASIGNH *
    ! ----------------------------

    OPEN (UNIT = 56, FILE = rut_dat_1( 1 : long_ruta )//'ASIGNH.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
     
    errleca = 0; unidad = 0
    if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( unidad .lt. NumUniHid ) 
            unidad = unidad + 1
            letaux = ''
            read ( 56, 100, iostat = ierror ) letaux
            read ( letaux, *, iostat = errleca )  ( AsignUH ( unidad, intervalo ), intervalo = 1, ntintr )
            if ( errleca .ne. 0 ) go to 1007
            bloque = ntintr / 24
            do k = 1, bloque
               write ( 1, 500 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
               write ( 1, 900 ) unidad, nombunih ( unidad ), 'ASIG:', ( AsignUH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do
            if ( ntintr - 24 * bloque .gt. 0 ) then
               write ( 1, 500 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
               write ( 1, 900 ) unidad, nombunih ( unidad ), 'ASIG:', ( AsignUH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end if
        end do
    else	
         bmensaje = 'ERROR DE LECTURA ARCHIVO ASIGNH.csv'
         call EnviaMensajeError ( bmensaje )
         call ParaProceso ( bmensaje )
    end if

1007 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 ) then
        write ( letr, "(i5)" ) unidad
        call Elimina_blancos ( letr, 5)
        bmensaje = 'ERROR DE LECTURA ARCHIVO ASIGNH.csv'
        call EnviaMensajeError ( bmensaje )
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunih ( unidad ))
       call ParaProceso ( bmensaje )
    endif 
    CLOSE ( UNIT = 56 )

    write ( 1, 100 ) ''
    
    ierror = 0
    
    write ( 1, 100 ) '-----------------------------'
    write ( 1, 100 ) 'DISPONIBILIDAD UNIDADES HIDRO'
    write ( 1, 100 ) '-----------------------------'
    ! ----------------------------
    ! * Se leen datos de DISPOH_DERS *
    ! ----------------------------

    OPEN (UNIT = 56, FILE = rut_dat_1( 1 : long_ruta )//'DISPOH.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
     
    errleca = 0; unidad = 0
    if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( unidad .lt. NumUniHid ) 
            unidad = unidad + 1
            letaux = ''
            read ( 56, 100, iostat = ierror ) letaux
            read ( letaux, *, iostat = errleca )  ( DispoUH ( unidad, intervalo ), intervalo = 1, ntintr )
            if ( errleca .ne. 0 ) go to 1008
            bloque = ntintr / 24
            do k = 1, bloque
               write ( 1, 500 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
               write ( 1, 900 ) unidad, nombunih ( unidad ), 'DISP:', ( DispoUH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do
            if ( ntintr - 24 * bloque .gt. 0 ) then
               write ( 1, 500 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
               write ( 1, 900 ) unidad, nombunih ( unidad ), 'DISP:', ( DispoUH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end if
        end do
    else	
         bmensaje = 'ERROR DE LECTURA ARCHIVO DISPOH.csv'
         call EnviaMensajeError ( bmensaje )
         call ParaProceso ( bmensaje )
    end if

1008 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 ) then
        write ( letr, "(i5)" ) unidad
        call Elimina_blancos ( letr, 5)
        bmensaje = 'ERROR DE LECTURA ARCHIVO DISPOH.csv'
        call EnviaMensajeError ( bmensaje )
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunih ( unidad ))
       call ParaProceso ( bmensaje )
    endif 

    CLOSE ( UNIT = 56 )

    write ( 1, 100 ) ''

    !Se calcula disponibilidad y coordinabilidad de unidades Hidro en base a su oferta
    call Dispo_Coord_H

    ierror = 0
    i = 0

    !Se leen datos de condiciones iniciales unidades hidro
    write ( 1, 100 ) '------------------------------------'
    write ( 1, 100 ) 'CONDICIONES INICIALES UNIDADES HIDRO'
    write ( 1, 100 ) '------------------------------------'

    ! ------------------------------
    ! * Se leen datos de UNIHCI *
    ! ------------------------------

!    write ( 1, 100 ) 'Unidad   Nombre           Estado'
    write ( 1, 100 ) 'Unidad   Nombre           Estado   No Paros'

    OPEN (UNIT = 57, FILE = rut_dat_1( 1 : long_ruta )//'UNIHCI.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
     
    errleca = 0; unidad = 0
    if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( unidad .lt. NumUniHid ) 
            unidad = unidad + 1
            letaux = ''
            read ( 57, 100, iostat = ierror ) letaux
            read ( letaux, *, iostat = errleca )  EstadoCIUH ( unidad )
            if ( errleca .ne. 0 ) go to 1009
            write ( 1, 700 ) unidad, nombunih ( unidad ), EstadoCIUH ( unidad )
!           read ( letaux, * )  EstadoCIUH ( unidad ), NoActParH ( unidad )
!           write ( 1, 700 ) unidad, nombunih ( unidad ), EstadoCIUH ( unidad ), NoActParH ( unidad )
        end do
    else	
         bmensaje = 'ERROR DE LECTURA ARCHIVO UNIHCI.csv'
         call EnviaMensajeError ( bmensaje )
         call ParaProceso ( bmensaje )
    end if

1009 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 ) then
        write ( letr, "(i5)" ) unidad
        call Elimina_blancos ( letr, 5)
        bmensaje = 'ERROR DE LECTURA ARCHIVO UNIHCI.csv'
        call EnviaMensajeError ( bmensaje )
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunih ( unidad ))
       call ParaProceso ( bmensaje )
    endif 
    
    CLOSE ( UNIT = 57 )

    write ( 1, 100 ) ''
    
    ierror = 0
    i = 0

    write ( 1, 100 ) '---------------------'
    write ( 1, 100 ) 'RAMPAS UNIDADES HIDRO'
    write ( 1, 100 ) '---------------------'
    ! ----------------------------
    ! * Se leen datos de RAMPASH *
    ! ----------------------------

    write ( 1,  90 ) ' Unidad Nombre           Emer. 10   Emer. ', minresup, '    Reg. ', minresre, '   z.p.1 min  z.p.1 max  z.p.2 min  z.p.2  max z.p.3  min z.p.3  max z.p.4 min  z.p.4 max'

    OPEN (UNIT = 58, FILE = rut_dat_1( 1 : long_ruta )//'RAMPASH.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
     
    errleca = 0; unidad = 0
    if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( unidad .lt. NumUniHid ) 
            unidad = unidad + 1
            letaux = ''
            read ( 58, 100, iostat = ierror ) letaux
            read ( letaux, *, iostat = errleca )  emergencia, regulacion, ( ZonProh ( unidad, contador ), contador = 1, maxzonproh * 2) 
            if ( errleca .ne. 0 ) go to 1010
            !Se verifica la rampa de emergencia de 10 minutos vs duracion del intervalo para reserva rodante de 10 min
            if ( durintr .lt. 10 ) then
               RamEmer10H ( unidad ) = emergencia * durintr
            else
               RamEmer10H ( unidad ) = emergencia * 10
            end if
            !Se verifica la rampa de emergencia de x minutos vs duracion del intervalo para reserva rodante suplementaria
            if ( durintr .lt. minresup  ) then
               RamEmerxH ( unidad ) = emergencia * durintr
            else
               RamEmerxH ( unidad ) = emergencia * minresup 
            end if
            !Se verifica la rampa de emergencia de x minutos vs duracion del intervalo para reserva rodante suplementaria
            if ( durintr .lt. minresre   ) then
               RamRegH ( unidad ) = regulacion * durintr
            else
               RamRegH ( unidad ) = regulacion * minresre 
            end if                    
            write ( 1, 1300 ) unidad, nombunih ( unidad ), RamEmer10H ( unidad ), RamEmerxH ( unidad ), RamRegH ( unidad ), ( ZonProh ( unidad, contador ), contador = 1, maxzonproh * 2 )
        end do
    else	
         bmensaje = 'ERROR DE LECTURA ARCHIVO RAMPASH.csv'
         call EnviaMensajeError ( bmensaje )
         call ParaProceso ( bmensaje )
    end if

1010 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 ) then
        write ( letr, "(i5)" ) unidad
        call Elimina_blancos ( letr, 5)
        bmensaje = 'ERROR DE LECTURA ARCHIVO RAMPASH.csv'
        call EnviaMensajeError ( bmensaje )
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunih ( unidad ))
        call ParaProceso ( bmensaje )
    endif 
 
    CLOSE ( UNIT = 58 )

    !Escalamiento
    
    RamEmer10H = RamEmer10H / Base
    RamEmerxH = RamEmerxH / Base
    RamRegH = RamRegH / Base
    ZonProh = ZonProh / Base
    
!Forma vectores de trabajo para zonas prohibidas unidades hidro
NoRaOpH = 0
RaOpSupH = 0
RaOpInfH = 0
ZonProhx = ZonProh

! Valida limites de regulacion de unidades hidro
call ValidaLimitesRegulacionUH

PotMinUniH = PotMinUniH / Base
PotMaxUniH = PotMaxUniH / Base
PotMinRUniH = PotMinRUniH / Base
PotMaxRUniH = PotMaxRUniH / Base

!Hacer para todas las unidades hidro
do unidad = 1,NumUniHid
    ! Identifica cuantas zonas prohibidas hay
    NuZonProh = 0
    do zona = 1, maxzonproh, 2
        if ( ZonProh ( unidad, zona ) .ne. 0 ) then
           NuZonProh  =  NuZonProh  + 1
        endif
    enddo
    if ( NuZonProh  .gt. 0 ) then
        do i = 1, NTINTR
            ! Guarda en arreglo auxiliar x
            ZonProh = ZonProhx
            ! Verifica si el rango de operación esta dentro de alguna de las bandas prohibidas
            sigue = .true.
            do iz = 1, NuZonProh,2
                if ( (PotMinUniH ( unidad, i ) .ge. ZonProhx ( unidad, iz ) .and. PotMaxUniH ( unidad, i ) .le. ZonProhx ( unidad, iz+1 ) )  ) then
                    ZonProh ( unidad, iz ) = PotMinUniH ( unidad, i )
                    ZonProh ( unidad, iz + 1 ) = PotMinUniH ( unidad, i ) 
                endif
            enddo
            if ( sigue ) then
                ! Acota para zonas prohibidad fuera del rango de operación
                do zona = 1,  NuZonProh*2
                   if ( ZonProh ( unidad, zona ) .lt.  PotMinUniH ( unidad, i )  ) then
                       ZonProh ( unidad, zona ) = PotMinUniH ( unidad, i )
                   else if ( ZonProh ( unidad, zona ) .gt.  PotMaxUniH ( unidad, i )  ) then
                       ZonProh ( unidad, zona  ) = PotMaxUniH ( unidad, i )
                   endif
                end do 
                !hacer para el numero maximo de zonas prohibidas
                contador = 0
                do zona = 1, NuZonProh*2
                    if ( ZonProh ( unidad, zona + contador ) .ne. 0 .or. zona .eq. 1 ) then
                        if ( contador .eq. 0 ) then
                            RaOpInfH ( unidad, zona, i ) = PotMinUniH ( unidad, i )
                            RaOpSupH ( unidad, zona, i ) = ZonProh ( unidad, zona + contador )
                            RaRegInfH( unidad, zona, i ) = PotMinRUniH ( unidad, i )  
                            RaRegSupH( unidad, zona, i ) = RaOpSupH ( unidad, zona, i )
                            if ( i .eq. 1 ) NoRaOpH ( unidad ) = NoRaOpH ( unidad ) + 1
                        else
                            RaOpInfH ( unidad, zona, i ) = ZonProh ( unidad, zona + contador - 1 ) 
                            RaOpSupH ( unidad, zona, i ) = ZonProh ( unidad, zona + contador )   
                            RaRegInfH( unidad, zona, i ) = RaOpInfH ( unidad, zona, i )
                            RaRegSupH( unidad, zona, i ) = RaOpSupH ( unidad, zona, i )
                            if ( i .eq. 1 ) NoRaOpH ( unidad ) = NoRaOpH ( unidad ) + 1
                        end if
                        contador = contador + 1
                        if ( contador .eq. NuZonProh*2 ) then
                            RaOpInfH ( unidad, zona + 1, i ) = ZonProh ( unidad, zona + contador ) 
                            RaOpSupH ( unidad, zona + 1, i ) = PotMaxUniH ( unidad, i )
                            RaRegInfH( unidad, zona, i ) = RaOpInfH ( unidad, zona + 1, i )
                            RaRegSupH( unidad, zona, i ) = PotMaxRUniH ( unidad, i ) 
                            if ( i .eq. 1 )  NoRaOpH ( unidad ) = NoRaOpH ( unidad ) + 1
                        end if            
                    else
                        if ( contador .gt. 0 ) then
                            RaOpInfH ( unidad, zona, i ) = ZonProh ( unidad, zona + contador - 1 ) 
                            RaOpSupH ( unidad, zona, i ) = PotMaxUniH ( unidad, i )
                            RaRegInfH( unidad, zona, i ) = RaOpInfH ( unidad, zona, i )
                            RaRegSupH( unidad, zona, i ) = PotMaxRUniH ( unidad, i ) 
                            if ( i .eq. 1 ) NoRaOpH ( unidad ) = NoRaOpH ( unidad ) + 1
                            exit
                        end if
                    end if
                end do 
            endif
        end do
    endif
end do

PotMinUniH = PotMinUniH*Base
PotMaxUniH = PotMaxUniH*Base
PotMinRUniH = PotMinRUniH*Base
PotMaxRUniH = PotMaxRUniH*Base

    write ( 1, 100 ) ''

    ierror = 0
    i = 0

    ! ---------------------------------
    ! * Se leen datos de POTRESRO10H *
    ! * Se leen datos de PRERESRO10H *
    ! ---------------------------------
    !Potencia oferta de reserva rodante de 10 min unidades hidro
    ! -----------------------------------------
    !Precio oferta de reserva rodante de 10 min unidades hidro
    write ( 1, 100 ) '-----------------------------------------------------'
    write ( 1, 100 ) 'OFERTA DE RESERVA RODANTE DE 10 MIN DE UNIDADES HIDRO'
    write ( 1, 100 ) '-----------------------------------------------------'

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 60, FILE = rut_dat_1( 1 : long_ruta )//'POTRESRO10H.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 61, FILE = rut_dat_1( 1 : long_ruta )//'PRERESRO10H.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

    errleca = 0; errlecb = 0; unidad = 0
    if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( unidad .lt. NumUniHid ) 
            unidad = unidad + 1
            letaux = ''; letaux_1 = ''
            read ( 60 , 100, iostat = ierror ) letaux
            read ( 61 , 100, iostat = ierror ) letaux_1
            read ( letaux, *, iostat = errleca )  ( OferResR10H ( unidad, intervalo ), intervalo = 1, ntintr )
            read ( letaux_1, *, iostat = errlecb )  ( PreVenResR10H ( unidad, intervalo ), intervalo = 1, ntintr )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1011
            bloque = ntintr / 24
            do k = 1, bloque
                 write ( 1, 500 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
                 write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot:', ( OferResR10H ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                 write ( 1, 600 ) unidad, nombunih ( unidad ), '$  :', ( PreVenResR10H ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do
            if ( ntintr - 24 * bloque .gt. 0 ) then
               write ( 1, 500 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
               write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot:', ( OferResR10H ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
               write ( 1, 600 ) unidad, nombunih ( unidad ), '$  :', ( PreVenResR10H ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end if
        end do
    else	
      if ( ierror .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESRO10H.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      if ( ierror_1 .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESRO10H.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      Bmensaje = ''
      call ParaProceso ( bmensaje )
    end if

1011 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
        write ( letr, "(i5)" ) unidad
        call Elimina_blancos ( letr, 5)
        if ( errleca .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESRO10H.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        if ( errlecb .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESRO10H.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunih ( unidad ))
        call ParaProceso ( bmensaje )
    endif           
    CLOSE ( UNIT = 60 )
    CLOSE ( UNIT = 61 )

    write ( 1, 100 ) ''

    !Escalamiento
    OferResR10H = OferResR10H / Base
    PreVenResR10H = PreVenResR10H * Base

    ierror = 0
    ierror_1 = 0
    i = 0

    ! ---------------------------------
    ! * Se leen datos de POTRESROSUH *
    ! * Se leen datos de PRERESROSUH *
    ! ---------------------------------
    !Potencia oferta de reserva rodante suplementaria hidro
    ! -----------------------------------------
    !Precio oferta de reserva rodante suplementaria hidro
    write ( 1, 100 ) '----------------------------------------------------------'
    write ( 1, 100 ) 'OFERTA DE RESERVA RODANTE SUPLEMENTARIA DE UNIDADES HIDRO'
    write ( 1, 100 ) '----------------------------------------------------------'

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 62, FILE = rut_dat_1( 1 : long_ruta )//'POTRESROSUH.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 63, FILE = rut_dat_1( 1 : long_ruta )//'PRERESROSUH.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

    errleca = 0; errlecb = 0; unidad = 0
    if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( unidad .lt. NumUniHid ) 
            unidad = unidad + 1
            letaux = ''; letaux_1 = ''
            read ( 62, 100, iostat = ierror ) letaux
            read ( 63, 100, iostat = ierror_1 ) letaux_1
            read ( letaux, *, iostat = errleca )  ( OferResRxH ( unidad, intervalo ), intervalo = 1, ntintr )
            read ( letaux_1, *, iostat = errlecb )  ( PreVenResRxH ( unidad, intervalo ), intervalo = 1, ntintr )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1012
            bloque = ntintr / 24
            do k = 1, bloque
                 write ( 1, 500 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
                 write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot:', ( OferResRxH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                 write ( 1, 600 ) unidad, nombunih ( unidad ), '$  :', ( PreVenResRxH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do
            if ( ntintr - 24 * bloque .gt. 0 ) then
                write ( 1, 500 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
                write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot:', ( OferResRxH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                write ( 1, 600 ) unidad, nombunih ( unidad ), '$  :', ( PreVenResRxH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end if
        end do
    else	
      if ( ierror .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESROSUH.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      if ( ierror_1 .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESROSUH.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      Bmensaje = ''
      call ParaProceso ( bmensaje )
    end if

1012 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
        write ( letr, "(i5)" ) unidad
        call Elimina_blancos ( letr, 5)
        if ( errleca .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESROSUH.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        if ( errlecb .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESROSUH.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunih ( unidad ))
        call ParaProceso ( bmensaje )
    endif
        
    CLOSE ( UNIT = 62 )
    CLOSE ( UNIT = 63 )

    write ( 1, 100 ) ''

    !Escalamiento
    OferResRxH = OferResRxH / Base
    PreVenResRxH = PreVenResRxH * Base

    ierror = 0
    ierror_1 = 0
    ierror_2 = 0
    
    ! ---------------------------------
    ! * Se leen datos de POTRESRESEH *
    ! * Se leen datos de PRERESRESEH *
    ! * Se leen datos de MINRESRESEH *
    ! ---------------------------------
    !Potencia oferta de reserva de regulacion secundaria hidro
    ! ----------------------------------------- 
    !Precio oferta de reserva de regulacion secundaria hidro
    ! -----------------------------------------
    !Minimo a asignar de reserva de regulacion secundaria
    write ( 1, 100 ) '------------------------------------------------------------'
    write ( 1, 100 ) 'OFERTA DE RESERVA DE REGULACION SECUNDARIA DE UNIDADES HIDRO'
    write ( 1, 100 ) '-------------------------------------------------------------'

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 64, FILE = rut_dat_1( 1 : long_ruta )//'POTRESRESEH.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 65, FILE = rut_dat_1( 1 : long_ruta )//'PRERESRESEH.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

    if ( SiResRegDis .eq. 1 ) then
!       Abre archivo de datos de reserva distribuida
        OPEN (UNIT = 66, FILE = rut_dat_1( 1 : long_ruta )//'MINRESRESEH.csv', IOSTAT = IERROR_2, STATUS='OLD', RECORDSIZE = 250)
    endif
    
    errleca = 0; errlecb = 0; errlecc = 0; unidad = 0
    if ( ierror .eq. 0 .and. ierror_1 .eq. 0 .and. ierror_2 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( unidad .lt. NumUniHid ) 
            unidad = unidad + 1
            letaux = ''; letaux_1 = ''; letaux_2 =''
            read ( 64, 100, iostat = ierror ) letaux
            read ( 65, 100, iostat = ierror_1 ) letaux_1
            if ( SiResRegDis .eq. 1 ) then
                read ( 66, 100, iostat = ierror_2 ) letaux_2
            endif
            read ( letaux, *, iostat = errleca )  ( OferResRegH ( unidad, intervalo ), intervalo = 1, ntintr )
            read ( letaux_1, *, iostat = errlecb )  ( PreVenResRegH ( unidad, intervalo ), intervalo = 1, ntintr )
            if ( SiResRegDis .eq. 1 ) then
              read ( letaux_2, *, iostat = errlecc )  ( MrreUH ( unidad, intervalo ), intervalo = 1, ntintr )
            endif
            if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0 ) go to 1013
            bloque = ntintr / 24
            do k = 1, bloque
               write ( 1, 500 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
               write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot:', ( OferResRegH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
               write ( 1, 600 ) unidad, nombunih ( unidad ), '$  :', ( PreVenResRegH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
               if ( SiResRegDis .eq. 1 ) then
                  write ( 1, 600 ) unidad, nombunih ( unidad ), 'PMi:', ( MrreUH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
               endif
            end do
            if ( ntintr - 24 * bloque .gt. 0 ) then
               write ( 1, 500 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
               write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot:', ( OferResRegH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
               write ( 1, 600 ) unidad, nombunih ( unidad ), '$  :', ( PreVenResRegH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
               if ( SiResRegDis .eq. 1 ) then
                  write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pmi:', ( MrreUH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
               endif
            end if
        end do
    else	
      if ( ierror .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESRESEH.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      if ( ierror_1 .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESRESEH.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      if ( ierror_2 .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO MINRESRESEH.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      Bmensaje = ''
      call ParaProceso ( bmensaje )
    end if

1013 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0 ) then
        write ( letr, "(i5)" ) unidad
        call Elimina_blancos ( letr, 5)
        if ( errleca .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESRESEH.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        if ( errlecb .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESRESEH.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        if ( errlecc .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO MINRESRESEH.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunih ( unidad ))
        call ParaProceso ( bmensaje )
    endif
    
    CLOSE ( UNIT = 64 )
    CLOSE ( UNIT = 65 )
    if ( SiResRegDis .eq. 1 ) then
        CLOSE ( UNIT = 66 )
    endif

    !Escalamiento
    OferResRegH = OferResRegH / Base
    PreVenResRegH = PreVenResRegH * Base
    MrreUH = MrreUH / Base

    write ( 1, 100 ) ''
    
    ierror = 0
    ierror_1 = 0
    
    ! ---------------------------------
    ! * Se leen datos de POTRESNR10H *
    ! * Se leen datos de PRERESNR10H *
    ! ---------------------------------
    !Potencia oferta de reserva no rodante de 10 min hidro
    ! -----------------------------------------
    !Precio oferta de reserva no rodante de 10 min hidro
    write ( 1, 100 ) '--------------------------------------------------------'
    write ( 1, 100 ) 'OFERTA DE RESERVA NO RODANTE DE 10 MIN DE UNIDADES HIDRO'
    write ( 1, 100 ) '--------------------------------------------------------'

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 66, FILE = rut_dat_1( 1 : long_ruta )//'POTRESNR10H.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 67, FILE = rut_dat_1( 1 : long_ruta )//'PRERESNR10H.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

    errleca = 0; errlecb = 0; unidad = 0
    if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( unidad .lt. NumUniHid ) 
            unidad = unidad + 1
            letaux = ''; letaux_1 = ''
            read ( 66, 100, iostat = ierror ) letaux
            read ( 67, 100, iostat = ierror_1 ) letaux_1
            read ( letaux, *, iostat = errleca )  ( OferResNR10H ( unidad, intervalo ), intervalo = 1, ntintr )
            read ( letaux_1, *, iostat = errlecb )  ( PreVenResNR10H ( unidad, intervalo ), intervalo = 1, ntintr )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1014
            bloque = ntintr / 24
            do k = 1, bloque
               write ( 1, 500 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
               write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot:', ( OferResNR10H ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
               write ( 1, 600 ) unidad, nombunih ( unidad ), '$  :', ( PreVenResNR10H ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do
            if ( ntintr - 24 * bloque .gt. 0 ) then
               write ( 1, 500 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
               write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot:', ( OferResNR10H ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
               write ( 1, 600 ) unidad, nombunih ( unidad ), '$  :', ( PreVenResNR10H ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end if
        end do
    else	
      if ( ierror .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESNR10H.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      if ( ierror_1 .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESNR10H.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      Bmensaje = ''
      call ParaProceso ( bmensaje )
    end if

1014 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
        write ( letr, "(i5)" ) unidad
        call Elimina_blancos ( letr, 5)
        if ( errleca .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESNR10H.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        if ( errlecb .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESNR10H.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunih ( unidad ))
        call ParaProceso ( bmensaje )
    endif
    
    CLOSE ( UNIT = 66 )
    CLOSE ( UNIT = 67 )

    write ( 1, 100 ) ''

    !Escalamiento
    OferResNR10H = OferResNR10H / Base
    PreVenResNR10H = PreVenResNR10H * Base

    ierror = 0
    ierror_1 = 0
    
    ! ---------------------------------
    ! * Se leen datos de POTRESNRSUH *
    ! * Se leen datos de PRERESNRSUH *
    ! ---------------------------------
    !Potencia oferta de reserva no rodante suplementaria hidro
    ! -----------------------------------------
    !Precio oferta de reserva no rodante suplementaria hidro
    write ( 1, 100 ) '-------------------------------------------------------------'
    write ( 1, 100 ) 'OFERTA DE RESERVA NO RODANTE SUPLEMENTARIA DE UNIDADES HIDRO'
    write ( 1, 100 ) '-------------------------------------------------------------'

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 68, FILE = rut_dat_1( 1 : long_ruta )//'POTRESNRSUH.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 69, FILE = rut_dat_1( 1 : long_ruta )//'PRERESNRSUH.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

    errleca = 0; errlecb = 0; unidad = 0
    if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( unidad .lt. NumUniHid ) 
            unidad = unidad + 1
            letaux = ''; letaux_1 = ''
            read ( 68, 100, iostat = ierror ) letaux
            read ( 69, 100, iostat = ierror_1 ) letaux_1
            read ( letaux, *, iostat = errleca )  ( OferResNRxH ( unidad, intervalo ), intervalo = 1, ntintr )
            read ( letaux_1, *, iostat = errlecb )  ( PreVenResNRxH ( unidad, intervalo ), intervalo = 1, ntintr )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1015
            bloque = ntintr / 24
            do k = 1, bloque
               write ( 1, 500 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
               write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot:', ( OferResNRxH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
               write ( 1, 600 ) unidad, nombunih ( unidad ), '$  :', ( PreVenResNRxH ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do
            if ( ntintr - 24 * bloque .gt. 0 ) then
               write ( 1, 500 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
               write ( 1, 600 ) unidad, nombunih ( unidad ), 'Pot:', ( OferResNRxH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
               write ( 1, 600 ) unidad, nombunih ( unidad ), '$  :', ( PreVenResNRxH ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            end if
        end do
    else	
      if ( ierror .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESNRSUH.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      if ( ierror_1 .ne. 0 ) then
         bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESNRSUH.csv'
         call EnviaMensajeError ( bmensaje )
      endif
      Bmensaje = ''
      call ParaProceso ( bmensaje )
    end if

1015 continue

    ! Verifica que no existan errores de lectura
    if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
        write ( letr, "(i5)" ) unidad
        call Elimina_blancos ( letr, 5)
        if ( errleca .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESNRSUH.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        if ( errlecb .ne. 0 ) then
           bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESNRSUH.csv'
           call EnviaMensajeError ( bmensaje )
        endif
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunih ( unidad ))
        call ParaProceso ( bmensaje )
    endif

    CLOSE ( UNIT = 68 )
    CLOSE ( UNIT = 69 )

    write ( 1, 100 ) ''

    !Escalamiento
    OferResNRxH = OferResNRxH / Base
    PreVenResNRxH = PreVenResNRxH * Base

    ierror = 0
    ierror_1 = 0
    i = 0


end if



 90  format ( a38, i2,a9, i2, a92 ) 
100  format ( a )
200  format ( i3, 5x, a15, 4x, a3, 12x, i4, 5x, a9, 3x, a5, 5x, I3, 10x, i3  )    
!200  format ( i3, 5x, a15, 4x, a3, 12x, i4, 5x, a9, 3x, a5, 5x, I3, 10x, i3, 5x, I3 )    
300  format ( i3, 7x, a6 )         
301  format ( a3, i3, 7x, a15 )    
302  format ( a6, i3, 7x, a15 )
303  format ( i3, 7x, a6, f9.2, 2x, f9.2, 13x, i1 )         
400  format ( i3, 2 ( 7x, a6 ), 10x, i3 )    
500  format ( a10, x, i3, x, a1, i3 )      
600  format ( i3, x, a15, x, a5, x, 24 (f9.2, 2x) )
700  format ( i3, 5x, a15, 6x, i1 )  
!700  format ( i3, 5x, a15, 6x, i1, 5x, i3 )  
900  format ( i3, x, a15, x, a4, x, 24 (i1, 2x) )  
1300 format ( i3, 5x, a15, 3 ( f9.2, 2x ), 2x, 8 ( f9.2, 2x ) )
1400 format ( i3, x, a15, x, 24 (i4, 2x) )
     


End Subroutine data_hidro