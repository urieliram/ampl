!
! !!! Hey  !!! ---> Pendientes
!
! 1) Proteger para cuando se rebasen las dimensiones ( generar rutina general )
!

!
! ***********************************************
! Lectrura de datos de red eléctrica
! ***********************************************
Subroutine LecDatRedElectrica

use ParAUHE, only: RUT_RES

! use ParGloRed, only: 

implicit none

integer ierror

! Se abre archivo donde se guardan las sensibilidades de perdidas por intervalo
OPEN ( UNIT = 876, FILE = RUT_RES//'SenPerNodIntIte.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1250 )

! Inicializa variables globales
call IniVarGlo

! Se lee horizonte
!call data_horizo

! Lee datos de areas
call lec_Areas

! Se lee informacion de regiones de precios
call lec_RegionesPrecios

! Se leen nodos del sistema electrico
call lee_nodos

! Lectura de disponibilidad de los nodos en el horizonte
call lec_disnod

! Prepara informacion de nodos eléctricos
call pre_nodos

! Se lee información de ramas eléctricas
call lec_ram

! Se leen datos de grupos de ramas eléctricas
call lec_GruRam

! Se lee información de elementos que forman parte de grupos de ramas
call lec_EleGruRam

! Identifica grupos de ramas por sistema
call Pre_GruposRamas

! Prepara información de regiones de precios
call Pre_RegionesPrecios

! Se lee informacion de regiones de precios
call lec_ZonasCarga

! Se leen precios tope y piso para precios de energía
call lec_PreciosTopePiso

return
end
    
! ***********************************************
! Lectrura de disponibilidad de nodos eléctricos
! ***********************************************
SUBROUTINE lec_disnod 

Use ParAUHE, only: rut_dat_1, NTINTR, bmensaje, NumNodos, TipoLec
Use ParGloRed, only: nomnod, indnod, disnodini, idnodo, sisnod, noddisint, estnoddisint, maxnod, &
                     lisnoddisint, apunoddisint
 
Implicit none

integer i, j, ierror, icuenta, int, numele, errleca

integer intchange ( 10*maxnod )
integer estado ( 10*maxnod )

character letr

character*250 letaux

character*1 let

OPEN (UNIT = 4, FILE = trim(rut_dat_1)//'NODOS'//trim(TipoLec)//'D.csv', IOSTAT = ierror, STATUS='OLD', RECORDSIZE = 250)
 

errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    i = 0
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
	    read ( 4, 100, iostat = ierror ) letaux
        !call QuitaComas ( letaux, len_trim(letaux) )
	    ! Guarda información del nodo
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
            if ( i .gt. 10*maxnod ) then
                  bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
                  call EnviaMensajeError ( bmensaje )
                  bmensaje = "DE NODOS QUE CAMBIAN DE"
                  call EnviaMensajeError ( bmensaje )
                  bmensaje = "DISPONIBILIDAD"
                  call ParaProceso ( bmensaje )
            endif
		    read ( letaux, *, IOSTAT = errleca ) noddisint(i), let, intchange(i), estado(i)
            if ( errleca .ne. 0 ) go to 1001
        endif
    enddo
else
    bmensaje = 'ERROR DE LECTURA ARCHIVO NODOS'//trim(TipoLec)//'D.csv'
    call ParaProceso ( bmensaje )
endif

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje =  'ERROR DE LECTURA ARCHIVO NODOS'//trim(TipoLec)//'D.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'RENGLON: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif

CLOSE ( unit = 4 )

numele = i

! Prepara lista de nodos que cambian disponibilidad por intervalo
icuenta = 0
apunoddisint(1) = 1
do int = 1, NTINTR
    do j = 1, numele
        ! Verifica si entra en el intervalo
        if ( intchange(j) .eq. int ) then
            icuenta = icuenta + 1
            lisnoddisint(icuenta) = noddisint(j)
            estnoddisint(icuenta) = estado(j)
        endif
    enddo
    apunoddisint(int+1) = icuenta + 1
enddo

100 format ( a )
    
return
    
END SUBROUTINE 
 
    
! *******************************************
! Lectrura de regiones de precios
! *******************************************
SUBROUTINE lec_RegionesPrecios

Use ParAUHE, only: rut_dat_1, NTINTR, bmensaje, TipoLec
Use ParGloRed, only: numregpre, nomregpre, clvregpre, indregpre, maxregpre

Implicit none

integer i, ierror, errleca

character letr*5

character*250 letaux

OPEN (UNIT = 4, FILE = trim(rut_dat_1)//'REGIONPRECIOS.csv', IOSTAT = ierror, STATUS='OLD', RECORDSIZE = 250)

errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    i = 0
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
	    read ( 4, 100, iostat = ierror ) letaux
        !call QuitaComas ( letaux, len_trim(letaux) )
	    ! Guarda información de la rama
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
            if ( i .gt. maxregpre ) then
                  bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
                  call EnviaMensajeError ( bmensaje )
                  bmensaje = "DE REGIONES PRECIOS"
                  call ParaProceso ( bmensaje )
            endif
		    read ( letaux, *, IOSTAT = errleca ) indregpre(i), nomregpre(i), clvregpre(i)
            if ( errleca .ne. 0 ) go to 1001
        endif
    enddo
else
   bmensaje = 'ERROR DE LECTURA ARCHIVO REGIONPRECIOS.csv'
   call ParaProceso ( bmensaje )
endif

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO REGIONPRECIOS.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'REGION: ['//trim(letr)//'] '//trim(nomregpre(i))
   call ParaProceso ( bmensaje )
endif
    
numregpre = i

CLOSE ( unit = 4 )

! Lectura de ramas que cambian de disponibilidad a lo largo del horizonte de estudio
call lec_disram

100 format ( a250 )
    
END SUBROUTINE lec_RegionesPrecios
    
! *******************************************
! Lectrura de areas del sistema electrico
! *******************************************
SUBROUTINE lec_Areas

Use ParAUHE, only: rut_dat_1, NTINTR, bmensaje
Use ParGloRed, only: numarea, indarea, nomarea, maxare

Implicit none

integer i, ierror, errleca

character letr*5

character*250 letaux


OPEN (UNIT = 4, FILE = trim(rut_dat_1)//'AREAMEM.csv', IOSTAT = ierror, STATUS='OLD', RECORDSIZE = 250)

errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    i = 0
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
	    read ( 4, 100, iostat = ierror ) letaux
        !call QuitaComas ( letaux, len_trim(letaux) )
	    ! Guarda información de la rama
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
            if ( i .gt. maxare ) then
                  bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
                  call EnviaMensajeError ( bmensaje )
                  bmensaje = "DE AREAS"
                  call ParaProceso ( bmensaje )
            endif
		    read ( letaux, *, IOSTAT = errleca )  nomarea(i)
            if ( errleca .ne. 0 ) go to 1001
        endif
    enddo
else
   bmensaje = 'ERROR DE LECTURA ARCHIVO AREAMEM.csv'
   call ParaProceso ( bmensaje )
endif

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO AREAMEM.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'AREA: ['//trim(letr)//'] '//trim(nomarea(i))
   call ParaProceso ( bmensaje )
endif

numarea = i

CLOSE ( unit = 4 )


100 format ( a250 )
    
END SUBROUTINE lec_Areas

    
! ***********************************************
! Lectrura de disponibilidad de ramas eléctricos
! ***********************************************
SUBROUTINE lec_disram

Use ParAUHE, only: rut_dat_1, NTINTR, bmensaje, TipoLec
Use ParGloRed, only: ramdisint, estramdisint, maxram, lisramdisint, apuramdisint
 
Implicit none

integer i, j, ierror, numele, icuenta, int, errleca

integer intchange ( 10*maxram )
integer estado ( 10*maxram )

character*1250 letaux

character*1 let

character*5 letr

OPEN (UNIT = 4, FILE = trim(rut_dat_1)//'RAMASD.csv', IOSTAT = ierror, STATUS='OLD', RECORDSIZE = 250)
 
errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    i = 0
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
	    read ( 4, 100, iostat = ierror ) letaux
        !call QuitaComas ( letaux, len_trim(letaux) )
	    ! Guarda información del ramo
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
            if ( i .gt. 10*maxram ) then
                  bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
                  call EnviaMensajeError ( bmensaje )
                  bmensaje = "DE RAMAS QUE CAMBIAN DE"
                  call EnviaMensajeError ( bmensaje )
                  bmensaje = "DISPONIBILIDAD"
                  call ParaProceso ( bmensaje )
            endif
		    read ( letaux, *, IOSTAT = errleca ) ramdisint(i), let, intchange(i), estado(i)
            if ( errleca .ne. 0 ) go to 1001
        endif
    enddo
else
   bmensaje = 'ERROR DE LECTURA ARCHIVO RAMASD.csv'
   call ParaProceso ( bmensaje )
endif

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO RAMASD.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'RENGLON: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif


CLOSE ( unit = 4 )

numele = i

! Prepara lista de ramas que cambian disponibilidad por intervalo
icuenta = 0
apuramdisint(1) = 1
do int = 1, NTINTR
    do j = 1, numele
        ! Verifica si entra en el intervalo
        if ( intchange(j) .eq. int ) then
            icuenta = icuenta + 1
            lisramdisint(icuenta) = ramdisint(j)
            estramdisint(icuenta) = estado(j)
        endif
    enddo
    apuramdisint(int+1) = icuenta + 1
enddo


return

100 format ( a )
    
END SUBROUTINE 

    
! *******************************************
! Lectrura de ramas eléctricas
! *******************************************
SUBROUTINE lec_ram 

Use ParAUHE, only: rut_dat_1, NTINTR, bmensaje, TipoLec, base
Use ParGloRed, only: numram, indram, nomram, oriram, desram, tipram, resram, rearam, disramini, &
                     conductram, suceptram, maxram

Implicit none

integer i, ierror, errleca

character letr*5

character*250 letaux

character*12 letori, letdes

OPEN (UNIT = 4, FILE = trim(rut_dat_1)//'RAMAS.csv', IOSTAT = ierror, STATUS='OLD', RECORDSIZE = 250)

errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    i = 0
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
	    read ( 4, 100, iostat = ierror ) letaux
        !call QuitaComas ( letaux, len_trim(letaux) )
	    ! Guarda información de la rama
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
            if ( i .gt. maxram ) then
                  bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
                  call EnviaMensajeError ( bmensaje )
                  bmensaje = "DE RAMAS"
                  call ParaProceso ( bmensaje )
            endif

		    read ( letaux, *, IOSTAT = errleca ) indram(i), nomram(i), oriram(i), letori, desram(i), letdes, &
                                                resram(i), rearam(i), tipram(i), disramini(i)
            if ( errleca .ne. 0 ) go to 1001
            
            call CalculaAdmitancia ( resram(i), rearam(i), conductram(i), suceptram(i) )
        endif
    enddo
else
   bmensaje = 'ERROR DE LECTURA ARCHIVO RAMAS.csv'
   call ParaProceso ( bmensaje )
endif

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO RAMAS.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'RAMA: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif

numram = i

CLOSE ( unit = 4 )

! Lectura de ramas que cambian de disponibilidad a lo largo del horizonte de estudio
call lec_disram

resram = resram*(base/100)

100 format ( a250 )
    
END SUBROUTINE lec_ram

                
! *******************************************
! Lectrura de grupos de ramas eléctricas
! *******************************************
SUBROUTINE lec_GruRam 

Use ParAUHE, only:  NTINTR, rut_dat_1, bmensaje, TipoLec

Use ParGloRed, only:  numgruram, nomgruram, indgruram, bangruram, potmingruram, potmaxgruram, BasMva, maxgruram

Implicit none

integer i, j, ierror, errleca, errlecb, ierror_1

character letr*5

character*250 letaux
character*2500 letauxb

OPEN (UNIT = 7, FILE = trim(rut_dat_1)//'GRUPOSRAMAS.csv', IOSTAT = ierror, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 8, FILE = trim(rut_dat_1)//'GRUPOSRAMASLIM.csv', IOSTAT = ierror_1, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0
if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    i = 0
    do while ( ierror .eq. 0 .and. ierror_1 .eq. 0  )
	    read ( 7, 100, iostat = ierror ) letaux
   	    read ( 8, 100, iostat = ierror_1 ) letauxb
        !call QuitaComas ( letaux, len_trim(letaux) )
        !call QuitaComas ( letauxb, len_trim(letaux) )
	    ! Guarda información del grupo de ramas
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
            if ( i .gt. maxgruram ) then
                  bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
                  call EnviaMensajeError ( bmensaje )
                  bmensaje = "DE GRUPOS DE RAMAS"
                  call ParaProceso ( bmensaje )
            endif
		    read ( letaux, *, IOSTAT = errleca ) indgruram(i), nomgruram(i), bangruram(i)
   		    read ( letauxb, *, IOSTAT = errlecb )  ( potmingruram(i,j), potmaxgruram(i,j), j = 1, NTINTR )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1001
        endif
    enddo
else
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO GRUPOSRAMAS.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO GRUPOSRAMASLIM.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS GRUPOSRAMAS.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO GRUPOSRAMASLIM.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = 'GRUPO: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif

! Convierte a valores en pu
potmingruram = potmingruram/BasMva
potmaxgruram = potmaxgruram/BasMva

! Guarda numero de grupos de ramas
numgruram = i

CLOSE ( unit = 7 )
CLOSE ( unit = 8 )

return 

100 format ( a )
    
END SUBROUTINE lec_GruRam
    
                    
! ***********************************************
! Lectrura de ramas que forman parte de un grupo
! ***********************************************
SUBROUTINE lec_EleGruRam

Use ParAUHE, only: rut_dat_1, NTINTR, bmensaje, TipoLec

Use ParGloRed, only:   numgruram, IndGruRam, ApuEleGruRam, LisEleGruRam, &
                       SentEleGruRam, maxgruram, nomgruram, nomram, maxelegruram

Implicit none

integer i, j, ierror, icuenta, errleca

integer numele, igru

character letr*5

character*250 letaux

character*20 letram, letgru

character*12 leto, letd

integer inod, jnod

integer sentido ( maxelegruram )
integer rama ( maxelegruram )
integer grupo ( maxelegruram )

OPEN (UNIT = 4, FILE = trim(rut_dat_1)//'ELEGRURAM.csv', IOSTAT = ierror, STATUS='OLD', RECORDSIZE = 250)
errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    i = 0
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
	    read ( 4, 100, iostat = ierror ) letaux
        ! call QuitaComas ( letaux, len_trim(letaux) )
	    ! Guarda información del grupo de ramas
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
            if ( i .gt. maxelegruram ) then
                  bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
                  call EnviaMensajeError ( bmensaje )
                  bmensaje = "DE ELEMENTOS DE GRUPOS DE RAMAS"
                  call ParaProceso ( bmensaje )
            endif
		    read ( letaux, *, IOSTAT = errleca ) letram, rama(i), leto, inod, letd, jnod, letgru, grupo(i), sentido(i) 
            if ( errleca .ne. 0 ) go to 1001
        endif
    enddo
else
   bmensaje = 'ERROR DE LECTURA ARCHIVO ELEGRURAM.csv'
   call ParaProceso ( bmensaje )
endif

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO ELEGRURAM.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'ELEMENTO: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif


numele = i

! Prepara lista de ramas que forman parte de grupos de ramas
icuenta = 0
ApuEleGruRam(1) = 1
LisEleGruRam = 0
do igru = 1, numgruram
    do j = 1, numele
        ! Verifica si entra en el intervalo
        if ( grupo(j) .eq. indgruram(igru)  ) then
            icuenta = icuenta + 1
            LisEleGruRam(icuenta) = rama(j)
            SentEleGruRam(icuenta) = sentido(j)
        endif
    enddo
    ApuEleGruRam(igru+1) = icuenta + 1
enddo

CLOSE ( unit = 4 )

return

100 format ( a250 )
    
END SUBROUTINE lec_EleGruRam
    
! *****************************************************
! Lectrura de preparacion de informacion de nodos
! *****************************************************
SUBROUTINE pre_nodos

Use ParAUHE, only: NumNodos, nodo_subsis, EstadoIsla, SlackIsla, numsis

Use ParGloRed, only:  sisnod, NumNodSis, tipnod, nomnod

Implicit none

integer i, isub, BuscaSubsistema, ie, sale

! Busca indice del subsistema al que esta conectado el nodo
do i = 1, NumNodos
    isub = BuscaSubsistema( nodo_subsis(i) )
    if ( isub .ne. 0 ) then
        sisnod(i) = isub
    else
        write ( *, * ) "El subsistema del nodo no esta definido: ", nomnod(i), nodo_subsis(i)
    endif
enddo

! Verifica que los nodos slack esten bien definidos
do i = 1, numsis
   ! Si hay inconcistencia en la definición del slack elige como slack al primer nodo del subsistema
   if ( i .ne. sisnod(SlackIsla(i) ) ) then
      ie = 1; sale = 0
      do while ( sale .eq. 0 .and. ie .le. NumNodos )
         if ( sisnod(ie) .eq. i ) then
             SlackIsla(i) = ie
             sale = 1
         endif
         ie = ie + 1
      enddo
   endif
enddo

! Calcula el número de nodos del sistema
NumNodSis = 0
do i = 1, Numnodos
   NumNodSis( sisnod(i) ) = NumNodSis( sisnod(i) ) + 1
enddo

return

end
    
! ***************************************************************
! Preparacion de informacion de nodos por subsistema
! ***************************************************************
SUBROUTINE PreDatosSistema ( isistema )

Use ParAUHE, only: NumNodos

Use ParGloRed, only:  sisnod, inaumnodsis, inrednodsis, PerIntervalo, perpacnod, SnsPerIntIny

Implicit none

integer i, isub, icuenta, isistema 

inaumnodsis = 0
inrednodsis = 0
icuenta = 0
! Busca indice del subsistema al que esta conectado el nodo
do i = 1, NumNodos
    isub = sisnod(i)
    if ( isub .eq. isistema ) then
        icuenta = icuenta + 1
        inaumnodsis(icuenta) = i
        inrednodsis(i) = icuenta
    endif
enddo

! Inicializa perdidas de potencia activa del sistema y nodales
PerIntervalo = 0.0
perpacnod = 0.0
SnsPerIntIny = 0.0

! Prepara tipo de nodos 
call PreparatipoNodo ( isistema )
   
return

end
 
    
                    
! *****************************************************************
! Prepara informacion de grupos de ramas por subsistema eléctrico
! *****************************************************************
SUBROUTINE Pre_GruposRamas

Use ParAUHE, only: EstadoIsla, numsis

Use ParGloRed, only: numgruram, NumGruRamSis, inagruram, ApuEleGruRam, LisEleGruRam, oriram, sisnod

Implicit none

integer i, j, rama, is


NumGruRamSis = 0
do is = 1, numsis
    do i = 1, numgruram
        ! Verifica que el primer elemento esta en la isla a evaluar
        do j = ApuEleGruRam(i), ApuEleGruRam(i)
            rama = LisEleGruRam(j)
            if ( rama .ne. 0 ) then
                if ( sisnod(oriram(rama)) .eq. is ) then
                   NumGruRamSis(is) = NumGruRamSis(is) + 1
                endif
            endif
        enddo
    enddo
enddo

return
    
END SUBROUTINE Pre_GruposRamas 
    
                        
! *****************************************************************
! Prepara informacion de regiones de precios
! *****************************************************************
SUBROUTINE Pre_RegionesPrecios

Use ParAUHE, only: NumNodos 

Use ParGloRed, only: numregpre, regnod, sisnod, sisregpre

Implicit none

integer i

! Identifica el subsistema al que pertenece la region de precios
do i = 1, NumNodos
    sisregpre(regnod(i)) = sisnod(i)
enddo


return
    
END SUBROUTINE Pre_RegionesPrecios
    
! ---------------------------------------------------------------------
! Subrutina que prepara tipos de nodos                                *
!                                                                     *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! ---------------------------------------------------------------------

Subroutine PreparatipoNodo (isistema ) 

Use ParAUHE, only: NumUniRC, NumUniRD, NumUNiHid, NumUniRE, NumUniNPR, NumNodInt, NumOferDem,  &
                  nodorc, nodocompurd, nodocar, nodoh, nodounre, nodonpr, NodoInt, NumBloDem, &
                  numsis, SlackIsla, CompXModo, ListCompURD, ApunCompURD, GenCompXModo, &
                  NumCompRD, NumModRD, NumCompXModo, Tempnodorc, Tempnodocar, Tempnodonpr

Use ParGloRed, only: tipnod

Implicit none

integer nodo, u, k, isistema
integer componente, componente_1, modo

! Inicializa vector de tipo de nodo
tipnod = 0 ! Este vector se usa solo en la impresion de resultados de costos marginales
        
! Incluye generacion de rango continuo
do u = 1 , NumUniRC
   nodo = Tempnodorc ( u, 1, 1 )  ! JLC
   tipnod(nodo) = 1
enddo

! Incluye generacion de rango discontinuo
do u = 1 , NumUniRD
!   para todos los modos de operacion
    do modo = 2, NumModRD(u)
!       para el modo de operacion asignado
        do componente = 1, NumCompXModo ( u, modo )
!           para todas la componentes de la unidad de rango discontinuo
            do componente_1 = 0, NumCompRD ( u ) - 1
                if ( CompXModo ( u, modo, componente ) .eq. ListCompURD ( ApunCompURD ( u ) + componente_1 ) ) then
                    nodo = nodocompurd ( ApunCompURD ( u ) + componente_1, 1 )
                    tipnod(nodo) = 1
                    exit
                endif
            enddo
        enddo
        exit
    enddo
enddo

! Incluye generacion hidro
do u = 1 , NumUniHid
  nodo = nodoh ( u, 1 )
  tipnod(nodo) = 1
enddo

! Incluye generacion renovables
do u = 1 , NumUniRE
   nodo = nodounre ( u, 1 )
   tipnod(nodo) = 1
enddo
        
! Incluye generaciones no programables
do k = 1, NumUniNPR
   !nodo = nodonpr(k, 1)
   nodo = Tempnodonpr ( k, 1, 1 )  ! JLC
   tipnod(nodo) = 1
enddo

! Incluye ofertas de demanda ( componente fija )
do k = 1, NumOferDem
    nodo = Tempnodocar ( k, 1, 1 )  ! JLC
    if ( tipnod(nodo) .eq. 0 ) then
       tipnod(nodo) = 2
    else if ( tipnod(nodo) .eq. 1 ) then
       tipnod(nodo) = 3
    endif
enddo

! Incluye nodos con de intercambios
do k = 1, NumNodInt
   nodo = NodoInt(k, 1)
   tipnod(nodo) = 3
enddo

! define el nodo de referencia del subsistema
nodo = SlackIsla(isistema)
tipnod(nodo) = 4

return

end Subroutine 

    
! *******************************************
! Lectrura de zonas de carga
! *******************************************
SUBROUTINE lec_ZonasCarga

Use ParAUHE, only: rut_dat_1, NTINTR, bmensaje, TipoLec
Use ParGloRed, only: numzoncar, nomzoncar, indzoncar, indzoncarems, maxzoncar, numnodzoncar, indnodzoncar, facdisnodzoncar, &
                     siszoncar

Implicit none

integer i, j, h, ierror, BuscaIndZonaCarga, BuscaSubsistema, izon, inod, iz, ino, is, errleca, errlecb, ie

character letr*12

character*250 letaux

character*3 leti

character*12 letnod

character*16 letzon

character*3 letsis

real*8 fac

OPEN (UNIT = 4, FILE = trim(rut_dat_1)//'ZONAS_CARGA.csv', IOSTAT = ierror, STATUS='OLD', RECORDSIZE = 250)

errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    i = 0
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
	    read ( 4, 100, iostat = ierror ) letaux
	    ! Guarda información de la rama
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            i = i + 1
            if ( i .gt. maxzoncar ) then
                  bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
                  call EnviaMensajeError ( bmensaje )
                  bmensaje = "DE ELEMENTOS DE ZONAS DE CARGA"
                  call ParaProceso ( bmensaje )
            endif

		    read ( letaux, *, IOSTAT = errleca ) leti, letsis, nomzoncar(i), indzoncarems(i)
            if ( errleca .ne. 0 ) go to 1001
            is = BuscaSubsistema ( letsis )
            indzoncar(i) = i
            siszoncar(i) = is
        endif
    enddo
else
   bmensaje = 'ERROR DE LECTURA ARCHIVO ZONAS_CARGA.csv'
   call ParaProceso ( bmensaje )
endif

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) i
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO ZONAS_CARGA.csv'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'ZONA: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif

numzoncar = i

CLOSE ( unit = 4 )

! Abre archivo de factores de distribucion
OPEN (UNIT = 7, FILE = trim(rut_dat_1)//'FACDISCAR.csv', IOSTAT = ierror, STATUS='OLD', RECORDSIZE = 250)

! Inicializa arreglos de factores de distribución por zona de carga
numnodzoncar = 0

errlecb = 0; ie = 0
if ( ierror .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )
        letaux = ''
	    read ( 7, 100, iostat = ierror ) letaux
	    ! Guarda información de la rama
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) then
            ie = ie + 1
		    read ( letaux, *, IOSTAT = errlecb ) letzon, izon, letnod, inod, h, fac
            if ( errlecb .ne. 0 ) go to 1002
            iz = BuscaIndZonaCarga ( izon )
            if ( iz .ne. 0 ) then
                if ( h .eq. 1 ) then
                    numnodzoncar(iz) = numnodzoncar(iz) + 1
                    indnodzoncar(iz,numnodzoncar(iz)) = inod
                    ino = numnodzoncar(iz);
                else
                    do j = 1, numnodzoncar(iz)
                        if ( inod .eq. indnodzoncar(iz,j) ) then
                           ino = j;
                           exit
                        endif
                    enddo
                endif
                if ( h .le. NTINTR ) then
                   facdisnodzoncar(iz,ino,h) = fac
                endif
            endif
        endif
    enddo
else
   bmensaje = 'ERROR DE LECTURA ARCHIVO FACDISCAR.cs'
   call ParaProceso ( bmensaje )
endif

1002 continue

! Verifica que no existan errores de lectura
if ( errlecb .ne. 0 ) then
    write ( letr, "(i12)" ) ie
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO FACDISCAR.cs'
    call EnviaMensajeError ( bmensaje )
    Bmensaje = 'ELEMENTO: ['//trim(letr)//'] '
   call ParaProceso ( bmensaje )
endif

close (7)

100 format ( a250 )
    
END SUBROUTINE lec_ZonasCarga   

    
! *******************************************
! Lectrura de precios tope y piso
! *******************************************

SUBROUTINE lec_PreciosTopePiso

Use ParAUHE, only: rut_dat_1, NTINTR, bmensaje, TipoLec, base
Use ParGloRed, only: preciotope, preciopiso

Implicit none

integer ierror

character*250 letaux

! Abre archivo de factores de distribucion
OPEN (UNIT = 7, FILE = trim(rut_dat_1)//'PRETOPEPISO.csv', IOSTAT = ierror, STATUS='OLD', RECORDSIZE = 250)

if ( ierror .eq. 0 ) then
    read ( 7, 100, iostat = ierror ) letaux
    read ( letaux, *, iostat = ierror ) preciotope, preciopiso
    preciotope = preciotope*base
    preciopiso = preciopiso*base
    preciopiso = -1.0E+10
else
    bmensaje = 'ERROR DE LECTURA ARCHIVO RETOPEPISO.csv'
    call ParaProceso ( bmensaje )
endif

close (7)

100 format ( a250 )

RETURN

END SUBROUTINE lec_PreciosTopePiso  
