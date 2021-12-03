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
!         Lee datos de generadores renovable intermitentes                *
!         y relaciona unidades                                            *
!         de generacion con los subsistemas                               * 
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Marzo 2015                            *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                   Febrero 2017                          *
!**************************************************************************
    
SUBROUTINE data_reno

use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, IslaGenRE, unidadre_area, nombunire, tiunidre, &
                  propre, nodounre, correspre, NumNodos, numsis, nodo_subsis, nomsis, EstadoIsla, &
                  nodo_area, NumUniRE, maxsegre, OferVenEnerRE, PreVenEnerRE, NumBloVRE, ntintr, &
                  base, PotMinGRE, PotMaxGRE, AsignURE, DispoURE, maxint, maxure, NomEjecu, SisUniRE
!
IMPLICIT NONE

integer ierror, ibanbit, dummy, tempnodre ( maxint ), nodo, subsistema, unidad, i, intervalo, &
                bloque, k, ierror_1, segmento, numseg, errleca, errlecb

CHARACTER fecha_Ej*19

character*3000 letaux, letaux_1 

character*20 nombre, propietario

character*7 tipo

character*5 letr

!Inicializacion de variables
!
i = 0
dummy = 0
IslaGenRE = 0
unidadre_area = ''
nombunire = ''
tiunidre = ''
propre = ''
nodounre = 0
correspre = 0
unidad = 0
NumUniRE = 0
! ---------------------------------
! * Se leen datos de UNITRE      *
! * Se leen datos de NODOSRE      *
! ---------------------------------

OPEN (UNIT = 109, FILE = rut_dat_1( 1 : long_ruta )//'UNITRE.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 197, FILE = rut_dat_1( 1 : long_ruta )//'NODOSRE.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)
write ( 1, 100 ) '---------------------------------' 
write ( 1, 100 ) 'UNIDADES RENOVABLES INTERMITENTES' 
write ( 1, 100 ) '---------------------------------' 
errleca = 0; errlecb = 0
if ( ierror .eq. 0 ) then
    write ( 1, 100 ) 'Unidad  Nombre          Tipo  Propietario    Nodo     Area        Subsistema' 
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	    read ( 109, 100, iostat = ierror ) letaux
        read ( 197, 100, iostat = ierror ) letaux_1
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. len_trim(letaux_1) .ne. 0 ) then
            read ( letaux, *, iostat = errleca )  nombre, tipo, propietario !, dummy, tempnodre
            read ( letaux_1, *, iostat = errlecb )  ( tempnodre ( intervalo ), intervalo = 1, ntintr )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1001
            i = i + 1
            if ( i .gt. maxure ) then
                  bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
                  call EnviaMensajeError ( bmensaje )
                  bmensaje = "DE UNIDADES RENOVABLES"
                  call ParaProceso ( bmensaje )
            endif
            !Hacer para todos los nodos
            do nodo = 1, NumNodos
                !Se asigna subsistema con el nodo del primer intervalo
                if ( tempnodre ( 1 ) .eq. nodo ) then
                    !Hacer para todos los subsistemas
                    do subsistema = 1, numsis
                        if ( nodo_subsis ( nodo ) .eq. nomsis ( subsistema ) ) then
                            !Ver si el subsistema esta activo
                            if ( EstadoIsla ( subsistema ) .eq. 1 ) then
                                unidad = unidad + 1
                                IslaGenRE ( unidad ) = subsistema
                                !se asigna area con el nodo del primer intervalo
                                unidadre_area ( unidad ) = nodo_area ( nodo )
                                nombunire ( unidad ) = nombre
                                tiunidre ( unidad ) = tipo
                                propre ( unidad ) = propietario                                
                                nodounre ( unidad, : ) = tempnodre ( : )
                                correspre ( unidad ) = i
                                !se escribe a debugger
                                write ( 1, 200 ) unidad, nombunire ( unidad ), tiunidre ( unidad ), propre ( unidad ), nodounre ( unidad, 1 ), unidadre_area ( unidad ), nomsis ( IslaGenRE ( unidad ) )
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
     bmensaje = 'ERROR DE LECTURA ARCHIVO UNITRE.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSRE.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS UNITRE.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSRE.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunire(unidad))
   call ParaProceso ( bmensaje )
endif
     
SisUniRE = i

NumUniRE = unidad

CLOSE ( UNIT = 109 )
close ( UNIT = 197 )

write ( 1, 100 ) ''

write ( 1, 100 ) '------------------------------'
write ( 1, 100 ) 'NODOS DE UNIDADES RENOVABLES  '
write ( 1, 100 ) '------------------------------'
!Hacer para todoas las cargas
do unidad = 1, NumUniRE
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        write ( 1, 1400 ) unidad, nombunire ( unidad ), ( nodounre ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        write ( 1, 1400 ) unidad, nombunire ( unidad ), ( nodounre ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
    end if
end do


write ( 1, 100 ) ''

ierror = 0
ierror_1 = 0
i = 0
k = 0

! ------------------------------
! * Se leen datos de POTVERE *
! * Se leen datos de PREVERE *
! ------------------------------
write ( 1, 100 ) '------------------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE VENTA DE ENERGIA UNIDADES RENOVABLES INTERMITENTES'
write ( 1, 100 ) '------------------------------------------------------------'

OPEN (UNIT = 110, FILE = rut_dat_1( 1 : long_ruta )//'POTVERE.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
OPEN (UNIT = 111, FILE = rut_dat_1( 1 : long_ruta )//'PREVERE.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 3000)

errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRE ) 
        unidad = unidad + 1
        !Hacer para todos los segmentos 
        do segmento = 1, maxsegre
           letaux = ''; letaux_1 = ''
           read ( 110, 100, iostat = ierror ) letaux
           read ( letaux, *, iostat = errleca  )  ( OferVenEnerRE ( unidad, segmento, intervalo ), intervalo = 1, ntintr )
           read ( 111, 100, iostat = ierror_1 ) letaux_1
           read ( letaux_1, *, iostat = errlecb )  ( PreVenEnerRE ( unidad, segmento, intervalo ), intervalo = 1, ntintr )
           if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1002
        end do                    
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO POTVERE.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PREVERE.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1002 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS POTVERE.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO PREVERE.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunire(unidad))
   call ParaProceso ( bmensaje )
end if 
   
CLOSE ( UNIT = 110 )
CLOSE ( UNIT = 111 )

!Escribir a bitacora
!Hacer para todas las unidades
do unidad = 1, NumUniRE
    !Hacer para todos los bloques de 24 intervalos
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        !Hacer para todos los segmentos
        do segmento = 1, maxsegre
            write ( 1, 500 ) unidad, nombunire ( unidad ), 'MW', ( OferVenEnerRE ( unidad, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 500 ) unidad, nombunire ( unidad ), '$/MWh', ( PreVenEnerRE ( unidad, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do        
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        do segmento = 1, maxsegre
            write ( 1, 500 ) unidad, nombunire ( unidad ), 'MW', ( OferVenEnerRE ( unidad, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 500 ) unidad, nombunire ( unidad ), '$/MWh', ( PreVenEnerRE ( unidad, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr )
        end do
    end if
end do

!Escalamiento
OferVenEnerRE = OferVenEnerRE / Base
PreVenEnerRE = PreVenEnerRE * Base

!Calcular cuantos segmentos se ofertaron por unidad y por intervalo
do unidad = 1, NumUniRE
    !Hacer para todos los intervalos
    do intervalo = 1, ntintr
        numseg = 0
        !Hacer para todos los segmentos
        do segmento = 1, maxsegre
            !Encontrar segmento con oferta de potencia cero
            if ( OferVenEnerRE ( unidad, segmento, intervalo ) .ne. 0 ) then
                numseg = numseg + 1
            end if            
        end do
        NumBloVRE   ( unidad, intervalo ) = numseg
    end do
end do

write ( 1, 100 ) ''

ierror = 0
ierror_1 = 0
i = 0

! -----------------------------
! * Se leen datos de LIUNITRE *
! * Se leen datos de LSUNITRE *
! -----------------------------
write ( 1, 100 ) '--------------------------------------------------------------'
write ( 1, 100 ) 'POTENCIAS MINIMA Y MAXIMA DE UNIDADES RENOVABLES INTERMITENTES'
write ( 1, 100 ) '--------------------------------------------------------------'

letaux = 'letaux'
letaux_1 = 'letaux1'

! Abre archivo de datos de generadores
OPEN (UNIT = 112, FILE = rut_dat_1( 1 : long_ruta )//'LIUNITRE.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 113, FILE = rut_dat_1( 1 : long_ruta )//'LSUNITRE.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRE ) 
        unidad = unidad + 1
        letaux = ''; letaux_1 = ''
        read ( 112, 100, iostat = ierror ) letaux
        read ( 113, 100, iostat = ierror_1 ) letaux_1
        read ( letaux, *, iostat = errleca )  ( PotMinGRE ( unidad, intervalo ), intervalo = 1, ntintr )
        read ( letaux_1, *, iostat = errlecb )  ( PotMaxGRE ( unidad, intervalo ), intervalo = 1, ntintr )
        if ( errleca .ne. 0 .or. errlecb .ne. 0 ) goto 1003
        bloque = ntintr / 24
        do k = 1, bloque
           write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
           write ( 1, 600 ) unidad, nombunire ( unidad ), 'Min:', ( PotMinGRE ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
           write ( 1, 600 ) unidad, nombunire ( unidad ), 'Max:', ( PotMaxGRE ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
           write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
           write ( 1, 600 ) unidad, nombunire ( unidad ), 'Min:', ( PotMinGRE ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
           write ( 1, 600 ) unidad, nombunire ( unidad ), 'Max:', ( PotMaxGRE ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end if
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO LIUNITRE.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO LSUNITRE.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1003 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS LIUNITRE.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO LSUNITRE.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunire(unidad))
   call ParaProceso ( bmensaje )
end if

CLOSE ( UNIT = 10 )
CLOSE ( UNIT = 11 )

!Escalamiento
PotMinGRE = PotMinGRE / Base
PotMaxGRE = PotMaxGRE / Base

write ( 1, 100 ) ''

ierror = 0
ierror_1 = 0
i = 0

letaux = 'letaux'
letaux_1 = 'letaux1'
write ( 1, 100 ) '---------------------------------------------------'
write ( 1, 100 ) 'ASIGNABILIDAD UNIDADES RENOVABLES INTERMITENTES'
write ( 1, 100 ) 'DISPONIBILIDAD  UNIDADES RENOVABLES INTERMITENTES'
write ( 1, 100 ) '---------------------------------------------------'
! ----------------------------
! * Se leen datos de ASIGNRE *
! * Se leen datos de DISPORE *
! ----------------------------

OPEN (UNIT = 114, FILE = rut_dat_1( 1 : long_ruta )//'ASIGNRE.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
OPEN (UNIT = 115, FILE = rut_dat_1( 1 : long_ruta )//'DISPORE.csv', IOSTAT = ierror_1, STATUS='OLD', RECORDSIZE = 3000)
letaux_1 = ' si '
letaux = "vacio"
     
errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRE ) 
        unidad = unidad + 1
        letaux = ''; letaux_1 = ''
	    read ( 114, 100, iostat = ierror ) letaux
        read ( 115, 100, iostat = ierror_1 ) letaux_1
        read ( letaux, *, iostat = errleca )  ( AsignURE ( unidad, intervalo ), intervalo = 1, ntintr )
        read ( letaux_1, *, iostat = errlecb  )  ( DispoURE ( unidad, intervalo ), intervalo = 1, ntintr )
        if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1004
        bloque = ntintr / 24
        do k = 1, bloque
           write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
           write ( 1, 900 ) unidad, nombunirE ( unidad ), 'ASIG:', ( AsignURE ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
           write ( 1, 900 ) unidad, nombunirE ( unidad ), 'DISP:', ( DispoURE ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
           write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
           write ( 1, 900 ) unidad, nombunirE ( unidad ), 'ASIG:', ( AsignURE ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
           write ( 1, 900 ) unidad, nombunirE ( unidad ), 'DISP:', ( DispoURE ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end if
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO ASIGNRE.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO DISPORE.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1004 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS ASIGNRE.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO LSUNITRE.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunire(unidad))
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 114 )
CLOSE ( UNIT = 115 )

!Calcula disponibilidad de unidades renovables intermitentes dependiendo de su oferta
!call Dispo_RE


write ( 1, 100 ) ''
ierror = 0
ierror_1 = 0
i = 0


100  format ( a )
200  format ( i3, 5x, a12, 4x, a2, 4x, a3, 12x, i4, 5x, a9, 3x, a5, 18x, i3 )    
300  format ( a10, x, i3, x, a1, i3 )      
500  format ( i3, x, a12, x, a5, x, 24 (f9.2, 2x) ) 
600  format ( i3, x, a12, x, a4, x, 24 (f9.2, 2x) )    
900  format ( i3, x, a12, x, a4, x, 24 (i1, 2x) )  
1400  format ( i3, x, a12, x, 24 (i4, 2x) )

    end subroutine data_reno
    
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
!         Calcula la disponibilidad de las unidades                       *
!         renovables intermitentes en base a sus ofertas de energia       *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Noviembre 2014                        *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
    
SUBROUTINE Dispo_RE

use ParAUHE, only: DispoURE, NumUniRE, ntintr, PotMaxGRE, &
                  nombunire
!
IMPLICIT NONE

integer unidad, intervalo, bloque, k

!Hacer para todas las unidades de rango continuo
do unidad = 1, NumUniRE
    !Hacer para todos los intervalos
    do intervalo = 1, ntintr
        !Para que unidad sea no disponible su potencia maxima esta en cero
        if ( PotMaxGRE ( unidad, intervalo ) .eq. 0.0 ) then
            !No disponible
            DispoURE ( unidad, intervalo ) = 0
        else
            !Disponible
            DispoURE ( unidad, intervalo ) = 1
        end if
    end do
end do

write ( 1, 100 ) '------------------------------------------------'
write ( 1, 100 ) 'DISPONIBILIDAD UNIDADES RENOVABLES INTERMITENTES'
write ( 1, 100 ) '------------------------------------------------'
do unidad = 1, NumUniRE
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        write ( 1, 900 ) unidad, nombunire ( unidad ), 'DISP:', ( DispoURE ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        write ( 1, 900 ) unidad, nombunire ( unidad ), 'DISP:', ( DispoURE ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
    end if
end do

write ( 1, 100 ) ''

100  format ( a )
300  format ( a10, x, i3, x, a1, i3 )      
900  format ( i3, x, a12, x, a4, x, 24 (i1, 2x) )  

end subroutine Dispo_RE    