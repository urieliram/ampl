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
!                                                                         *
!**************************************************************************
    
SUBROUTINE data_rd

use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, nombunird, proprd, &
                  numnodos, numsis, nodo_subsis, EstadoIsla, IslaGenRD, &
                  nomsis, unidadrd_area_rd, nodo_area, maxurd, NumCompRD, &
                  ListCompURD, maxcompurd, tiunidrd, corresprd, ApunCompURD, NumUniRD, &
                  nombcomurd, ticompurd, nodocompurd, TransFacti, maxmodos, NumModRD, CostoMinGRD, &
                  ntintr, PreVenEnerRD, OferVenEnerRD, maxsegrd, Base, NumBloVRD, PotMinGRD, PotMaxGRD, &
                  EstadoCIURD, NumHCIURD, GenCIURD, PotSincURD, PotSincNR10URD, PotSincNRSURD, RampArraURD, &
                  durintr, TminModoURD, NumMaxTrans, TiempoTrans, CostoTrans, AsignURD, TiemInicioArrRDS, &
                  CostoArrRDS, maxsegarrd, NmBloArrURD, RampaBajURD, RampaSubURD, RamEmer10RD, RamEmerxRD, &
                  RamRegRD, minresre, minresup, OferResR10RD, PreVenResR10RD, PreVenResRxRD, OferResRxRD, &
                  OferResRegRD, PreVenResRegRD, OferResNR10RD, PreVenResNR10RD, PreVenResNRxRD, OferResNRxRD, &
                  UniGruResRD, maxgrure, CompXModo, NumCompXModo, GenCompXModo, maxgrute, UniGruTerRD, maxint, &
                  maxcompurd, maxurd, NomEjecu, SisUniRD, PotMinRRD, PotMaxRRD, DispoURD

use ProblemaAUHE

IMPLICIT NONE

integer ierror, ierror_1, ierror_2, dummy, unidad, componente, i, nodo, subsistema, &
        componentes ( maxcompurd ), NodoV ( maxcompurd * maxurd, maxint ), TempNumComp, &
        columna, contador, fila, bloque, k, modo, segmento, intervalo, numseg, modo_prev, auxiliar ( maxgrure ), &
        zona, componente_1, grupo, auxiliar_1 ( maxgrute ), errleca, errlecb, errlecc

character*3000 letaux, letaux_1, letaux_2

character*20 propietario, nombre, NombreV ( maxcompurd * maxurd )
             

character*7 tipo, TipoV ( maxcompurd * maxurd )

character*5 letr

real*8 rampaup, rampadown, emergencia, regulacion, regimen

!Inicializacion de variables

unidad = 0
componente = 0
nombunird = ''
tipo = ''
propietario = ''
proprd = ''
ListCompURD = 0
NumModRD = 0
TransFacti = 0
DispoURD = 0
TiempoTrans = 0
TiemInicioArrRDS = 0 
CostoArrRDS = 0.0
MrreURD = 0.0

!
! ---------------------------------
! * Se leen datos de COMPOURD      *
! * Se leen datos de NODOSCOMPOURD *
! ---------------------------------

OPEN (UNIT = 73, FILE = rut_dat_1( 1 : long_ruta )//'COMPOURD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 196, FILE = rut_dat_1( 1 : long_ruta )//'NODOSCOMPOURD.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)
errleca = 0; errlecb = 0; letaux = ''; i = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    letaux = ''; letaux_1 = ''
	read ( 73, 100, iostat = ierror ) letaux
    read ( 196, 100, iostat = ierror_1 ) letaux_1
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. len_trim(letaux_1) .ne. 0 ) then
            i = i + 1
            read ( letaux, *, iostat = errleca )  NombreV ( i ), TipoV ( i ) !, NodoV ( i )
            read ( letaux_1, *, iostat = errlecb )  ( NodoV ( i, intervalo ), intervalo = 1, ntintr )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1001
        endif
        letaux = ''; letaux_1 = ''
	    read ( 73, 100, iostat = ierror ) letaux
        read ( 196, 100, iostat = ierror_1 ) letaux_1
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO COMPOURD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSCOMPOURD.csv'
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
       bmensaje = 'ERROR DE LECTURA ARCHIVOS COMPOURD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSCOMPOURD.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = 'COMPONENTE: ['//trim(letr)//'] '//trim(NombreV ( i ))
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 73 )
CLOSE ( UNIT = 196 )

ierror = 0
i = 0
auxiliar = 0
!
! ---------------------------------
! * Se leen datos de UNITRD      *
! * Se leen datos de ZONASRESURD *
! * Se leen datos de GPORD      *
! ---------------------------------

unidad = 0
OPEN (UNIT = 72, FILE = rut_dat_1( 1 : long_ruta )//'UNITRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 99, FILE = rut_dat_1( 1 : long_ruta )//'ZONASRESURD.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 118, FILE = rut_dat_1( 1 : long_ruta )//'GPORD.csv', IOSTAT = IERROR_2, STATUS='OLD', RECORDSIZE = 250)
errleca = 0; errlecb = 0; errlecc = 0; letaux = ''
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 .and. ierror_2 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
    letaux = ' '; letaux_1 = ' '; letaux_2 = ' '
	read ( 72, 100, iostat = ierror ) letaux
    read ( 99, 100, iostat = ierror_1 ) letaux_1
    read ( 118, 100, iostat = ierror_2 ) letaux_2
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. len_trim(letaux_1) .ne. 0 .and. len_trim(letaux_2) .ne. 0 ) then
            i = i + 1
            read ( letaux, *, iostat = errleca )  nombre, tipo, propietario, dummy, TempNumComp, ( componentes ( componente ), componente = 1, TempNumComp )
            read ( letaux_1, *, iostat = errlecb )  ( auxiliar ( zona ), zona = 1, maxgrure )
            read ( letaux_2, *, iostat = errlecc )  ( auxiliar_1 ( grupo ), grupo = 1, maxgrute )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0 ) go to 1002
            !Hacer para todos los nodos
            do nodo = 1, NumNodos
                !se asigna subsistema con el nodo del primer intervalo
                if ( NodoV ( componentes ( 1 ), 1 ) .eq. nodo ) then
                    !Hacer para todos los subsistemas
                    do subsistema = 1, numsis
                        if ( nodo_subsis ( nodo ) .eq. nomsis ( subsistema ) ) then
                            !Ver si el subsistema esta activo
                            if ( EstadoIsla ( subsistema ) .eq. 1 ) then
                                unidad = unidad + 1
                                IslaGenRD ( unidad ) = subsistema
                                !se asigna area con el nodo del primer intervalo
                                unidadrd_area_rd ( unidad ) = nodo_area ( nodo )
                                nombunird ( unidad ) = nombre
                                tiunidrd ( unidad ) = tipo
                                proprd ( unidad ) = propietario
                                corresprd ( unidad ) = i
                                UniGruResRD ( unidad, : ) =  auxiliar
                                UniGruTerRD ( unidad, : ) =  auxiliar_1
                                NumCompRD ( unidad ) = TempNumComp
                                ApunCompURD ( unidad ) = unidad
                                !Hacer para todas las componentes de la unidad de rango discontinio
                                do componente = 1, TempNumComp
                                    ListCompURD ( unidad ) = componentes ( componente )
                                    nombcomurd ( unidad ) = NombreV ( componentes ( componente ) )
                                    ticompurd ( unidad ) = TipoV ( componentes ( componente ) )                                    
                                    nodocompurd ( unidad, : ) = NodoV ( componentes ( componente ), : )
                                    unidad = unidad + 1
                                end do
                                exit
                            end if
                        end if
                    end do
                end if
            end do
        endif
        letaux = ''; letaux_1 = ''; letaux_2 = ''
	    read ( 72, 100, iostat = ierror ) letaux
        read ( 99, 100, iostat = ierror_1 ) letaux_1
        read ( 118, 100, iostat = ierror_2 ) letaux_2
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO UNITRD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO ZONASRESURD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   if ( ierror_2 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO GPORD.csv'
     call EnviaMensajeError ( bmensaje )
  endif

   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1002 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS UNITRD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO ZONASRESURD.csv'
       call EnviaMensajeError ( bmensaje )
   endif
   if ( errlecc .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO GPORD.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( i ))
   call ParaProceso ( bmensaje )
endif

!Numero de unidades de rango discontinuo
NumUniRD = i

SisUniRD = unidad

CLOSE ( UNIT = 72 )
CLOSE ( UNIT = 99 )


ierror = 0

!Escribe a debugger los datos de unidades de rango discontinuo
write ( 1, 100 ) '------------------------------' 
write ( 1, 100 ) 'UNIDADES DE RANGO DISCONTINUO' 
write ( 1, 100 ) '------------------------------' 
!Hacer para todas las unidades de rango discontinuo
do unidad = 1, NumUniRD
    write ( 1, 100 ) 'Unidad  Nombre         Tipo  Propietario    Area        Subsistema'
    write ( 1, 200 ) unidad, nombunird ( unidad ), tiunidrd ( unidad ), proprd ( unidad ),  unidadrd_area_rd ( unidad ), nomsis ( IslaGenRD ( unidad ) )
    !hacer para todas las componentes de la unidad de rango discontinuo
    write ( 1, 100 ) '  Componente  Nombre        Tipo   Nodo'
    do componente = 0, NumCompRD ( unidad ) - 1
        write ( 1, 300 ) ListCompURD ( ApunCompURD ( unidad ) + componente ), nombcomurd ( ApunCompURD ( unidad ) + componente ), ticompurd ( ApunCompURD ( unidad ) + componente ), nodocompurd ( ApunCompURD ( unidad ) + componente, 1 )
    end do
end do

write ( 1, 100 ) ''


!Escribe a debugger los datos de unidades de rango discontinuo
write ( 1, 100 ) '------------------------------------------------------' 
write ( 1, 100 ) 'NODOS DE COMPONENTES DE UNIDADES DE RANGO DISCONTINUO' 
write ( 1, 100 ) '------------------------------------------------------' 
!Hacer para todas las unidades de rango discontinuo
do unidad = 1, NumUniRD    
    !hacer para todas las componentes de la unidad de rango discontinuo
    do componente = 0, NumCompRD ( unidad ) - 1
        bloque = ntintr / 24
        do k = 1, bloque
            write ( 1, 600 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
            write ( 1, 2500 ) ListCompURD ( ApunCompURD ( unidad ) + componente ), nombcomurd ( ApunCompURD ( unidad ) + componente ), ( nodocompurd ( ApunCompURD ( unidad ) + componente, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
            write ( 1, 600 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
            write ( 1, 2500 ) ListCompURD ( ApunCompURD ( unidad ) + componente ), nombcomurd ( ApunCompURD ( unidad ) + componente ), ( nodocompurd ( ApunCompURD ( unidad ) + componente, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end if
    end do
end do

write ( 1, 100 ) ''

!
! ---------------------------------
! * Se leen datos de TRANSRD      *
! ---------------------------------
OPEN (UNIT = 74, FILE = rut_dat_1( 1 : long_ruta )//'TRANSRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0
if ( ierror .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        do modo = 1, maxmodos
           letaux = ' '            
           read ( 74, 100, iostat = ierror ) letaux
           read ( letaux, *, iostat = errleca )  ( TransFacti ( unidad, modo, i ), i = 1, maxmodos )
           if ( errleca .ne. 0 ) go to 1003
        end do
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO TRANSRD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1003 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS TRANSRD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endif



CLOSE ( UNIT = 74 )

ierror = 0

!Se calculan los numeros de modos para las unidades de rango disncontino
!Hacer para todas las unidades de rango discontinio
do unidad = 1, NumUniRD
    !Hacer para todas las filas
    do fila = 1, maxmodos
        !Hacer para todas las columnas        
        do columna = 1, maxmodos
            if ( TransFacti ( unidad, fila, columna ) .eq. 1 ) then
                if ( NumModRD ( unidad ) .lt. columna ) then
                    NumModRD ( unidad ) = columna
                end if
            end if
        end do
    end do    
end do

!Imprime a debugger transiciones

write ( 1, 100 ) '----------------------------------------------------' 
write ( 1, 100 ) 'TRANSICIONES FACTIBLES UNIDADES DE RANGO DISCONTINUO' 
write ( 1, 100 ) '----------------------------------------------------' 
!Hacer para todas las unidades de rango discontinuo
do unidad = 1, NumUniRD
    write ( 1, 100 ) 'Unidad  Nombre'
    write ( 1, 200 ) unidad, nombunird ( unidad )
    !Hacer para todas las filas
    do fila = 1, NumModRD ( unidad )
        write ( 1, 500 ) ( TransFacti ( unidad, fila, columna ), columna = 1, NumModRD ( unidad ) )
    end do
end do

write ( 1, 100 ) ''

!
! ---------------------------------
! * Se leen datos de COMPXMODO    *
! * Se leen datos de GECOXMOD     *
! ---------------------------------
unidad = 1
OPEN (UNIT = 100, FILE = rut_dat_1( 1 : long_ruta )//'COMPXMODO.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 101, FILE = rut_dat_1( 1 : long_ruta )//'GECOXMOD.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)
errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        letaux = ' '; letaux_1 = ' '            
        do modo = 1, maxmodos
            letaux = ' '; letaux_1 = ' '            
            read ( 100, 100, iostat = ierror ) letaux
            read ( 101, 100, iostat = ierror ) letaux_1
            read ( letaux, *, iostat = errleca )  ( CompXModo ( unidad, modo, i ), i = 1, maxcompurd )
            read ( letaux_1, *, iostat = errlecb )  ( GenCompXModo ( unidad, modo, i ), i = 1, maxcompurd )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1004
        end do
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO COMPXMODO.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO GECOXMOD.csv'
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
       bmensaje = 'ERROR DE LECTURA ARCHIVOS COMPXMODO.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO GECOXMOD.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 100 )
CLOSE ( UNIT = 101 )

ierror = 0

NumCompXModo = 0
!Se calculan el numero de componentes por modo para las unidades de rango disncontino
!Hacer para todas las unidades de rango discontinio
do unidad = 1, NumUniRD
    !Hacer para todas las filas
    do fila = 1, maxmodos
        !Hacer para todas las columnas        
        do columna = 1, maxcompurd
            if ( CompXModo ( unidad, fila, columna ) .ne. 0 ) then
                NumCompXModo ( unidad, fila ) = NumCompXModo ( unidad, fila ) + 1
            end if
        end do
    end do    
end do

!Se Escriben a debugger los datos de las componentes por modo de unidades de rango discontinuo
write ( 1, 100 ) '------------------------------------------------------' 
write ( 1, 100 ) 'COMPONENTES POR MODO DE UNIDADES DE RANGO DISCONTINUO' 
write ( 1, 100 ) '------------------------------------------------------' 
!Hacer para todas las unidades de rango discontinuo
do unidad = 1, NumUniRD
    write ( 1, 100 ) 'Unidad  Nombre   Tipo'
    write ( 1, 200 ) unidad, nombunird ( unidad ), tiunidrd ( unidad )
    !Hacer para todos los modos
    do modo = 1, NumModRD ( unidad )
        write ( 1, 800 ) 'Modo', modo 
        write ( 1, 100 ) '    Componete  Nombre      Tipo    Nodo   % Gen'
        !Hacer para todas la componentes de modo
        do componente = 1, NumCompXModo ( unidad, modo )
            !Buscar componente del modo en la lista de componentes de la unida de rango discontinuo
            !Hacer para todas la componentes de la unida de rango discontinuo
            do componente_1 = 0, NumCompRD ( unidad ) - 1
                if ( CompXModo ( unidad, modo, componente ) .eq. ListCompURD ( ApunCompURD ( unidad ) + componente_1 ) ) then
                    write ( 1, 301 ) ListCompURD ( ApunCompURD ( unidad ) + componente_1 ), nombcomurd ( ApunCompURD ( unidad ) + componente_1), ticompurd ( ApunCompURD ( unidad ) + componente_1 ), nodocompurd ( ApunCompURD ( unidad ) + componente_1, 1 ), GenCompXModo  ( unidad, modo, componente )
                    exit
                end if
            end do
        end do
    end do
end do

write ( 1, 100 ) ''

! ------------------------------
! * Se leen datos de CGMRD *
! ------------------------------
write ( 1, 100 ) '--------------------------------------------------------'
write ( 1, 100 ) 'COSTO DE GENERACION MINIMO UNIDADES DE RANGO DISCONTINUO'
write ( 1, 100 ) '--------------------------------------------------------'
OPEN (UNIT = 75, FILE = rut_dat_1( 1 : long_ruta )//'CGMRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)

errleca = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        letaux = ' '           
        read ( 75, 100, iostat = ierror ) letaux
        do modo = 1, maxmodos
            read ( 75, 100, iostat = ierror ) letaux
            read ( letaux, *, iostat = errleca )  ( CostoMinGRD ( unidad, modo, i ), i = 1, ntintr )
            if ( errleca .ne. 0 ) go to 1005
            if ( modo .le. NumModRD ( unidad ) ) then
               bloque = ntintr / 24
               do k = 1, bloque
                  write ( 1, 800 ) 'Modo', modo
                  write ( 1, 900 ) ( CostoMinGRD ( unidad, modo, i ), i = 24 * k - 23 , 24 * k  )
               end do
               if ( ntintr - 24 * bloque .gt. 0 ) then
                  write ( 1, 800 ) 'Modo', modo
                  write ( 1, 900 ) ( CostoMinGRD ( unidad, modo, i ), i = 24 * bloque + 1 , ntintr  )
               end if
            end if
        end do
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO CGMRD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1005 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS CGMRD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 75 )

write ( 1, 100 ) ''

ierror = 0
ierror_1 = 0

! ------------------------------
! * Se leen datos de POTVERD *
! * Se leen datos de PREVERD *
! ------------------------------
write ( 1, 100 ) '-----------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE VENTA DE ENERGIA UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '-----------------------------------------------------'

OPEN (UNIT = 76, FILE = rut_dat_1( 1 : long_ruta )//'POTVERD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
OPEN (UNIT = 77, FILE = rut_dat_1( 1 : long_ruta )//'PREVERD.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 3000)

errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
            !Hacer para todos los modos de operación
            do modo = 1, maxmodos
                !Hacer para todos los segmentos
                do segmento = 1, maxsegrd
                   letaux = ' '; letaux_1 = ' '            
                   read ( 76, 100, iostat = ierror ) letaux
                   read ( letaux, *, iostat = errleca )  ( OferVenEnerRD ( unidad, modo, segmento, intervalo ), intervalo = 1, ntintr )
                   read ( 77, 100, iostat = ierror_1 ) letaux_1
                   read ( letaux_1, *, iostat = errlecb  )  ( PreVenEnerRD ( unidad, modo, segmento, intervalo ), intervalo = 1, ntintr )
                   if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1006
                end do                    
            end do
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO POTVERD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO GECOXMOD.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1006 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS POTVERD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO GECOXMOD.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 76 )
CLOSE ( UNIT = 77 )

!Escribir a bitacora
!Hacer para todas las unidades
do unidad = 1, NumUniRD
    !Hacer para todos los bloques de 24 intervalos
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 600 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        !Hacer para todos los modos de operacion
        do modo = 1, NumModRD ( unidad )
            !Hacer para todos los segmentos
            write ( 1, 800 ) 'Modo', modo
            do segmento = 1, maxsegrd
                write ( 1, 800 ) 'Seg:', segmento
                write ( 1, 1000 ) unidad, nombunird ( unidad ), 'MW', ( OferVenEnerRD ( unidad, modo, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                write ( 1, 1000 ) unidad, nombunird ( unidad ), '$/MWh', ( PreVenEnerRD ( unidad, modo, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            end do        
        end do
    end do
    
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 600 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        !Hacer para todos los modos de operacion
        do modo = 1, NumModRD ( unidad )
            write ( 1, 800 ) 'Modo', modo
            !Hacer para todos los segmentos
            do segmento = 1, maxsegrd
                write ( 1, 800 ) 'Seg:', segmento
                write ( 1, 1000 ) unidad, nombunird ( unidad ), 'MW', ( OferVenEnerRD ( unidad, modo, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                write ( 1, 1000 ) unidad, nombunird ( unidad ), '$/MWh', ( PreVenEnerRD ( unidad, modo, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
           end do
        end do
    end if
end do

!Escalamiento
OferVenEnerRD = OferVenEnerRD / Base
PreVenEnerRD = PreVenEnerRD * Base

!Calcular cuantos segmentos se ofertaron por unidad, por modo de operacion y por intervalo
do unidad = 1, NumUniRD
    !Hacer para todos los intervalos
    do intervalo = 1, ntintr
        !Hacer para todos los modos de operacion
        do modo = 1, NumModRD ( unidad )
            numseg = 0
            !Hacer para todos los segmentos
            do segmento = 1, maxsegrd
            !Encontrar segmento con oferta de potencia cero
                if ( OferVenEnerRD ( unidad, modo, segmento, intervalo ) .ne. 0 ) then
                    numseg = numseg + 1
                else
                    if ( segmento .eq. 1 .and. OferVenEnerRD ( unidad, modo, 2, intervalo ) .gt. 0.0 ) then
                        numseg = numseg + 1
                    endif
                end if            
            end do
            NumBloVRD   ( unidad, modo, intervalo ) = numseg
        end do
    end do
end do

write ( 1, 100 ) ''

ierror = 0
ierror_1 = 0

! -----------------------------
! * Se leen datos de LIUNITRD *
! * Se leen datos de LSUNITRD *
! -----------------------------
!Potencia mínima de unidades térmicas, PotMinGRD
! -----------------------------------------
!Potencia máxima de unidades térmicas, PotMaxGRD
write ( 1, 100 ) '--------------------------------------------------------'
write ( 1, 100 ) 'POTENCIAS MINIMA Y MAXIMA DE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '--------------------------------------------------------'

OPEN (UNIT = 78, FILE = rut_dat_1( 1 : long_ruta )//'LIUNITRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
OPEN (UNIT = 79, FILE = rut_dat_1( 1 : long_ruta )//'LSUNITRD.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 3000)
     
errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        do modo = 1, maxmodos
            letaux = ' '; letaux_1 = ' '            
            read ( 78, 100, iostat = ierror ) letaux
            read ( 79, 100, iostat = ierror_1 ) letaux_1
            read ( letaux, *, iostat = errleca )  ( PotMinGRD ( unidad, modo, i ), i = 1, ntintr )
            read ( letaux_1, *, iostat = errlecb )  ( PotMaxGRD ( unidad, modo, i ), i = 1, ntintr )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1007
            bloque = ntintr / 24
            do k = 1, bloque
               write ( 1, 800 ) 'Modo', modo
               write ( 1, 1200 ) 'Min:',( PotMinGRD ( unidad, modo, i ), i = 24 * k - 23 , 24 * k  )
               write ( 1, 1200 ) 'Max:',( PotMaxGRD ( unidad, modo, i ), i = 24 * k - 23 , 24 * k  )
            end do
            if ( ntintr - 24 * bloque .gt. 0 ) then
               write ( 1, 800 ) 'Modo', modo
               write ( 1, 1200 ) 'Min:',( PotMinGRD ( unidad, modo, i ), i = 24 * bloque + 1 , ntintr  )
               write ( 1, 1200 ) 'Max:',( PotMaxGRD ( unidad, modo, i ), i = 24 * bloque + 1 , ntintr  )
            end if
        end do
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO LIUNITRD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO LSUNITRD.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1007 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS LIUNITRD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO LSUNITRD.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 78 )
CLOSE ( UNIT = 79 )

write ( 1, 100 ) ''

!Escalamiento
PotMinGRD = PotMinGRD / Base
PotMaxGRD = PotMaxGRD / Base


! si se desea considerar limites de regulacion
if ( SiLimReg .eq. 1 ) then

    ierror = 0
    ierror_1 = 0

    ! -----------------------------
    ! * Se leen datos de LIRUNITRD *
    ! * Se leen datos de LSRUNITRD *
    ! -----------------------------
    !Potencia mínima de unidades térmicas, PotMinRRD
    ! -----------------------------------------
    !Potencia máxima de unidades térmicas, PotMaxRRD
    write ( 1, 100 ) '----------------------------------------------------------------------'
    write ( 1, 100 ) 'POTENCIAS MINIMA Y MAXIMA DE REGULACION DE UNIDADES RANGO DISCONTINUO'
    write ( 1, 100 ) '----------------------------------------------------------------------'

    OPEN (UNIT = 78, FILE = rut_dat_1( 1 : long_ruta )//'LIRUNITRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
    OPEN (UNIT = 79, FILE = rut_dat_1( 1 : long_ruta )//'LSRUNITRD.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 3000)
     
    errleca = 0; errlecb = 0; unidad = 0
    if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
!      Lee información hasta encontrar fin de información
       do while ( unidad .lt. NumUniRD ) 
          unidad = unidad + 1
          do modo = 1, maxmodos
             letaux = ' '; letaux_1 = ' '            
             read ( 78, 100, iostat = ierror ) letaux
             read ( 79, 100, iostat = ierror_1 ) letaux_1
             read ( letaux, *, iostat = errleca )  ( PotMinRRD ( unidad, modo, i ), i = 1, ntintr )
             read ( letaux_1, *, iostat = errlecb  )  ( PotMaxRRD ( unidad, modo, i ), i = 1, ntintr )
             if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1008
             if ( modo .le. NumModRD ( unidad ) ) then
                bloque = ntintr / 24
                do k = 1, bloque
                   write ( 1, 800 ) 'Modo', modo
                   write ( 1, 1200 ) 'Min:',( PotMinRRD ( unidad, modo, i ), i = 24 * k - 23 , 24 * k  )
                   write ( 1, 1200 ) 'Max:',( PotMaxRRD ( unidad, modo, i ), i = 24 * k - 23 , 24 * k  )
                end do
                if ( ntintr - 24 * bloque .gt. 0 ) then
                   write ( 1, 800 ) 'Modo', modo
                   write ( 1, 1200 ) 'Min:',( PotMinRRD ( unidad, modo, i ), i = 24 * bloque + 1 , ntintr  )
                   write ( 1, 1200 ) 'Max:',( PotMaxRRD ( unidad, modo, i ), i = 24 * bloque + 1 , ntintr  )
                 end if
             end if
          end do
       enddo
    else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO LIRUNITRD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO LSRUNITRD.csv'
     call EnviaMensajeError ( bmensaje )
   endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1008 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS LIRUNITRD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO LSRUNITRD.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endif
    CLOSE ( UNIT = 78 )
    CLOSE ( UNIT = 79 )

    write ( 1, 100 ) ''

    !Escalamiento
    PotMinRRD = PotMinRRD / Base
    PotMaxRRD = PotMaxRRD / Base
else
    PotMinRRD = PotMinGRD
    PotMaxRRD = PotMaxGRD
endif

ierror = 0
ierror_1 = 0

!Se leen datos de condiciones iniciales
write ( 1, 100 ) '----------------------------------------------------'
write ( 1, 100 ) 'CONDICIONES INICIALES UNIDADES DE RANGO DISCONTINUO'
write ( 1, 100 ) '----------------------------------------------------'

! ------------------------------
! * Se leen datos de UNITRDCI *
! ------------------------------

write ( 1, 100 ) 'Unidad  Nombre        Estado    Horas   Generacion'

OPEN (UNIT = 80, FILE = rut_dat_1( 1 : long_ruta )//'UNITRDCI.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
     
! Lee información hasta encontrar fin de información
errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
       unidad = unidad + 1
       do modo = 1, maxmodos
          letaux = ' '            
          read ( 80, 100, iostat = ierror ) letaux
          read ( letaux, *, iostat = errleca )  EstadoCIURD ( unidad, modo ), NumHCIURD ( unidad, modo ), GenCIURD ( unidad, modo )
          if ( errleca .ne. 0 ) go to 1009
          if ( modo .le. NumModRD ( unidad ) ) then
             !Calcula el numero de periodos en condiciones iniciales dependiendo de la duracion del intervalo
             NumHCIURD ( unidad, modo ) = NumHCIURD ( unidad, modo ) * 60
             NumHCIURD ( unidad, modo ) = NumHCIURD ( unidad, modo ) / durintr
             write ( 1, 800 ) 'Modo', modo
             write ( 1, 1400 ) EstadoCIURD ( unidad, modo ), NumHCIURD ( unidad, modo ), GenCIURD ( unidad, modo )
          end if
      end do
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO UNITRDCI.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1009 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS UNITRDCI.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endiF

CLOSE ( UNIT = 80 )

write ( 1, 100 ) ''

!Escalamiento
GenCIURD = GenCIURD / Base

ierror = 0
! ----------------------------
! * Se leen datos de TMINMODRD *
! ----------------------------

OPEN (UNIT = 80, FILE = rut_dat_1( 1 : long_ruta )//'TMINMODRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        letaux = ' '            
        read ( 80, 100, iostat = ierror ) letaux
        read ( letaux, *, iostat = errleca )  ( TminModoURD ( unidad, modo ), modo = 1, maxmodos )
        if ( errleca .ne. 0 ) go to 1010
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO UNITRDCI.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1010 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS TMINMODRD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endiF

CLOSE ( UNIT = 80 )

ierror = 0

!Imprime a debugger tiempos minimos de operacion de los modos

write ( 1, 100 ) '---------------------------------------------------------------------'
write ( 1, 100 ) 'TIEMPO MINIMO DE OPERACION DE LOS MODOS DE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '---------------------------------------------------------------------'
!Hacer para todas las unidades de rango discontinuo
do unidad = 1, NumUniRD
    write ( 1, 100 ) 'Unidad  Nombre'
    write ( 1, 200 ) unidad, nombunird ( unidad )
    !Hacer para todas las columnas
    do columna = 1, NumModRD ( unidad )
        !Calcula el tiempo minimo de operacion del modo dependiendo de la duracion del intervalo
        TminModoURD ( unidad, columna ) = TminModoURD ( unidad, columna ) * 60
        TminModoURD ( unidad, columna ) = TminModoURD ( unidad, columna ) / durintr
        write ( 1, 1700 ) 'modo',columna,':',TminModoURD ( unidad, columna )
    end do
end do

write ( 1, 100 ) ''
!
! ---------------------------------
! * Se leen datos de MAXTRANSRD      *
! ---------------------------------

OPEN (UNIT = 83, FILE = rut_dat_1( 1 : long_ruta )//'MAXTRANSRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        do contador = 1, maxmodos
           letaux = ' '           
           read ( 83, 100, iostat = ierror ) letaux
           read ( letaux, * )  ( NumMaxTrans ( unidad, contador, columna ), columna = 1, maxmodos )
        end do
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO MAXTRANSRD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1011 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS MAXTRANSRD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endiF
CLOSE ( UNIT = 83 )

ierror = 0

!Imprime a debugger maximo de transiciones entre modos

write ( 1, 100 ) '----------------------------------------------------------------------------------' 
write ( 1, 100 ) 'NUMERO MAXIMO DE TRANSICIONES FACTIBLES ENTRE MODOS UNIDADES DE RANGO DISCONTINUO' 
write ( 1, 100 ) '----------------------------------------------------------------------------------' 
!Hacer para todas las unidades de rango discontinuo
do unidad = 1, NumUniRD
    write ( 1, 100 ) 'Unidad  Nombre'
    write ( 1, 200 ) unidad, nombunird ( unidad )
    !Hacer para todas las filas
    do fila = 1, NumModRD ( unidad )
        write ( 1, 500 ) ( NumMaxTrans ( unidad, fila, columna ), columna = 1, NumModRD ( unidad ) )
    end do
end do

write ( 1, 100 ) ''
!
! ---------------------------------
! * Se leen datos de TIETRANSRD      *
! ---------------------------------

OPEN (UNIT = 84, FILE = rut_dat_1( 1 : long_ruta )//'TIETRANSRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        do modo = 1, maxmodos
           letaux = ' '           
           read ( 84, 100, iostat = ierror ) letaux
           read ( letaux, *, iostat = errleca )  ( TiempoTrans ( unidad, modo, columna ), columna = 1, maxmodos )
           if ( errleca .ne. 0 ) go to 1012
        end do
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO TIETRANSRD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1012 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS TIETRANSRD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endiF

CLOSE ( UNIT = 84 )

ierror = 0

!Imprime a debugger maximo de transiciones entre modos

write ( 1, 100 ) '---------------------------------------------------------------' 
write ( 1, 100 ) 'TIEMPO DE TRANSICION ENTRE MODOS UNIDADES DE RANGO DISCONTINUO' 
write ( 1, 100 ) '---------------------------------------------------------------' 
!Hacer para todas las unidades de rango discontinuo
do unidad = 1, NumUniRD
    write ( 1, 100 ) 'Unidad  Nombre'
    write ( 1, 200 ) unidad, nombunird ( unidad )
    !Hacer para todas las filas
    do fila = 1, NumModRD ( unidad )
        !Hacer para todos los modos
        do columna = 1, NumModRD ( unidad )
            !Calcular los intervalos de transision dependiendo de la duracion del intervalo
            TiempoTrans ( unidad, fila, columna ) = TiempoTrans ( unidad, fila, columna ) * 60
            TiempoTrans ( unidad, fila, columna ) = TiempoTrans ( unidad, fila, columna ) / durintr
        end do
        write ( 1, 500 ) ( TiempoTrans ( unidad, fila, columna ), columna = 1, NumModRD ( unidad ) )
    end do
end do

write ( 1, 100 ) ''

!
! ---------------------------------
! * Se leen datos de COSTRANSRD      *
! ---------------------------------
OPEN (UNIT = 85, FILE = rut_dat_1( 1 : long_ruta )//'COSTRANSRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        do modo = 1, maxmodos
           letaux = ' '
           read ( 85, 100, iostat = ierror ) letaux
           read ( letaux, *, iostat = errleca )  ( CostoTrans ( unidad, modo, columna ), columna = 1, maxmodos )
           if ( errleca .ne. 0 ) go to 1013
        end do
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO COSTRANSRD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1013 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS COSTRANSRD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endiF


CLOSE ( UNIT = 85 )

ierror = 0

!Imprime a debugger costo de transiciones entre modos

write ( 1, 100 ) '--------------------------------------------------------------' 
write ( 1, 100 ) 'COSTO DE TRANSICION ENTRE MODOS UNIDADES DE RANGO DISCONTINUO' 
write ( 1, 100 ) '--------------------------------------------------------------' 
!Hacer para todas las unidades de rango discontinuo
do unidad = 1, NumUniRD
    write ( 1, 100 ) 'Unidad  Nombre'
    write ( 1, 200 ) unidad, nombunird ( unidad )
    !Hacer para todas las filas
    do fila = 1, NumModRD ( unidad )
        !Hacer para todas las columnas
        do columna = 1, NumModRD ( unidad )
            if ( CostoTrans ( unidad, fila, columna ) .eq. 0.0 .and. fila .ne. columna .and. fila .gt. 1 &
                 .and. TransFacti ( unidad, fila, columna ) .eq. 1 ) then
                !Pone costo de transicion en uno para evitar inicio de asignacion de modo tenga valor si no es necesario
                CostoTrans ( unidad, fila, columna ) = 1.0
            end if
        end do
        write ( 1, 1800 ) ( CostoTrans ( unidad, fila, columna ), columna = 1, NumModRD ( unidad ) )
    end do
end do

write ( 1, 100 ) ''

write ( 1, 100 ) '----------------------------------------------------'
write ( 1, 100 ) 'ASIGNABILIDAD DE MODOS DE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '----------------------------------------------------'
! ----------------------------
! * Se leen datos de ASIGNRD *
! ----------------------------
ierror = 0


OPEN (UNIT = 86, FILE = rut_dat_1( 1 : long_ruta )//'ASIGNRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
     
errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        do modo = 1, maxmodos
           letaux = ' '
           read ( 86, 100, iostat = ierror ) letaux
           read ( letaux, *, iostat = errleca )  ( AsignURD ( unidad, modo, i ), i = 1, ntintr )
           if ( errleca .ne. 0 ) go to 1014
           if ( modo .le. NumModRD ( unidad ) ) then
              bloque = ntintr / 24
              do k = 1, bloque
                 write ( 1, 800 ) 'Modo', modo
                 write ( 1, 2000 ) ( AsignURD ( unidad, modo, i ), i = 24 * k - 23 , 24 * k  )
              end do
              if ( ntintr - 24 * bloque .gt. 0 ) then
                 write ( 1, 800 ) 'Modo', modo
                 write ( 1, 2000 ) ( AsignURD ( unidad, modo, i ), i = 24 * bloque + 1 , ntintr  )
              end if
            end if
        end do
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO ASIGNRD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1014 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS ASIGNRD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endiF

CLOSE ( UNIT = 86 )
!goto 2222
write ( 1, 100 ) ''

write ( 1, 100 ) '----------------------------------------------------'
write ( 1, 100 ) 'DISPONIBILIDAD DE MODOS DE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '----------------------------------------------------'
! ----------------------------
! * Se leen datos de DISPORD *
! ----------------------------
ierror = 0

OPEN (UNIT = 86, FILE = rut_dat_1( 1 : long_ruta )//'DISPORD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
     
errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        do modo = 1, maxmodos
           letaux = ' '
           read ( 86, 100, iostat = ierror ) letaux
           read ( letaux, *, iostat = errleca )  ( DispoURD ( unidad, modo, i ), i = 1, ntintr )
           if ( errleca .ne. 0 ) go to 1015
           if ( modo .le. NumModRD ( unidad ) ) then
              bloque = ntintr / 24
              do k = 1, bloque
                 write ( 1, 800 ) 'Modo', modo
                 write ( 1, 2000 ) ( DispoURD ( unidad, modo, i ), i = 24 * k - 23 , 24 * k  )
              end do
              if ( ntintr - 24 * bloque .gt. 0 ) then
                 write ( 1, 800 ) 'Modo', modo
                 write ( 1, 2000 ) ( DispoURD ( unidad, modo, i ), i = 24 * bloque + 1 , ntintr  )
              end if
            endif
        end do
    enddo
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO DISPORD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1015 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS DISPORD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endiF


CLOSE ( UNIT = 86 )
2222 continue

write ( 1, 100 ) ''

!Se calcula disponibilidad y coordinabilidad de los modos de unidades de Rango Discontinuo en base a su oferta
call Dispo_Coord_RD

ierror = 0

write ( 1, 100 ) '-------------------------------------------------------------'
write ( 1, 100 ) 'INFORMACION DE PROCESO DE ARRANQUE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '-------------------------------------------------------------'
! ----------------------------
! * Se leen datos de ARRARD *
! ----------------------------

write ( 1, 100 ) 'Unidad  Nombre               Pot. Sinc.        Pot. Sinc. NR 10    Pot. Sinc. NR Su  Rampa Arr. MW/interv.'

OPEN (UNIT = 81, FILE = rut_dat_1( 1 : long_ruta )//'ARRARD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
     
errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        do modo = 1, maxmodos
            letaux = ' '
            read ( 81, 100, iostat = ierror ) letaux
            read ( letaux, *, iostat = errleca )  PotSincURD ( unidad, modo ), PotSincNR10URD ( unidad, modo ), PotSincNRSURD ( unidad, modo ), RampArraURD ( unidad, modo )
            if ( errleca .ne. 0 ) go to 1016
            RampArraURD ( unidad, modo ) = RampArraURD ( unidad, modo ) * durintr
            if ( modo .le. NumModRD ( unidad ) ) then
               write ( 1, 800 ) 'Modo', modo
               write ( 1, 1600 ) PotSincURD ( unidad, modo ), PotSincNR10URD ( unidad, modo ), PotSincNRSURD ( unidad, modo ), RampArraURD ( unidad, modo )
            end if
        end do
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO ARRARD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1016 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS ARRARD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endiF

CLOSE ( UNIT = 81 )

!Escalamiento
PotSincURD = PotSincURD / Base
PotSincNR10URD =  PotSincNR10URD / Base
PotSincNRSURD = PotSincNRSURD / Base
RampArraURD = RampArraURD / Base


write ( 1, 100 ) ''

ierror = 0

! ------------------------------
! * Se leen datos de COVAARRD *
! ------------------------------
write ( 1, 100 ) '-------------------------------------------------------'
write ( 1, 100 ) 'COSTOS VARIABLES DE ARRANQUE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '-------------------------------------------------------'

OPEN (UNIT = 87, FILE = rut_dat_1( 1 : long_ruta )//'COVAARRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
     
errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        do segmento = 1, maxsegarrd
            letaux = ' '
            read ( 87, 100, iostat = ierror ) letaux
            read ( letaux, *, iostat = errleca )  ( TiemInicioArrRDS ( unidad, modo, segmento ), modo = 1, maxmodos ),  ( CostoArrRDS ( unidad, modo, segmento ), modo = 1, maxmodos )
            if ( errleca .ne. 0 ) go to 1017
        end do                    
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO COVAARRD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1017 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS COVAARRD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 87 )

!Calcular cuantos segmentos de costos de arranque variable por unidad 
do unidad = 1, NumUniRD
    !Hacer para todos los modos de la unidad
    do modo = 1, NumModRD(unidad)
        numseg = 0
        !Hacer para todos los segmentos
        do segmento = 1, maxsegarrd
        !Encontrar segmento con tiempo cero
            if ( TiemInicioArrRDS ( unidad, modo, segmento ) .ne. 0 ) then
                numseg = numseg + 1
            end if            
        end do
        NmBloArrURD ( unidad, modo ) = numseg
    end do
end do
!Escribir a bitacora
!Hacer para todas las unidades
do unidad = 1, NumUniRD
    modo_prev = 0
    do modo = 1, NumModRD(unidad)
        !Hacer para todos los segmentos del modo
        do segmento = 1, NmBloArrURD ( unidad, modo )
            if ( modo_prev .ne. modo ) then
                write ( 1, 800 ) 'Modo', modo
                modo_prev = modo
            end if
            !Modificar intervalos en paro dependiendo de la duracion del intervalo
            TiemInicioArrRDS ( unidad, modo, segmento ) = TiemInicioArrRDS ( unidad, modo, segmento ) * 60
            TiemInicioArrRDS ( unidad, modo, segmento ) = TiemInicioArrRDS ( unidad, modo, segmento ) /durintr
            write ( 1, 2200 ) unidad, nombunird ( unidad ), 'Hr', TiemInicioArrRDS ( unidad, modo, segmento )
            write ( 1, 2100 ) unidad, nombunird ( unidad ), '$', CostoArrRDS ( unidad, modo, segmento )
        end do        
    end do
end do

ierror = 0

write ( 1, 100 ) ''

unidad = 1
write ( 1, 100 ) '---------------------------------'
write ( 1, 100 ) 'RAMPAS UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '---------------------------------'
! ----------------------------
! * Se leen datos de RAMPASRD *
! ----------------------------

write ( 1,  90 ) 'Unidad  Nombre             Op. Subida Op. Bajada   Emer. 10   Emer. ', minresup, '    Reg. ', minresre, 'Reg Termico'

OPEN (UNIT = 88, FILE = rut_dat_1( 1 : long_ruta )//'RAMPASRD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
     
errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        do modo = 1, maxmodos
           letaux = ' '
           read ( 88, 100, iostat = ierror ) letaux
           read ( letaux, *, iostat = errleca )  rampaup, rampadown, emergencia, regulacion, regimen
           if ( errleca .ne. 0 ) go to 1018
           RampaSubURD ( unidad, modo ) = rampaup * durintr
           RampaBajURD ( unidad, modo ) = rampadown * durintr
           !Se verifica la rampa de emergencia de 10 minutos vs duracion del intervalo para reserva rodante de 10 min
           if ( durintr .lt. 10 ) then
              RamEmer10RD ( unidad, modo ) = emergencia * durintr
           else
              RamEmer10RD ( unidad, modo ) = emergencia * 10
           end if
           !Se verifica la rampa de emergencia de x minutos vs duracion del intervalo para reserva rodante suplementaria
           if ( durintr .lt. minresup  ) then
              RamEmerxRD ( unidad, modo ) = emergencia * durintr
           else
              RamEmerxRD ( unidad, modo ) = emergencia * minresup 
           end if
           !Se verifica la rampa de emergencia de x minutos vs duracion del intervalo para reserva rodante suplementaria
           if ( durintr .lt. minresre   ) then
             RamRegRD ( unidad, modo ) = regulacion * durintr
           else
             RamRegRD ( unidad, modo ) = regulacion * minresre 
           end if
           RTerURD ( unidad, modo ) = regimen 
           if ( modo .le. NumModRD ( unidad ) ) then
             write ( 1, 800 ) 'Modo', modo
             write ( 1, 2400 ) RampaSubURD ( unidad, modo ), RampaBajURD ( unidad, modo ), RamEmer10RD ( unidad, modo ), &
                               RamEmerxRD ( unidad, modo ), RamRegRD ( unidad, modo ), RTerURD ( unidad, modo )
           end if
        end do
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO RAMPASRD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1018 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS RAMPASRD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 88 )

!Escalamiento
RampaSubURD = RampaSubURD / Base
RampaBajURD = RampaBajURD / Base
RamEmer10RD = RamEmer10RD / Base
RamEmerxRD = RamEmerxRD / Base
RamRegRD = RamRegRD / Base
!RTerURD = RTerURD / Base


write ( 1, 100 ) ''

ierror = 0

! ---------------------------------
! * Se leen datos de POTRESRO10RD *
! * Se leen datos de PRERESRO10RD *
! ---------------------------------
!Potencia oferta de reserva rodante de 10 min
! -----------------------------------------
!Precio oferta de reserva rodante de 10 min
write ( 1, 100 ) '-----------------------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE RESERVA RODANTE DE 10 MIN DE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '-----------------------------------------------------------------'

! Abre archivo de datos de generadores
OPEN (UNIT = 89, FILE = rut_dat_1( 1 : long_ruta )//'POTRESRO10RD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 90, FILE = rut_dat_1( 1 : long_ruta )//'PRERESRO10RD.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
        unidad = unidad + 1
        do modo = 1, maxmodos
           letaux = ' '; letaux_1 = ' '
           read ( 89, 100, iostat = ierror ) letaux
           read ( 90, 100, iostat = ierror_1 ) letaux_1
           read ( letaux, *, iostat = errleca )  ( OferResR10RD ( unidad, modo, intervalo ), intervalo = 1, ntintr )
           read ( letaux_1, *, iostat = errlecb )  ( PreVenResR10RD ( unidad, modo, intervalo ), intervalo = 1, ntintr )
           if ( errleca .ne. 0 .and. errlecb .ne. 0 ) go to 1019
           if ( modo .le. NumModRD ( unidad ) ) then
              bloque = ntintr / 24
              do k = 1, bloque
                 write ( 1, 800 ) 'Modo', modo
                 write ( 1, 1200 ) 'Pot:', ( OferResR10RD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                 write ( 1, 1200 ) '$  :', ( PreVenResR10RD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
              end do
              if ( ntintr - 24 * bloque .gt. 0 ) then
                 write ( 1, 800 ) 'Modo', modo
                 write ( 1, 1200 ) 'Pot:', ( OferResR10RD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                 write ( 1, 1200 ) '$  :', ( PreVenResR10RD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
              end if
           end if
        end do
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESRO10RD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESRO10RD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1019 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS POTRESRO10RD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS PRERESRO10RD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endif
        
CLOSE ( UNIT = 89 )
CLOSE ( UNIT = 90 )

write ( 1, 100 ) ''

!Escalamiento
OferResR10RD = OferResR10RD / Base
PreVenResR10RD = PreVenResR10RD * Base

ierror = 0
ierror_1 = 0

! ---------------------------------
! * Se leen datos de POTRESROSURD *
! * Se leen datos de PRERESROSURD *
! ---------------------------------
!Potencia oferta de reserva rodante suplementaria
! -----------------------------------------
!Precio oferta de reserva rodante suplementaria
write ( 1, 100 ) '--------------------------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE RESERVA RODANTE SUPLEMENTARIA DE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '---------------------------------------------------------------------'

! Abre archivo de datos de generadores
OPEN (UNIT = 91, FILE = rut_dat_1( 1 : long_ruta )//'POTRESROSURD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 92, FILE = rut_dat_1( 1 : long_ruta )//'PRERESROSURD.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
       unidad = unidad + 1
       do contador = 1, maxmodos
          letaux = ' '; letaux_1 = ' '
          read ( 91, 100, iostat = ierror ) letaux
          read ( 92, 100, iostat = ierror_1 ) letaux_1
          read ( letaux, *, iostat = errleca )  ( OferResRxRD ( unidad, modo, intervalo ), intervalo = 1, ntintr )
          read ( letaux_1, *, iostat = errlecb )  ( PreVenResRxRD ( unidad, modo, intervalo ), intervalo = 1, ntintr )
          if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1020
          if ( modo .le. NumModRD ( unidad ) ) then
             bloque = ntintr / 24
             do k = 1, bloque
               write ( 1, 800 ) 'Modo', modo
               write ( 1, 1200 ) 'Pot:', ( OferResRxRD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
               write ( 1, 1200 ) '$  :', ( PreVenResRxRD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
             end do
             if ( ntintr - 24 * bloque .gt. 0 ) then
               write ( 1, 800 ) 'Modo', modo
               write ( 1, 1200 ) 'Pot:', ( OferResRxRD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
               write ( 1, 1200 ) '$  :', ( PreVenResRxRD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
             end if
          end if
       end do
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESROSURD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESROSURD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1020 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS POTRESROSURD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS PRERESROSURD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endif
        
CLOSE ( UNIT = 91 )
CLOSE ( UNIT = 92 )

write ( 1, 100 ) ''

!Escalamiento
OferResRxRD = OferResRxRD / Base
PreVenResRxRD = PreVenResRxRD * Base

ierror = 0
ierror_1 = 0
ierror_2 = 0
! ---------------------------------
! * Se leen datos de POTRESRESERD *
! * Se leen datos de PRERESRESERD *
! * Se leen datos de MINRESRESERD *
! ---------------------------------
!Potencia oferta de reserva de regulacion secundaria
! -----------------------------------------
!Precio oferta de reserva de regulacion secundaria
write ( 1, 100 ) '------------------------------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE RESERVA DE REGULACION SECUNDARIA DE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '------------------------------------------------------------------------'
! Abre archivo de datos de generadores
OPEN (UNIT = 93, FILE = rut_dat_1( 1 : long_ruta )//'POTRESRESERD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 94, FILE = rut_dat_1( 1 : long_ruta )//'PRERESRESERD.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

if ( SiResRegDis .eq. 1 ) then
!   Abre archivo de datos de reserva distribuida
    OPEN (UNIT = 95, FILE = rut_dat_1( 1 : long_ruta )//'MINRESRESERD.csv', IOSTAT = IERROR_2, STATUS='OLD', RECORDSIZE = 250)
endif
if ( SiResRegDis .eq. 0 ) IERROR_2 = 0

errleca = 0; errlecb = 0; errlecc = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 .and. ierror_2 .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
       unidad = unidad + 1
       do modo = 1, maxmodos
          read ( 93, 100, iostat = ierror ) letaux
          read ( 94, 100, iostat = ierror_1 ) letaux_1
          if ( SiResRegDis .eq. 1 ) then
             read ( 95, 100, iostat = ierror_2 ) letaux_2
          endif
          letaux = ' '; letaux_1 = ' '; letaux_2 = ' '
          read ( letaux, *, iostat = errleca )  ( OferResRegRD ( unidad, modo, intervalo ), intervalo = 1, ntintr )
          read ( letaux_1, *, iostat = errlecb )  ( PreVenResRegRD ( unidad, modo, intervalo ), intervalo = 1, ntintr )
          if ( SiResRegDis .eq. 1 ) then
             read ( letaux_2, *, iostat = errlecc )  ( MrreURD ( unidad, modo, intervalo ), intervalo = 1, ntintr )
          endif
          if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0 ) go to 1021
          if ( modo .le. NumModRD ( unidad ) ) then
             bloque = ntintr / 24
             do k = 1, bloque
                 write ( 1, 800 ) 'Modo', modo
                 write ( 1, 1200 ) 'Pot:', ( OferResRegRD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                 write ( 1, 1200 ) '$  :', ( PreVenResRegRD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                 if ( SiResRegDis .eq. 1 ) then
                    write ( 1, 1200 ) 'PMi:', ( MrreURD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                 endif
              end do
              if ( ntintr - 24 * bloque .gt. 0 ) then
                 write ( 1, 800 ) 'Modo', modo
                 write ( 1, 1200 ) 'Pot:', ( OferResRegRD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                 write ( 1, 1200 ) '$  :', ( PreVenResRegRD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                 if ( SiResRegDis .eq. 1 ) then
                    write ( 1, 1200 ) 'Pmi:', ( MrreURD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                 endif
              end if
          end if
        end do
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESRESERD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESRESERD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_2 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO MINRESRESERD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  bmensaje = ' ' 
  call ParaProceso ( bmensaje )
end if

1021 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0  ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS POTRESROSURD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS PRERESROSURD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    if ( errlecc .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS MINRESRESERD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endif
        
CLOSE ( UNIT = 93 )
CLOSE ( UNIT = 94 )
if ( SiResRegDis .eq. 1 ) then
    CLOSE ( UNIT = 95 )
endif

write ( 1, 100 ) ''

!Escalamiento
OferResRegRD = OferResRegRD / Base
PreVenResRegRD = PreVenResRegRD * Base
MrreURD = MrreURD / Base

ierror = 0
ierror_1 = 0

! ---------------------------------
! * Se leen datos de POTRESNR10RD *
! * Se leen datos de PRERESNR10RD *
! ---------------------------------
!Potencia oferta de reserva no rodante de 10 min
! -----------------------------------------
!Precio oferta de reserva no rodante de 10 min
write ( 1, 100 ) '--------------------------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE RESERVA NO RODANTE DE 10 MIN DE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '--------------------------------------------------------------------'
! Abre archivo de datos de generadores
OPEN (UNIT = 95, FILE = rut_dat_1( 1 : long_ruta )//'POTRESNR10RD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 96, FILE = rut_dat_1( 1 : long_ruta )//'PRERESNR10RD.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
       unidad = unidad + 1
       do modo = 1, maxmodos
          letaux = ' '; letaux_1 = ' '
          read ( 95, 100, iostat = ierror ) letaux
          read ( 96, 100, iostat = ierror_1 ) letaux_1
          read ( letaux, *, iostat = errleca )  ( OferResNR10RD ( unidad, modo, intervalo ), intervalo = 1, ntintr )
          read ( letaux_1, *, iostat = errlecb )  ( PreVenResNR10RD ( unidad, modo, intervalo ), intervalo = 1, ntintr )
          if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1022
          if ( modo .le. NumModRD ( unidad ) ) then
              bloque = ntintr / 24
              do k = 1, bloque
                 write ( 1, 800 ) 'Modo', modo
                 write ( 1, 1200 ) 'Pot:', ( OferResNR10RD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                 write ( 1, 1200 ) '$  :', ( PreVenResNR10RD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
              end do
              if ( ntintr - 24 * bloque .gt. 0 ) then
                 write ( 1, 800 ) 'Modo', modo
                 write ( 1, 1200 ) 'Pot:', ( OferResNR10RD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                 write ( 1, 1200 ) '$  :', ( PreVenResNR10RD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
              end if
         end if
       end do
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESNR10RD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESNR10RD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1022 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS POTRESNR10RD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS PRERESNR10RD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 95 )
CLOSE ( UNIT = 96 )

write ( 1, 100 ) ''

!Escalamiento
OferResNR10RD = OferResNR10RD / Base
PreVenResNR10RD = PreVenResNR10RD * Base

ierror = 0
ierror_1 = 0
i = 0

unidad = 1
! ---------------------------------
! * Se leen datos de POTRESNRSURD *
! * Se leen datos de PRERESNRSURD *
! ---------------------------------
!Potencia oferta de reserva no rodante suplementaria
! -----------------------------------------
!Precio oferta de reserva no rodante suplementaria
write ( 1, 100 ) '------------------------------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE RESERVA NO RODANTE SUPLEMENTARIA DE UNIDADES RANGO DISCONTINUO'
write ( 1, 100 ) '------------------------------------------------------------------------'
! Abre archivo de datos de generadores
OPEN (UNIT = 97, FILE = rut_dat_1( 1 : long_ruta )//'POTRESNRSURD.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 98, FILE = rut_dat_1( 1 : long_ruta )//'PRERESNRSURD.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRD ) 
       unidad = unidad + 1
       do modo = 1, maxmodos
          letaux = ' '; letaux_1 = ' '
          read ( 97, 100, iostat = ierror ) letaux
          read ( 98, 100, iostat = ierror_1 ) letaux_1
          read ( letaux, *, iostat = errleca )  ( OferResNRxRD ( unidad, modo, intervalo ), intervalo = 1, ntintr )
          read ( letaux_1, *, iostat = errlecb )  ( PreVenResNRxRD ( unidad, modo, intervalo ), intervalo = 1, ntintr )
          if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1023
          if ( modo .le. NumModRD ( unidad ) ) then
              bloque = ntintr / 24
              do k = 1, bloque
                  write ( 1, 800 ) 'Modo', modo
                  write ( 1, 1200 ) 'Pot:', ( OferResNRxRD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
                  write ( 1, 1200 ) '$  :', ( PreVenResNRxRD ( unidad, modo, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
              end do
              if ( ntintr - 24 * bloque .gt. 0 ) then
                  write ( 1, 800 ) 'Modo', modo
                  write ( 1, 1200 ) 'Pot:', ( OferResNRxRD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
                  write ( 1, 1200 ) '$  :', ( PreVenResNRxRD ( unidad, modo, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
              end if
           end if
       end do
    end do
else	
  if ( ierror .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESNRSURD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
  if ( ierror_1 .ne. 0 ) then   
     bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESNRSURD.csv'
     call EnviaMensajeError ( bmensaje )
  endif
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1023 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS POTRESNRSURD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS PRERESNRSURD.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunird ( unidad ))
   call ParaProceso ( bmensaje )
endif
        
CLOSE ( UNIT = 97 )
CLOSE ( UNIT = 98 )

write ( 1, 100 ) ''

!Escalamiento
OferResNRxRD = OferResNRxRD / Base
PreVenResNRxRD = PreVenResNRxRD * Base

90  format ( a60, i2,a9, i2 ) 
100  format ( a )
200  format ( i3, 5x, a12, 4x, a2, 7x, a3, 8x, a9, 4x, a9 )    
300  format ( 5x, i3, 7x, a12, x, a2, 4x, i4 ) 
301  format ( 5x, i3, 7x, a12, x, a2, 4x, i4, f9.2 )  
400  format ( i3, 5x, a5 )    
500  format ( 10 (i2, 2x) )  
600  format ( a10, x, i3, x, a1, i3 )     
700  format ( i3, x, a12, x, 24 (f9.2, 2x) )   
800  format ( a4, x, i1 )   
900  format ( 10x, 24 (f9.2, 2x) )   
1000 format ( i3, x, a12, x, a5, x, 24 (f9.2, 2x) )  
1100  format ( i3, x, a12, x, a4, x, 24 (f9.2, 2x) )   
1200  format ( 17x, a4, x, 24 (f9.2, 2x) )   
1300  format ( i3, 5x, a12, 3x, i2, 6x, i3, 3x, f9.2 )   
1400  format ( 23x, i2, 6x, i3, 3x, f9.2 )   
1500  format ( i3, 5x, a12, 6x, 3 (f9.2, 11x), f9.2 )  
1600  format ( 26x, 3 (f9.2, 11x), f9.2 )  
1700  format ( a4, x, i2, a1,x, 10 (i2, 2x) )  
1800  format ( 10 (f9.2, 2x) ) 
1900  format ( i3, x, a12, x, 24 (i1, 2x) )   
2000  format ( 17x, 24 (i1, 2x) )   
2100 format ( i3, x, a12, x, a2, x, f10.2 )    
2200 format ( i3, x, a12, x, a2, x, i10 )    
2300 format ( i3, 5x, a12, 6x, 5 ( f9.2, 2x ), f10.4 ) 
2400 format ( 26x, 5 ( f9.2, 2x ), f10.4 )
2500 format ( i3, x, a12, x, 24 (i4, 2x) )    


end subroutine data_rd