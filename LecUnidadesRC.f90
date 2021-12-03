! *************************************************************************
! Lectura de datos de unidades de rango continuo                          *
!**************************************************************************
!
Subroutine LecUnidadesRC

Implicit none

   ! Lectura de generadores de rango continuo
   call LecGeneradoresRC
   
   call data_rc
   
return
end
    
! *************************************************************************
! Lectura de datos generales de unidades de rango continuo                *
!**************************************************************************
Subroutine LecGeneradoresRC

use ParAUHE


implicit none

integer i, k, ierror, ierror_1, intervalo, nodo, unidad, noddist, IndiceTemp, zona, grupo, auxiliar ( maxgrure ), auxiliar_1 ( maxgrute ), &
        tempnodrc ( maxint ), subsistema, Indice ( maxzondist ), contador, bloque, ZonaInd, nodi, NodDisZona ( maxzondist, maxnodist, maxint ), &
        NoNodDisZona ( maxzondist, maxint ), nodomax ( maxurc ), Importacion, ierror_2, ierror_3, errleca, errlecb, errlecc, errlecd

logical  YaEsta

character*3000 letaux, letaux_1, letaux_2 , letaux_3, letaux_4, letaux_5
character*20 nombre, propietario
character*7 tipo
character*1 let
character*5 letr

real*8  FacDisGen ( maxzondist, maxnodist, maxint ), regimen

!Inicializacion de variables
unidad = 0
nombunirc = ''
tiunidrc = ''
nodorc = 0
unidadrc_area = ''
IslaGenRC = 0
nombre = ''
tipo = ''
propietario = ''
tempnodrc = 0
NumUniRC = 0
corresprc = 0
i = 0
ierror = 0
auxiliar = 0 
zona = 0
UniGruResRC = 0
NodDist = 0
Indice = 0
IndiceTemp = 0
ierror_1 = 0
NodDisZona = 0
NoNodDisZona = 0
nodomax = 0
ImpEnt = 0
Importacion = 0
regimen = 0.0

! ---------------------------------
! * Se leen datos de UNITRC      *
! * Se leen datos de ZONASRESURC *
! * Se leen datos de GPORC       *
! * Se leen datos de NODOSRC     *
! ---------------------------------

OPEN (UNIT = 5, FILE = rut_dat_1( 1 : long_ruta )//'UNITRC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 33, FILE = rut_dat_1( 1 : long_ruta )//'ZONASRESURC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 117, FILE = rut_dat_1( 1 : long_ruta )//'GPORC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
OPEN (UNIT = 191, FILE = rut_dat_1( 1 : long_ruta )//'NODOSRC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
write ( 1, 100 ) '--------------------------' 
write ( 1, 100 ) 'UNIDADES DE RANGO CONTINUO' 
write ( 1, 100 ) '--------------------------' 
errleca = 0; errlecb = 0; errlecc = 0; errlecd = 0;
if ( ierror .eq. 0 ) then
!    write ( 1, 100 ) 'Unidad  Nombre          Tipo  Propietario    Nodo     Area        Subsistema      Nodo Dist.       Indice   Reg Termico' 
    write ( 1, 100 ) 'Unidad  Nombre          Tipo  Propietario    Nodo     Area        Subsistema      Nodo Dist.       Indice   Reg Termico   Importacion' 
    ! Lee información hasta encontrar fin de información
    do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 )        
	    read ( 5, 100, iostat = ierror ) letaux
        read ( 33, 100, iostat = ierror_1 ) letaux_1
        read ( 117, 100, iostat = ierror_2 ) letaux_2
        read ( 191, 100, iostat = ierror_3 ) letaux_3
	    if ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 .and. len_trim(letaux_1) .ne. 0 .and. len_trim(letaux_2) .ne. 0 .and. len_trim(letaux_3) .ne. 0 ) then
            if ( ierror .eq. 0 .and. ( ierror_1 .ne. 0 .or. ierror_2 .ne. 0 .or. ierror_3 .ne. 0 ) ) then
                if ( ierror_1 .ne. 0 ) then
                    bmensaje = '!!! INFORMACION INCOMPLETA EN ZONASRESURC.csv'
                    Call EnviaMensajeError ( bmensaje )
                endif
                if ( ierror_2 .ne. 0 ) then
                    bmensaje = '!!! INFORMACION INCOMPLETA EN GPORC.csv'
                    Call EnviaMensajeError ( bmensaje )
                endif
                if ( ierror_3 .ne. 0 ) then
                    bmensaje = '!!! INFORMACION INCOMPLETA EN  NODOSRC_DERS'
                    Call EnviaMensajeError ( bmensaje )
                endif
            endif
!           read ( letaux, *, iostat = errleca )  nombre, tipo, propietario, let, let, NodDist, IndiceTemp
            read ( letaux, *, iostat = errleca )  nombre, tipo, propietario, let, let, NodDist, IndiceTemp, regimen
!           read ( letaux, *, iostat = errleca )  nombre, tipo, propietario, let, let, NodDist, IndiceTemp, regimen, Importacion
            read ( letaux_1, *, iostat = errlecb )  ( auxiliar ( zona ), zona = 1, maxgrure )
            read ( letaux_2, *, iostat = errlecc )  ( auxiliar_1 ( grupo ), grupo = 1, maxgrute )
            if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0 ) go to 1001
            !Si tiene nodos distribuidos se leen archivos de nodos distribuidos
            if ( NodDist .eq. 0 ) then
               read ( letaux_3, *, iostat = errlecd )  ( tempnodrc ( intervalo ), intervalo = 1, ntintr )
               if ( errlecd .ne. 0 ) go to 1001
               i = i + 1
               if ( i .gt. maxurc ) then
                  bmensaje = 'ERROR SE REBASA EL MAXIMO NUMERO'
                  call EnviaMensajeError ( bmensaje )
                  bmensaje = "DE UNIDADES DE RANGO CONTINUO"
                  call ParaProceso ( bmensaje )
               endif
               !Hacer para todos los nodos
               do nodo = 1, NumNodos
                  !Se revisa pertenencia al subsitema con el nodo del primer intervalo
                  if ( tempnodrc ( 1 ) .eq. nodo ) then
                     !Hacer para todos los subsistemas
                     do subsistema = 1, numsis
                        if ( nodo_subsis ( nodo ) .eq. nomsis ( subsistema ) ) then
                            !Ver si el subsistema esta activo
                            if ( EstadoIsla ( subsistema ) .eq. 1 ) then
                                unidad = unidad + 1
                                IslaGenRC ( unidad ) = subsistema
                                !Se asigna area con el nodo del primer intervalo
                                unidadrc_area ( unidad ) = nodo_area ( nodo )
                                nombunirc ( unidad ) = nombre
                                tiunidrc ( unidad ) = tipo
                                proprc ( unidad ) = propietario                   
                                !nodorc ( unidad, : ) =  tempnodrc ( : )
                                !Numero de nodos distribuidos por unidad de rango continuo
                                NoNodDisRC ( unidad, : ) = 1
                                Tempnodorc ( unidad, 1, : ) = tempnodrc ( : )
                                facdistgen ( unidad, 1, : ) = 1
                                corresprc ( unidad ) = i
                                ImpEnt ( unidad ) = Importacion
                                UniGruResRC ( unidad, : ) =  auxiliar
                                UniGruTerRC ( unidad, : ) =  auxiliar_1
                                !se escribe a debugger
!                               write ( 1, 200 ) unidad, nombunirc ( unidad ), tiunidrc ( unidad ), proprc ( unidad ), Tempnodorc ( unidad, 1, 1 ), &
!                               unidadrc_area ( unidad ), nomsis ( IslaGenRC ( unidad ) ), NodDist, IndiceTemp, regimen
                                write ( 1, 200 ) unidad, nombunirc ( unidad ), tiunidrc ( unidad ), proprc ( unidad ), Tempnodorc ( unidad, 1, 1 ), &
                                unidadrc_area ( unidad ), nomsis ( IslaGenRC ( unidad ) ), NodDist, IndiceTemp, regimen, ImpEnt ( unidad )
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
311                  end do
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
                !Se revisa pertenencia al subsitema con el primer nodo del primer intervalo
                if ( NodDisZona ( IndiceTemp, 1, 1 ) .eq. nodo ) then
                  !Hacer para todos los subsistemas
                  do subsistema = 1, numsis
                     if ( nodo_subsis ( nodo ) .eq. nomsis ( subsistema ) ) then
                        !Ver si el subsistema esta activo
                        if ( EstadoIsla ( subsistema ) .eq. 1 ) then
                           unidad = unidad + 1
                           IslaGenRC ( unidad ) = subsistema
                           !Se asigna area con el nodo del primer intervalo
                           unidadrc_area ( unidad ) = nodo_area ( nodo )
                           nombunirc ( unidad ) = nombre
                           tiunidrc ( unidad ) = tipo
                           proprc ( unidad ) = propietario         
                           !!!!!!!!!!!!!!
                           !Hacer para todos los intervalos
                           do intervalo = 1, ntintr
                              !Numero de nodos distribuidos por unidad de rango continuo
                              NoNodDisRC ( unidad, intervalo ) = NoNodDisZona ( IndiceTemp, intervalo )
                              !NoNodDisRC ( unidad ) = NoNodDisZona ( IndiceTemp, intervalo )
                              !hacer para todos los nodos distribuidos
                              do NoDi = 1, NoNodDisZona ( IndiceTemp, intervalo )
                                  if ( NoDi .gt. nodomax ( unidad ) ) then
                                      nodomax ( unidad ) = NoDi
                                  end if
                                  !Tempnodorc ( unidad, NoDi, : ) = NodDisZona ( IndiceTemp, NoDi )
                                  !facdistgen ( unidad, NoDi, : ) = FacDisGen ( IndiceTemp, NoDi, : )
                                  Tempnodorc ( unidad, NoDi, intervalo ) = NodDisZona ( IndiceTemp, NoDi, intervalo )
                                  facdistgen ( unidad, NoDi, intervalo ) = FacDisGen ( IndiceTemp, NoDi, intervalo )
                              end do                                    
                            end do
                            !!!!!!!!!!!!!!!!!
                            corresprc ( unidad ) = i
                            UniGruResRC ( unidad, : ) =  auxiliar
                            UniGruTerRC ( unidad, : ) =  auxiliar_1
                            !se escribe a debugger
    !                       write ( 1, 200 ) unidad, nombunirc ( unidad ), tiunidrc ( unidad ), proprc ( unidad ), Tempnodorc ( unidad, 1, 1 ), &
    !                       unidadrc_area ( unidad ), nomsis ( IslaGenRC ( unidad ) ), NodDist, IndiceTemp, regimen
                            write ( 1, 200 ) unidad, nombunirc ( unidad ), tiunidrc ( unidad ), proprc ( unidad ), Tempnodorc ( unidad, 1, 1 ), &
                            unidadrc_area ( unidad ), nomsis ( IslaGenRC ( unidad ) ), NodDist, IndiceTemp, regimen, ImpEnt ( unidad )
                            exit
                         end if
                      end if
                   end do
                end if
             end do
          end if
          RTerURC ( unidad ) = regimen
       endif
    enddo
else	
   if ( ierror .ne. 0 ) then
     bmensaje = 'ERROR DE LECTURA ARCHIVO UNITRC.csv'
     call EnviaMensajeError ( bmensaje )
   endif 
   if ( ierror_1 .ne. 0 ) then
     bmensaje = 'ERROR DE LECTURA ARCHIVO ZONASRESURC.csv'
     call EnviaMensajeError ( bmensaje )
   endif  
   if ( ierror_2 .ne. 0 ) then
     bmensaje = 'ERROR DE LECTURA ARCHIVO GPORC.csv'
     call EnviaMensajeError ( bmensaje )
   endif  
   if ( ierror_3 .ne. 0 ) then
     bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSRC_DERS'
     call EnviaMensajeError ( bmensaje )
   endif  
   bmensaje = ' ' 
   call ParaProceso ( bmensaje )
end if

1001 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0 .or. errlecd .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS UNITRC.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO ZONASRESURC.csv'
       call EnviaMensajeError ( bmensaje )
   endif
   call ParaProceso ( bmensaje )
    if ( errlecc .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS GPORC.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecd .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO NODOSRC_DERS'
       call EnviaMensajeError ( bmensaje )
   endif
   Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
   call ParaProceso ( bmensaje )
endif

NumUniRC = unidad
!Total de unidades de todos los sistemas de la base de datos
SisUniRC = i

!RTerURC = RTerURC / Base

CLOSE ( UNIT = 5 )
CLOSE ( UNIT = 33 )
CLOSE ( UNIT = 117 )
close ( UNIT = 191 )



write ( 1, 100 ) ''

write ( 1, 100 ) '-----------------------------------'
write ( 1, 100 ) 'NODOS DE UNIDADES DE RANGO CONTINUO'
write ( 1, 100 ) '-----------------------------------'
!Hacer para todoas las unidades de rango continuo
do unidad = 1, NumUniRC
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        write ( 1, 1400 ) unidad, nombunirc ( unidad ), ( Tempnodorc ( unidad, 1, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        write ( 1, 1400 ) unidad, nombunirc ( unidad ), ( Tempnodorc ( unidad, 1, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
    end if
end do

write ( 1, 100 ) ''


write ( 1, 100 ) '-------------------------------------'
write ( 1, 100 ) 'FACTORES DE DITRIBUCION DE GENERACION'
write ( 1, 100 ) '-------------------------------------'
!Hacer para todoas las unidades de rango continuo
do unidad = 1, NumUniRC
    !hacer para todos los nodos distribuidos
    do NoDi = 1, nodomax ( unidad ) !NoNodDisRC ( unidad )
        bloque = ntintr / 24
        do k = 1, bloque
            write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
            write ( 1, 1400 ) unidad, 'Nodo:', ( Tempnodorc ( unidad, NoDi, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 1600 ) unidad, 'FDG :', ( facdistgen ( unidad, NoDi, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
            write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
            write ( 1, 1400 ) unidad, 'Nodo:', ( Tempnodorc ( unidad, NoDi, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 1600 ) unidad, 'FDG :', ( facdistgen ( unidad, NoDi, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end if
    end do
end do

write ( 1, 100 ) ''

100  format ( a )
!200  format ( i3, 5x, a12, 4x, a2, 4x, a3, 12x, i4, 5x, a9, 3x, a5, 18x, i1, 18x, i2, 5x, f10.4 ) 
200  format ( i3, 5x, a12, 4x, a2, 4x, a3, 12x, i4, 5x, a9, 3x, a5, 18x, i1, 10x, i2, 5x, f10.4, 5x, i2 )
300  format ( a10, x, i3, x, a1, i3 )      
1400 format ( i3, x, a12, x, 24 (i6, 2x) )    
1500 format ( i1 )
1600 format ( i3, x, a12, x, 24 (f6.3, 2x) )    
     
return
end
    
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
    
SUBROUTINE data_rc

use ParAUHE, only: rut_dat_1, long_ruta, bmensaje, nombunirc, nodorc, NumUniRC, &
                  proprc, NumNodos, numsis, nodo_subsis, nomsis, EstadoIsla, &
                  IslaGenRC, unidadrc_area, nodo_area, corresprc, CostoMinGRC, ntintr, &
                  OferVenEnerRC, PreVenEnerRC, maxsegrc, NumBloVRC, PotMaxGRC, PotMinGRC, &
                  EstadoCIURC, NumHCIURC, GenCIURC, TminParoURC, TminOperURC, NumMaxParoURC, &
                  AsignURC, PotSincURC, RampArraURC, TiempoArraURC, CostArrUniURC, &
                  TiemInicioArrRCS, CostoArrRCS, maxsegarrc, NmBloArrURC, durintr, RampaSubURC, &
                  RampaBajURC, RamEmer10RC, RamEmerxRC, RamRegRC, minresre, minresup, PotSincNR10URC, &
                  PotSincNRSURC, OferResR10RC, PreVenResR10RC, OferResNR10RC, PreVenResNR10RC, &
                  OferResRxRC, PreVenResRxRC, OferResNRxRC, PreVenResNRxRC, OferResRegRC, PreVenResRegRC, &
                  maxgrure, maxurc, UniGruResRC, Base, maxgrute, UniGruTerRC, maxint, &
                  NomEjecu, SisUniRC, SisUniH, maxzonproh, NoRaOpRC, RaOpSupRC, RaOpInfRC, maxzondist, maxnodist, maxdem, NoNodDisRC, &
                  Tempnodorc, facdistgen, NoActParRC
use ProblemaAUHE
!
IMPLICIT NONE

integer ierror, ibanbit, dummy, unidad, i, intervalo, bloque, k, ierror_1, ierror_2, segmento, &
        numseg, auxiliar ( maxgrure ), zona, contador, nodomax ( maxurc ), errleca, errlecb, errlecc

CHARACTER fecha_Ej*19

character*3000 letaux, letaux_1, letaux_2

character*20 nombre, propietario

character*7 tipo

character*5 letr

integer j, NuZonProh, sigue, iz

real*8 rampaup, rampadown, emergencia, regulacion, ZonProh ( maxurc, maxzonproh * 2), Pmin, ZonProhx ( maxurc, maxzonproh * 2)


!Inicializacion de variables
unidad = 0
nombre = ''
tipo = ''
propietario = ''
dummy = 0
i = 0
ierror = 0
CostoMinGRC = 0
OferVenEnerRC = 0.0
PreVenEnerRC = 0.0
NumBloVRC = 0
PotMinGRC = 0.0
PotMaxGRC = 0.0
EstadoCIURC = 0
NumHCIURC = 0
GenCIURC = 0.0
TminParoURC = 0
TminOperURC = 0
NumMaxParoURC = 0
AsignURC = 0
PotSincURC = 0.0
RampArraURC = 0.0
TiempoArraURC = 0
CostArrUniURC = 0.0 
TiemInicioArrRCS = 0
CostoArrRCS = 0.0
rampaup = 0.0
rampadown = 0.0
emergencia = 0.0
regulacion = 0.0
RampaSubURC = 0.0
RampaBajURC = 0.0
RamEmer10RC = 0.0
RamEmerxRC = 0.0
RamRegRC = 0.0
PotSincNR10URC = 0.0
PotSincNRSURC = 0.0
OferResR10RC = 0.0
PreVenResR10RC = 0.0 
OferResNR10RC = 0.0 
PreVenResNR10RC = 0.0
OferResRxRC = 0.0 
PreVenResRxRC = 0.0
OferResNRxRC = 0.0
PreVenResNRxRC = 0.0
OferResRegRC = 0.0
PreVenResRegRC = 0.0
auxiliar = 0 
zona = 0
ierror_1 = 0
nodomax = 0
MrreURC = 0
ierror_2 = 0
NoActParRC = 0


ierror = 0
i = 0

! ------------------------------
! * Se leen datos de CGMRC *
! ------------------------------
write ( 1, 100 ) '-----------------------------------------------------'
write ( 1, 100 ) 'COSTO DE GENERACION MINIMO UNIDADES DE RANGO CONTINUO'
write ( 1, 100 ) '-----------------------------------------------------'
OPEN (UNIT = 7, FILE = rut_dat_1( 1 : long_ruta )//'CGMRC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)

errleca = 0
if ( ierror .eq. 0 ) then
!  Lee información hasta encontrar fin de información
   do unidad = 1, NumUniRC
	 read ( 7, 100, iostat = ierror ) letaux
     !Hacer para todoas las unidades de rango continuo
     read ( letaux, *, IOSTAT = errleca )  ( CostoMinGRC ( unidad, intervalo ), intervalo = 1, ntintr )
     if ( errleca .ne. 0 ) then
        write ( letr, "(i5)" ) unidad
        call Elimina_blancos ( letr, 5)
        bmensaje = 'ERROR DE LECTURA ARCHIVO CGMRC_DERS.csv'
        Call EnviaMensajeError ( bmensaje )
        Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
        call ParaProceso ( bmensaje )
     endif
     
     bloque = ntintr / 24
     do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        write ( 1, 400 ) unidad, nombunirc ( unidad ), ( CostoMinGRC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
     end do
     if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        write ( 1, 400 ) unidad, nombunirc ( unidad ), ( CostoMinGRC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
      end if
   end do
else	
    bmensaje = 'ERROR DE LECTURA ARCHIVO CGMRC_DERS.csv'
    call ParaProceso ( bmensaje )
end if


CLOSE ( UNIT = 7 )

write ( 1, 100 ) ''

ierror = 0
i = 0

! ------------------------------
! * Se leen datos de POTVERC *
! ------------------------------
write ( 1, 100 ) '-------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE VENTA DE ENERGIA UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '-------------------------------------------------'

OPEN (UNIT = 8, FILE = rut_dat_1( 1 : long_ruta )//'POTVERC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)
OPEN (UNIT = 9, FILE = rut_dat_1( 1 : long_ruta )//'PREVERC.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 3000)

errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRC )
        ! Hacer para los todos los segmentos
        unidad = unidad + 1
        do segmento = 1, maxsegrc
           read ( 8, 100, iostat = ierror ) letaux
           read ( letaux, *, iostat = errleca )  ( OferVenEnerRC ( unidad, segmento, intervalo ), intervalo = 1, ntintr )
           read ( 9, 100, iostat = ierror ) letaux_1
           read ( letaux_1, *, iostat = errlecb)  ( PreVenEnerRC ( unidad, segmento, intervalo ), intervalo = 1, ntintr )
           if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1002
        end do                    
    end do
else	
    if ( ierror .ne. 0 ) then
        bmensaje = 'ERROR DE LECTURA ARCHIVO POTVERC_DERS.csv'
    endif
    if ( ierror_1 .ne. 0 ) then
        bmensaje = 'ERROR DE LECTURA ARCHIVO PREVERC_DERS.csv'
    endif
    bmensaje = ' '
    call ParaProceso ( bmensaje )
end if

CLOSE ( UNIT = 8 )
CLOSE ( UNIT = 9 )

1002 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVOS POTVERC_DERS.csv'
       call EnviaMensajeError ( bmensaje )
    endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO PREVERC_DERS.csv'
       call EnviaMensajeError ( bmensaje )
   endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
   call ParaProceso ( bmensaje )
endif

!Escribir a bitacora
!Hacer para todas las unidades
do unidad = 1, NumUniRC
    !Hacer para todos los bloques de 24 intervalos
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        !Hacer para todos los segmentos
        do segmento = 1, maxsegrc
            write ( 1, 500 ) unidad, nombunirc ( unidad ), 'MW', ( OferVenEnerRC ( unidad, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 500 ) unidad, nombunirc ( unidad ), '$/MWh', ( PreVenEnerRC ( unidad, segmento, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do        
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        do segmento = 1, maxsegrc
            write ( 1, 500 ) unidad, nombunirc ( unidad ), 'MW', ( OferVenEnerRC ( unidad, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 500 ) unidad, nombunirc ( unidad ), '$/MWh', ( PreVenEnerRC ( unidad, segmento, intervalo ), intervalo = 24 * bloque + 1 , ntintr )
        end do
    end if
end do

!Escalamiento
OferVenEnerRC = OferVenEnerRC / Base
PreVenEnerRC = PreVenEnerRC * Base


write ( 1, 100 ) ''

ierror = 0
ierror_1 = 0
i = 0

! -----------------------------
! * Se leen datos de LIUNITRC *
! * Se leen datos de LSUNITRC *
! -----------------------------
!Potencia mínima de unidades térmicas, PotMinGRC
! -----------------------------------------
!Potencia máxima de unidades térmicas, PotMaxGRC
write ( 1, 100 ) '---------------------------------------------------'
write ( 1, 100 ) 'POTENCIAS MINIMA Y MAXIMA DE UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '---------------------------------------------------'

! Abre archivo de datos de generadores
OPEN (UNIT = 10, FILE = rut_dat_1( 1 : long_ruta )//'LIUNITRC.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 11, FILE = rut_dat_1( 1 : long_ruta )//'LSUNITRC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRC ) 
        unidad = unidad + 1
        read ( 10, 100, iostat = ierror ) letaux
        read ( 11, 100, iostat = ierror_1 ) letaux_1
        read ( letaux, *, iostat = errleca )  ( PotMinGRC ( unidad, intervalo ), intervalo = 1, ntintr )
        read ( letaux_1, *, iostat = errlecb )  ( PotMaxGRC ( unidad, intervalo ), intervalo = 1, ntintr )
        if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1003
        bloque = ntintr / 24
        do k = 1, bloque
            write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
            write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Min:', ( PotMinGRC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
            write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Max:', ( PotMaxGRC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
         end do
         if ( ntintr - 24 * bloque .gt. 0 ) then
            write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
            write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Min:', ( PotMinGRC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
            write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Max:', ( PotMaxGRC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
         end if
    end do
else	
    if ( ierror .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO LIUNITRC_DERS.csv'
    endif
    if ( ierror_1 .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO LSUNITRC_DERS.csv'
    endif
    bmensaje = ' '
    call ParaProceso ( bmensaje )
end if
        
CLOSE ( UNIT = 10 )
CLOSE ( UNIT = 11 )

1003 continue
     
! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO LIUNITRC_DERS.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    if ( errlecb .ne. 0 ) then
       bmensaje ='ERROR DE LECTURA ARCHIVO LSUNITRC_DERS.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
    call ParaProceso ( Bmensaje )
endif

!Escalamiento
PotMinGRC = PotMinGRC / Base
PotMaxGRC = PotMaxGRC / Base

! Lectura de limites minimos de oferta de unidades de rango continuo
!call LECLIMINFOFE_URC

!Calcular cuantos segmentos se ofertaron por unidad y por intervalo
do unidad = 1, NumUniRC
    if ( unidad == 182 ) then
        continue
    endif
    !Hacer para todos los intervalos
    do intervalo = 1, ntintr
        numseg = 0
        !Hacer para todos los segmentos
        do segmento = 1, maxsegrc
            !Encontrar segmento con oferta de potencia cero
            if ( OferVenEnerRC ( unidad, segmento, intervalo ) .ne. 0.0 ) then
                numseg = numseg + 1
            else
                if ( segmento .eq. 1 .and. OferVenEnerRC ( unidad, 2, intervalo ) .gt. 0.0 ) then
                    numseg = numseg + 1
                endif
            end if            
        end do
        NumBloVRC   ( unidad, intervalo ) = numseg
        if ( TipoEjecu .eq. 2 .and. numseg .eq. 0 .and. PotMaxGRC ( unidad, intervalo ) .gt. 0.0 ) then
            NumBloVRC   ( unidad, intervalo ) = 1
        endif
    end do
end do


write ( 1, 100 ) ''

! si se desea considerar limites de regulacion
if ( SiLimReg .eq. 1 ) then
    ierror = 0
    ierror_1 = 0
    i = 0

    ! -----------------------------
    ! * Se leen datos de LIRUNITRC *
    ! * Se leen datos de LSRUNITRC *
    ! -----------------------------
    !Potencia mínima de regulacion de unidades térmicas, PotMinRRC
    ! -----------------------------------------
    !Potencia máxima de regulacion unidades térmicas, PotMaxRRC
    write ( 1, 100 ) '----------------------------------------------------------------------'
    write ( 1, 100 ) 'POTENCIAS MINIMA Y MAXIMA DE DE REGULACION DE UNIDADES RANGO CONTINUO'
    write ( 1, 100 ) '----------------------------------------------------------------------'

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 10, FILE = rut_dat_1( 1 : long_ruta )//'LIRUNITRC.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

    ! Abre archivo de datos de generadores
    OPEN (UNIT = 11, FILE = rut_dat_1( 1 : long_ruta )//'LSRUNITRC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

    errleca = 0; errlecb = 0; unidad = 0
    if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
    ! Lee información hasta encontrar fin de información
        do while ( unidad .lt. NumUniRC ) 
           unidad = unidad + 1
           read ( 10, 100, iostat = ierror ) letaux
           read ( 11, 100, iostat = ierror_1 ) letaux_1
           read ( letaux, *, iostat = errleca )  ( PotMinRRC ( unidad, intervalo ), intervalo = 1, ntintr )
           read ( letaux_1, *, iostat = errlecb )  ( PotMaxRRC ( unidad, intervalo ), intervalo = 1, ntintr )
           if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1004
           bloque = ntintr / 24
           do k = 1, bloque
               write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
               write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Min:', ( PotMinRRC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
               write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Max:', ( PotMaxRRC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
           end do
           if ( ntintr - 24 * bloque .gt. 0 ) then
              write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
              write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Min:', ( PotMinRRC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
              write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Max:', ( PotMaxRRC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
           end if
        end do
    else	
	    ibanbit = 1
        ierror = 0
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '//NomEjecu//'LEC ERROR DE LECTURA ARCHIVO LIRUNITRC.csv / LSRUNITRC.csv'
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write(*,*) '1'
    !   Se ecribe resultado de semaforos
        call EscSemaforosError
!       algoritmo no termina bien
        call SalidaError
        stop
    end if
        
    CLOSE ( UNIT = 10 )
    CLOSE ( UNIT = 11 )

    !Escalamiento
    PotMinRRC = PotMinRRC / Base
    PotMaxRRC = PotMaxRRC / Base
else
    PotMinRRC = PotMinGRC
    PotMaxRRC = PotMaxGRC
endif

1004 continue
     
! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO LIRUNITRC_DERS.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO LSRUNITRC_DERS.csv'
        call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
    call ParaProceso ( bmensaje )    
endif

ierror = 0
ierror_1 = 0
i = 0

! ---------------------------
! * Se leen datos de COMPSRC *
! -----------------------------
!Bandera de condensador sincrono, CompSincRC
! -----------------------------------------
write ( 1, 100 ) '----------------------------------------------------------'
write ( 1, 100 ) 'BANDERA DE COMPENSADOR SINCRONO DE UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '----------------------------------------------------------'

! Abre archivo de datos de generadores
OPEN (UNIT = 54, FILE = rut_dat_1( 1 : long_ruta )//'COMPSRC.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
   ! Lee información hasta encontrar fin de información
   do while ( unidad .lt. NumUniRC )
     unidad = unidad + 1
     read ( 54, 100, iostat = ierror ) letaux
     read ( letaux, *, iostat = errleca )  ( CompSincRC ( unidad, intervalo ), intervalo = 1, ntintr )
     if ( errleca .ne. 0 ) go to 1006
        bloque = ntintr / 24
        do k = 1, bloque
           write ( 1, 501 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
           write ( 1, 601 ) unidad, nombunirc ( unidad ), 'Ban:', ( CompSincRC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
           write ( 1, 501 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
           write ( 1, 601 ) unidad, nombunirc ( unidad ), 'Ban:', ( CompSincRC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end if
   end do
else	
   bmensaje = 'ERROR DE LECTURA ARCHIVO COMPSRC.csv'
   call ParaProceso ( bmensaje )
end if

1006 continue
     
CLOSE ( UNIT = 54 )

if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO COMPSRC.csv'
    call EnviaMensajeError ( bmensaje )    
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
   call ParaProceso ( bmensaje )
endif

501  format ( a10, x, i3, x, a1, i3 )         
601 format ( i3, x, a12, x, a12, x, 24 (I3, 2x) )
        
1111 continue

write ( 1, 100 ) ''

write ( 1, 100 ) ''
!Se leen datos de condiciones iniciales
write ( 1, 100 ) '-----------------------------------------------'
write ( 1, 100 ) 'CONDICIONES INICIALES UNIDADES DE RANGO CONTINUO'
write ( 1, 100 ) '-----------------------------------------------'

ierror = 0
i = 0
letaux = 'inicio'
! ------------------------------
! * Se leen datos de UNITRCCI *
! ------------------------------

!write ( 1, 100 ) 'Unidad  Nombre          Estado    Interv  Generacion'
write ( 1, 100 ) 'Unidad  Nombre          Estado    Interv  Generacion  No Paros'

OPEN (UNIT = 16, FILE = rut_dat_1( 1 : long_ruta )//'UNITRCCI.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)

errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRC  )        
        unidad = unidad + 1
	    read ( 16, 100, iostat = ierror ) letaux
        read ( letaux, *, iostat = errleca )  EstadoCIURC ( unidad ), NumHCIURC ( unidad ), GenCIURC ( unidad )
        if ( errleca .ne. 0 ) go to 1007
!       read ( letaux, * )  EstadoCIURC ( unidad ), NumHCIURC ( unidad ), GenCIURC ( unidad ), NoActParRC ( unidad )
        !Calcular no. de intervalos en condiciones iniciales dependiendo de la duracion del intervalo
        NumHCIURC ( unidad ) = NumHCIURC ( unidad ) * 60
        NumHCIURC ( unidad ) = NumHCIURC ( unidad ) / durintr
        write ( 1, 700 ) unidad, nombunirc ( unidad ), EstadoCIURC ( unidad ), NumHCIURC ( unidad ), GenCIURC ( unidad )
!       write ( 1, 700 ) unidad, nombunirc ( unidad ), EstadoCIURC ( unidad ), NumHCIURC ( unidad ), GenCIURC ( unidad ), NoActParRC ( unidad )
    end do
else	
   bmensaje = 'ERROR LECTURA ARCHIVO UNITRCCI_DERS.csv'
   call ParaProceso ( bmensaje )
end if

1007 continue
     
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR LECTURA ARCHIVO UNITRCCI_DERS.csv'
    call EnviaMensajeError ( bmensaje )    
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 16 )

write ( 1, 100 ) ''

!Escalamiento
GenCIURC = GenCIURC / Base

ierror = 0
i = 0

write ( 1, 100 ) '------------------------------------------------------'
write ( 1, 100 ) 'INFORMACION DE OPERACION Y PARO UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '------------------------------------------------------'
! ----------------------------
! * Se leen datos de OPPARORC *
! ----------------------------

write ( 1, 100 ) 'Unidad  Nombre         Min. Paro    Min. Oper   Max. Paros'

OPEN (UNIT = 17, FILE = rut_dat_1( 1 : long_ruta )//'OPPARORC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)

unidad = 0; errleca = 0
if ( ierror .eq. 0 ) then
    !Hacer para todoas las unidades de rango continuo
    do while ( unidad .lt. NumUniRC )  
        unidad = unidad + 1
	    read ( 17, 100, iostat = ierror ) letaux
        read ( letaux, *, iostat = errleca )  TminParoURC ( unidad ), TminOperURC ( unidad ), NumMaxParoURC ( unidad )
        if ( errleca .ne. 0 ) go to 1008
        !calcula tiempo minimo de paro y de operacion dependiendo de la duracion del intervalo
        TminParoURC ( unidad ) = TminParoURC ( unidad ) * 60
        TminParoURC ( unidad ) = TminParoURC ( unidad ) / durintr
        TminOperURC ( unidad ) = TminOperURC ( unidad ) * 60
        TminOperURC ( unidad ) = TminOperURC ( unidad ) / durintr
!       se multiplica el maximo numero de paros al dia por el numero de dias
        NumMaxParoURC( unidad ) = NumMaxParoURC ( unidad) !* durdia
        write ( 1, 800 ) unidad, nombunirc ( unidad ), TminParoURC ( unidad ), TminOperURC ( unidad ), NumMaxParoURC ( unidad )
    end do
else	
   bmensaje = 'ERROR DE LECTURA ARCHIVO OPPARORC_DERS.csv'
   call ParaProceso ( bmensaje )
end if

1008 continue

CLOSE ( UNIT = 17 )

if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO OPPARORC_DERS.csv'
    call EnviaMensajeError ( bmensaje )    
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
   call ParaProceso ( bmensaje )
endif

write ( 1, 100 ) ''

ierror = 0
i = 0

write ( 1, 100 ) '-------------------------------------'
write ( 1, 100 ) 'ASIGNABILIDAD UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '-------------------------------------'
! ----------------------------
! * Se leen datos de ASIGNRC *
! ----------------------------

OPEN (UNIT = 18, FILE = rut_dat_1( 1 : long_ruta )//'ASIGNRC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)

unidad = 0; errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee información de asignabilidad de las unidades de rango continuo
    do while ( unidad .lt. NumUniRC  )   
        unidad = unidad + 1
	    read ( 18, 100, iostat = ierror ) letaux
        read ( letaux, *, iostat = errleca )  ( AsignURC ( unidad, intervalo ), intervalo = 1, ntintr )
        if ( errleca .ne. 0 ) go to 1009
        bloque = ntintr / 24
        do k = 1, bloque
           write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
           write ( 1, 900 ) unidad, nombunirc ( unidad ), 'ASIG:', ( AsignURC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
           write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
           write ( 1, 900 ) unidad, nombunirc ( unidad ), 'ASIG:', ( AsignURC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end if
    end do
else	
   bmensaje = 'ERROR DE LECTURA ARCHIVO ASIGNRC.csv'
   call ParaProceso ( bmensaje )
end if

1009 continue
     
CLOSE ( UNIT = 18 )

if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO ASIGNRC.csv'
    call EnviaMensajeError ( bmensaje )    
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
   call ParaProceso ( bmensaje )
endif

write ( 1, 100 ) ''

ierror = 0
i = 0
!goto 2222
write ( 1, 100 ) '--------------------------------------'
write ( 1, 100 ) 'DISPONIBILIDAD UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '--------------------------------------'
! ----------------------------
! * Se leen datos de DISPORC *
! ----------------------------

OPEN (UNIT = 18, FILE = rut_dat_1( 1 : long_ruta )//'DISPORC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)

unidad = 0; errleca = 0
if ( ierror .eq. 0 ) then
    ! Lee información de las unidades de rango continuo
    do while ( unidad .lt. NumUniRC  )
       unidad = unidad + 1
       read ( 18, 100, iostat = ierror ) letaux
       read ( letaux, *, iostat = errleca )  ( DispoURC ( unidad, intervalo ), intervalo = 1, ntintr )
       if ( errleca .ne. 0 ) go to 1010
       bloque = ntintr / 24
       do k = 1, bloque
          write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
          write ( 1, 900 ) unidad, nombunirc ( unidad ), 'DISP:', ( DispoURC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
       end do
       if ( ntintr - 24 * bloque .gt. 0 ) then
          write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
          write ( 1, 900 ) unidad, nombunirc ( unidad ), 'DISP:', ( DispoURC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
       end if
    end do
else
    bmensaje = 'ERROR DE LECTURA ARCHIVO DISPORC.csv'
   call ParaProceso ( bmensaje )
end if

1010 continue
     
CLOSE ( UNIT = 18 )

if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO DISPORC.csv'
    call EnviaMensajeError ( bmensaje )    
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
   call ParaProceso ( bmensaje )
endif

2222 continue
write ( 1, 100 ) ''

!Se calcula disponibilidad y coordinabilidad de unidades de Rango Continuo en base a su oferta
call Dispo_Coord_RC

ierror = 0
i = 0
letaux = 'inicio'

write ( 1, 100 ) '---------------------------------------------------------'
write ( 1, 100 ) 'INFORMACION DE PROCESO DE ARRANQUE UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '---------------------------------------------------------'
! ----------------------------
! * Se leen datos de ARRARC *
! ----------------------------

write ( 1, 100 ) 'Unidad  Nombre              Pot. Sinc.        Pot. Sinc. NR 10    Pot. Sinc. NR Su  Rampa Arr. MW/interv.   Tiempo Arra.   Cos. Arr. Unico'

OPEN (UNIT = 19, FILE = rut_dat_1( 1 : long_ruta )//'ARRARC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)

errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
! Lee información de todas las unidades de rango continuo
    do while ( unidad .lt. NumUniRC )  
        unidad = unidad + 1
	    read ( 19, 100, iostat = ierror ) letaux
        !Hacer para todoas las unidades de rango continuo
        Pmin = 1.0e10
        do j = 1, NTINTR
          if ( PotMinGRC ( unidad, j ) .ne. 0.0 .and. PotMinGRC ( unidad, j ) .lt. Pmin ) then
              Pmin = PotMinGRC ( unidad, j )
          endif
        enddo
        read ( letaux, *, iostat = errleca )  PotSincURC ( unidad ), PotSincNR10URC ( unidad ), PotSincNRSURC ( unidad ), RampArraURC ( unidad ), TiempoArraURC ( unidad ), CostArrUniURC ( unidad )
       if ( errleca .ne. 0 ) go to 1011
        ! Se calcula rampa y tiempo de arranque dependiendo de la duracion del intervalo
        RampArraURC ( unidad ) = RampArraURC ( unidad ) * durintr                    
        TiempoArraURC ( unidad ) = TiempoArraURC ( unidad ) * 60
        TiempoArraURC ( unidad ) = TiempoArraURC ( unidad ) / durintr
        PotSincURC ( unidad ) = PotSincURC ( unidad ) / Base
        if ( TiempoArraURC ( unidad ) .gt. 0 ) then
           intervalo = 0
           do j = 1, NTINTR
              if ( PotMinGRC ( unidad, j ) .gt. 0 ) then
                 intervalo = j
                 exit
              endif
           enddo
           if ( intervalo .gt. 0 ) then
             if ( PotMinGRC ( unidad, intervalo ) .gt. 0 .and. PotMinGRC ( unidad, intervalo ) .ge. PotSincURC ( unidad ) .and. TiempoArraURC ( unidad ) .gt. 0 ) then
                !Se calcula la rampa de arranque en base a su psinc, pmin, e intervalos de arranque 
!               RampArraURC ( unidad ) = ( ( PotMinGRC ( unidad, intervalo ) - PotSincURC ( unidad ) ) / TiempoArraURC ( unidad ) ) * Base
                RampArraURC ( unidad ) = ( ( Pmin - PotSincURC ( unidad ) ) / TiempoArraURC ( unidad ) ) * Base
             end if
           endif
        endif   
        write ( 1, 1000 ) unidad, nombunirc ( unidad ), PotSincURC ( unidad ) * Base, PotSincNR10URC ( unidad ), PotSincNRSURC ( unidad ), RampArraURC ( unidad ), TiempoArraURC ( unidad ), CostArrUniURC ( unidad )
    end do
else	
  bmensaje ='ERROR DE LECTURA ARCHIVO ARRARC.csv'
  call ParaProceso ( bmensaje )    
end if

1011 continue
     
CLOSE ( UNIT = 19 )

if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO ARRARC.csv'
    call EnviaMensajeError ( bmensaje )    
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
   call ParaProceso ( bmensaje )
endif

!Escalamiento
!PotSincURC = PotSincURC / Base
PotSincNR10URC =  PotSincNR10URC / Base
PotSincNRSURC = PotSincNRSURC / Base
RampArraURC = RampArraURC / Base


write ( 1, 100 ) ''

ierror = 0
i = 0

! ------------------------------
! * Se leen datos de COVAARRC *
! ------------------------------
write ( 1, 100 ) '---------------------------------------------------'
write ( 1, 100 ) 'COSTOS VARIABLES DE ARRANQUE UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '---------------------------------------------------'

OPEN (UNIT = 20, FILE = rut_dat_1( 1 : long_ruta )//'COVAARRC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)

errleca = 0; unidad = 0
if ( ierror .eq. 0  ) then
! Lee información de todas las unidades de rango continuo
  do while ( unidad .lt. NumUniRC )  
    unidad = unidad + 1
    !Hacer para los segmentos del segundo en adelante
    do segmento = 1, maxsegarrc
	   read ( 20, 100, iostat = ierror ) letaux
       read ( letaux, *, iostat = errleca )  TiemInicioArrRCS ( unidad, segmento ), CostoArrRCS ( unidad, segmento )
       if ( errleca .ne. 0 ) go to 1012
       
    end do                    
  end do
else	
  bmensaje ='ERROR DE LECTURA ARCHIVO COVAARRC.csv'
  call ParaProceso ( bmensaje ) 
end if

1012 continue
     
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO COVAARRC.csv'
    call EnviaMensajeError ( bmensaje )    
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 20 )

!Escribir a bitacora
!Hacer para todas las unidades
do unidad = 1, NumUniRC
    do segmento = 1, maxsegarrc
        !Modificar intervalos en paro dependiendo de la duracion del intervalo
        TiemInicioArrRCS ( unidad, segmento ) = TiemInicioArrRCS ( unidad, segmento ) * 60
        TiemInicioArrRCS ( unidad, segmento ) = TiemInicioArrRCS ( unidad, segmento ) / durintr
        write ( 1, 1200 ) unidad, nombunirc ( unidad ), 'Hr', TiemInicioArrRCS ( unidad, segmento )
        write ( 1, 1100 ) unidad, nombunirc ( unidad ), '$', CostoArrRCS ( unidad, segmento )
    end do        
end do
numseg = 0
!Calcular cuantos segmentos de costos de arranque variable por unidad 
do unidad = 1, NumUniRC
    numseg = 0
    !Hacer para todos los segmentos
    do segmento = 1, maxsegarrc
        !Encontrar segmento con oferta de potencia cero
        if ( TiemInicioArrRCS ( unidad, segmento ) .ne. 0 ) then
            numseg = numseg + 1
        end if            
    end do
    if ( numseg .eq. 1 ) then
        NmBloArrURC (unidad ) = 0
        CostArrUniURC ( unidad ) = CostoArrRCS ( unidad, 1 )
    else
        NmBloArrURC (unidad ) = numseg
    endif
end do



write ( 1, 100 ) ''

ierror = 0
i = 0

write ( 1, 100 ) '------------------------------'
write ( 1, 100 ) 'RAMPAS UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '------------------------------'
! ----------------------------
! * Se leen datos de RAMPASRC *
! ----------------------------

write ( 1,  90 ) ' Unidad  Nombre              Op. Subida Op. Bajada   Emer. 10   Emer. ', minresup, '    Reg. ', minresre,'       z.p.1 min  z.p.1 max  z.p.2 min  z.p.2  max z.p.3  min z.p.3  max z.p.4 min  z.p.4 max'

OPEN (UNIT = 21, FILE = rut_dat_1( 1 : long_ruta )//'RAMPASRC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 3000)

errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
! Lee información hasta encontrar fin de información
! Lee información de todas las unidades de rango continuo
  do while ( unidad .lt. NumUniRC )  
     unidad = unidad + 1
     read ( 21, 100, iostat = ierror ) letaux
     read ( letaux, *, iostat = errleca )  rampaup, rampadown, emergencia, regulacion, ( ZonProh ( unidad, contador ), contador = 1, maxzonproh * 2) 
     if ( errleca .ne. 0 ) go to 1013
     RampaSubURC ( unidad ) = rampaup * durintr
     RampaBajURC ( unidad ) = rampadown * durintr
     !Se verifica la rampa de emergencia de 10 minutos vs duracion del intervalo para reserva rodante de 10 min
     if ( durintr .lt. 10 ) then
        RamEmer10RC ( unidad ) = emergencia * durintr
     else
        RamEmer10RC ( unidad ) = emergencia * 10
     end if
     !Se verifica la rampa de emergencia de x minutos vs duracion del intervalo para reserva rodante suplementaria
     if ( durintr .lt. minresup  ) then
        RamEmerxRC ( unidad ) = emergencia * durintr
     else
        RamEmerxRC ( unidad ) = emergencia * minresup 
     end if
     !Se verifica la rampa de emergencia de x minutos vs duracion del intervalo para reserva rodante suplementaria
     if ( durintr .lt. minresre   ) then
       RamRegRC ( unidad ) = regulacion * durintr
     else
       RamRegRC ( unidad ) = regulacion * minresre 
     end if                    
     write ( 1, 1300 ) unidad, nombunirc ( unidad ), RampaSubURC ( unidad ), RampaBajURC ( unidad ), RamEmer10RC ( unidad ), RamEmerxRC ( unidad ), RamRegRC ( unidad ), ( ZonProh ( unidad, contador ), contador = 1, maxzonproh * 2 )
  end do
else	
  bmensaje ='ERROR DE LECTURA ARCHIVO RAMPASRC.csv'
  call ParaProceso ( bmensaje ) 
end if

1013 continue
     
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    bmensaje = 'ERROR DE LECTURA ARCHIVO RAMPASRC.csv'
    call EnviaMensajeError ( bmensaje )    
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 21 )

!Escalamiento
RampaSubURC = RampaSubURC / Base
RampaBajURC = RampaBajURC / Base
RamEmer10RC = RamEmer10RC / Base
RamEmerxRC = RamEmerxRC / Base
RamRegRC = RamRegRC / Base
ZonProh = ZonProh / Base

! Forma vectores de trabajo para zonas prohibidas unidades de RC
NoRaOpRC = 0
RaOpSupRC = 0
RaOpInfRC = 0
ZonProhx = ZonProh

! Valida límites de regulación de unidades de rango continuo
call ValidaLimitesRegulacionRC

!Hacer para todas las unidades de rango continuo
do unidad = 1, NumUniRC
    if ( unidad .eq. 189 ) then
        continue
    endif
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
                if ( (PotMinGRC ( unidad, i ) .ge. ZonProhx ( unidad, iz ) .and. PotMaxGRC ( unidad, i ) .le. ZonProhx ( unidad, iz+1 ) )  ) then
                    ZonProh ( unidad, iz ) = PotMinGRC ( unidad, i )
                    ZonProh ( unidad, iz + 1 ) = PotMinGRC ( unidad, i ) 
!                    ZonProh ( unidad, iz + 1 ) = PotMaxGRC ( unidad, i )
                endif
            enddo
            if ( sigue ) then
                ! Acota para zonas prohibidad fuera del rango de operación
                do zona = 1,  NuZonProh*2
                   if ( ZonProh ( unidad, zona ) .lt.  PotMinGRC ( unidad, i )  ) then
                       ZonProh ( unidad, zona ) = PotMinGRC ( unidad, i )
                   else if ( ZonProh ( unidad, zona ) .gt.  PotMaxGRC ( unidad, i )  ) then
                       ZonProh ( unidad, zona  ) = PotMaxGRC ( unidad, i )
                   endif
                end do 
                !hacer para el numero maximo de zonas prohibidas
                contador = 0
                do zona = 1, NuZonProh*2
                    if ( ZonProh ( unidad, zona + contador ) .ne. 0 .or. zona .eq. 1 ) then
                        if ( contador .eq. 0 ) then
                            RaOpInfRC ( unidad, zona, i ) = PotMinGRC ( unidad, i )
                            RaOpSupRC ( unidad, zona, i ) = ZonProh ( unidad, zona + contador )
                            RaRegInfRC( unidad, zona, i ) = PotMinRRC ( unidad, i )  
                            RaRegSupRC( unidad, zona, i ) = RaOpSupRC ( unidad, zona, i )
                            if ( i .eq. 1 ) NoRaOpRC ( unidad ) = NoRaOpRC ( unidad ) + 1
                        else
                            RaOpInfRC ( unidad, zona, i ) = ZonProh ( unidad, zona + contador - 1 ) 
                            RaOpSupRC ( unidad, zona, i ) = ZonProh ( unidad, zona + contador )   
                            RaRegInfRC( unidad, zona, i ) = RaOpInfRC ( unidad, zona, i )
                            RaRegSupRC( unidad, zona, i ) = RaOpSupRC ( unidad, zona, i )
                            if ( i .eq. 1 ) NoRaOpRC ( unidad ) = NoRaOpRC ( unidad ) + 1
                        end if
                        contador = contador + 1
                        if ( contador .eq. NuZonProh*2 ) then
                            RaOpInfRC ( unidad, zona + 1, i ) = ZonProh ( unidad, zona + contador ) 
                            RaOpSupRC ( unidad, zona + 1, i ) = PotMaxGRC ( unidad, i )
                            RaRegInfRC( unidad, zona, i ) = RaOpInfRC ( unidad, zona + 1, i )
                            RaRegSupRC( unidad, zona, i ) = PotMaxRRC ( unidad, i ) 
                            if ( i .eq. 1 )  NoRaOpRC ( unidad ) = NoRaOpRC ( unidad ) + 1
                        end if            
                    else
                        if ( contador .gt. 0 ) then
                            RaOpInfRC ( unidad, zona, i ) = ZonProh ( unidad, zona + contador - 1 ) 
                            RaOpSupRC ( unidad, zona, i ) = PotMaxGRC ( unidad, i )
                            RaRegInfRC( unidad, zona, i ) = RaOpInfRC ( unidad, zona, i )
                            RaRegSupRC( unidad, zona, i ) = PotMaxRRC ( unidad, i ) 
                            if ( i .eq. 1 ) NoRaOpRC ( unidad ) = NoRaOpRC ( unidad ) + 1
                            exit
                        end if
                    end if
                end do 
            endif
        end do
    endif
end do

write ( 1, 100 ) ''

ierror = 0
ierror_1 = 0
i = 0
letaux = 'letaux'
letaux_1 = 'letaux1'

! ---------------------------------
! * Se leen datos de POTRESRO10RC *
! * Se leen datos de PRERESRO10RC *
! ---------------------------------
!Potencia oferta de reserva rodante de 10 min
! -----------------------------------------
!Precio oferta de reserva rodante de 10 min
write ( 1, 100 ) '-------------------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE RESERVA RODANTE DE 10 MIN DE UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '-------------------------------------------------------------'

! Abre archivo de datos de generadores
OPEN (UNIT = 22, FILE = rut_dat_1( 1 : long_ruta )//'POTRESRO10RC.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 23, FILE = rut_dat_1( 1 : long_ruta )//'PRERESRO10RC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRC  )  
       unidad = unidad + 1
       read ( 22, 100, iostat = ierror ) letaux
       read ( 23, 100, iostat = ierror_1 ) letaux_1
       !Hacer para todoas las unidades de rango continuo
       read ( letaux, *, iostat = errleca )  ( OferResR10RC ( unidad, intervalo ), intervalo = 1, ntintr )
       read ( letaux_1, *, iostat = errlecb )  ( PreVenResR10RC ( unidad, intervalo ), intervalo = 1, ntintr )
       if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1014
       bloque = ntintr / 24
       do k = 1, bloque
          write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
          write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot:', ( OferResR10RC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
          write ( 1, 600 ) unidad, nombunirc ( unidad ), '$  :', ( PreVenResR10RC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
       end do
       if ( ntintr - 24 * bloque .gt. 0 ) then
          write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
          write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot:', ( OferResR10RC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
          write ( 1, 600 ) unidad, nombunirc ( unidad ), '$  :', ( PreVenResR10RC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
       end if
    end do
else
  if ( IERROR .ne. 0 ) then
      bmensaje ='ERROR DE LECTURA ARCHIVO PRERESRO10RC.csv'
      call EnviaMensajeError ( bmensaje )
  endif
  if ( IERROR_1 .ne. 0 ) then
      bmensaje ='ERROR DE LECTURA ARCHIVO POTRESRO10RC.csv'
      call EnviaMensajeError ( bmensaje )
  endif
  bmensaje = ' '
  call ParaProceso ( bmensaje ) 
end if

1014 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
   write ( letr, "(i5)" ) unidad
   if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR LECTURA ARCHIVO POTRESRO10RC_DERS.csv'
       call EnviaMensajeError ( bmensaje )
   endif
   if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR LECTURA ARCHIVO PRERESRO10RC_DERS.csv'
       call EnviaMensajeError ( bmensaje )
   endif
   Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
   call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 22 )
CLOSE ( UNIT = 23 )

write ( 1, 100 ) ''

!Escalamiento
OferResR10RC = OferResR10RC / Base
PreVenResR10RC = PreVenResR10RC * Base

ierror = 0
ierror_1 = 0
i = 0

! ---------------------------------
! * Se leen datos de POTRESNR10RC *
! * Se leen datos de PRERESNR10RC *
! ---------------------------------
!Potencia oferta de reserva no rodante de 10 min
! -----------------------------------------
!Precio oferta de reserva no rodante de 10 min
write ( 1, 100 ) '----------------------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE RESERVA NO RODANTE DE 10 MIN DE UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '----------------------------------------------------------------'

! Abre archivo de datos de generadores
OPEN (UNIT = 24, FILE = rut_dat_1( 1 : long_ruta )//'POTRESNR10RC.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 25, FILE = rut_dat_1( 1 : long_ruta )//'PRERESNR10RC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
! Lee información hasta encontrar fin de información
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRC  )  
       unidad = unidad + 1
        read ( 24, 100, iostat = ierror ) letaux
        read ( 25, 100, iostat = ierror_1 ) letaux_1
        i = i + 1
        !Hacer para todoas las unidades de rango continuo
        read ( letaux, *, iostat = errleca )  ( OferResNR10RC ( unidad, intervalo ), intervalo = 1, ntintr )
        read ( letaux_1, *, iostat = errlecb )  ( PreVenResNR10RC ( unidad, intervalo ), intervalo = 1, ntintr )
        if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1015
        bloque = ntintr / 24
        do k = 1, bloque
           write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
           write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot:', ( OferResNR10RC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
           write ( 1, 600 ) unidad, nombunirc ( unidad ), '$  :', ( PreVenResNR10RC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
           write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
           write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot:', ( OferResNR10RC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
           write ( 1, 600 ) unidad, nombunirc ( unidad ), '$  :', ( PreVenResNR10RC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end if
    end do
else	
    if ( ierror_1 .ne. 0 ) then
         bmensaje = 'ERROR LECTURA ARCHIVO POTRESNR10RC_DERS.csv'
    endif
    if ( ierror .ne. 0 ) then
         bmensaje = 'ERROR LECTURA ARCHIVO PRERESNR10RC_DERS.csv'
    endif
    bmensaje = ' '
    call ParaProceso ( bmensaje )
end if
 
1015 continue
     
! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESNR10RC_DERS.csv'
       call EnviaMensajeError ( bmensaje )       
    endif
    if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESNR10RC_DERS.csv'
       call EnviaMensajeError ( bmensaje )       
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
    call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 24 )
CLOSE ( UNIT = 25 )

write ( 1, 100 ) ''

!Escalamiento
OferResNR10RC = OferResNR10RC / Base
PreVenResNR10RC = PreVenResNR10RC * Base

ierror = 0
ierror_1 = 0
i = 0

! ---------------------------------
! * Se leen datos de POTRESROSURC *
! * Se leen datos de PRERESROSURC *
! ---------------------------------
!Potencia oferta de reserva rodante suplementaria
! -----------------------------------------
!Precio oferta de reserva rodante suplementaria
write ( 1, 100 ) '-----------------------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE RESERVA RODANTE SUPLEMENTARIA DE UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '-----------------------------------------------------------------'

! Abre archivo de datos de generadores
OPEN (UNIT = 26, FILE = rut_dat_1( 1 : long_ruta )//'POTRESROSURC.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 27, FILE = rut_dat_1( 1 : long_ruta )//'PRERESROSURC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

! Lee información hasta encontrar fin de información
errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRC  )  
       unidad = unidad + 1
        read ( 26, 100, iostat = ierror ) letaux
        read ( 27, 100, iostat = ierror_1 ) letaux_1
        read ( letaux, *, iostat = errleca )  ( OferResRxRC ( unidad, intervalo ), intervalo = 1, ntintr )
        read ( letaux_1, *, iostat = errlecb )  ( PreVenResRxRC ( unidad, intervalo ), intervalo = 1, ntintr )
        if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1016
        bloque = ntintr / 24
        do k = 1, bloque
           write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
           write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot:', ( OferResRxRC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
           write ( 1, 600 ) unidad, nombunirc ( unidad ), '$  :', ( PreVenResRxRC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
           write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
           write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot:', ( OferResRxRC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
           write ( 1, 600 ) unidad, nombunirc ( unidad ), '$  :', ( PreVenResRxRC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end if
    end do
else	
    if ( ierror_1 .ne. 0 ) then
         bmensaje = 'ERROR LECTURA ARCHIVO POTRESROSURC.csv'
    endif
    if ( ierror .ne. 0 ) then
         bmensaje = 'ERROR LECTURA ARCHIVO PRERESROSURC.csv'
    endif
    bmensaje = ' '
    call ParaProceso ( bmensaje )
end if
 
1016 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESROSURC.csv'
       call EnviaMensajeError ( bmensaje )       
    endif
    if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESROSURC.csv'
       call EnviaMensajeError ( bmensaje )       
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
    call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 26 )
CLOSE ( UNIT = 27 )

write ( 1, 100 ) ''

!Escalamiento
OferResRxRC = OferResRxRC / Base
PreVenResRxRC = PreVenResRxRC * Base

ierror = 0
ierror_1 = 0
i = 0

! ---------------------------------
! * Se leen datos de POTRESNRSURC *
! * Se leen datos de PRERESNRSURC *
! ---------------------------------
!Potencia oferta de reserva no rodante suplementaria
! -----------------------------------------
!Precio oferta de reserva no rodante suplementaria
write ( 1, 100 ) '--------------------------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE RESERVA NO RODANTE SUPLEMENTARIA DE UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '--------------------------------------------------------------------'

! Abre archivo de datos de generadores
OPEN (UNIT = 28, FILE = rut_dat_1( 1 : long_ruta )//'POTRESNRSURC.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 29, FILE = rut_dat_1( 1 : long_ruta )//'PRERESNRSURC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

! Lee información hasta encontrar fin de información
errleca = 0; errlecb = 0; unidad = 0
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRC  )  
       unidad = unidad + 1
       read ( 28, 100, iostat = ierror ) letaux
       read ( 29, 100, iostat = ierror_1 ) letaux_1
       read ( letaux, *, iostat = errleca )  ( OferResNRxRC ( unidad, intervalo ), intervalo = 1, ntintr )
       read ( letaux_1, *, iostat = errlecb )  ( PreVenResNRxRC ( unidad, intervalo ), intervalo = 1, ntintr )
       if ( errleca .ne. 0 .or. errlecb .ne. 0 ) go to 1017
       bloque = ntintr / 24
       do k = 1, bloque
          write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
          write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot:', ( OferResNRxRC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
          write ( 1, 600 ) unidad, nombunirc ( unidad ), '$  :', ( PreVenResNRxRC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
       end do
       if ( ntintr - 24 * bloque .gt. 0 ) then
          write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
          write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot:', ( OferResNRxRC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
          write ( 1, 600 ) unidad, nombunirc ( unidad ), '$  :', ( PreVenResNRxRC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end if
    end do
else	
    if ( ierror_1 .ne. 0 ) then
         bmensaje = 'ERROR LECTURA ARCHIVO POTRESNRSURC.csv'
    endif
    if ( ierror .ne. 0 ) then
         bmensaje = 'ERROR LECTURA ARCHIVO PRERESNRSURC.csv'
    endif
    bmensaje = ' '
    call ParaProceso ( bmensaje )
end if
 
1017 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO POTRESNRSURC.csv'
       call EnviaMensajeError ( bmensaje )       
    endif
    if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESNRSURC.csv'
       call EnviaMensajeError ( bmensaje )       
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
    call ParaProceso ( bmensaje )
endif
        
CLOSE ( UNIT = 28 )
CLOSE ( UNIT = 29 )

write ( 1, 100 ) ''

!Escalamiento
OferResNRxRC = OferResNRxRC / Base
PreVenResNRxRC = PreVenResNRxRC * Base

ierror = 0
ierror_1 = 0
i = 0

! ---------------------------------
! * Se leen datos de POTRESRESERC *
! * Se leen datos de PRERESRESERC *
! * Se leen datos de MINRESRESERC *
! ---------------------------------
!Potencia oferta de reserva de regulacion secundaria
! -----------------------------------------
!Precio oferta de reserva de regulacion secundaria
! -----------------------------------------
!Minimo a asignar de reserva de regulacion secundaria
write ( 1, 100 ) '--------------------------------------------------------------------'
write ( 1, 100 ) 'OFERTA DE RESERVA DE REGULACION SECUNDARIA DE UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '--------------------------------------------------------------------'

! Abre archivo de datos de generadores
OPEN (UNIT = 30, FILE = rut_dat_1( 1 : long_ruta )//'POTRESRESERC.csv', IOSTAT = IERROR_1, STATUS='OLD', RECORDSIZE = 250)

! Abre archivo de datos de generadores
OPEN (UNIT = 31, FILE = rut_dat_1( 1 : long_ruta )//'PRERESRESERC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

if ( SiResRegDis .eq. 1 ) then
!   Abre archivo de datos de reserva distribuida
    OPEN (UNIT = 32, FILE = rut_dat_1( 1 : long_ruta )//'MINRESRESERC.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)
endif

errleca = 0; errlecb = 0; errlecc = 0; unidad = 0; ierror_2 = 0; letaux_2 = "Inicio"
if ( ierror .eq. 0 .and. ierror_1 .eq. 0 ) then
!   Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRC  )  
       unidad = unidad + 1
        read ( 30, 100, iostat = ierror ) letaux
        read ( 31, 100, iostat = ierror_1 ) letaux_1
        if ( SiResRegDis .eq. 1 ) then
            read ( 32, 100, iostat = ierror_2 ) letaux_2
        endif
        if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0 ) go to 1018
        read ( letaux, *, iostat = errleca )  ( OferResRegRC ( unidad, intervalo ), intervalo = 1, ntintr )
        read ( letaux_1, *, iostat = errlecb )  ( PreVenResRegRC ( unidad, intervalo ), intervalo = 1, ntintr )
        if ( SiResRegDis .eq. 1 ) then
           read ( letaux_2, *, iostat = errlecc )  ( MrreURC ( unidad, intervalo ), intervalo = 1, ntintr )
        endif
        if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0 ) go to 1018
        bloque = ntintr / 24
        do k = 1, bloque
           write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
           write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot:', ( OferResRegRC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
           write ( 1, 600 ) unidad, nombunirc ( unidad ), '$  :', ( PreVenResRegRC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
           if ( SiResRegDis .eq. 1 ) then
              write ( 1, 600 ) unidad, nombunirc ( unidad ), 'PMi:', ( MrreURC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
           endif
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
           write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
           write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pot:', ( OferResRegRC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
           write ( 1, 600 ) unidad, nombunirc ( unidad ), '$  :', ( PreVenResRegRC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
           if ( SiResRegDis .eq. 1 ) then
              write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Pmi:', ( MrreURC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
           endif
        end if
    end do
else	
    if ( ierror .ne. 0 ) then
         bmensaje = 'ERROR LECTURA ARCHIVO PRERESRESERC.csv'
    endif
    if ( ierror_1 .ne. 0 ) then
         bmensaje = 'ERROR LECTURA ARCHIVOP POTRESRESERC.csv'
    endif
    if ( ierror_2 .ne. 0 .and. SiResRegDis .eq. 1 ) then
         bmensaje = 'ERROR LECTURA ARCHIVOP MINRESRESERC.csv'
    endif
    bmensaje = '  '
    call ParaProceso ( bmensaje )
end if

1018 continue

! Verifica que no existan errores de lectura
if ( errleca .ne. 0 .or. errlecb .ne. 0 .or. errlecc .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje ='ERROR DE LECTURA ARCHIVO POTRESRESERC.csv.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    if ( errlecb .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO PRERESRESERC.csv UNIDAD'
       call EnviaMensajeError ( bmensaje )
    endif
    if ( errlecc .ne. 0 ) then
        bmensaje = 'ERROR DE LECTURA ARCHIVO MINRESRESERC.csv UNIDAD'
        call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
    call ParaProceso ( bmensaje )
endif

CLOSE ( UNIT = 30 )
CLOSE ( UNIT = 31 )
if ( SiResRegDis .eq. 1 ) then
    CLOSE ( UNIT = 32 )
endif

!Escalamiento
OferResRegRC = OferResRegRC / Base
PreVenResRegRC = PreVenResRegRC * Base
MrreURC = MrreURC / Base

! Hacer para todoas las unidades de rango continuo
do unidad = 1, NumUniRC*0
    i = 0
    do intervalo = 1, NTINTR
        if ( AsignURC ( unidad, intervalo ) .gt. 0 ) then
            i = i + 1
        endif
    enddo
    if ( i .eq. 0 ) then
        TiempoArraURC ( unidad ) = 0
    endif
enddo

! Hacer para todoas las importaciones
do unidad = 1, NumUniRC*0
    do intervalo = 1, NTINTR
        if ( DispoURC ( unidad, intervalo ) .eq. 1 .and. AsignURC ( unidad, intervalo ) .eq. 1 &
            .and. TiempoArraURC ( unidad ) .eq. 0.0 .and. CostoArrRCS ( unidad, 1 ) .eq. 0.0 .and. &
            PotMinGRC ( unidad, intervalo ) .eq. 0.0 .and. OferVenEnerRC ( unidad, 1, intervalo ) .gt. 0.0 &
            .and. PotMaxGRC ( unidad, intervalo ) .gt. 0.0 ) then
            PotMinGRC ( unidad, intervalo ) = PotMinGRC ( unidad, intervalo ) + 1.0e-5
        endif
    enddo
enddo



write ( 1, 100 ) ''

 90  format ( a66, i2,a9, i2, a93 ) 
100  format ( a )
200  format ( i3, 5x, a12, 4x, a2, 4x, a3, 12x, i4, 5x, a9, 3x, a5, 18x, i1, 18x, i2 )    
300  format ( a10, x, i3, x, a1, i3 )      
400  format ( i3, x, a12, x, 24 (f12.2, 2x) )    
500  format ( i3, x, a12, x, a5, x, 24 (f9.2, 2x) )    
600  format ( i3, x, a12, x, a4, x, 24 (f9.2, 2x) )    
700  format ( i3, 5x, a12, 6x, i2, 7x, i3, 4x, f9.2 )  
!700  format ( i3, 5x, a12, 6x, i2, 7x, i3, 4x, f9.2, 4x, i3 )  
800  format ( i3, 5x, a12, 6x, i2, 11x, i2, 13x, i2 )  
900  format ( i3, x, a12, x, a4, x, 24 (i1, 2x) )  
1000 format ( i3, 5x, a12, 6x, 3 (f9.2, 11x), f9.2, 13x, i2, 8x, f9.2 )  
1100 format ( i3, x, a12, x, a2, x, f12.2 )    
1200 format ( i3, x, a12, x, a2, x, i10 )    
1300 format ( i3, 5x, a12, 6x, 5 ( f9.2, 2x ), 2x, 8 ( f9.2, 2x ) ) 
1400 format ( i3, x, a12, x, 24 (i6, 2x) )    
1500 format ( i1 )
1600 format ( i3, x, a12, x, 24 (f6.3, 2x) )    
     

END SUBROUTINE data_rc

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
!         de rango continuo en base a sus ofertas de energia              *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Noviembre 2014                        *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                  Febrero 2017                           *
!**************************************************************************
    
SUBROUTINE Dispo_Coord_RC

use ParAUHE, only: DispoURC, CoordURC, NumUniRC, ntintr, PotMaxGRC, PotMinGRC, &
                  nombunirc, CompSincRC
!
IMPLICIT NONE

integer unidad, intervalo, bloque, k

!Hacer para todas las unidades de rango continuo
do unidad = 1, NumUniRC
    !Hacer para todos los intervalos
    do intervalo = 1, ntintr
!       compensador sincrono con bandera 1 debe ser unidad no disponible
        if ( CompSincRC ( unidad, intervalo ) .eq. 1 ) then
            !No disponible
            DispoURC ( unidad, intervalo ) = 0
        else
            !Disponible
!            DispoURC ( unidad, intervalo ) = 1
        end if
        !Para que unidad sea no coordinable su potencia minima y maxima son iguales
        if ( PotMaxGRC ( unidad, intervalo ) .eq. PotMinGRC ( unidad, intervalo ) ) then
            !No coordinable
            CoordURC ( unidad, intervalo ) = 0
        else
            !Coordinable
            CoordURC ( unidad, intervalo ) = 1
        end if        
    end do
end do

!write ( 1, 100 ) '-------------------------------------'
!write ( 1, 100 ) 'DISPONIBILIDAD UNIDADES RANGO CONTINUO'
!write ( 1, 100 ) '-------------------------------------'
do unidad = 1, NumUniRC*0
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        write ( 1, 900 ) unidad, nombunirc ( unidad ), 'DISP:', ( DispoURC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        write ( 1, 900 ) unidad, nombunirc ( unidad ), 'DISP:', ( DispoURC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
    end if
end do

!write ( 1, 100 ) ''

write ( 1, 100 ) '--------------------------------------'
write ( 1, 100 ) 'COORDINABILIDAD UNIDADES RANGO CONTINUO'
write ( 1, 100 ) '--------------------------------------'
do unidad = 1, NumUniRC
    bloque = ntintr / 24
    do k = 1, bloque
        write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
        write ( 1, 900 ) unidad, nombunirc ( unidad ), 'COOR:', ( CoordURC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
    end do
    if ( ntintr - 24 * bloque .gt. 0 ) then
        write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
        write ( 1, 900 ) unidad, nombunirc ( unidad ), 'COOR:', ( CoordURC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
    end if
end do

write ( 1, 100 ) ''

100  format ( a )
300  format ( a10, x, i3, x, a1, i3 )      
900  format ( i3, x, a12, x, a4, x, 24 (i1, 2x) )  

end subroutine Dispo_Coord_RC    
!**************************************************************************
! Lectura de limite inferior de referencia (oferta)                       *
!                                                                         *
! Febrero 2020                                                            *
!                                                                         *
!**************************************************************************
SUBROUTINE LECLIMINFOFE_URC

use ParAUHE
implicit none

integer errleca, k, bloque, ierror, unidad, intervalo

character*3000 letaux

character*5 letr

! Abre archivo de datos de generadores
OPEN (UNIT = 10, FILE = rut_dat_1( 1 : long_ruta )//'LIOUNITRC_DERS.csv', IOSTAT = IERROR, STATUS='OLD', RECORDSIZE = 250)

PotMinOfeGRC = PotMinGRC*BASE

errleca = 0; unidad = 0
if ( ierror .eq. 0 ) then
! Lee información hasta encontrar fin de información
    do while ( unidad .lt. NumUniRC ) 
        unidad = unidad + 1
        read ( 10, 100, iostat = errleca ) letaux
        if ( errleca .ne. 0 ) go to 1001
        read ( letaux, *, iostat = errleca )  ( PotMinOfeGRC ( unidad, intervalo ), intervalo = 1, ntintr )
        if ( errleca .ne. 0  ) go to 1001
        bloque = ntintr / 24
        do k = 1, bloque
           write ( 1, 300 ) 'intervalo:', 24 * k - 23, 'a', 24 * k
           write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Min:', ( PotMinOfeGRC ( unidad, intervalo ), intervalo = 24 * k - 23 , 24 * k  )
        end do
        if ( ntintr - 24 * bloque .gt. 0 ) then
           write ( 1, 300 ) 'intervalo:', 24 * bloque + 1 , 'a', ntintr
           write ( 1, 600 ) unidad, nombunirc ( unidad ), 'Min:', ( PotMinOfeGRC ( unidad, intervalo ), intervalo = 24 * bloque + 1 , ntintr  )
        end if
    end do
else
    if ( ierror .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO LIOUNITRC_DERS.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    bmensaje = ' '
    call ParaProceso ( bmensaje )
end if

1001 continue
     
! Verifica que no existan errores de lectura
if ( errleca .ne. 0 ) then
    write ( letr, "(i5)" ) unidad
    call Elimina_blancos ( letr, 5)
    if ( errleca .ne. 0 ) then
       bmensaje = 'ERROR DE LECTURA ARCHIVO LIOUNITRC_DERS.csv'
       call EnviaMensajeError ( bmensaje )
    endif
    Bmensaje = 'UNIDAD: ['//trim(letr)//'] '//trim(nombunirc(unidad))
    call ParaProceso ( Bmensaje )
endif

PotMinOfeGRC = PotMinOfeGRC/BASE

CLOSE ( UNIT = 10 )

100 format ( a )
300  format ( a10, x, i3, x, a1, i3 )      
600  format ( i3, x, a12, x, a4, x, 24 (f9.2, 2x) )    

return

end    