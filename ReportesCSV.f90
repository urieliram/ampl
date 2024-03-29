! ---------------------------------------------------------------------
! Se escriben resultados a archivos csv de volumenes turbinados y     *
! alturas en unidades hidroelectricas.                                *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre del 2016                                                    *
! ---------------------------------------------------------------------
Subroutine VolCarUH ( volumhR, cargahR )

use ParAUHE
use ProblemaAUHE

Implicit none

integer h, u, ierror

real*8   volumhR ( maxuh, maxint ), cargahR ( maxuh, maxint )

! Abre archivo de volumenes unidades hidroelectricas para lectura
OPEN ( UNIT = 218, FILE = rut_dat_1( 1 : long_ruta )//'VOLUMH_1.csv', IOSTAT = IERROR, &
         STATUS='UNKNOWN', RECORDSIZE = 4000                )

! Abre archivo de cagas de unidades hidroelectricas para lectura
OPEN ( UNIT = 219, FILE = rut_dat_1( 1 : long_ruta )//'CARGAH_1.csv', IOSTAT = IERROR, &
         STATUS='UNKNOWN', RECORDSIZE = 4000                )

do u = 1, NumUniHid
   write ( 218, 102 ) ( volumhR ( u, h ), h = 1, NTINTR )
   write ( 219, 102 ) ( cargahR ( u, h ), h = 1, NTINTR )
enddo

close ( 218 )
close ( 219 )

102 format ( 169 (F12.2, ',') )
100 format ( a )

return
end


! ---------------------------------------------------------------------
! Se escriben resultados de produccion diaria en vasos a archivo csv. *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre de 2018                                                   *
! ---------------------------------------------------------------------
Subroutine RDVAAU ( aa4 )

use ParAUHE
use ProblemaAUHE

Implicit none

integer i, j, k, IERROR, embalse, dia

character*1  coma
real*8       aa4(nmxemb, maxdia, 7), RdvaauR4(nmxemb*maxdia, 7)
character*3  RdvaauR3(nmxemb*maxdia), nomdia
character*1  aaux1

RdvaauR4 = 0.0
RdvaauR3 = '  '

! Se abre el archivo 250 RDVAAU (Produccion diaria en vasos AU)
OPEN (UNIT = 250, FILE = rut_dat_1( 1 : long_ruta )//'RDVAAU_1.csv', &
     IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 400)

! para todos los embalses (vasos) y dias
do embalse = 1, Numembalses
   k = (embalse-1)*MAXDIA
   do dia = 1, durdia
      do j = 1, 7
         RdvaauR4 ( k + dia, j) = aa4 ( embalse, dia, j ) 
      enddo
      write ( aaux1, 5103 ) dia
      NomDia = 'D'//aaux1
      RdvaauR3 (k + dia) = NomDia
   enddo
enddo

! Imprimir los resultados al archivo  RDVAAU
coma = ','
do i = 1 , Numembalses*MAXDIA
    write (250, 103)  RdvaauR3(i), coma, ( RdvaauR4(i,j), coma, j=1,7 )
enddo

close ( unit = 250 ) 

aa4 ( :, :, 7 ) = aa4 ( :, :, 7 )/1000.0
goto 456
! Se abre el archivo VASOCI (Resultados dia-vaso de condiciones iniciales)
OPEN (UNIT = 187, FILE = rut_dat_1( 1 : long_ruta )//'VASOCI.csv', &
     IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 400)

   ! para todos los embalses (vasos)
   do i = 1, Numembalses*MAXDIA
      read (187,*) RdvaauR3 (i), RdvaauR4 (i,1), RdvaauR4 (i,2), RdvaauR4 (i,3),  &
	               RdvaauR4 (i,4), RdvaauR4 (i,5), RdvaauR4 (i,6), RdvaauR4 (i,7)
   enddo

CLOSE ( unit = 187 ) 
456 continue 

! para todos los embalses (vasos) y dias
do embalse = 1, Numembalses
   k = (embalse-1)*MAXDIA
   do dia = 1, durdia
      do j = 1, 7
         RdvaauR4 ( k + dia, j) = aa4 ( embalse, dia, j ) 
      enddo
      write ( aaux1, 5103 ) dia
      NomDia = 'D'//aaux1
      RdvaauR3 (k + dia) = NomDia
   enddo
enddo

! Se abre el archivo 187 VASOCI (Resultados dia-vaso de condiciones iniciales)
OPEN (UNIT = 187, FILE = rut_dat_1( 1 : long_ruta )//'VASOCI.csv', &
     IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 400)

!     Imprimir los resultados al archivo VASOCI
      coma = ','
      do i = 1 , Numembalses*MAXDIA
         write (187,103)  RdvaauR3(i), coma, ( RdvaauR4(i,j), coma, j=1,7 )
      enddo
close ( unit = 187 ) 

103   format('"', a3, '"', a1, 7(F12.3, a1) )
5103 FORMAT (I1)

return
end


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
!     Propósito:                                                          *
!       Escritura de resultados en formato *.csv para los volumenes       *
!		finales en embalses                                               *
!                                                                         *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Octubre 2013                          *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!        Jose Luis Ceciliano Meza   Julio 2017                            *
!**************************************************************************
Subroutine ResultVASOEN2

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro, only: poliemb, WEmbFin, vol_util_res

Implicit none

integer ierror, embalse

real*8 VolUtMax ( nmxemb ), VolFnPol ( nmxemb ), VolFnDes ( nmxemb ) 

VolUtMax = 0.0
VolFnPol = 0.0
VolFnDes = 0.0

!Archivo de resultados archivo 171
OPEN (UNIT = 213, FILE = rut_dat_1( 1 : long_ruta )//'VASOEN2_1.csv', &
     IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 3000)

!Volumen al final de escenario de planeacion
!Hacer para todos los embales
do embalse = 1, NumEmbalses
    if ( PoliEmb ( embalse ) .ne. 3 ) then
        ! Se considera que para todas las demas politicas el volumen final segun politica es el mismo
        ! volumen con desviaciones
        VolFnPol ( embalse ) = vol_util_res ( embalse, NTINTR ) / 1000.0
        VolFnDes ( embalse ) = VolFnPol ( embalse )
    else
        ! Para politica de cota final fija en m. ya convertida a m^3 el nivel segun politica es el solicitado
        ! y el volumen con desviaciones es le volumen calculado
        VolFnPol ( embalse ) = WEmbFin ( embalse ) / 1000.0
        VolFnDes ( embalse ) = vol_util_res ( embalse, NTINTR ) / 1000.0
    end if
end do

do embalse = 1, NumEmbalses
    write ( 213, 102 ) VolUtMax ( embalse ), VolFnPol ( embalse ), 0.0, 0.0, 0.0, VolFnDes ( embalse ), 0  
end do

CLOSE ( UNIT = 213 )



100 format ( a )
102 format ( 6 (F12.2, ','), i1, ',' )

return
end
    
! ---------------------------------------------------------------------
! Se escriben resultados de resumen de vasos a archivo csv.           *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre del 2016                                                  *
! ---------------------------------------------------------------------
Subroutine REVAAU ( a1, a2, a3, a4, a5, a6 )

use ParAUHE
use ProblemaAUHE

Implicit none

integer i, IERROR

character*1  coma
character*5  a1(nmxemb)
real*8       a2(nmxemb), a3(nmxemb)
character*11 a4(nmxemb), a5(nmxemb), a6(nmxemb)

! Se abre el archivo 72 REVAAU (Resumen de vasos AU)
OPEN (UNIT = 72, FILE = rut_dat_1( 1 : long_ruta )//'REVAAU_1.csv', &
     IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 400)

   coma = ','
   ! para todos los embalses (vasos)
   do i = 1, Numembalses
       write (72,103) a1(i), coma, a2(i), coma, a3(i), coma, a4(i), coma, a5(i), coma, a6(i), coma, -dualenerbal ( i )
   enddo

CLOSE (UNIT = 72)

103   FORMAT('"',a3,'"',a1,F12.3,a1,F12.3,a1,'"',a11,'"',a1,'"',a11,'"',a1,'"',a11,'"',a1,F16.4)

return
end


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
!     Propósito:                                                          *
!       Escritura de resultados en formato *.csv para los turbinados y    *
!		derrames programados sobre las vias                               *
!                                                                         *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Octubre 2013                          *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!        Jose Luis Ceciliano Meza   Noviembre 2016                        *
!**************************************************************************
Subroutine ResultVIAHO ( a_tra_res )  

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro, only: VERTDO, TiViAgu

Implicit none


integer ierror, intervalo, hora, via

real*8 a_tra_res ( maxint + 25, nmxvia ), TurbViaTemp ( nmxvia, maxint ), &
       VertViaTemp ( nmxvia, maxint )  


TurbViaTemp = 0.0
VertViaTemp = 0.0

!Archivo de resultados turbinados y derrames programados sobre las vias
OPEN (UNIT = 209, FILE = rut_dat_1( 1 : long_ruta )//'VIAHO_1.csv', &
     IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000)

!Hacer para todas las vias
do via = 1, nmxvia 
    do intervalo = 1, ntintr
        TurbViaTemp ( via, intervalo ) = a_tra_res ( intervalo + TiViAgu ( via ), via ) - VERTDO ( via, intervalo )
        VertViaTemp ( via, intervalo ) = VERTDO ( via, intervalo )
    end do    
end do
   
do via = 1, nmxvia * 2
    if ( via .le. nmxvia ) then
	    write ( 209, 102 ) ( TurbViaTemp ( via, hora ), hora = 1, NTINTR )
	else
	    write ( 209, 102 ) ( VertViaTemp ( via - nmxvia, hora ), hora = 1, NTINTR )
	end if
end do

CLOSE ( UNIT = 209 )

100 format ( a )
102 format ( 169 (F16.2, ',') )

    
return
end


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
!     Propósito:                                                          *
!       Escritura de resultados en formato *.csv para las aportaciones    *
!		vasos arriba y niveles en m y m^3 en embalses (horario)           *
!                                                                         *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Octubre 2013                          *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!        Jose Luis Ceciliano Meza  enero 2019                             *
!**************************************************************************
Subroutine ResultVASOHE ( volumen, altura, aa4 ) 


USE ParAuHeHidro
use ParAUHE, only: rut_dat_1, durdia, bmensaje, intdia, NumEmbalses, NomEjecu

IMPLICIT NONE
!----------------------------
!* Declaracion de variables *
!----------------------------
CHARACTER fecha_Ej*19
!
INTEGER d, e, i, j, ierror, ierr
INTEGER ibanbit
real*8  let
real*8 ApoNatNet ( nmxemb, maxint ), ExtOtros ( nmxemb, maxint ), ApoVaUpTemp ( nmxemb, maxint ), &
       aa4 ( nmxemb, maxdia, 7 ), NivMTemp, VolMMCTemp, altura ( nmxemb, maxint ), volumen ( nmxemb, maxint )

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
			        read ( letaux, *, iostat = ierr ) ApoVaUpTemp ( e, j ), ApoNatNet ( e, j ), ExtOtros ( e, j ), &
			                                  let, NivMTemp, let, VolMMCTemp
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

CLOSE ( UNIT = 208, status = 'delete' )

! Para todos los embalses
do e = 1, NumEmbalses
    j = 0
!   Para todos los dias
    do d = 1, durdia
        aa4 ( e, d, 2 ) = 0.0
        aa4 ( e, d, 3 ) = 0.0
        aa4 ( e, d, 4 ) = 0.0
        aa4 ( e, d, 5 ) = 0.0
!       Para todos los intervalos del dia
        do i = 1, intdia ( d )
            j = j + 1
            aa4 ( e, d, 3 ) = aa4 ( e, d, 3 ) + ( apoemb ( e, j ) - AguaEnViaje ( e, j ) ) / 1000.0
            ApoVaUpTemp ( e, j ) = ApoVaUp ( e, j )
            aa4 ( e, d, 2 ) = aa4 ( e, d, 2 ) +  ApoVaUp ( e, j ) / 1000.0
            aa4 ( e, d, 4 ) = altura ( e, j )
            aa4 ( e, d, 5 ) = volumen ( e, j )
        end do
    end do
end do

!---------------------------------------
!* VASOHE (Vasos horario)*
!---------------------------------------
OPEN ( UNIT = 130, DEFAULTFILE = rut_dat_1, FILE='VASOHE.csv', STATUS='OLD', IOSTAT = ierror, RECORDSIZE = 250 )
if ( ierror .eq. 0 ) then
    write ( 522, * ) 'Embalse  Dia  Intervalo   ApoArriba   ApoNatural   ExtOUsos   Nivel   Volumen' 
!   Para todos los embalses
    do e = 1, NumEmbalses
        j = 0
!       Para todos los dias
        do d = 1, durdia
!           Para todos los intervalos del dia
            do i = 1, intdia ( d )
                j = j + 1
	            write ( 130, 102 ) ApoVaUpTemp ( e, j ), ApoNatNet ( e, j ), 0.0, &
                                   0.0, altura ( e, j ), 0.0, volumen ( e, j )
!	            write ( 130, 102 ) ApoVaUpTemp ( e, j ), apoemb ( e, j )/ 1000.0, 0.0, &
!                                   0.0, aa4 ( e, d, 4 ), 0.0, aa4 ( e, d, 5 )
                write ( 522, 300 ) e, d, j, ApoVaUpTemp ( e, j ), apoemb ( e, j )/ 1000.0, 0.0, &
                                            altura ( e, j ), volumen ( e, j )
!                write ( 522, 300 ) e, d, j, ApoVaUpTemp ( e, j ), ApoNatNet ( e, j ), 0.0, &
!                                            aa4 ( e, d, 4 ), aa4 ( e, d, 5 )
            enddo
        enddo
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
close ( 522 )

100 format ( a )
102 format ( 7 (f16.3, ',') )
300 format ( 3(3x,I3), 5(f16.3, ',') )

return
end


! ---------------------------------------------------------------------
! Se escriben resultados de consumo de combustible para todos los     *
! grupos definidos.                                                   *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre de 2018                                                  *
! ---------------------------------------------------------------------
Subroutine EscConsGas ( sistema )

use ParAUHE
use ProblemaAUHE

Implicit none

integer IERROR, grupo, dia, IntIni, intervalo, iniurc, unidad, u, sistema, iniurd

real*8  consumo(maxgrute, maxdia), dual, consumoURC (maxurc, maxint), &
        consumoURD (maxurd, maxint)
character*1 ssistema

consumo = 0.0
consumoURC = 0.0
consumoURD = 0.0

! Se abre el archivo comsumo diario de combustible
OPEN (UNIT = 250, FILE = rut_dat_1( 1 : long_ruta )//'RESGASD_1.csv', &
     IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 1350)

! Se abre el archivo comsumo por unidad
OPEN (UNIT = 252, FILE = rut_dat_1( 1 : long_ruta )//'CONSUMOXU_1.csv', &
     IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 6500)

write( ssistema, '(I1)' )  sistema
OPEN ( UNIT = 251, FILE = RUT_RES//'r_consumo'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )

!write ( 250, * ) 'GRUPO,  Limite Inferior,  Consumo,  Limite Superior,  Precio Sombra'
write ( 251, * ) ' GRUPO              Limite Inferior     Consumo     Limite Superior    Precio Sombra'
write ( 251, * ) '                       (MMBTU)          (MMBtu)        (MMBtu)           ($/MMBtu)'

! para todos los grupos y dias
do grupo = 1, NumGruGas
    IntIni = 1
    do dia = 1, MAXDIA
!       para todos los intervalos del dia
        do intervalo = IntIni, IntIni + intdia (dia) - 1
            iniurc = ApunURCxGpoGas ( grupo )
!           para todas las unidades de rango continuo que estan en ese grupo
            do unidad = 1 , NumURCxGpoGas ( grupo )
                u = UniRCxGpoGas ( iniurc )
!               si es unidad disponible
                if ( DispoURC ( u , intervalo ) .eq. 1 ) then
!                   consumo de combustible de la unidad
                    consumo ( grupo, dia ) = consumo ( grupo, dia ) + RTerURC ( u ) * GENUNRC ( u, intervalo)
                    consumoURC ( unidad, intervalo ) = RTerURC ( u )*GENUNRC ( u, intervalo)*Base
                endif
                iniurc = iniurc + 1
            enddo
!           para las unidades de rango discontinuo que estan en ese grupo
            iniurd = ApunURDxGpoGas ( grupo )
            do unidad = 1 , NumURDxGpoGas ( grupo )
                u = UniRDxGrupo ( iniurd )
!               consumo de combustible de la unidad
                consumo ( grupo, dia ) = consumo ( grupo, dia ) +  RTerURD ( u, RESMODO(u, intervalo) ) * GENUNRD ( u, intervalo )
                consumoURD ( unidad, intervalo ) = RTerURD ( u, RESMODO(u, intervalo) )*GENUNRD ( u, intervalo )*Base
                iniurd = iniurd + 1
            enddo
        enddo
        dual = ( dualigpogas (grupo, dia) + dualsgpogas (grupo, dia) ) / Base
        write (250, 103)  NomGpoGas ( grupo ), LimInfGas (grupo, dia)*Base, consumo ( grupo, dia)*base, LimSupGas (grupo, dia)*Base, -dual
        write (251, 104)  NomGpoGas ( grupo ), LimInfGas (grupo, dia)*Base, consumo ( grupo, dia)*base, LimSupGas (grupo, dia)*Base, -dual
        IntIni = IntIni + intdia ( dia )
    enddo
    iniurc = ApunURCxGpoGas ( grupo )
!   para todas las unidades de rango continuo que estan en ese grupo
    do unidad = 1 , NumURCxGpoGas ( grupo )
        u = UniRCxGpoGas ( iniurc )
        write (252, 105) NomGpoGas ( grupo ), nombunirc(u), 0, ( consumoURC ( unidad, intervalo ), intervalo = 1, NTINTR )
        iniurc = iniurc + 1
    enddo
!   para las unidades de rango discontinuo que estan en ese grupo
    iniurd = ApunURDxGpoGas ( grupo )
    do unidad = 1 , NumURDxGpoGas ( grupo )
        u = UniRDxGrupo ( iniurd )
        write (252, 105) NomGpoGas ( grupo ), nombunird(u), 1, ( consumoURD ( unidad, intervalo ), intervalo = 1, NTINTR )
        iniurd = iniurd + 1
    enddo
enddo

close ( unit = 250 )
close ( unit = 251 )
close ( unit = 252 )

103 format(a15,',',4(f15.2,',') )
104 format(a15, 4(f15.2, 2x) )
105 format(2(a15,','),I2,',',169(f15.2,',') )


return
end


! ---------------------------------------------------------------------
! Se escriben resultados de consumo de combustible para todas las     *
! restricciones definidas.                                            *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Mayo de 2019                                                        *
! ---------------------------------------------------------------------
Subroutine EscConsGas_new ( sistema )

use ParAUHE
use ProblemaAUHE

Implicit none

integer i, IERROR, ibanbit, grupo, intervalo, iniurc, unidad, u, sistema, iniurd

real*8  consumo, dualinf, dualsup, consumoURC (maxurc, maxint), consumoURD (maxurd, maxint), aux
character*1 ssistema

CHARACTER fecha_Ej*19
character*5 aaux1
character*20 aaux2
character*15 aaux3

ibanbit = 1
ierror = 0
consumoURC = 0.0
consumoURD = 0.0

! Se abre el archivo comsumo diario de combustible
OPEN (UNIT = 250, FILE = rut_dat_1( 1 : long_ruta )//'RESGASD_1.csv', &
     IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 1350)

! Se abre el archivo comsumo por unidad
OPEN (UNIT = 252, FILE = rut_dat_1( 1 : long_ruta )//'CONSUMOXU_1.csv', &
     IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 6500)

write( ssistema, '(I1)' )  sistema
OPEN ( UNIT = 251, FILE = RUT_RES//'r_consumo'//ssistema//'.res',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 1350 )


write ( 251, * ) ''
!Para todas las restricciones
write ( 251, * ) 'Restriccion  Grupo    NomGrupo   HoraIni   HoraFin     LInfComb      LSupComb       Consumo       DualInfer   DualSuper'

! para todos los grupos con limitacion de combustible
do grupo = 1, NResComb
    consumo = 0.0
    i = grupo
!   para todos los intervalos del grupo
    do intervalo = HIResComb(grupo), HFResComb(grupo)
        iniurc = ApunURCxGpoGas ( grupo )
!       para todas las unidades de rango continuo que estan en ese grupo
        do unidad = 1 , NumURCxGpoGas ( grupo )
            u = UniRCxGpoGas ( iniurc )
!           si es unidad disponible
            if ( DispoURC ( u , intervalo ) .eq. 1 ) then
!               consumo de combustible de la unidad
                consumo = consumo + RTerURC ( u ) * GENUNRC ( u, intervalo)
                consumoURC ( u, intervalo ) = RTerURC ( u )*GENUNRC ( u, intervalo)*Base
            endif
            iniurc = iniurc + 1
        enddo
!       para las unidades de rango discontinuo que estan en ese grupo
        iniurd = ApunURDxGpoGas ( grupo )
        do unidad = 1 , NumURDxGpoGas ( grupo )
            u = UniRDxGrupo ( iniurd )
!           consumo de combustible de la unidad
            consumo = consumo + RTerURD ( u, RESMODO(u, intervalo) ) * GENUNRD ( u, intervalo )
            consumoURD ( u, intervalo ) = RTerURD ( u, RESMODO(u, intervalo) )*GENUNRD ( u, intervalo )*Base
            iniurd = iniurd + 1
        enddo
    enddo
    dualinf = dualinfgpogas (grupo) / Base
    dualsup = dualsupgpogas (grupo) / Base
    write (250, 103)  i, NGpoResComb ( i ), NomGpoResComb ( i ), HIResComb ( i ), HFResComb ( i ), LInfResComb ( i )*Base, LSupResComb ( i )*Base, consumo*Base, -dualinf, -dualsup
    write (251, 104)  i, NGpoResComb ( i ), NomGpoResComb ( i ), HIResComb ( i ), HFResComb ( i ), LInfResComb ( i )*Base, LSupResComb ( i )*Base, consumo*Base, -dualinf, -dualsup

!   Si existe infactibilidad en el grupo
    if ( xMILP ( IARCG + grupo - 1 ) .gt. 0.001 .and. SiGpoGas .gt. 0 ) then
        SemBandera ( 9 ) = 1
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '//NomEjecu//'100 INFACTIBILIDAD EN LIMITES COMBU'
        Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write ( aaux2, 5102 ) NomGpoResComb ( grupo )
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' '//NomEjecu//'100 RESTR DE COMB: '//aaux2
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        write ( aaux1, 5100 ) grupo 
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' '//NomEjecu//'100 RESTRICCION  : '//aaux1
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
        aux = xMILP ( IARCG + grupo - 1 )*Base
        write ( aaux3, 5101 )  aux
        Call FechaEjecucion (fecha_Ej)
        BMensaje = fecha_Ej//' '//NomEjecu//'100 INFACTIBILIDAD (MMBtu): '//aaux3
        call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    endif

    iniurc = ApunURCxGpoGas ( grupo )
!   para todas las unidades de rango continuo que estan en ese grupo
    do unidad = 1 , NumURCxGpoGas ( grupo )
        u = UniRCxGpoGas ( iniurc )
        write (252, 105) NomGpoResComb ( grupo ), nombunirc(u), 0, ( consumoURC ( u, intervalo ), intervalo = 1, NTINTR )
        iniurc = iniurc + 1
    enddo
!   para las unidades de rango discontinuo que estan en ese grupo
    iniurd = ApunURDxGpoGas ( grupo )
    do unidad = 1 , NumURDxGpoGas ( grupo )
        u = UniRDxGrupo ( iniurd )
        write (252, 105) NomGpoResComb ( grupo ), nombunird(u), 1, ( consumoURD ( u, intervalo ), intervalo = 1, NTINTR )
        iniurd = iniurd + 1
    enddo

enddo

close ( unit = 250 )
close ( unit = 251 )
close ( unit = 252 )

103 format(2(i4,8x,','), a12, ',', 2(I3, 7x, ','), 5(f9.2, 5x,',') )
104 format(2(i4,8x), a12, 2(I3, 7x), 5(f9.2, 5x))
105 format(2(a15,','),I2,',',169(f15.2,',') )
5100 FORMAT (I4)
5101 FORMAT (F8.2)
5102 FORMAT (A20)


return
end