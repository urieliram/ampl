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
!         Se validan rampas para el escenario contra condiciones          *
!         iniciales y potencias minima y maxima                           *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Julio 2015                            *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                   Julio 2019                            *
!**************************************************************************
    
SUBROUTINE Valid_Rampa_RC

use ParAUHE, only: RampaSubURC, RampaBajURC, NumUniRC, NTINTR, PotMinGRC, PotMaxGRC, bmensaje, &
                  EstadoCIURC, GenCIURC, dispourc, CalOferResRegRC, CalOferResR10RC, CalOferResRxRC, &
                  base, nombunirc, CoordURC

IMPLICIT NONE

integer unidad, intervalo

real*8 prevRS ( NumUniRC ), prevRB ( NumUniRC ), PrevGen, PrevGenMax, PrevGenMin
CHARACTER fecha_Ej*19
character*7 aaux4
character*12 aaux1

integer ierror, ibanbit, mult

prevRS = 0
prevRB = 0
ierror = 0
ibanbit = 1



!
!Hacer para todas las unidades de rango continuo
do unidad = 1, NumUniRC
    mult = 0
    PrevGen = 0.0
    PrevGenMax = 0.0
    PrevGenMin = 0.0
    !Hacer para todos los intervalos
    do intervalo = 1, NTINTR
        !Si la unidad es no coordinable no se cuidan rampas
        if ( CoordURC ( unidad, intervalo ) .eq. 1 ) then 
            !se revisan condiciones iniciales
            if ( intervalo .eq. 1 ) then
                !si la unidad esta encendida 
                if ( EstadoCIURC ( unidad ) .eq. 1 ) then
                    PrevGen = GenCIURC ( unidad )
                    PrevGenMax = GenCIURC ( unidad )
                    PrevGenMin = GenCIURC ( unidad )
                    if ( GenCIURC ( unidad ) + RampaSubURC ( unidad ) .lt. PotMinGRC ( unidad, intervalo ) .and. dispourc ( unidad, intervalo ) .eq. 1 ) then
                        !mantener la rampa mayor para todo el escenario
                        if ( RampaSubURC ( unidad ) .gt. prevRS ( unidad ) ) then
                            prevRS ( unidad ) = RampaSubURC ( unidad )
                            RampaSubURC ( unidad ) = PotMinGRC ( unidad, intervalo ) - GenCIURC ( unidad ) 
                            Call FechaEjecucion (fecha_Ej)
                            bmensaje = fecha_Ej//' '
                            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                            Call FechaEjecucion (fecha_Ej)
                            write ( aaux4, 5103 ) RampaSubURC ( unidad ) * base
                            write ( aaux1, 5101 ) nombunirc ( unidad )
                            !write ( aaux2, 5102 ) intervalo
                            Call FechaEjecucion (fecha_Ej)
                            !BMensaje = fecha_Ej//' DERS-I100 INT, RampaSub REL:'//aaux1//','//aaux2//','//aaux4
                            BMensaje = fecha_Ej//' DERS-I100, RampaSub REL:'//aaux1//','//aaux4
                            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                            Call FechaEjecucion (fecha_Ej)
                            BMensaje = fecha_Ej//'         '
                            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                        end if
                    end if            
                    if ( GenCIURC ( unidad ) - RampaBajURC ( unidad ) .gt. PotMaxGRC ( unidad, intervalo ) .and. dispourc ( unidad, intervalo ) .eq. 1 ) then
                        !mantener la rampa mayor para todo el escenario
                        if ( RampaBajURC ( unidad ) .gt. prevRB ( unidad ) ) then
                            prevRB ( unidad ) = RampaBajURC ( unidad )
                            RampaBajURC ( unidad ) = GenCIURC ( unidad ) - PotMaxGRC ( unidad, intervalo )   
                            Call FechaEjecucion (fecha_Ej)
                            bmensaje = fecha_Ej//' '
                            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                            Call FechaEjecucion (fecha_Ej)
                            write ( aaux4, 5103 ) RampaBajURC ( unidad ) * base
                            write ( aaux1, 5101 ) nombunirc ( unidad )
                            !write ( aaux2, 5102 ) intervalo
                            Call FechaEjecucion (fecha_Ej)
                            !BMensaje = fecha_Ej//' DERS-I100 INT, RampaBajURC REL:'//aaux1//','//aaux2//','//aaux4
                            BMensaje = fecha_Ej//' DERS-I100, RampaBaj REL:'//aaux1//','//aaux4
                            call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                            Call FechaEjecucion (fecha_Ej)
                            BMensaje = fecha_Ej//'         '
                            Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                        end if
                    end if             
                else
                    !En condiciones iniciales apagado y puede encender
                    PrevGenMax = PotMinGRC ( unidad, intervalo )
                    PrevGenMin = PotMinGRC ( unidad, intervalo ) + RampaBajURC ( unidad )
                end if
            else
                !se revisan todos los demas intervalos 2, 3, ...
                !Si esta disponible en el intervalo actual
                if ( dispourc ( unidad, intervalo ) .eq. 1 ) then
                    !Si esta disponible en el intervalo anterior
                    if ( dispourc ( unidad, intervalo - 1 ) .eq. 1 ) then
                        !Se aumenta el contador de rampas
                        mult = mult + 1
                        !No alcanza la rampa a subir
                        if ( PrevGenMax + mult * RampaSubURC ( unidad ) + RampaSubURC ( unidad ) .lt. PotMinGRC ( unidad, intervalo ) ) then
                            !se relaja    
                            !mantener la rampa mayor para todo el escenario
                            if ( RampaSubURC ( unidad ) .gt. prevRS ( unidad ) ) then
                                prevRS ( unidad ) = RampaSubURC ( unidad )
                                RampaSubURC ( unidad ) = PotMinGRC ( unidad, intervalo ) - PrevGenMax - mult * RampaSubURC ( unidad )
                                Call FechaEjecucion (fecha_Ej)
                                bmensaje = fecha_Ej//' '
                                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                Call FechaEjecucion (fecha_Ej)
                                write ( aaux4, 5103 ) RampaSubURC ( unidad ) * base
                                write ( aaux1, 5101 ) nombunirc ( unidad )
                                !write ( aaux2, 5102 ) intervalo
                                Call FechaEjecucion (fecha_Ej)
                                !BMensaje = fecha_Ej//' DERS-I100 INT, RampaSub REL:'//aaux1//','//aaux2//','//aaux4
                                BMensaje = fecha_Ej//' DERS-I100, RampaSub REL:'//aaux1//','//aaux4
                                call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//'         '
                                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                            end if
                        end if
                        !No alcanza la rampa a bajar
                        if ( PrevGenMin - mult * RampaBajURC ( unidad ) - RampaBajURC ( unidad ) .gt. PotMaxGRC ( unidad, intervalo ) ) then
                            !mantener la rampa mayor para todo el escenario
                            if ( RampaBajURC ( unidad ) .gt. prevRB ( unidad ) ) then
                                prevRB ( unidad ) = RampaBajURC ( unidad )
                                RampaBajURC ( unidad ) = PrevGenMin - mult * RampaBajURC ( unidad ) - PotMaxGRC ( unidad, intervalo ) 
                                Call FechaEjecucion (fecha_Ej)
                                bmensaje = fecha_Ej//' '
                                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                Call FechaEjecucion (fecha_Ej)
                                write ( aaux4, 5103 ) RampaBajURC ( unidad ) * base
                                write ( aaux1, 5101 ) nombunirc ( unidad )
                                !write ( aaux2, 5102 ) intervalo
                                Call FechaEjecucion (fecha_Ej)
                                !BMensaje = fecha_Ej//' DERS-I100 INT, RampaBajURC REL:'//aaux1//','//aaux2//','//aaux4
                                BMensaje = fecha_Ej//' DERS-I100, RampaBaj REL:'//aaux1//','//aaux4
                                call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//'         '
                                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                            end if
                        end if                        
                    else
                        !Se resetea el contador
                        mult = 0
                        !Intervalo anterior apagado pero puede encender
                        PrevGenMax = PotMinGRC ( unidad, intervalo )
                        PrevGenMin = PotMinGRC ( unidad, intervalo ) + RampaBajURC ( unidad )
                    end if
                end if
            end if
        else
            !se revisan condiciones iniciales
            if ( intervalo .eq. 1 ) then
                PrevGen = GenCIURC ( unidad )
                PrevGenMax = GenCIURC ( unidad )
                PrevGenMin = GenCIURC ( unidad )
            else
                PrevGenMax = PotMinGRC ( unidad, intervalo-1 )
                PrevGenMin = PotMinGRC ( unidad, intervalo-1 ) + RampaBajURC ( unidad )
            endif
        end if
    end do
end do


5103 FORMAT (F7.2)
5101 FORMAT (a12)
5102 FORMAT (i3)
end subroutine Valid_Rampa_RC
    
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
!         Se validan rampas para el escenario contra condiciones          *
!         iniciales y potencias minima y maxima                           *
!                                                                         *
!     Nombre y fecha de implementación:                                   *
!        DR. JUAN ALVAREZ LOPEZ     Septiembre 2015                       *
!                                                                         *
!                                                                         *
!     Nombre y fecha de revision(es):                                     *
!                                                                         *
!**************************************************************************
    
SUBROUTINE Valid_Rampa_RD

use ParAUHE, only: RampaSubURD, RampaBajURD, NumUniRD, NTINTR, PotMinGRD, PotMaxGRD, &
                  EstadoCIURD, GenCIURD, dispourd, bmensaje, CalOferResRegRD, CalOferResR10RD, CalOferResRxRD, &
                  base, nombunird, CoordURD, maxmodos, NumModRD

IMPLICIT NONE

integer unidad, intervalo, modo

real*8 prevRS ( NumUniRD, maxmodos ), prevRB ( NumUniRD, maxmodos ), PrevGen ( maxmodos ), PrevGenMax ( maxmodos ), PrevGenMin ( maxmodos )
CHARACTER fecha_Ej*19
character*7 aaux4
character*20 aaux1
character*3 aaux2 

integer ierror, ibanbit, mult

prevRS = 0
prevRB = 0
ierror = 0
ibanbit = 1
!
!
ierror = 0
ibanbit = 1

!
!Hacer para todas las unidades de rango continuo
do unidad = 1, NumUniRD
    PrevGen = 0.0
    PrevGenMax = 0.0
    PrevGenMin = 0.0
    !hacer para todos los modos de la unidad de rango discontinuo
    do modo = 2, NumModRD ( unidad )
        mult = 0
        !Hacer para todos los intervalos
        do intervalo = 1, NTINTR
            !Si la unidad es no coordinable no se cuidan rampas
            if ( CoordURD ( unidad, modo, intervalo ) .eq. 1 ) then 
                !se revisan condiciones iniciales
                if ( intervalo .eq. 1 ) then
                    !si la unidad esta en dicho modo en C.I
                    if ( EstadoCIURD ( unidad, modo ) .eq. 1 ) then
                        PrevGen ( modo ) = GenCIURD ( unidad, modo )
                        PrevGenMax ( modo ) = GenCIURD ( unidad, modo )
                        PrevGenMin ( modo ) = GenCIURD ( unidad, modo )
                        if ( GenCIURD ( unidad, modo ) + RampaSubURD ( unidad, modo ) .lt. PotMinGRD ( unidad, modo, intervalo ) .and. dispourd ( unidad, modo, intervalo ) .eq. 1 ) then
                            !mantener la rampa mayor para todo el escenario
                            if ( RampaSubURD ( unidad, modo ) .gt. prevRS ( unidad, modo ) ) then
                                prevRS ( unidad, modo ) = RampaSubURD ( unidad, modo )
                                RampaSubURD ( unidad, modo ) = PotMinGRD ( unidad, modo, intervalo ) - GenCIURD ( unidad, modo ) 
                                Call FechaEjecucion (fecha_Ej)
                                bmensaje = fecha_Ej//' '
                                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                Call FechaEjecucion (fecha_Ej)
                                write ( aaux4, 5103 ) RampaSubURD ( unidad, modo ) * base
                                write ( aaux1, 5101 ) nombunird ( unidad )
                                write ( aaux2, 5102 ) modo
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//' DERS-I100, Modo, RampaSub REL:'//aaux1//','//aaux2//','//aaux4
                                call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//'         '
                                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                            end if
                        end if            
                        if ( GenCIURD ( unidad, modo ) - RampaBajURD ( unidad, modo ) .gt. PotMaxGRD ( unidad, modo, intervalo ) .and. dispourd ( unidad, modo, intervalo ) .eq. 1 ) then
                            !mantener la rampa mayor para todo el escenario
                            if ( RampaBajURD ( unidad, modo ) .gt. prevRB ( unidad, modo ) ) then
                                prevRB ( unidad, modo ) = RampaBajURD ( unidad, modo )
                                RampaBajURD ( unidad, modo ) = GenCIURD ( unidad, modo ) - PotMaxGRD ( unidad, modo, intervalo )  
                                Call FechaEjecucion (fecha_Ej)
                                bmensaje = fecha_Ej//' '
                                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                Call FechaEjecucion (fecha_Ej)
                                write ( aaux4, 5103 ) RampaBajURD ( unidad, modo ) * base
                                write ( aaux1, 5101 ) nombunird ( unidad )
                                write ( aaux2, 5102 ) modo
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//' DERS-I100, RampaBaj REL:'//aaux1//','//aaux2//','//aaux4
                                call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                Call FechaEjecucion (fecha_Ej)
                                BMensaje = fecha_Ej//'         '
                                Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                            end if
                        end if                
                    else
                        !no esta en ese modo en condiciones iniciales
                        !revisar si el modo esta disponible
                        if ( dispourd ( unidad, modo, intervalo ) .eq. 1 ) then
                            !Puede iniciar generacion
                            PrevGenMax ( modo ) = PotMinGRD ( unidad, modo, intervalo )
                            PrevGenMin ( modo ) = PotMinGRD ( unidad, modo, intervalo ) + RampaBajURD ( unidad, modo )
                        end if
                    end if
                else
                    !se revisan todos los demas intervalos 2, 3, ...
                    !Si esta disponible en el intervalo actual
                    if ( dispourd ( unidad, modo, intervalo ) .eq. 1 ) then
                        !Si esta disponible en el intervalo anterior
                        if ( dispourd ( unidad, modo, intervalo - 1 ) .eq. 1 ) then
                            !Se aumenta el contador de rampas
                            mult = mult + 1
                            !No alcanza la rampa a subir
                            if ( PrevGenMax ( modo ) + mult * RampaSubURD ( unidad, modo ) + RampaSubURD ( unidad, modo ) .lt. PotMinGRD ( unidad, modo, intervalo ) ) then
                                !se relaja    
                                !mantener la rampa mayor para todo el escenario
                                if ( RampaSubURD ( unidad, modo ) .gt. prevRS ( unidad, modo ) ) then
                                    prevRS ( unidad, modo ) = RampaSubURD ( unidad, modo )
                                    RampaSubURD ( unidad, modo ) = PotMinGRD ( unidad, modo, intervalo ) - PrevGenMax ( modo ) - mult * RampaSubURD ( unidad, modo )
                                    Call FechaEjecucion (fecha_Ej)
                                    bmensaje = fecha_Ej//' '
                                    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                    Call FechaEjecucion (fecha_Ej)
                                    write ( aaux4, 5103 ) RampaSubURD ( unidad, modo ) * base
                                    write ( aaux1, 5101 ) nombunird ( unidad )
                                    !write ( aaux2, 5102 ) intervalo
                                    Call FechaEjecucion (fecha_Ej)
                                    !BMensaje = fecha_Ej//' DERS-I100 INT, RampaSub REL:'//aaux1//','//aaux2//','//aaux4
                                    BMensaje = fecha_Ej//' DERS-I100, RampaSub REL:'//aaux1//','//aaux4
                                    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                    Call FechaEjecucion (fecha_Ej)
                                    BMensaje = fecha_Ej//'         '
                                    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                end if
                            end if
                            !No alcanza la rampa a bajar
                            if ( PrevGenMin ( modo ) - mult * RampaBajURD ( unidad, modo ) - RampaBajURD ( unidad, modo ) .gt. PotMaxGRD ( unidad, modo, intervalo ) ) then
                                !mantener la rampa mayor para todo el escenario
                                if ( RampaBajURD ( unidad, modo ) .gt. prevRB ( unidad, modo ) ) then
                                    prevRB ( unidad, modo ) = RampaBajURD ( unidad, modo )
                                    RampaBajURD ( unidad, modo ) = PrevGenMin ( modo ) - mult * RampaBajURD ( unidad, modo ) - PotMaxGRD ( unidad, modo, intervalo )
                                    Call FechaEjecucion (fecha_Ej)
                                    bmensaje = fecha_Ej//' '
                                    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                    Call FechaEjecucion (fecha_Ej)
                                    write ( aaux4, 5103 ) RampaBajURD ( unidad, modo ) * base
                                    write ( aaux1, 5101 ) nombunird ( unidad )
                                    !write ( aaux2, 5102 ) intervalo
                                    Call FechaEjecucion (fecha_Ej)
                                    !BMensaje = fecha_Ej//' DERS-I100 INT, RampaBajURC REL:'//aaux1//','//aaux2//','//aaux4
                                    BMensaje = fecha_Ej//' DERS-I100, RampaBaj REL:'//aaux1//','//aaux4
                                    call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                    Call FechaEjecucion (fecha_Ej)
                                    BMensaje = fecha_Ej//'         '
                                    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
                                end if
                            end if                        
                        else
                            !Se resetea el contador
                            mult = 0
                            !Intervalo anterior apagado pero puede encender
                            PrevGenMax ( modo ) = PotMinGRD ( unidad, modo, intervalo )
                            PrevGenMin ( modo ) = PotMinGRD ( unidad, modo, intervalo ) + RampaBajURD ( unidad, modo )
                        end if
                    end if
                end if
            end if
        end do
    end do
end do


5103 FORMAT (F7.2)
5101 FORMAT (a20)
5102 FORMAT (i3)

end subroutine Valid_Rampa_RD

    
subroutine ValidaNumeroMinimoUniRegulacion
! ---------------------------------------------------------------------
! Valida el numero minimo de unidades en regulación                   *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre de 2017                                                  *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE

implicit none

integer i, r, unidad, u, k, iniciou, imax, modo

CHARACTER fecha_Ej*19

! para todo los grupos de reserva
do r = 1, NumGruRes
!    imax = NminRegZ ( r )
    imax = NminRegZ ( r, 1 )
!   para todos los intervalos
    do i = 1, NTINTR
        k = 0
!       si existe requerimiento de reserva de regulacion
        if ( ReqResReg (r, 1, i) .gt. 0.0 ) then
!           para las unidades de rango continuo que estan en ese grupo (zona)
            iniciou = ApunURCxZona ( r )
            do unidad = 1 , NumURCxZona ( r )*SiUniRC
                u = UniRCxZona ( iniciou )
!               si es unidad disponible en este periodo y oferta reserva de regulacion secundaria
                if ( DispoURC ( u , i ) .eq. 1 .and. CompSincRC ( u, i ) .eq. 0 .and. CalOferResRegRC ( u, i ) .gt. 0.0 ) then
                   k = k + 1
                endif
                iniciou = iniciou + 1
            enddo
            
    !       para las unidades de rango discontinuo que estan en ese grupo (zona)
            iniciou = ApunURDxZona ( r )
            do unidad = 1 , NumURDxZona ( r )*SiUniRD
                u = UniRDxZona ( iniciou )
    !           para los modos excepto el apagado
                do modo = 2, NumModRD ( u )
    !               si es unidad disponible en este periodo y oferta reserva de regulacion secundaria
                    if ( DispoURD ( u, modo, i ) .eq. 1 .and. CalOferResRegRD ( u, modo, i ) .gt. 0.0 ) then
    !                   coeficiente de la variable de reserva
                        k = k + 1
                        exit
                    endif
                enddo
                iniciou = iniciou + 1
            enddo
    !       para las unidades hidro que estan en ese grupo (zona)
            iniciou = ApunUHxZona ( r )
            do unidad = 1 , NumUHxZona ( r )*SiUniH
                u = UniHxZona ( iniciou )
    !           si es unidad disponible y cordinable en este periodo
                if ( DispoUH ( u , i ) .eq. 1 .and. CalOferResRegH ( u, i ) .gt. 0.0 ) then
                    k = k + 1
                endif
                iniciou = iniciou + 1
            enddo
            imax = min ( imax, k )
        endif
    enddo
!    if ( imax .lt. NminRegZ ( r ) ) then
    if ( imax .lt. NminRegZ ( r, 1 ) ) then
        Call FechaEjecucion (fecha_Ej)
        bmensaje = fecha_Ej//' '//NomEjecu//'001 PROBLEMAS EN MIN UNI REG ZONA '//NomZonaRes(r)
        Call Mensaje_AuSeg ( 0, 1, BMensaje )
!        NminRegZ ( r ) = imax
        NminRegZ ( r, 1 ) = imax
        SemBandera ( 7 ) = 1
        write(*,*) '1'
        write(998,*) '1'
!       terminacion anormal del algoritmo
        call SalidaError
!       Se ecribe resultado de semaforos
        write ( UniSemaf, * ) SemBandera ( 1 ),',',' Corte de energia'
        write ( UniSemaf, * ) SemBandera ( 2 ),',',' Excedente de energia'
        write ( UniSemaf, * ) SemBandera ( 3 ),',',' Escasez de reservas'
        write ( UniSemaf, * ) SemBandera ( 4 ),',',' Infactibilidad en transmision'
        write ( UniSemaf, * ) SemBandera ( 5 ),',',' Violacion de limites de energia termo'
        write ( UniSemaf, * ) SemBandera ( 6 ),',',' Violacion de limites de energia hidro'
        write ( UniSemaf, * ) SemBandera ( 7 ),',',' Problema de optimizacion infactible'
        write ( UniSemaf, * ) SemBandera ( 8 ),',',' Violacion de limites unidades hidro'
!        write ( UniSemaf, * ) SemBandera ( 9 ),',',' Violacion de limites de combustible'
        stop
    endif
enddo

return
end
    
subroutine ValidaLimitesRegulacionRC
! ---------------------------------------------------------------------
! Valida limites de regulacion de unidades de rango continuo          *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre de 2018                                                   *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE

implicit none

integer u, i

alertaregRC = 0

! Verifica consistencia en límites de regulacion
do u = 1, NumUniRC
  do i = 1, NTINTR    
     if ( DISPOURC ( u, i ) .eq. 1 ) then
        if ( PotMinRRC ( u, i ) .gt. PotMaxGRC ( u, i ) .and. PotMaxRRC(u,i) .gt. PotMaxGRC ( u, i ) ) then
            PotMinRRC ( u, i ) = PotMaxGRC ( u, i ) 
            PotMaxRRC ( u, i ) = PotMaxGRC ( u, i )
            if ( OferResRegRC ( u, i ) .gt. 0.0 ) alertaregRC ( u, i ) = 1
        else if ( PotMinRRC ( u, i ) .lt. PotMinGRC ( u, i ) .and. PotMaxRRC(u,i) .lt. PotMinGRC ( u, i ) ) then
             PotMinRRC ( u, i ) = PotMinGRC ( u, i ) 
             PotMaxRRC ( u, i ) = PotMinGRC ( u, i ) 
             if ( OferResRegRC ( u, i ) .gt. 0.0 ) alertaregRC ( u, i ) = 1
        endif
        if ( PotMinRRC ( u, i ) .lt. PotMinGRC ( u, i )   ) then
            PotMinRRC ( u, i ) = PotMinGRC ( u, i )
            if ( OferResRegRC ( u, i ) .gt. 0.0 ) alertaregRC ( u, i ) = 1
        endif
        if ( PotMaxRRC ( u, i ) .gt. PotMaxGRC ( u, i ) ) then
            PotMaxRRC ( u, i ) = PotMaxGRC ( u, i )
            if ( OferResRegRC ( u, i ) .gt. 0.0 ) alertaregRC ( u, i ) = 1
        endif        
    endif
 enddo
enddo

return
end

subroutine ValidaLimitesRegulacionUH
! ---------------------------------------------------------------------
! Valida limites de regulacion de unidades hidro                      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Noviembre de 2018                                                   *
! ---------------------------------------------------------------------
use ParAUHE
use ProblemaAUHE

implicit none

integer u, i

alertaregUH = 0

! Verifica consistencia en límites de regulacion
do u = 1, NumUniHid
 do i = 1, NTINTR    
    if ( DISPOUH ( u, i ) .eq. 1 ) then
        if ( PotMinRUniH ( u, i ) .gt. PotMaxUniH ( u, i ) .and. PotMaxRUniH ( u, i ) .gt. PotMaxUniH ( u, i ) ) then
            PotMinRUniH ( u, i ) = PotMaxUniH ( u, i ) 
            PotMaxRUniH ( u, i ) = PotMaxUniH ( u, i )
            if ( OferResRegH ( u, i ) .gt. 0.0 ) alertaregUH ( u, i ) = 1
        else if ( PotMinRUniH ( u, i ) .lt.  PotMinUniH ( u, i ) .and. PotMaxRUniH ( u, i ) .lt.  PotMinUniH  ( u, i ) ) then
             PotMinRUniH ( u, i ) =  PotMinUniH  ( u, i ) 
             PotMaxRUniH ( u, i ) =  PotMinUniH  ( u, i ) 
            if ( OferResRegH ( u, i ) .gt. 0.0 ) alertaregUH ( u, i ) = 1
        endif
        if ( PotMinRUniH ( u, i ) .lt.  PotMinUniH  ( u, i )   ) then
            PotMinRUniH ( u, i ) =  PotMinUniH  ( u, i )
            if ( OferResRegH ( u, i ) .gt. 0.0 ) alertaregUH ( u, i ) = 1
        endif
        if ( PotMaxRUniH  (u, i) .gt. PotMaxUniH ( u, i ) ) then
            PotMaxRUniH ( u, i ) = PotMaxUniH ( u, i )
            if ( OferResRegH ( u, i ) .gt. 0.0 ) alertaregUH ( u, i ) = 1
        endif        
    endif
 enddo
enddo

return
end
    
subroutine ValidaCurvaCostosURC
! ---------------------------------------------------------------------
! Valida que la curva de costos este acorde con límites de despacho   *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Febrero de 2020                                                     *
! ---------------------------------------------------------------------
use ParAUHE, only: CostoMinGRC, NumUniRC, NTINTR, NumBloVRC, PotMinGRC, OferVenEnerRC, PreVenEnerRC, PotMinOfeGRC, CoordURC, DispoURC, PotMaxGRC

Implicit none

integer u, i, s

real*8 sumpot, sumcosto, pot

! Para todas la unidades de rango continuo
do u = 1, numunirc
    do i = 1, NTINTR
!        if ( CoordURC ( u, i ) .eq. 1 .and. DispoURC ( u, i ) .eq. 1 ) then
        if ( DispoURC ( u, i ) .eq. 1 ) then
            ! Verifica si es necesario un ajuste en la curva de costos
            if ( PotMinGRC ( u, i ) - PotMinOfeGRC ( u, i ) .gt. 0.0 ) then
!              Ajusta curva a nuevo limite de despacho               
               sumpot = PotMinOfeGRC ( u, i ) 
               do s = 1, NumBloVRC ( u, i )
                 sumpot = sumpot + OferVenEnerRC ( u, s, i )
                 if ( sumpot .le. PotMinGRC ( u, i ) ) then
                     CostoMinGRC ( u, i ) = CostoMinGRC ( u, i ) + OferVenEnerRC ( u, s, i )*PreVenEnerRC ( u, s, i )
                     OferVenEnerRC ( u, s, i ) = 0.0
                 else
                     pot = sumpot - PotMinGRC ( u, i )
                     CostoMinGRC ( u, i ) = CostoMinGRC ( u, i ) + ( OferVenEnerRC ( u, s, i ) - pot )*PreVenEnerRC ( u, s, i )
                     OferVenEnerRC ( u, s, i ) = pot
                     exit
                 endif
               enddo
            endif
            ! Verifica si es necesario un ajuste en la curva de costos
            if ( PotMinOfeGRC ( u, i ) - PotMinGRC ( u, i )  .gt. 0.0 ) then
!              Ajusta curva a nuevo limite de despacho               
               pot = PotMinOfeGRC ( u, i ) - PotMinGRC ( u, i )
               s = 1
               CostoMinGRC ( u, i ) = max ( CostoMinGRC ( u, i ) - pot*OferVenEnerRC ( u, s, i ), 0.0 )
               if ( PotMinGRC ( u, i ) .eq. 0 ) CostoMinGRC ( u, i ) = 0.0
               OferVenEnerRC ( u, s, i ) = OferVenEnerRC ( u, s, i ) + pot
            endif
!           Verifica que la oferta cumpla con todo el rango de generación               
            sumpot = PotMinGRC ( u, i )
            do s = 1, NumBloVRC ( u, i )
               sumpot = sumpot + OferVenEnerRC ( u, s, i )
            enddo
            if ( sumpot .lt.  PotMaxGRC ( u, i ) ) then
                s = NumBloVRC ( u, i )
                if ( s .eq. 0 ) s = 1
                OferVenEnerRC ( u, s, i ) = OferVenEnerRC ( u, s, i ) +   PotMaxGRC ( u, i )  - sumpot
            endif
        endif
    enddo
enddo

return
end
    
! ---------------------------------------------------------------------
! Valida que los generadores en islas muertas no tengan ofertas de    *
! reserva ni energía                                                  *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Marzo de 2020                                                       *
! ---------------------------------------------------------------------
Subroutine ValidaGeneradoresEnIslasMuertas

Use ParAUHE, only: bmensaje, NTINTR, NumNodos, SlackIsla, EstadoIsla, maxnod, numunirc, NoNodDisRC, Tempnodorc, nodorc, OferVenEnerRC, &
                   OferResR10RC, OferResNR10RC, OferResRxRC, OferResNRxRC, OferResRegRC, nombunirc, &
                   NumUniRD, NumModRD, nodocompurd, NumCompXModo, NumCompRD, ApunCompURD, CompXModo, ListCompURD, &
                   NumBloVRD, OferVenEnerRD, OferResR10RD, OferResNR10RD, OferResRxRD, OferResNRxRD, OferResRegRD, &
                   NumUniHid, OferResR10H, OferResNR10H, OferResRxH, OferResNRxH, OferResRegH, &
                   PotMinUniH, PotMaxUniH, PotMinRUniH, PotMaxRUniH, nodoh, NumOferDem, NoNodDisCar, Tempnodocar, &
                   DemFija, facdistcar, NumNodInt, NodoInt, PotNodInt, PotMinGRD, PotMinGRC, PotMaxGRC, PotMaxGRD

Use ParGloRed, only: nmnod, inanod, inrnod, disnodini, sisnod, disnod, apnois, linois, &
                     numram, disram, disramini, oriram, desram, numisl, islnod, &
                     lisnoddisint, apunoddisint, estnoddisint, &
                     lisramdisint, apuramdisint, estramdisint, nomnod, maxisl

use ProblemaAUHE, only: INDDE, IDF, IEXC, xMILP, INIURDI, IARD

implicit none

integer i,  icambia, isl, u, nodi, nodo, ban, nmax, modo, componente, componente_1, k

integer islactiva ( maxisl )

icambia = 0; nmax = 3

do i = 1, NTINTR
    ! Configura la red para identificar los nodos de islas muertas
    call PreparaRedIntervalo ( i, icambia )

    ! Identifica islas muertas
    islactiva = 0;
    do isl = 1, numisl
       if ( ( apnois(isl+1) -1 - apnois(isl) ) .gt. nmax ) then
          islactiva(isl) = 1
       endif
    enddo

 
    ! Verifica que unidades de rango continuo estan en islas muertas
    do u = 1 , NumUniRC  
        ban = 0
        if ( NoNodDisRC ( u, i ) .gt. 0 ) then
    !       para todos los nodos distribuidos
            do NoDi = 1, NoNodDisRC ( u, i )
                nodo = Tempnodorc ( u, NoDi, i )
                isl = islnod ( nodo )
                if ( islactiva(isl) .eq. 1 ) then
                   ban = 1; exit     
                endif
            enddo
        else
            nodo = nodorc ( u, i )
            if ( nodo .ne. 0 ) then
                isl = islnod ( nodo )
                if ( islactiva(isl) .eq. 1 ) then
                   ban = 1     
                endif
            else
               bmensaje = '! UNIDAD DE RANGO CONTINUO ['//trim(nombunirc(u))//']'        
               call EnviaMensajeError ( bmensaje )
               bmensaje = '! NO ESTA CONECTADO A ALGUN NODO'
               call EnviaMensajeError ( bmensaje )
            endif
        endif
    !   Verifica si esta en una isla muerta    
        if ( ban .eq. 0 ) then
           OferVenEnerRC ( u , :, i )  = 0.0
           OferResR10RC ( u, i ) = 0.0
           OferResNR10RC ( u, i ) = 0.0
           OferResRxRC ( u, i ) = 0.0
           OferResNRxRC ( u, i ) = 0.0
           OferResRegRC ( u, i ) = 0.0
           PotMinGRC ( u, i ) = 0.0           
       	   PotMaxGRC ( u, i ) = 0.0
        endif
    
    enddo

    ! Verifica unidades de rango discontinuo estan en islas muertas
    do u = 1 , NumUniRD
        ban = 0
    !   para todos los modos de operacion
        do modo = 2, NumModRD(u)
    !       para el modo de operacion asignado
            if ( xMILP(IARD + INIURDI ( u, i ) + modo - 1) .gt. 0.8  ) then
                do componente = 1, NumCompXModo ( u, modo )
        !           para todas la componentes de la unidad de rango discontinuo
                    do componente_1 = 0, NumCompRD ( u ) - 1
                        if ( CompXModo ( u, modo, componente ) .eq. ListCompURD ( ApunCompURD ( u ) + componente_1 ) ) then
                            nodo = nodocompurd ( ApunCompURD ( u ) + componente_1, i )
                            isl = islnod ( nodo )
                            if ( islactiva(isl) .eq. 1 ) then
                               ban = 1     
                            endif
                            exit
                        endif
                    enddo
                enddo
                exit
            endif
        enddo
    !   Verifica si esta en una isla muerta    
        if ( ban .eq. 0 ) then
           OferVenEnerRD ( u, :, :, i ) = 0.0
           OferResR10RD ( u, :, i ) = 0.0
           OferResNR10RD ( u, :, i ) = 0.0
           OferResRxRD ( u, :, i ) = 0.0
           OferResNRxRD ( u, :, i ) = 0.0
           OferResRegRD ( u, :, i ) = 0.0
           PotMinGRD ( u, :, i ) = 0.0           
           PotMaxGRD ( u, :, i ) = 0.0           
        endif
    enddo

    ! Verifica unidades hidro estan en islas muertas
    do u = 1 , NumUniHid
      ban = 0
      nodo = nodoh ( u, i )
      isl = islnod ( nodo )
      if ( islactiva(isl) .eq. 1 ) then
         ban = 1     
      endif
    ! Verifica si esta en una isla muerta    
      if ( ban .eq. 0 ) then
      !     GenCIH ( u ) = 0.0
           PotMinUniH ( u, i ) = 0.0
           PotMaxUniH ( u, i ) = 0.0
           PotMinRUniH ( u, i ) = 0.0
           PotMaxRUniH ( u, i ) = 0.0
           OferResR10H ( u, i ) = 0.0
           OferResNR10H ( u, i ) = 0.0
           OferResRxH ( u, i ) = 0.0
           OferResNRxH ( u, i ) = 0.0
           OferResRegH ( u, i ) = 0.0
      endif
    enddo

!   Verifica porcion de carga conectada a isla muerta
    do k = 1, NumOferDem  
!       para todos los nodos distribuidos
        do NoDi = 1, NoNodDisCar ( k, i )
            nodo = Tempnodocar ( k, NoDi, i )
            isl = islnod ( nodo )
            if ( islactiva(isl) .eq. 0 ) then
               DemFija ( k, i ) =  DemFija ( k, i ) - DemFija ( k, i )*facdistcar ( k, NoDi, i )
               facdistcar ( k, NoDi, i ) = 0.0
            endif
         enddo
    enddo


!   Nodos con nodos de intercambio    
    do k = 1, NumNodInt
       nodo = NodoInt ( k, i )
       isl = islnod ( nodo )
       if ( islactiva(isl) .eq. 0 ) then
           PotNodInt ( k, i ) = 0.0
       endif
    enddo

enddo

return 
end    