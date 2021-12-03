! -----------------------------------------------------------------
! * Calcula costos marginales nodales y regionales por subsistema *
! -----------------------------------------------------------------

Subroutine CalculaImprimeLMP_PMR ( isistema )

Use ParAUHE, only: NumNodos, NTINTR, RUT_RES, RUT_DAT_1, base
use ProblemaAUHE, only: SiEscResRed

use ParGloRed, only:  nomnod, snspernod, numram, nomram, fluramint, oriram, desram, perdramint, angvolnodint

Implicit none

integer isistema, i, n, ierror

!       calcula marginales de la region
        call CalculaMarginalesRegion ( isistema )
        
        ! Imprime costos marginales nodales
        call ImprimeLMPcsv ( isistema )

        ! Imprime costos marginales regionales
        call ImprimePMRcsv ( isistema )
        
        ! Imprime costos marginales por zona de carga
        call ImprimePMZcsv ( isistema )

        ! Imprime sensibilidades de perdidas
        OPEN (UNIT = 10, FILE = trim(RUT_RES)//'SnsPerdNodales.txt', &
              IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000)

        OPEN (UNIT = 13, FILE = trim(RUT_DAT_1)//'ANGVOLNODAL_1.csv', &
              IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000)

        OPEN (UNIT = 16, FILE = trim(RUT_DAT_1)//'SNSPERDNODALES_1.csv', &
              IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000)

        ! Se imprimen las sensibilidades de perdidas para todos los intervalos
        do n = 1, NumNodos*SiEscResRed
            IERROR = 0
!            write ( 10, 100, IOSTAT = IERROR ) n, nomnod(n), ( snspernod ( n, i ),  i = 1, NTINTR )
            write ( 13, 100, IOSTAT = IERROR ) n, nomnod(n), ( angvolnodint ( n, i )*57.3,  i = 1, NTINTR )
            write ( 16, 100, IOSTAT = IERROR ) n, nomnod(n), ( snspernod ( n, i ),  i = 1, NTINTR )
        enddo

        ! Imprime flujos y perdidas para todos las ramas por intervalo
        OPEN (UNIT = 11, FILE = trim(RUT_DAT_1)//'FLUPOTACTRAMA_1.csv', &
              IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000)
        
        OPEN (UNIT = 12, FILE = trim(RUT_DAT_1)//'PERPOTACTRAMA_1.csv', &
              IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000)

        ! Imprime flujos y perdidas para todas las ramas por intervalo
        do n = 1, NumRam*SiEscResRed
            IERROR = 0
            write ( 11, 200, IOSTAT = IERROR ) n, nomram(n), nomnod( oriram(n) ), nomnod ( desram(n) ), ( fluramint(n,i)*base,  i = 1, NTINTR )
            write ( 12, 200, IOSTAT = IERROR ) n, nomram(n), nomnod( oriram(n) ), nomnod ( desram(n) ), ( perdramint(n,i)*base,  i = 1, NTINTR )
        enddo
        
        close (10)
        close (11)
        close (12)
        close (13)
        close (16)
        
 
100 format (i5, ',', '"', a20,'"', ',', 169(f13.6,',') )
200 format (i5, ',', '"', a30,'"', 2(',', '"', a20,'"'), ',', 169(f13.6,',') )
        
return

end

! ------------------------------------------------------
! * Calcula costos marginales regionales por intervalo *
! ------------------------------------------------------
Subroutine CalculaMarginalesRegion ( isistema )

Use ParAUHE, only: NumNodos, NTINTR, maxint, maxnod, maxiteper, rut_dat_1

Use ProblemaAUHE, only: IterPerdidas, PrecioTopeEner

Use ParGloRed, only: MargRegional, MargRegionalEnergia, MargRegionalPerdidas, MargRegionalTransmision, NumRegPre, MargNodal, MargEnergia, &
               MargPerdidas, MargTransmision, regnod, maxregpre, pcanod, sisnod, nomregpre, numzoncar, numnodzoncar, &
               indnodzoncar, facdisnodzoncar, MargZonCar, MargZonCarPerdidas, MargZonCarEnergia, MargZonCarTransmision, &
               MargZonCarMod, MargZonCarPerdidasMod, MargZonCarEnergiaMod, MargZonCarTransmisionMod, &
               MargNodalMod, MargEnergiaMod, MargPerdidasMod, MargTransmisionMod, preciotope, basmva, preciopiso, pgenod, disnod, nomgruram, nmgruram

Implicit none

integer intervalo, isistema, i, j, n, iter, ierror, ierror1, igrurar

integer numnodreg (maxregpre)

real*8 sumdemreg (maxregpre),  sumgenreg (maxregpre)

real*8 sumprereg (maxregpre)

real*8 sumpreponreg (maxregpre), sumprepongenreg (maxregpre)

real*8 sumpreregene (maxregpre)

real*8 sumpreregper (maxregpre)

real*8 sumpreregtra (maxregpre)

real*8 sumpreponregene (maxregpre)

real*8 sumpreponregper (maxregpre)

real*8 sumpreponregtra (maxregpre)

real*8 facsup, preciotecho, sum

real*8 SenPerIteInt ( maxiteper, maxint, maxnod )

real*8 CorteReg ( maxregpre, maxint ), ExceReg ( maxregpre, maxint ), PmlMaxReg ( maxregpre, maxint ), PmlMinReg ( maxregpre, maxint ), DesvStdReg ( maxregpre, maxint )
real*8 MargPonDemReg  ( maxregpre, maxint ),  MargPonGenReg  ( maxregpre, maxint ), vecval ( maxregpre, maxnod ), PotGenReg ( maxregpre, maxnod ), DemReg ( maxregpre, maxnod )

! corte y excedente de potencia activa del nodo para el intervalo a evaluar
real*8 cornod(maxnod),  excenod ( maxnod ), potgennod (maxnod) , demnod ( maxnod )

character*2 letite
character*5 letn
character*12 letnom
character*3 leti
character*250 letaux

! Lee sensibilidades de perdidas
rewind ( 876 )
iter = 0
ierror = 0
read ( 876, 200, iostat = ierror ) letaux

do while ( ierror .eq. 0 .and. len_trim(letaux) .ne. 0 ) 
    iter = iter + 1
    i = 1
    do while ( i .le. NTINTR ) 
        n = 1
        do while (  ierror .eq. 0 .and. len_trim(letaux) .ne. 0  .and. n .le. NumNodos )
            if ( ierror .eq. 0 ) then
               read ( letaux, *, iostat = ierror1 ) letite, letn,  letnom, leti, SenPerIteInt ( iter, i, n )
            endif
            read ( 876, 200, iostat = ierror ) letaux
            n = n + 1
        enddo
        i = i + 1
    enddo
enddo

CLOSE ( UNIT = 876, status = 'delete' )

preciotecho = preciotope
!preciotecho = PrecioTopeEner

! Inicializa arreglos asociados a regiones de precios
CorteReg = 0.0
ExceReg = 0.0
PmlMaxReg = -1.0e-10
PmlMinReg = 1.0e+10
DesvStdReg = 0.0
MargPonDemReg = 0.0
MargPonGenReg = 0.0
PotGenReg = 0.0
DemReg = 0.0

! Inicializa marginales nodales por intervalo y zonas de carga
MargNodal = 0.0
MargZonCar = 0.0
MargNodalMod = 0.0
MargZonCarMod = 0.0
MargZonCarEnergia = 0.0
MargZonCarEnergiaMod = 0.0
MargZonCarPerdidas = 0.0
MargZonCarPerdidasMod = 0.0
MargZonCarTransmision = 0.0
MargZonCarTransmisionMod = 0.0

! Imprime sensibilidades de flujos
!OPEN (UNIT = 10, FILE = trim(rut_dat_1)//'SNSFLUGRUPOSRAMAS.csv', &
!      IOSTAT = IERROR, STATUS='UNKNOWN', RECORDSIZE = 4000)
!write ( 10, 300 )  '"Hora", "indNod", "clv_nodo",', ( nomgruram( igrurar ), igrurar = 1, nmgruram )

!300 format ( a30, 120(a25,",") )

do intervalo = 1, NTINTR
   
     ! Prepara la generacion y carga del intervalo   
     call PreparaGenIntervalo ( isistema, intervalo )

     ! Prepara los cortes y excedentes del intervalo
     call PreparaCorteExcedenteIntervalo ( isistema, intervalo, cornod, excenod, potgennod, demnod )
     
     ! Inicializa arreglo donde se almacena y calcula el precio marginal regional por intervalo
     sumdemreg = 0.0
     sumpreponreg = 0.0
     sumprereg = 0.0
     sumpreregene = 0.0
     sumpreregper = 0.0
     sumpreregtra = 0.0
     sumpreponregene = 0.0
     sumpreponregper = 0.0
     sumpreponregtra = 0.0
     sumgenreg = 0.0
     sumprepongenreg = 0.0
     numnodreg = 0
     vecval = 0.0
     
       
     ! Calcula sensibilidades de flujos de grupos de ramas con respecto a inyecciones nodales
     call CalculaSensibilidadesFlujos ( isistema, intervalo, 0, 1 )
       
     ! Calcula el costo marginal nodal del intervalo
     call CalMargNodal ( isistema, intervalo,  SenPerIteInt )
     
     ! Acumula el costo marginal nodal por region
     do i = 1, NumNodos
         if ( sisnod(i) .eq. isistema ) then
             
             ! Verifica si se rebasa el precio tope
             if ( MargNodal(i, intervalo) .gt. preciotecho  ) then
                 if ( MargTransmision(i, intervalo) .gt. preciotecho .and. MargEnergia(i, intervalo) .lt. preciotecho ) then
                     MargEnergiaMod(i, intervalo) = MargEnergia(i, intervalo)
                     MargPerdidasMod(i, intervalo) = MargPerdidas(i, intervalo)
                     MargTransmisionMod(i, intervalo) = preciotecho -  MargEnergia(i, intervalo) + MargPerdidas(i, intervalo)
                     MargNodalMod(i, intervalo) =  MargEnergiaMod(i, intervalo) - MargPerdidasMod(i, intervalo) +  MargTransmisionMod(i, intervalo)
                 else if ( MargNodal(i, intervalo) .ne. 0.0 ) then
                     facsup = preciotecho/ MargNodal(i, intervalo)
                     MargEnergiaMod(i, intervalo) = facsup*MargEnergia(i, intervalo)
                     MargPerdidasMod(i, intervalo) = facsup*MargPerdidas(i, intervalo)
                     MargTransmisionMod(i, intervalo) = facsup*MargTransmision(i, intervalo)
                     MargNodalMod(i, intervalo) =  MargEnergiaMod(i, intervalo) - MargPerdidasMod(i, intervalo) +  MargTransmisionMod(i, intervalo)
                 endif
             ! Verifica si se rebasa el precio piso
             else if ( MargNodal(i, intervalo) .lt. preciopiso  ) then
                 if ( MargTransmision(i, intervalo) .lt. ( MargEnergia(i, intervalo) - MargPerdidas(i, intervalo ) ) .and. MargEnergia(i, intervalo) .gt. preciopiso  ) then
                     MargEnergiaMod(i, intervalo) = MargEnergia(i, intervalo)
                     MargPerdidasMod(i, intervalo) = MargPerdidas(i, intervalo)
                     MargTransmisionMod(i, intervalo) = preciopiso -  MargEnergia(i, intervalo) + MargPerdidas(i, intervalo)
                     MargNodalMod(i, intervalo) =  MargEnergiaMod(i, intervalo) - MargPerdidasMod(i, intervalo) +  MargTransmisionMod(i, intervalo)
                 else if ( MargNodal(i, intervalo) .ne. 0.0 ) then
                     facsup = preciopiso/ MargNodal(i, intervalo)
                     MargEnergiaMod(i, intervalo) = facsup*MargEnergia(i, intervalo)
                     MargPerdidasMod(i, intervalo) = facsup*MargPerdidas(i, intervalo)
                     MargTransmisionMod(i, intervalo) = facsup*MargTransmision(i, intervalo)
                     MargNodalMod(i, intervalo) =  MargEnergiaMod(i, intervalo) - MargPerdidasMod(i, intervalo) +  MargTransmisionMod(i, intervalo)
                 endif
             else
                MargNodalMod(i, intervalo) = MargNodal(i, intervalo)
                MargEnergiaMod(i, intervalo) = MargEnergia(i, intervalo)
                MargPerdidasMod(i, intervalo) = MargPerdidas(i, intervalo)
                MargTransmisionMod(i, intervalo) = MargTransmision(i, intervalo)
             endif
!            Valores para ponderacion por demanda             
             if ( pcanod(i) .gt. 0.0 ) then
                  sumpreponreg(regnod(i)) = sumpreponreg(regnod(i)) + MargNodalMod(i, intervalo)* pcanod(i)
                  sumpreponregene(regnod(i)) = sumpreponregene(regnod(i)) + MargEnergiaMod(i, intervalo)*pcanod(i)
                  sumpreponregper(regnod(i)) = sumpreponregper(regnod(i)) + MargPerdidasMod(i, intervalo)*pcanod(i)
                  sumpreponregtra(regnod(i)) = sumpreponregtra(regnod(i)) + MargTransmisionMod(i, intervalo)*pcanod(i)
                  sumdemreg(regnod(i)) = sumdemreg(regnod(i)) + pcanod(i)
             endif
!            Valores para ponderacion por generacion            
             if ( potgennod(i) .gt. 0.0 ) then
                sumgenreg(regnod(i)) = sumgenreg(regnod(i)) + potgennod(i)
                sumprepongenreg(regnod(i)) = sumprepongenreg(regnod(i)) +  MargNodalMod(i, intervalo)*potgennod(i)
             endif 
             if (disnod(i) .eq. 1 ) then
                numnodreg ( regnod(i) ) =  numnodreg ( regnod(i) ) + 1
             	vecval ( regnod(i), numnodreg ( regnod(i) ) ) = MargNodalMod(i, intervalo)
             endif
             sumprereg(regnod(i)) = sumprereg(regnod(i)) + MargNodalMod(i, intervalo)
             sumpreregene(regnod(i)) = sumpreregene(regnod(i)) + MargEnergiaMod(i, intervalo)
             sumpreregper(regnod(i)) = sumpreregper(regnod(i)) + MargPerdidasMod(i, intervalo)
             sumpreregtra(regnod(i)) = sumpreregtra(regnod(i)) + MargTransmisionMod(i, intervalo)
!            Calcula cortes excedentes y precios maximos y minimos por region              
             CorteReg ( regnod(i), intervalo ) = CorteReg ( regnod(i), intervalo ) + cornod ( i )
             ExceReg ( regnod(i), intervalo ) = ExceReg ( regnod(i), intervalo ) + excenod ( i )
             if ( MargNodalMod(i, intervalo) .gt. PmlMaxReg ( regnod(i), intervalo ) ) then 
                 PmlMaxReg ( regnod(i), intervalo ) = MargNodalMod(i, intervalo)
             endif
             if ( MargNodalMod(i, intervalo) .lt. PmlMinReg ( regnod(i), intervalo ) .and. disnod(i) .eq. 1 ) then 
                 PmlMinReg ( regnod(i), intervalo ) = MargNodalMod(i, intervalo)
             endif
         endif
     enddo
     
     do i = 1, NumRegPre
         ! Calcula el precio promedio por region
         if ( numnodreg(i) .gt. 0 ) then
               MargRegional(i,intervalo) = sumprereg(i) / numnodreg(i)
               MargRegionalEnergia(i,intervalo) = sumpreregene(i) / numnodreg(i)
               MargRegionalPerdidas(i,intervalo) = sumpreregper(i) / numnodreg(i)
               MargRegionalTransmision(i,intervalo) = sumpreregtra(i) / numnodreg(i)
!              Calcula la desviasion estandar               
               sum = 0
               do n = 1, numnodreg(i)
                   sum = sum + ( vecval ( i, n ) - MargRegional(i,intervalo) )**2
               enddo   
               if ( numnodreg(i) .gt. 1 ) then
                  DesvStdReg ( i, intervalo ) = sqrt ( sum/ ( numnodreg(i) - 1)  )             
               endif
         else
             PmlMaxReg ( i, : ) = 0.0
             PmlMinReg ( i, : ) = 0.0
         endif
         ! Calcula el precio ponderado por carga por region
         if ( abs(sumdemreg(i)) .gt. 0.0 ) then
            MargPonDemReg(i,intervalo) = sumpreponreg(i) / sumdemreg(i)
         endif
          ! Calcula el precio ponderado por generacion por region
         if ( abs(sumgenreg(i)) .gt. 0.0 ) then
            MargPonGenReg(i,intervalo) = sumprepongenreg(i) / sumgenreg(i)
         endif
         DemReg ( i, intervalo ) = sumdemreg ( i )
         PotGenReg ( i, intervalo ) = sumgenreg ( i )
     enddo
     
     do i = 1, Numzoncar
         do j = 1, numnodzoncar(i)
            MargZonCar( i, intervalo ) = MargZonCar( i, intervalo ) +  MargNodal(indnodzoncar(i,j), intervalo)*facdisnodzoncar(i,j,intervalo)
            MargZonCarEnergia( i, intervalo ) = MargZonCarEnergia( i, intervalo ) +  MargEnergia(indnodzoncar(i,j), intervalo)*facdisnodzoncar(i,j,intervalo)
            MargZonCarPerdidas( i, intervalo ) = MargZonCarPerdidas( i, intervalo ) +  MargPerdidas(indnodzoncar(i,j), intervalo)*facdisnodzoncar(i,j,intervalo)
            MargZonCarTransmision( i, intervalo ) = MargZonCarTransmision( i, intervalo ) +  MargTransmision(indnodzoncar(i,j), intervalo)*facdisnodzoncar(i,j,intervalo)
            
            ! Los modificados en caso de que se hallan rebasado el precio tope
            MargZonCarMod( i, intervalo ) = MargZonCarMod( i, intervalo ) +  MargNodalMod(indnodzoncar(i,j), intervalo)*facdisnodzoncar(i,j,intervalo)
            MargZonCarEnergiaMod( i, intervalo ) = MargZonCarEnergiaMod( i, intervalo ) +  MargEnergiaMod(indnodzoncar(i,j), intervalo)*facdisnodzoncar(i,j,intervalo)
            MargZonCarPerdidasMod( i, intervalo ) = MargZonCarPerdidasMod( i, intervalo ) +  MargPerdidasMod(indnodzoncar(i,j), intervalo)*facdisnodzoncar(i,j,intervalo)
            MargZonCarTransmisionMod( i, intervalo ) = MargZonCarTransmisionMod( i, intervalo ) +  MargTransmisionMod(indnodzoncar(i,j), intervalo)*facdisnodzoncar(i,j,intervalo)

         enddo
     enddo
enddo

! imprime costos marginales regionales
call ImprimeMargReg ( CorteReg, ExceReg, PotGenReg, DemReg, PmlMaxReg, PmlMinReg, DesvStdReg, MargPonDemReg, MargPonGenReg )


!close (10)

100 format ( i2, "," ,i5,  "," , a12, ",", i3,  "," , f19.16 ","   )
200 format ( a )
    
return
end
    
! ------------------------------------------------------
! * Calcula costos marginales nodales por intervalo    *
! ------------------------------------------------------
Subroutine CalMargNodal ( isistema, intervalo, SenPerIteInt )

Use ParAUHE, only: NumNodos, maxint, maxiteper
Use ProblemaAUHE, only: dualbalance, DualPerdidas 
Use ParGloRed, only: maxnod, maxgruram, &
                     sisnod, nmgruram, inagruram, &
                     dualgrurams, dualgrurami, SnsPerNod, SnsGruRarInyNod, &
                     MargNodal, nomnod , MargTransmision, MargPerdidas, MargEnergia, &
                     MulPerdidas, ApuMulPerInt, LisMulPerInt, IteMulPerInt, numgruram

Implicit none

integer isistema, i, n, j, icuenta

integer igrurar

integer intervalo

real*8 SenPerIteInt ( maxiteper, maxint, maxnod )

! Inicializa valores
do n = 1, NumNodos
   if ( sisnod(n) .eq. isistema ) then
      MargNodal(n,intervalo) = 0.0
      MargPerdidas(n,intervalo) = 0.0
      MargTransmision(n,intervalo) = 0.0
      MargEnergia(n,intervalo) = 0.0
   endif
enddo

! Agrega elemento asociado a la ecuacion de balance de la isla, y perdidas de la isla
do n = 1, NumNodos
   if ( sisnod(n) .eq. isistema ) then
!     DualPerdidas (isistema, Intervalo) = dualbalance (isistema, Intervalo)
      do j =  ApuMulPerInt ( isistema, intervalo  ),  ApuMulPerInt ( isistema, intervalo + 1 ) - 1
          icuenta = LisMulPerInt ( j )
          MargPerdidas(n,intervalo) = MargPerdidas(n,intervalo) + MulPerdidas ( icuenta )*SenPerIteInt ( IteMulPerInt(icuenta), intervalo, n )
      enddo 
      MargNodal(n,intervalo) = dualbalance (isistema, Intervalo) -  MargPerdidas(n,intervalo)

     ! MargNodal(n,intervalo) = dualbalance (isistema, Intervalo) - DualPerdidas (isistema, Intervalo)*SnsPerNod(n,intervalo)
     ! MargPerdidas(n,intervalo) = MargPerdidas(n,intervalo) + DualPerdidas (isistema, Intervalo)*SnsPerNod(n,intervalo)
      MargEnergia(n,intervalo) = dualbalance (isistema, Intervalo) 
   endif
enddo

! Agrega elemento asociado a grupos de ramas restringidas
do i = 1, nmgruram
  igrurar = inagruram(i)
  do n = 1, NumNodos
    if ( sisnod(n) .eq. isistema ) then
        MargNodal(n,intervalo) = MargNodal(n,intervalo) + ( dualgrurams   ( i, intervalo ) + & 
	                             dualgrurami ( i, intervalo ) ) * SnsGruRarInyNod ( igrurar, n )
!        if (  n .eq. 4577 .and. intervalo .eq. 1 .and.  abs ( ( dualgrurams ( i, intervalo ) + dualgrurami ( i, intervalo ) ) * SnsGruRarInyNod ( igrurar, n ) ) .gt. 0.0 ) then
!            write ( 8888, *  ) igrurar, MargNodal(n,intervalo), ( dualgrurams ( i, intervalo ) + dualgrurami ( i, intervalo ) ), SnsGruRarInyNod ( igrurar, n )
!            150 format ( i3, x, 3(f16.2, x ) )
!        endif
        MargTransmision(n,intervalo) = MargTransmision(n,intervalo) + ( dualgrurams   ( i, intervalo ) + & 
	                             dualgrurami ( i, intervalo ) ) * SnsGruRarInyNod ( igrurar, n )
     endif
  enddo
enddo

! Imprime sensibilidades de grupos de ramas restringidas
!do n = 1, NumNodos
!     if ( sisnod(n) .eq. isistema .and. n .eq. 4577 .or. n .eq. 4578 ) then
!        write ( 10, 100 )  intervalo, n, nomnod(n), ( SnsGruRarInyNod ( igrurar, n ), igrurar = 1, numgruram )
!100 format ( i3, ",", i5, ",", a20, ",", 169(f16.2, ",") )
!     endif
!enddo

return

end
           
! ---------------------------------------------------------------------
! Imprime resultados de costos marginales por region.                 *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Abril del 2015                                                      *
! ---------------------------------------------------------------------

Subroutine ImprimePMRcsv ( subsistema )

Use ParAUHE, only: NTINTR, Base, Unipmr, Unipmrgen, Unipmrper, Unipmrcon, rut_dat_1

Use ParGloRed, only:  NumRegPre, nomregpre, sisregpre, MargRegional, &
                      MargRegionalEnergia, MargRegionalPerdidas, MargRegionalTransmision

Implicit none

integer i, r, ierror, subsistema

character*1 let

write ( let, 200 ) subsistema

! Resultados de costos marginales por region

Unipmr = 110
OPEN ( UNIT = Unipmr, FILE = trim(rut_dat_1)//'PMR_'//let//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )

! Resultados de costos marginales por region componente de balance de energía
Unipmrgen = 111
OPEN ( UNIT = Unipmrgen, FILE = trim(rut_dat_1)//'PMRGEN_'//let//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )

! Resultados de costos marginales por region componente de pérdidas
Unipmrper = 112
OPEN ( UNIT = Unipmrper, FILE = trim(rut_dat_1)//'PMRPER_'//let//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )

! Resultados de costos marginales por region componente de congestión
Unipmrcon = 113
OPEN ( UNIT = Unipmrcon, FILE = trim(rut_dat_1)//'PMRCON_'//let//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )

! Imprime costos marginales regionales
do r = 1, NumRegPre
!   if ( sisregpre(r) .eq. subsistema ) then
      write ( Unipmr, 100, iostat = ierror ) r, nomregpre(r), (  MargRegional( r , i )/Base, i=1, NTINTR )
!   endif
enddo

! Imprime costos marginales regionales, componente de balance de energía
do r = 1, NumRegPre
!   if ( sisregpre(r) .eq. subsistema ) then
      write ( Unipmrgen, 100, iostat = ierror ) r, nomregpre(r), (  MargRegionalEnergia( r , i )/Base, i=1, NTINTR )
!   endif
enddo

! Imprime costos marginales regionales, componente de pérdidas
do r = 1, NumRegPre
!  if ( sisregpre(r) .eq. subsistema ) then
      write ( Unipmrper, 100, iostat = ierror ) r, nomregpre(r), (  -MargRegionalPerdidas( r , i )/Base, i=1, NTINTR )
!  endif
enddo

! Imprime costos marginales regionales, componente congestión de la transmisión
do r = 1, NumRegPre
!   if ( sisregpre(r) .eq. subsistema ) then
      write ( Unipmrcon, 100, iostat = ierror ) r, nomregpre(r), (  MargRegionalTransmision( r , i )/Base, i=1, NTINTR )
!   endif
enddo

! cierra archivo de resultados de costos marginales regionales por intervalo
close ( UNIT = Unipmr )

! cierra archivo de resultados de costos marginales regionales por intervalo ( componenete de energia )
close ( UNIT = Unipmrgen )

! cierra archivo de resultados de costos marginales regionales por intervalo ( componenete de pérdidas )
close ( UNIT = Unipmrper )

! cierra archivo de resultados de costos marginales regionales por intervalo ( componenete de congestion de la transmisión )
close ( UNIT = Unipmrcon )

100 format ( i4, ',', '"',a20,'"', ',',  169(f16.3,',') )
200 format ( i1 )
    
return
end

! ---------------------------------------------------------------------
! Imprime resultados de costos marginales por de zona de carga.                 *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Abril del 2015                                                      *
! ---------------------------------------------------------------------

Subroutine ImprimePMZcsv ( subsistema )

Use ParAUHE, only: NTINTR, Base, Unipmr, rut_dat_1

Use ParGloRed, only:  NumZonCar, nomzoncar, indzoncarems, siszoncar, &
                      MargZonCar, MargZonCarEnergia, MargZonCarPerdidas, MargZonCarTransmision, &
                      MargZonCarMod, MargZonCarEnergiaMod, MargZonCarPerdidasMod, MargZonCarTransmisionMod

Implicit none

integer i, r, ierror, subsistema, Unipmz, Unipmzori
character*1 let

write ( let, 200 ) subsistema

! Resultados de costos marginales por region

Unipmz = 110
OPEN ( UNIT = Unipmz, FILE = trim(rut_dat_1)//'PRECIOS_ZONALES_'//let//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )

! Resultados de costos marginales por de zona de carga componente de balance de energía
Unipmzori = 111
OPEN ( UNIT = Unipmzori, FILE = trim(rut_dat_1)//'PRECIOS_ZONALES_ORI_'//let//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )


! Imprime costos marginales de zona de carga
do r = 1, NumZonCar
   if ( siszoncar(r) .eq. subsistema ) then
      do i = 1, NTINTR
          write ( Unipmzori, 100, iostat = ierror ) nomzoncar(r), indzoncarems(r), i, MargZonCar( r , i )/Base, MargZonCarEnergia( r , i )/Base, &
          -MargZonCarPerdidas( r , i )/Base, MargZonCarTransmision( r , i )/Base
          write ( Unipmz, 100, iostat = ierror ) nomzoncar(r), indzoncarems(r), i, MargZonCarMod( r , i )/Base, MargZonCarEnergiaMod( r , i )/Base, &
          -MargZonCarPerdidasMod( r , i )/Base, MargZonCarTransmisionMod( r , i )/Base
      enddo
   endif
enddo

! cierra archivo de resultados de costos marginales de zona de carga por intervalo rasurados
close ( UNIT = Unipmz )

! cierra archivo de resultados de costos marginales de zona de carga por intervalo originales
close ( UNIT = Unipmzori )

100 format ( '"',a16,'"', ',', i4, ',', i3, ',', 4(f16.3,',') )
200 format ( i1 )
    
return
end 
    
! ---------------------------------------------------------------------
! Imprime resultados de costos marginales nodales (LMP).              *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Diciembre del 2015                                                  *
! ---------------------------------------------------------------------

Subroutine ImprimeLMPcsv ( subsistema )

Use ParAUHE, only: NTINTR, Base, NumNodos, rut_dat_1

Use ParGloRed, only:  nomnod, MargNodal, MargEnergia, MargPerdidas, MargTransmision, sisnod, tipnod, &
                      MargNodalMod, MargEnergiaMod, MargPerdidasMod, MargTransmisionMod

Implicit none

integer i, n, ierror, subsistema, Unilmp, Unilmpori

character*1 let

write ( let, 200 ) subsistema

! Resultados de costos marginales nodales
Unilmp = 130 
OPEN ( UNIT = Unilmp, FILE = trim(rut_dat_1)//'PRECIOS_NODALES_'//let//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )

! Resultados de costos marginales nodales componente de balance de energía
Unilmpori = 131
OPEN ( UNIT =  Unilmpori, FILE = trim(rut_dat_1)//'PRECIOS_NODALES_ORI_'//let//'.csv',IOSTAT = IERROR,  STATUS='UNKNOWN', RECORDSIZE = 3000 )


! Imprime costos marginales nodales
do n = 1, NumNodos
   if ( sisnod(n) .eq. subsistema ) then
      do i = 1, NTINTR
          write ( Unilmpori, 100, iostat = ierror ) n, nomnod(n), tipnod(n), i, MargNodal( n , i )/Base, MargEnergia( n , i )/Base, -MargPerdidas( n , i )/Base, &
                                                 MargTransmision( n , i )/Base
          write ( Unilmp, 100, iostat = ierror ) n, nomnod(n), tipnod(n), i, MargNodalMod( n , i )/Base, MargEnergiaMod( n , i )/Base, -MargPerdidasMod( n , i )/Base, &
                                                 MargTransmisionMod( n , i )/Base
      enddo
   endif
enddo



! cierra archivo de resultados de costos marginales nodales por intervalo
close ( UNIT = Unilmp )

! cierra archivo de resultados de costos marginales nodales originales
close ( UNIT = Unilmpori )

100 format ( i4, ',', '"',a12,'"', ',', '"',I1,'"', ',', I3, ',', 4(f15.3,',') )
200 format ( i1 )

return
end
