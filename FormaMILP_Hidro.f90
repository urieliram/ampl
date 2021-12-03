Subroutine BalanceEmbalRest ( k, m )
! ---------------------------------------------------------------------
! Se forman las restricciones de balance de agua en embalses en el    *
! problema de asignacion (MILP).                                      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre del 2016                                                 *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro

implicit none

INTEGER   i, j, k, m, u, cuenca, embalse, via, intervalo

INTEGER   planta, localidad, localidad_1, localidad_2, localidad_3

character*3 leti


write ( 777,* ) 'Inicia restricciones de balance de agua en embalses              :', m + 1

! restricciones de balance de agua en embalses
j = 1
! para todas las cuencas
do cuenca = 1, nmcuen
!  para los embalses que estan en la cuenca
   do localidad_2 = 0, nmembc ( cuenca ) - 1
      embalse = dnemva ( cuenca ) + localidad_2
!     para el primer intervalo del horizonte
!     coeficientes de volumen en el intervalo
      aaMILP ( k ) = 1.0
      jcolMILP ( k ) = IVOLU + j - 1
      k = k + 1
!     coeficientes de variable artificial de deficit en volumen
      aaMILP ( k ) = -1.0
      jcolMILP ( k ) = IDBAL + j - 1
      k = k + 1
!     coeficientes de variable artificial de excedente en volumen
      aaMILP ( k ) = 1.0
      jcolMILP ( k ) = IEBAL + j - 1
      k = k + 1
!     para las vias convergentes del embalse
      do localidad_3 = 0, nmviasc ( embalse ) - 1 
         via = DNVICO ( embalse )
!        si el embalse tiene vias convergentes
         if ( via .gt. 0 ) then
!           si el tiempo de viaje del agua no hace ir al pasado
            intervalo = 1 - TiViAgu (via)
            if ( intervalo .gt. 0 ) then
!              para todas las plantas que descargan sobre la vía
               do localidad = dnphvi ( via ), dnphvi ( via ) + nphvi ( via ) - 1
                  planta = plantas_x_via ( localidad )
!                 para todas las unidades de la planta
                  do localidad_1 = 0, NOUN ( planta ) - 1
                     u = DNUNPH ( planta ) + localidad_1
!                    coeficientes de turbinado
                     aaMILP ( k ) = -MValor
                     jcolMILP ( k ) = ITURB + u - 1
                     k = k + 1
                  enddo
               enddo
            endif
         endif
      enddo
!     para las vias divergentes del embalse
      do localidad_3 = 0, nmviasd ( embalse ) - 1 
         via = DNVIOU ( embalse ) + localidad_3 
!        para todas las plantas que descargan sobre la vía
         do localidad = dnphvi ( via ), dnphvi ( via ) + nphvi ( via ) - 1
            planta = plantas_x_via ( localidad )
!           para todas las unidades de la planta
            do localidad_1 = 0, NOUN ( planta ) - 1
               u = DNUNPH ( planta ) + localidad_1
!              coeficientes de turbinado
               aaMILP ( k ) = MValor
               jcolMILP ( k ) = ITURB + u - 1
               k = k + 1
            enddo
         enddo
      enddo
      m = m + 1
      if ( m == 41245 ) then
         continue
      endif
!     lados derechos de las restriciones
      bMILP ( m ) = VMINI (embalse) + ApoNetEmb (embalse, 1)
!     sentidos de las restriciones
      sMILP ( m ) = 'E'
!     apuntador al siguiente renglon
      irowMILP ( m + 1 ) = k
      i = 1
      write ( leti, 200 ) i
      write ( 779,* ) m, ',', '"Balance de agua en el embalse: '//trim(nomemb (embalse))//' intervalo: '//trim(leti)//'"'
!     para los intervalos siguientes
      do i = 2 , NTINTR
!        coeficientes de volumen en el intervalo
         aaMILP ( k ) = 1.0
         jcolMILP ( k ) = IVOLU + j + (i-1)*Numembalses - 1
         k = k + 1
!        coeficientes de volumen en el intervalo anterior
         aaMILP ( k ) = -1.0
         jcolMILP ( k ) = IVOLU + j + (i-2)*Numembalses - 1
         k = k + 1
!        coeficientes de variable artificial de deficit en volumen
         aaMILP ( k ) = 1.0
         jcolMILP ( k ) = IDBAL + j + (i-1)*Numembalses - 1
         k = k + 1
!        coeficientes de variable artificial de excedente en volumen
         aaMILP ( k ) = -1.0
         jcolMILP ( k ) = IEBAL + j + (i-1)*Numembalses - 1
         k = k + 1
!        para las vias convergentes del embalse
         do localidad_3 = 0, nmviasc ( embalse ) - 1 
            via = DNVICO ( embalse )
!           si el embalse tiene vias convergentes
            if ( via .gt. 0 ) then
!              si el tiempo de viaje del agua no hace ir al pasado
               intervalo = i - TiViAgu (via)
               if ( intervalo .gt. 0 ) then
!                 para todas las plantas que descargan sobre la vía
                  do localidad = dnphvi ( via ), dnphvi ( via ) + nphvi ( via ) - 1
                     planta = plantas_x_via ( localidad )
!                    para todas las unidades de la planta
                     do localidad_1 = 0, NOUN ( planta ) - 1
                        u = DNUNPH ( planta ) + localidad_1
!                       coeficientes de turbinado
                        aaMILP ( k ) = -MValor
                        jcolMILP ( k ) = ITURB + u + (intervalo-1)*NumUniHid - 1
                        k = k + 1
                     enddo
                  enddo
               endif
            endif
         enddo
!        para las vias divergentes del embalse
         do localidad_3 = 0, nmviasd ( embalse ) - 1 
            via = DNVIOU ( embalse ) + localidad_3 
!           para todas las plantas que descargan sobre la vía
            do localidad = dnphvi ( via ), dnphvi ( via ) + nphvi ( via ) - 1
               planta = plantas_x_via ( localidad )
!              para todas las unidades de la planta
               do localidad_1 = 0, NOUN ( planta ) - 1
                  u = DNUNPH ( planta ) + localidad_1
!                 coeficientes de turbinado
                  aaMILP ( k ) = MValor
                  jcolMILP ( k ) = ITURB + u + (i-1)*NumUniHid - 1
                  k = k + 1
               enddo
            enddo
         enddo
         m = m + 1
         if ( m == 41245 ) then
            continue
         endif
!        lados derechos de las restriciones
         bMILP ( m ) = ApoNetEmb (embalse, i)
!        sentidos de las restriciones
         sMILP ( m ) = 'E'
!        apuntador al siguiente renglon
         irowMILP ( m + 1 ) = k
         write ( leti, 200 ) i
         write ( 779,* ) m, ',', '"Balance de agua en el embalse: '//trim(nomemb (embalse))//' intervalo: '//trim(leti)//'"'
      enddo
      j = j + 1
   enddo
enddo

200 format (i3)

return
end


Subroutine PoliticasOperEmbalRest ( k, m )
! ---------------------------------------------------------------------
! Se forman las restricciones de politicas de operacion en embalses   *
! en el problema de asignacion (MILP).                                *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre del 2016                                                 *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro

implicit none

INTEGER   i, j, k, m, u, embalse, unidad, UniIni

! Apuntador asociado a las restricciones de energia fija en los embalses
ApResEner = 1

! restricciones de limites en embalses
do j = 1, Numembalses
!  para todos los intervalos
   do i = 1 , NTINTR
      lbMILP ( IVOLU + j + (i-1)*Numembalses - 1 ) = VolMnEmb ( j )
      ubMILP ( IVOLU + j + (i-1)*Numembalses - 1 ) = VolMxEmb ( j )
   enddo
end do

write ( 777,* ) 'Inicia restricciones de politicas de operacion en embalses       :', m + 1

! se revisan politicas de operacion
! para todos los embalses
do embalse = 1, Numembalses
!  segun la politica de operacion
   SELECT CASE ( PoliEmb(embalse) )
!     maxima extraccion
      CASE (1)
!        localidad asociada a la primera unidad del embalse
         UniIni = local_uni_emb (embalse)
!        para todas las unidades asociadas al embalse
         do unidad = 1 , numuni_x_embalse (embalse)
!           unidad asociada 
            u = unidad_x_embalse ( UniIni + unidad - 1 )
!           para todos los intervalos
            do i = 1, NTINTR
!              coeficientes de generacion en la funcion objetivo
               objMILP ( IGH + u + (i-1)*NumUniHid - 1 ) = - EMxExt
            enddo
         enddo
!     minima extraccion
      CASE (2)
!        localidad asociada a la primera unidad del embalse
         UniIni = local_uni_emb (embalse)
!        para todas las unidades asociadas al embalse
         do unidad = 1 , numuni_x_embalse (embalse)
!           unidad asociada 
            u = unidad_x_embalse ( UniIni + unidad - 1 )
!           para todos los intervalos
            do i = 1, NTINTR
!              coeficientes de generacion en la funcion objetivo
               objMILP ( IGH + u + (i-1)*NumUniHid - 1 ) = EMnExt
            enddo
         enddo
!     cota final fija
      CASE (3)
!        coeficientes de volumen al final del horizonte
         aaMILP ( k ) = 1.0
         jcolMILP ( k ) = IVOLU + embalse + (NTINTR-1)*Numembalses - 1
         k = k + 1
!        para el volumen del ultimo intervalo del horizonte
!        coeficientes de variable artificial de corte de politica
         aaMILP ( k ) = 1.0
         jcolMILP ( k ) = IDPOL + embalse - 1
         k = k + 1
!        coeficientes de variable artificial de excedente de politica
         aaMILP ( k ) = -1.0
         jcolMILP ( k ) = IEPOL + embalse - 1
         k = k + 1
!        coeficientes de variable artificial de corte de politica en la funcion objetivo
         objMILP ( IDPOL + embalse - 1 ) = CostoCorte
!        coeficientes de variable artificial de excedente de politica en la funcion objetivo
         objMILP ( IEPOL + embalse - 1 ) = CostoCorte
!        limite superior de variable artificial de corte de politica
         ubMILP ( IDPOL + embalse - 1 ) = WEmbFin ( embalse )
!        limite superior de variable artificial de excedente de politica
         ubMILP ( IEPOL + embalse - 1 ) = WEmbFin ( embalse )
!        limite inferior de variable artificial de corte de politica
         lbMILP ( IDPOL + embalse - 1 ) = 0.0
!        limite inferior de variable artificial de excedente de politica
         lbMILP ( IEPOL + embalse - 1 ) = 0.0
         m = m + 1
!        lados derechos de las restriciones
         bMILP ( m ) = WEmbFin ( embalse )
!        sentidos de las restriciones
         sMILP ( m ) = 'E'
!        apuntador al siguiente renglon
         irowMILP ( m + 1 ) = k
!     generacion fija
      CASE (6)
!        localidad asociada a la primera unidad del embalse
         UniIni = local_uni_emb (embalse)
!        para todas las unidades asociadas al embalse
         do unidad = 1 , numuni_x_embalse (embalse)
!           unidad asociada 
            u = unidad_x_embalse ( UniIni + unidad - 1 )
!           para todos los intervalos
            do i = 1, NTINTR
!              coeficientes de generacion
               aaMILP ( k ) = 1.0
               jcolMILP ( k ) = IGH + u + (i-1)*NumUniHid - 1
               k = k + 1
            enddo
         enddo
!        coeficientes de variable artificial de corte de politica
         aaMILP ( k ) = 1.0
         jcolMILP ( k ) = IDPOL + embalse - 1
         k = k + 1
!        coeficientes de variable artificial de excedente de politica
         aaMILP ( k ) = -1.0
         jcolMILP ( k ) = IEPOL + embalse - 1
         k = k + 1
!        coeficientes de variable artificial de corte de politica en la funcion objetivo
         objMILP ( IDPOL + embalse - 1 ) = CostoCorte/2.0
!        coeficientes de variable artificial de excedente de politica en la funcion objetivo
         objMILP ( IEPOL + embalse - 1 ) = CostoCorte/2.0
!        limite superior de variable artificial de corte de politica
         ubMILP ( IDPOL + embalse - 1 ) = LimSEnerEmb (embalse)
!        limite superior de variable artificial de excedente de politica
         ubMILP ( IEPOL + embalse - 1 ) = LimSEnerEmb (embalse)
!        limite inferior de variable artificial de corte de politica
         lbMILP ( IDPOL + embalse - 1 ) = 0.0
!        limite inferior de variable artificial de excedente de politica
         lbMILP ( IEPOL + embalse - 1 ) = 0.0
         m = m + 1
!        apuntador asociado a la restriccion
         ApResEner ( embalse ) = m
!        lados derechos de las restriciones
         bMILP ( m ) = EFijEmb (embalse)
!        sentidos de las restriciones
         sMILP ( m ) = 'L'
!        apuntador al siguiente renglon
         irowMILP ( m + 1 ) = k
!     volumen turbinado maximo
      CASE (5) 
!        localidad asociada a la primera unidad del embalse
         UniIni = local_uni_emb (embalse)
!        para todas las unidades asociadas al embalse
         do unidad = 1 , numuni_x_embalse (embalse)
!           unidad asociada 
            u = unidad_x_embalse ( UniIni + unidad - 1 )
!           para todos los intervalos
            do i = 1, NTINTR
!              coeficientes de turbinado
               aaMILP ( k ) = MValor
               jcolMILP ( k ) = ITURB + u + (i-1)*NumUniHid - 1
               k = k + 1
            enddo
         enddo
!        coeficientes de variable artificial de corte de politica
         aaMILP ( k ) = 1.0
         jcolMILP ( k ) = IDPOL + embalse - 1
         k = k + 1
!        coeficientes de variable artificial de excedente de politica
         aaMILP ( k ) = -1.0
         jcolMILP ( k ) = IEPOL + embalse - 1
         k = k + 1
!        coeficientes de variable artificial de corte de politica en la funcion objetivo
         objMILP ( IDPOL + embalse - 1 ) = CostoCorte/1.0e3
!        coeficientes de variable artificial de excedente de politica en la funcion objetivo
         objMILP ( IEPOL + embalse - 1 ) = CostoCorte/1.0e3
!        limite superior de variable artificial de corte de politica
         ubMILP ( IDPOL + embalse - 1 ) = TFijEmb (embalse)
!        limite superior de variable artificial de excedente de politica
         ubMILP ( IEPOL + embalse - 1 ) = TFijEmb (embalse)
!        limite inferior de variable artificial de corte de politica
         lbMILP ( IDPOL + embalse - 1 ) = 0.0
!        limite inferior de variable artificial de excedente de politica
         lbMILP ( IEPOL + embalse - 1 ) = 0.0
         m = m + 1
!        lados derechos de las restriciones
         bMILP ( m ) = TFijEmb (embalse)
!        sentidos de las restriciones
         sMILP ( m ) = 'L'
!        apuntador al siguiente renglon
         irowMILP ( m + 1 ) = k
   END SELECT
   write ( 779,* ) m, ',', '"Politica de operacion en el embalse: '//trim(nomemb (embalse))//'"'
enddo

return
end


Subroutine LimiteGastoViasRest ( k, m )
! ---------------------------------------------------------------------
! Se forman las restricciones de limites al gasto turbinado en el     *
! problema de asignacion (MILP).                                      *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! septiembre del 2016                                                 *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro

implicit none

INTEGER   i, j, k, m, u, cuenca, embalse, via

INTEGER   planta, localidad, localidad_1, localidad_2, localidad_3

character*3 leti

write ( 777,* ) 'Inicia restricciones de limites super al gasto turbinado en vias :', m + 1

! restricciones de cotas superiores de variables de generacion hidro
j = 1
! para todas las cuencas
do cuenca = 1, nmcuen
!  para los embalses que estan en la cuenca
   do localidad_2 = 0, nmembc ( cuenca ) - 1
      embalse = dnemva ( cuenca ) + localidad_2
!     para todos los intervalos
      do i = 1 , NTINTR
!        para las vias divergentes del embalse
         do localidad_3 = 0, nmviasd ( embalse ) - 1 
            via = DNVIOU ( embalse ) + localidad_3 
!           para todas las plantas que descargan sobre la vía
            do localidad = dnphvi ( via ), dnphvi ( via ) + nphvi ( via ) - 1
               planta = plantas_x_via ( localidad )
!              para todas las unidades de la planta
               do localidad_1 = 0, NOUN ( planta ) - 1
                  u = DNUNPH ( planta ) + localidad_1
!                 coeficientes de turbinado
                  aaMILP ( k ) = 1.0
                  jcolMILP ( k ) = ITURB + u + (i-1)*NumUniHid - 1
                  k = k + 1
               enddo
            enddo
            m = m + 1
!           lados derechos de las restriciones
            bMILP ( m ) = GAMXVI (via)
!           sentidos de las restriciones
            sMILP ( m ) = 'L'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Limite superior al gasto turbinado en la via: '//trim(NOMVIA (via))//' intervalo: '//trim(leti)//'"'
         enddo
      enddo
      j = j + 1
   enddo
enddo

write ( 777,* ) 'Inicia restricciones de limites infer al gasto turbinado en vias :', m + 1

! restricciones de cotas inferiores de variables de generacion hidro
j = 1
! para todas las cuencas
do cuenca = 1, nmcuen
!  para los embalses que estan en la cuenca
   do localidad_2 = 0, nmembc ( cuenca ) - 1
      embalse = dnemva ( cuenca ) + localidad_2
!     para todos los intervalos
      do i = 1 , NTINTR
!        para las vias divergentes del embalse
         do localidad_3 = 0, nmviasd ( embalse ) - 1 
            via = DNVIOU ( embalse ) + localidad_3 
!           para todas las plantas que descargan sobre la vía
            do localidad = dnphvi ( via ), dnphvi ( via ) + nphvi ( via ) - 1
               planta = plantas_x_via ( localidad )
!              para todas las unidades de la planta
               do localidad_1 = 0, NOUN ( planta ) - 1
                  u = DNUNPH ( planta ) + localidad_1
!                 coeficientes de turbinado
                  aaMILP ( k ) = 1.0
                  jcolMILP ( k ) = ITURB + u + (i-1)*NumUniHid - 1
                  k = k + 1
               enddo
            enddo
            m = m + 1
!           lados derechos de las restriciones
            bMILP ( m ) = GAMNVI (via)
!           sentidos de las restriciones
            sMILP ( m ) = 'G'
!           apuntador al siguiente renglon
            irowMILP ( m + 1 ) = k
            write ( leti, 200 ) i
            write ( 779,* ) m, ',', '"Limite inferior al gasto turbinado en la via: '//trim(NOMVIA (via))//' intervalo: '//trim(leti)//'"'
         enddo
      enddo
      j = j + 1
   enddo
enddo

200 format (i3)

return
end


Subroutine VolTransViasFinRest ( k, m )
! ---------------------------------------------------------------------
! Se forman las restricciones de volumen en transito en vias durante  *
! el intervalo final, en el problema de asignacion (MILP).            *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Septiembre del 2016                                                 *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro

implicit none

INTEGER   i, j, k, m, u, cuenca, embalse, via, inicio, ifinal

INTEGER   planta, localidad, localidad_1, localidad_2, localidad_3

character*3 leti

write ( 777,* ) 'Inicia restricciones de volumen en transito en vias interva final:', m + 1

! restricciones de restricciones de volumen en transito en vias durante el intervalo final

! intervalo final
ifinal = NTINTR
j = 1
! para todas las cuencas
do cuenca = 1, nmcuen
!  para los embalses que estan en la cuenca
   do localidad_2 = 0, nmembc ( cuenca ) - 1
      embalse = dnemva ( cuenca ) + localidad_2
!     para las vias convergentes del embalse
      do localidad_3 = 0, nmviasd ( embalse ) - 1 
         via = DNVICO ( embalse )
!        si el embalse tiene vias convergentes
         if ( via .gt. 0 ) then
!           para los intervalos que interfieran con el tiempo de viaje del agua en la via
            inicio = ifinal + 1 - TiViAgu (via)
            if ( inicio .le. ifinal ) then
               do i = inicio , ifinal
!                 para todas las plantas que descargan sobre la vía
                  do localidad = dnphvi ( via ), dnphvi ( via ) + nphvi ( via ) - 1
                     planta = plantas_x_via ( localidad )
!                    para todas las unidades de la planta
                     do localidad_1 = 0, NOUN ( planta ) - 1
                        u = DNUNPH ( planta ) + localidad_1
!                       coeficientes de turbinado
                        aaMILP ( k ) = MValor
                        jcolMILP ( k ) = ITURB + u + (i-1)*NumUniHid - 1
                        k = k + 1
                     enddo
                  enddo
               enddo
               m = m + 1
!              lados derechos de las restriciones
               bMILP ( m ) = QTvfin (via)
!              sentidos de las restriciones
               sMILP ( m ) = 'E'
!              apuntador al siguiente renglon
               irowMILP ( m + 1 ) = k
               write ( leti, 200 ) NTINTR
               write ( 779,* ) m, ',', '"Volumen en transito final en la via: '//trim(NOMVIA (via))//' intervalo: '//trim(leti)//'"'
            endif
         endif
      enddo
      j = j + 1
   enddo
enddo

200 format (i3)

return
end



Subroutine LimGastoUniHidRest ( k, m )
! ---------------------------------------------------------------------
! Se forman las restricciones de limites superiores e inferiores de   *
! variables de gasto hidro en el problema de asignacion (MILP).       *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Octubre de 2018                                                     *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro

implicit none

INTEGER   i, k, m, u
character*3 leti

IRTUNH = m + 1

write ( 777,* ) 'Inicia restricciones de limites superiores de variables de gasto :', m + 1

! restricciones de limites superiores de variables de gasto hidro
do u = 1 , NumUniHid
   do i = 1 , NTINTR
!     coeficientes de gasto
      aaMILP ( k ) = 1.0
      jcolMILP ( k ) = ITURB + u + (i-1)*NumUniHid - 1
      k = k + 1
!     coeficientes de asignacion
      aaMILP ( k ) = -qmxgastodes(u,i)
      jcolMILP ( k ) = IAH + u + (i-1)*NumUniHid - 1
	  k = k + 1
	  m = m + 1
	  if ( m == 90821 ) then
	     continue
	  endif
!     lados derechos de las restriciones
      bMILP ( m )   = 0.0
!     sentidos de las restriciones
      sMILP ( m ) = 'L'
!     apuntador al siguiente renglon
      irowMILP ( m + 1 ) = k
      write ( leti, 200 ) i
      write ( 779,* ) m, ',', '"Limite superior de la variable de gasto en la unidad UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
   enddo
enddo

write ( 777,* ) 'Inicia restricciones de limites inferiores de variables de gasto :', m + 1

! restricciones de limites inferiores de variables de gasto hidro
do u = 1 , NumUniHid
   do i = 1 , NTINTR
!     coeficientes de gasto
      aaMILP ( k ) = 1.0
      jcolMILP ( k ) = ITURB + u + (i-1)*NumUniHid - 1
      k = k + 1
!     coeficientes de asignacion
      aaMILP ( k ) = -qmngastodes(u,i)
      jcolMILP ( k ) = IAH + u + (i-1)*NumUniHid - 1
	  k = k + 1
      m = m + 1
	  if ( m == 95342 ) then
	     continue
	  endif
      bMILP ( m ) = 0.0
!     sentidos de las restriciones
      sMILP ( m ) = 'G'
!     apuntador al siguiente renglon
      irowMILP ( m + 1 ) = k
      write ( leti, 200 ) i
      write ( 779,* ) m, ',', '"Limite inferior de la variable de gasto en la unidad UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
   enddo
enddo

200 format (i3)

return
end


Subroutine PotenciaGenHidroRest ( k, m, pasada )
! ---------------------------------------------------------------------
! Se forman las restricciones de potencia de generacion de las        *
! unidadesHidro en el problema de asignacion (MILP).                  *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! noviembre de 2018                                                   *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro

implicit none

INTEGER   i, j, k, m, u, cuenca, embalse, via, pasada

INTEGER   planta, localidad, localidad_1, localidad_2, localidad_3
character*3 leti

IRGUNH = m + 1

write ( 777,* ) 'Inicia restricciones de potencia de generacion de unidades hidro :', m + 1

! restricciones de potencia de generacion de unidades hidro
j = 1
! para todas las cuencas
do cuenca = 1, nmcuen
!  para los embalses que estan en la cuenca
   do localidad_2 = 0, nmembc ( cuenca ) - 1
      embalse = dnemva ( cuenca ) + localidad_2
!     para todos los intervalos
      do i = 1 , NTINTR
!        para las vias divergentes del embalse
         do localidad_3 = 0, nmviasd ( embalse ) - 1 
            via = DNVIOU ( embalse ) + localidad_3 
!           para todas las plantas que descargan sobre la vía
            do localidad = dnphvi ( via ), dnphvi ( via ) + nphvi ( via ) - 1
               planta = plantas_x_via ( localidad )
!              para todas las unidades de la planta
               do localidad_1 = 0, NOUN ( planta ) - 1
                  u = DNUNPH ( planta ) + localidad_1
!                 si la unidad esta disponible
                  if ( DispoUH ( u, i ) .eq. 1 ) then
                  
!                    coeficientes de generacion
                     aaMILP ( k ) = 1.0
                     jcolMILP ( k ) = IGH + u + (i-1)*NumUniHid - 1
                     k = k + 1
                  
!                    coeficientes de asignacion
!                     aaMILP ( k ) = -CIndGLH ( u, i )
!                     jcolMILP ( k ) = IAH + u + (i-1)*NumUniHid - 1
!                     k = k + 1

!                    coeficientes de turbinado
!                     aaMILP ( k ) = -CLinGLH ( u, i )
!                     jcolMILP ( k ) = ITURB + u + (i-1)*NumUniHid - 1
!                     k = k + 1

!                    coeficientes de asignacion
                     aaMILP ( k ) = -CIndGLHWQ ( u, i )
                     jcolMILP ( k ) = IAH + u + (i-1)*NumUniHid - 1
                     k = k + 1

!                    coeficientes de turbinado
                     aaMILP ( k ) = -CLinGLHQ ( u, i )
                     jcolMILP ( k ) = ITURB + u + (i-1)*NumUniHid - 1
                     k = k + 1
                     
!                    coeficientes de volumen util almacenado
!                     if ( i .gt. 1 .and. NTINTR .le. 25 ) then
!                        aaMILP ( k ) = -CLinGLHW ( u, i )
!                        jcolMILP ( k ) = IVOLU + embalse + (i-2)*Numembalses - 1
!                        k = k + 1
!                     endif
                     
                     m = m + 1
                     
                     bMILP ( m ) = 0.0

!                     if ( i .eq. 1 ) then
                        bMILP ( m ) = bMILP ( m ) + CLinGLHW ( u, i )*vmini(embalse)
!                     else
!                       si es un extendido
!                        if ( NTINTR .gt. 25 ) then
!                           bMILP ( m ) = bMILP ( m ) + CLinGLHW ( u, i )*vmini(embalse)
!                        else
!                           bMILP ( m ) = bMILP ( m ) + CLinGLHW ( u, i )
!                        endif
!                     endif
                     
!                    sentidos de las restriciones
                     sMILP ( m ) = 'L'
!                    apuntador al siguiente renglon
                     irowMILP ( m + 1 ) = k
                     write ( leti, 200 ) i
                     write ( 779,* ) m, ',', '"Potencia de generacion en la unidad UHI: '//trim(nombunih (u))//' intervalo: '//trim(leti)//'"'
                  endif
               enddo
            enddo
         enddo
      enddo
      j = j + 1
   enddo
enddo

200 format (i3)

return
end

    
Subroutine ActPotenciaGenHidro ( volumhR, cargahR, UniOnHid, a_tra_res, &
                                 gplah, qplah, aa4, pasada )
! ---------------------------------------------------------------------
! Se actualizan las restricciones de potencia de generacion de las    *
! unidadesHidro en el problema de asignacion (LP).                    *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio de 2019                                                       *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro, only:  nmcuen, nmembc, dnemva, nmviasd, DNVIOU, dnphvi, &
                         nphvi, plantas_x_via, NOUN, DNUNPH, CIndGLHWQ, &
                         CLinGLHQ, CLinGLHW, vmini, vol_util_res, qgasto, &
                         VolEmb, qmngastodes, qmxgastodes
use symtypes

!use cplex_ifaces, only: CPXchgrhs , CPXwriteprob, CPXchgcoeflist    ! Windows (comentarizar para Linux)

implicit none

integer                 CPXchgrhs , CPXwriteprob, CPXchgcoeflist    ! Linux (comentarizar para Windows)

INTEGER   i, ierror, ibanbit, j, m, n, u, cuenca, embalse, via, pasada

INTEGER   planta, localidad, localidad_1, localidad_2, localidad_3

real*8    volumhR ( maxuh, MAXINT ), cargahR ( maxuh, MAXINT ), a_tra_res ( maxint + 25, nmxvia ), &
          gplah ( nmxpla, maxint ), qplah ( nmxpla, maxint ), aa4 ( nmxemb, maxdia, 7 )
integer*4 UniOnHid ( nmxpla, maxint )

integer status, indices, rowlist, collist, numcoefs, cnt
REAL*8  values, vallist

CHARACTER fecha_Ej*19

DIMENSION   values  ( maxuh*maxint )
DIMENSION   indices ( 2*maxuh*maxint )
DIMENSION   rowlist ( 5*maxuh*maxint )
DIMENSION   collist ( 5*maxuh*maxint )
DIMENSION   vallist ( 5*maxuh*maxint )

m = IRGUNH - 1
n = IRTUNH - 1

numcoefs = 0
cnt = 0
data status  / 0 /

ibanbit = 1

!Resultado de evaluacion cuadratica del problema de asignacion     
!bandera 2 para que solo imprima a bitacora la evaluacion del problema de asignacion
!if ( pasada .eq. 2 ) then
!   solucion de volumenes turbinados (gasto)
    do u = 1 , NumuniHid
       do i = 1 , NTINTR
          qgasto ( u, i ) = xMILP ( ITURB + u + (i-1)*NumuniHid - 1 )
       enddo
    enddo
!   solucion de volumenes almacenados
    do embalse = 1, NumEmbalses
       do i = 1 , NTINTR
          VolEmb ( embalse, i ) = xMILP ( IVOLU + embalse + (i-1)*NumEmbalses - 1 )
       enddo
    enddo
!   guarda solucion de generacion de unidades hidro
    do u = 1, NumUniHid
!   para todos los intervalos de planeacion
        do i = 1, NTINTR
            GENUNH ( u, i ) = xMILP ( IGH + u + (i-1)*NumUniHid - 1 )
            if ( GENUNH ( u, i ) .lt. 0.0001 ) then
                GENUNH ( u, i ) = 0.0
            endif
        enddo
    enddo
    call res_agua_quad ( volumhR, cargahR, UniOnHid, a_tra_res, gplah, qplah, aa4, 2 )
!endif

!Recalcular coeficientes de generacion para despacho con las unidades asignadas del problema de asignacion
call Recalcula_Hidro ( 2 )

! se fija la asignacion de las unidades obtenida previamente
call FijaAsignacion

! restricciones de potencia de generacion de unidades hidro
j = 1
! para todas las cuencas
do cuenca = 1, nmcuen
!  para los embalses que estan en la cuenca
   do localidad_2 = 0, nmembc ( cuenca ) - 1
      embalse = dnemva ( cuenca ) + localidad_2
!     para todos los intervalos
      do i = 1 , NTINTR
!        para las vias divergentes del embalse
         do localidad_3 = 0, nmviasd ( embalse ) - 1 
            via = DNVIOU ( embalse ) + localidad_3 
!           para todas las plantas que descargan sobre la vía
            do localidad = dnphvi ( via ), dnphvi ( via ) + nphvi ( via ) - 1
               planta = plantas_x_via ( localidad )
!              para todas las unidades de la planta
               do localidad_1 = 0, NOUN ( planta ) - 1
                  u = DNUNPH ( planta ) + localidad_1
!                 si la unidad esta disponible
                  if ( DispoUH ( u, i ) .eq. 1 ) then
    !                 si la unidad esta asignada
                      if ( xMILP ( IAH + u + (i-1)*NumUniHid - 1  ) .gt. 0.5 ) then
                  
                         numcoefs = numcoefs + 1
    !                    coeficientes de asignacion
                         vallist ( numcoefs ) = 0.0 !-CIndGLHWQ ( u, i )
                         collist ( numcoefs ) = IAH + u + (i-1)*NumUniHid - 2                     
                         rowlist ( numcoefs ) = m

                         numcoefs = numcoefs + 1
    !                    coeficientes de turbinado
                         vallist ( numcoefs ) = -CLinGLHQ ( u, i )
                         collist ( numcoefs ) = ITURB + u + (i-1)*NumUniHid - 2                     
                         rowlist ( numcoefs ) = m
                     
    !                    coeficientes de volumen util almacenado
                         if ( i .gt. 1 ) then

                             numcoefs = numcoefs + 1
                             vallist ( numcoefs ) = -CLinGLHW ( u, i )
                             collist ( numcoefs ) = IVOLU + embalse + (i-2)*Numembalses - 2                     
                             rowlist ( numcoefs ) = m
                         
                         endif
                     
                         cnt = cnt + 1
                         if ( i .gt. 1 ) then
                             values ( cnt ) = 0.0
                         else
                             values ( cnt ) = CLinGLHW ( u, i )*vmini(embalse)
                         endif
                         values ( cnt ) = values ( cnt ) + CIndGLHWQ ( u, i )

                         indices ( cnt ) = m

                      endif
                      m = m + 1
                  endif
               enddo
            enddo
         enddo
      enddo
      j = j + 1
   enddo
enddo

! coeficientes de limites de variables de gasto hidro
do u = 1 , NumUniHid
   do i = 1 , NTINTR
!     si la unidad esta asignada
      if ( xMILP ( IAH + u + (i-1)*NumUniHid - 1  ) .gt. 0.9 ) then
         numcoefs = numcoefs + 1
!        coeficientes de maximo de gasto para la unidad
         vallist ( numcoefs ) = -qmxgastodes (u, i)
         collist ( numcoefs ) = IAH + u + (i-1)*NumUniHid - 2                     
         rowlist ( numcoefs ) = n 
         numcoefs = numcoefs + 1
!        coeficientes de minimo de gasto para la unidad
         vallist ( numcoefs ) = -qmngastodes (u, i)
         collist ( numcoefs ) = IAH + u + (i-1)*NumUniHid - 2                     
         rowlist ( numcoefs ) = n + NumUniHid*NTINTR
      endif
      n = n + 1
   enddo
enddo

! Se cambian coeficientes de restricciones
status = CPXchgcoeflist (enb, lpMILP, numcoefs, rowlist, collist, vallist )
if ( status .ne. 0 ) then
    write (*,*) ' Error al actualizar coeficientes de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

! se actualizan lados derechos de restricciones
status = CPXchgrhs (enb, lpMILP, cnt, indices, values)
if ( status .ne. 0) then
    write (*,*) ' Error al actualizar lados derechos de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

!Se escribe el modelo actuaizado MILP a un archivo
!status = CPXwriteprob (enb, lpMILP, 'RH1.lp', 'LP')
!status = CPXwriteprob (enb, lpMILP, 'RH.sav', 'SAV')

return
end


Subroutine ActPotenciaGenHidroL
! ---------------------------------------------------------------------
! Se actualizan las restricciones de potencia de generacion de las    *
! unidadesHidro en el problema de asignacion (MILP).                  *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! noviembre de 2018                                                   *
! ---------------------------------------------------------------------

use ParAUHE
use ProblemaAUHE
use ParAuHeHidro, only:  nmcuen, nmembc, dnemva, nmviasd, DNVIOU, dnphvi, &
                         nphvi, plantas_x_via, NOUN, DNUNPH, CIndGLHWQ, &
                         CLinGLHQ, CLinGLHW, vmini, vol_util_res, qgasto, &
                         VolEmb, qmngastodes, qmxgastodes
use symtypes

!use cplex_ifaces, only: CPXchgrhs , CPXwriteprob, CPXchgcoeflist, CPXchgbds    ! Windows (comentarizar para Linux)

implicit none

integer                 CPXchgrhs , CPXwriteprob, CPXchgcoeflist, CPXchgbds    ! Linux (comentarizar para Windows)

INTEGER   i, ierror, ibanbit, j, m, n, u, cuenca, embalse, via, modo

INTEGER   planta, localidad, localidad_1, localidad_2, localidad_3

integer status, indices, rowlist, collist, numcoefs, cnt
REAL*8  values, vallist, bd
character*1 lu

CHARACTER fecha_Ej*19

DIMENSION   bd      ( maxvarMILP )
DIMENSION   lu      ( maxvarMILP )
DIMENSION   indices ( maxvarMILP )
DIMENSION   values  ( maxuh*maxint )
DIMENSION   rowlist ( 5*maxuh*maxint )
DIMENSION   collist ( 5*maxuh*maxint )
DIMENSION   vallist ( 5*maxuh*maxint )

m = IRGUNH - 1
n = IRTUNH - 1

cnt = 0
indices = 0
bd = 0.0
lu = ' '
data status  / 0 /

ibanbit = 1

!Recalcular coeficientes de generacion para asignacion de unidades
call Recalcula_Hidro ( 1 )

cnt = 0    
! para todas las unidades de rango continuo
do u = 1, NumUniRC
!   para cada intervalo
    do i = 1, NTINTR
        cnt = cnt + 1
        indices ( cnt ) = IARC + u + (i-1)*NumUniRC - 2
        bd ( cnt ) = ubOMILP( IARC + u + (i-1)*NumUniRC - 1 )
        lu ( cnt ) = 'U'
        cnt = cnt + 1
        indices ( cnt ) = IARC + u + (i-1)*NumUniRC - 2
        bd ( cnt ) = lbOMILP( IARC + u + (i-1)*NumUniRC - 1 )
        lu ( cnt ) = 'L'
    enddo
enddo

! para todas las unidades de rango discontinuo
do u = 1, NumUniRD
!   para cada intervalo
    do i = 1, NTINTR
!       para todos los modos
        do modo = 1, NumModRD ( u )
            cnt = cnt + 1
            indices ( cnt ) = IARD + INIURDI ( u, i ) + modo - 2
            bd ( cnt ) = ubOMILP( IARD + INIURDI ( u, i ) + modo - 1 )
            lu ( cnt ) = 'U'
            cnt = cnt + 1
            indices ( cnt ) = IARD + INIURDI ( u, i ) + modo - 2
            bd ( cnt ) = lbOMILP( IARD + INIURDI ( u, i ) + modo - 1 )
            lu ( cnt ) = 'L'
        enddo
    enddo
enddo

! para todas las unidades hidro
do u = 1 , NumUniHid
!   Para todos los intervalos
    do i = 1 , NTINTR
        cnt = cnt + 1
        indices ( cnt ) = IAH + u + (i-1)*NumUniHid - 2 
        bd ( cnt ) = ubOMILP( IAH + u + (i-1)*NumUniHid - 1 )
        lu ( cnt ) = 'U'
        cnt = cnt + 1
        indices ( cnt ) = IAH + u + (i-1)*NumUniHid - 2 
        bd ( cnt ) = lbOMILP( IAH + u + (i-1)*NumUniHid - 1 )
        lu ( cnt ) = 'L'
    enddo
enddo

! para todas las unidades renovables
do u = 1 , NumUniRE
!   Para todos los intervalos
    do i = 1 , NTINTR
        cnt = cnt + 1
        indices ( cnt ) = IARE + u + (i-1)*NumUniRE - 2
        bd ( cnt ) = ubOMILP( IARE + u + (i-1)*NumUniRE - 1 )
        lu ( cnt ) = 'U'
        cnt = cnt + 1
        indices ( cnt ) = IARE + u + (i-1)*NumUniRE - 2
        bd ( cnt ) = lbOMILP( IARE + u + (i-1)*NumUniRE - 1 )
        lu ( cnt ) = 'L'
    enddo
enddo

! se actualizan cotas superiores de variables de asignacion
status = CPXchgbds (enb, lpMILP, cnt, indices, lu, bd)
if ( status .ne. 0) then
    write (*,*) ' Error al actualizar cotas de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

numcoefs = 0
cnt = 0
indices = 0
vallist = 0.0
collist = 0
rowlist = 0
! restricciones de potencia de generacion de unidades hidro
j = 1
! para todas las cuencas
do cuenca = 1, nmcuen
!  para los embalses que estan en la cuenca
   do localidad_2 = 0, nmembc ( cuenca ) - 1
      embalse = dnemva ( cuenca ) + localidad_2
!     para todos los intervalos
      do i = 1 , NTINTR
!        para las vias divergentes del embalse
         do localidad_3 = 0, nmviasd ( embalse ) - 1 
            via = DNVIOU ( embalse ) + localidad_3 
!           para todas las plantas que descargan sobre la vía
            do localidad = dnphvi ( via ), dnphvi ( via ) + nphvi ( via ) - 1
               planta = plantas_x_via ( localidad )
!              para todas las unidades de la planta
               do localidad_1 = 0, NOUN ( planta ) - 1
                  u = DNUNPH ( planta ) + localidad_1
!                 si la unidad esta disponible
                  if ( DispoUH ( u, i ) .eq. 1 ) then
                  
                         numcoefs = numcoefs + 1
    !                    coeficientes de asignacion
                         vallist ( numcoefs ) = -CIndGLHWQ ( u, i )
                         collist ( numcoefs ) = IAH + u + (i-1)*NumUniHid - 2                     
                         rowlist ( numcoefs ) = m

                         numcoefs = numcoefs + 1
    !                    coeficientes de turbinado
                         vallist ( numcoefs ) = -CLinGLHQ ( u, i )
                         collist ( numcoefs ) = ITURB + u + (i-1)*NumUniHid - 2                     
                         rowlist ( numcoefs ) = m
                     
    !                    coeficientes de volumen util almacenado
!                         if ( i .gt. 1 .and. NTINTR .le. 25 ) then
!                             numcoefs = numcoefs + 1
!                             vallist ( numcoefs ) = -CLinGLHW ( u, i )
!                             collist ( numcoefs ) = IVOLU + embalse + (i-2)*Numembalses - 2                     
!                             rowlist ( numcoefs ) = m
!                         endif
!                         if ( i .gt. 1 .and. NTINTR .gt. 25 ) then
                             numcoefs = numcoefs + 1
                             vallist ( numcoefs ) = 0.0
!                             vallist ( numcoefs ) = -CLinGLHW ( u, i )
                             collist ( numcoefs ) = IVOLU + embalse + (i-2)*Numembalses - 2
                             rowlist ( numcoefs ) = m
!                         endif

                         cnt = cnt + 1
                         values ( cnt ) = 0.0
                         if ( i .eq. 1 ) then
                             values ( cnt ) = values ( cnt ) + CLinGLHW ( u, i )*vmini(embalse)
                         else
!                             if ( NTINTR .le. 25 ) then
!                                 values ( cnt ) = values ( cnt ) + CLinGLHW ( u, i )
!                             else
                                 values ( cnt ) = values ( cnt ) + CLinGLHW ( u, i )*vol_util_res(embalse, i-1)  !vmini(embalse)
!                             endif
                         endif

                         indices ( cnt ) = m

                         m = m + 1
                  endif
               enddo
            enddo
         enddo
      enddo
      j = j + 1
   enddo
enddo

! coeficientes de limites de variables de gasto hidro
do u = 1 , NumUniHid
   do i = 1 , NTINTR
!     si la unidad esta asignada
      if ( xMILP ( IAH + u + (i-1)*NumUniHid - 1  ) .gt. 0.9 ) then
         numcoefs = numcoefs + 1
!        coeficientes de maximo de gasto para la unidad
         vallist ( numcoefs ) = -qmxgastodes (u, i)
         collist ( numcoefs ) = IAH + u + (i-1)*NumUniHid - 2                     
         rowlist ( numcoefs ) = n 
         numcoefs = numcoefs + 1
!        coeficientes de minimo de gasto para la unidad
         vallist ( numcoefs ) = -qmngastodes (u, i)
         collist ( numcoefs ) = IAH + u + (i-1)*NumUniHid - 2                     
         rowlist ( numcoefs ) = n + NumUniHid*NTINTR
      endif
      n = n + 1
   enddo
enddo

! Se cambian coeficientes de restricciones
status = CPXchgcoeflist (enb, lpMILP, numcoefs, rowlist, collist, vallist )
if ( status .ne. 0 ) then
    write (*,*) ' Error al actualizar coeficientes de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

! se actualizan lados derechos de restricciones
status = CPXchgrhs (enb, lpMILP, cnt, indices, values)
if ( status .ne. 0) then
    write (*,*) ' Error al actualizar lados derechos de MILP'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
end if

!Se escribe el modelo actuaizado MILP a un archivo
!status = CPXwriteprob (enb, lpMILP, 'RH2.lp', 'LP')
!status = CPXwriteprob (enb, lpMILP, 'RH.sav', 'SAV')

return
end
