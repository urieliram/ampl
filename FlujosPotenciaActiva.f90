
!**************************************************************
!*                                                            *
!* INSTITUTO DE INVESTIGACIONES ELECTRICAS                    *
!* GERENCIA DE ANALISIS DE REDES                              *
!* DIVISION DE SISTEMAS DE CONTROL                            *
!*                                                            *
!* OBJETIVO:                                                  *
!*                                                            *
!* - CALCULA FLUJOS Y SENSIBILIDADES DE PERDIDAS ANTE CAMBIOS *
!*   EN LAS GENERACIONES NODALES                              *
!*                                                            *
!*  > EL MODELO DE RED ES FLUJOS DE DC CON LAS PERDIDAS       *
!*    REPARTIDAS EN LOS NODOS EXTREMOS DE LAS RAMAS           *
!*                                                            *
!*   ( ATS )                              SEPTIEMBRE DE 1999  *
!*                                                            *
!**************************************************************

Subroutine fludcper ( nren, iu, ju, umat, dimat, teta, perdidas, per_nod, flu_pac )

use parglored, only: maxnod, maxgen, maxres, maxele, maxram

implicit none

integer iter, itemax

real*8 difp, tol

real*8 teta(maxnod) , per_nod(maxnod), flu_pac ( maxram )
 
real*8 perdidas, perdref

logical sigue, degenera

integer nren

integer iu,ju

real*8 umat, dimat

real*8 demtot, tolper


! Matriz factorizada
dimension iu ( maxres )
dimension ju ( maxele )
dimension umat ( maxele )
dimension dimat ( maxres )


 sigue = .true.
 degenera = .false.
 iter  = 0
 itemax = 20
 tol   = 0.0000000001
 tolper = 0.20 ! tolerancia maxima para evitar problemas de degeneracion (% de la demanda)

!------------------------------------------------------------
!* calcula flujos de dc sin perdidas como condicion inicial *
!------------------------------------------------------------
per_nod = 0.0; demtot = 0
call FlujosDC( nren, iu, ju, umat, dimat, teta, per_nod, demtot )

!----------------------------------------------
!* calcula perdidas para la condicion inicial *
!----------------------------------------------
call calper ( teta, perdidas, per_nod, flu_pac )
perdref = perdidas

do while ( iter .le. itemax .and. sigue )

!---------------------------------------------------
! calcula flujos considerando que las perdidas se *
! reparten en los nodos extremos de las ramas     *
!---------------------------------------------------
     call FlujosDC ( nren, iu, ju, umat, dimat, teta, per_nod, demtot )

!    ----------------------------------------------
!    * calcula perdidas para la condicion inicial *
!    ----------------------------------------------
     call calper ( teta, perdidas, per_nod, flu_pac )

     difp = abs( ( perdidas - perdref ) )
     if ( perdref .gt. 0.0 ) then
         sigue = ( difp/perdref .gt. tol )
     else
         sigue = ( difp .gt. tol )
     endif

     iter = iter + 1
     perdref = perdidas

     ! Verifica si degenera el problema
     if ( perdidas .gt. tolper*demtot ) then
         degenera = .true.; exit
     endif
     
enddo

! Si degenera el esquema estima las pérdidas con la primer solución de flujos
if ( degenera ) then
    per_nod = 0.0; demtot = 0
    call FlujosDC( nren, iu, ju, umat, dimat, teta, per_nod, demtot )
    call calper ( teta, perdidas, per_nod, flu_pac )
endif

return

end
!**************************************************************
!*                                                            *
!* INSTITUTO DE INVESTIGACIONES ELECTRICAS                    *
!* GERENCIA DE ANALISIS DE REDES                              *
!* DIVISION DE SISTEMAS ELÉCTRICOS                            *
!*                                                            *
!* OBJETIVO:                                                  *
!*                                                            *
!* - CALCULA FLUJOS DE CARGA SE USA CPLEX PARA LA SOLUCIÓN    *
!*   DEL SISTEMA DE ECUACIONES LINEALES                       *
!*                                                            *
!*  > EL MODELO DE RED ES FLUJOS DE DC CON LAS PERDIDAS       *
!*    REPARTIDAS EN LOS NODOS EXTREMOS DE LAS RAMAS           *
!*                                                            *
!*   ( ATS )                              Julio del 2015      *
!*                                                            *
!**************************************************************

Subroutine fludcpera (  teta, perdidas, per_nod, flu_pac )

Use ParAUHE, only: enb

use parglored, only: maxnod, maxgen, maxres, maxele, llp, maxram

implicit none

integer iter, itemax

real*8 difp, tol

real*8 teta(maxnod) , per_nod(maxnod), flu_pac ( maxram )
 
real*8 perdidas, perdref

logical sigue, degenera

real*8 demtot, tolper

 sigue = .true.
 degenera = .false.
 iter  = 0
 itemax = 10
 tol   = 0.00000001
 tolper = 0.20 ! tolerancia maxima para evitar problemas de degeneracion (% de la demanda)

!------------------------------------------------------------
!* calcula flujos de dc sin perdidas como condicion inicial *
!------------------------------------------------------------
per_nod = 0.0; demtot = 0
call FlujosPacIni ( teta, per_nod, demtot )

!----------------------------------------------
!* calcula perdidas para la condicion inicial *
!----------------------------------------------
call calper ( teta, perdidas, per_nod, flu_pac )
perdref = perdidas

do while ( iter .le. itemax .and. sigue )

     !---------------------------------------------------
     ! calcula flujos considerando que las perdidas se *
     ! reparten en los nodos extremos de las ramas     *
     !---------------------------------------------------
     call FlujosPac ( teta, per_nod, demtot  )
     
!    ----------------------------------------------
!    * calcula perdidas para la condicion inicial *
!    ----------------------------------------------
     call calper ( teta, perdidas, per_nod, flu_pac )

     difp = abs( ( perdidas - perdref ) )
     if ( perdref .gt. 0.0 ) then
         sigue = ( difp/perdref .gt. tol )
     else
         sigue = ( difp .gt. tol )
     endif

     iter = iter + 1
     perdref = perdidas

     ! Verifica si degenera el problema
     if ( perdidas .gt. tolper*demtot ) then
         degenera = .true.; exit
     endif
     
enddo

! Si degenera el esquema estima las pérdidas con la primer solución de flujos
if ( degenera ) then
    per_nod = 0.0; demtot = 0
    call FlujosPac ( teta, per_nod, demtot  )
    call calper ( teta, perdidas, per_nod, flu_pac )
endif

return

end
 
! -----------------------------------------------------------------------
! Subrutina que calcula flujos de potencia activa sin perdidas          *
!                                                                       *
! Instituto de investigaciones Electricas                               *
! Gerencia de analisis de redes                                         *
! Division de sistemas electricos                                       *
!                                                                       *
! Marzo del 2012                                                        *
! -----------------------------------------------------------------------
Subroutine FlujosDC ( nren, iu, ju, umat, dimat, teta, per_nod, demtot  )

Use ParGloRed, only: nmnod, inanod, inrnod, pcanod,  &
                     maxnod, maxres, maxele, pgenod

Implicit none

integer nren, i

integer iu,ju

real*8 umat, dimat

real*8 vlder, vsol, teta, per_nod

real*8 demtot

! Matriz factorizada
dimension iu ( maxres )
dimension ju ( maxele )
dimension umat ( maxele )
dimension dimat ( maxres )

! Lados derechos y vector solución
dimension vlder ( maxres )
dimension vsol  ( maxres )

dimension teta ( maxnod )
dimension per_nod ( maxnod )


! Incluye la demanda y generación en el lado derecho
vlder = 0.0; demtot = 0.0
do i = 1, nmnod
  vlder(i) = -pcanod(inanod(i)) - per_nod(inanod(i)) + pgenod(inanod(i))
  demtot = demtot + pcanod(inanod(i)) 
enddo

! * resuelve sistema de ecuaciones lineales *
call solfac ( nren, iu, ju, umat, dimat, vlder, vsol )

! Guarda solución de angulos de voltaje
teta = 0.0
do i = 1, nmnod
   teta(inanod(i)) = vsol(i)
enddo

return

end

    
!**************************************************************
!                                                            *
! INSTITUTO DE INVESTIGACIONES ELECTRICAS                    *
! GERENCIA DE ANALISIS DE REDES                              *
! DIVISION DE SISTEMAS ELECTRICOS                            *
!                                                            *
! OBJETIVO:                                                  *
!                                                            *
! - CALCULA PERDIDAD DEL SISTEMA ELECTRICO PARA UNA SOLUCION *
!   DADA                                                     *
!                                                            *
!   ( ATS )                              MARZO DEL 2015      *
!                                                            *
!* ************************************************************
     
Subroutine calper ( teta, perdidas, per_nod, flu_pac  )

Use ParAUHE, only: NumNodos, maxnod, maxram

Use ParGloRed, only: resram, suceptram, numram, oriram, desram, disram

implicit none

integer i, j, rama

real*8  perdidas, flujo

real*8  teta(maxnod), per_nod ( maxnod ), flu_pac ( maxram )

!-------------------------------------------------------------
! calcula las perdidas de potencia activa                   *
!-------------------------------------------------------------
perdidas = 0.0
per_nod = 0.0
flu_pac = 0.0

! Barre todas las ramas y asigna las perdidas en los extremos de las ramas
! No es necesario discriminar las ramas de subsistemas inactivos, 
! Puesto que los angulos tienen valor cero
do rama = 1, numram
  if ( disram(rama) .eq. 1 ) then
     i = oriram(rama); j = desram(rama)
     flujo = suceptram(rama)*(teta(i)-teta(j))
     per_nod(i) = per_nod(i) + 0.5*resram(rama)*flujo**2
     per_nod(j) = per_nod(j) + 0.5*resram(rama)*flujo**2
     perdidas = perdidas + resram(rama)*flujo**2
     flu_pac ( rama ) = flujo
  endif
enddo
   
return

end

! ---------------------------------------------------------------------
! Subrutina que prepara matriz de flujos de DC                        *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Junio del 2010                                                      *
! ---------------------------------------------------------------------
Subroutine PreMatFluDC ( ires, iren, jcol, matdc, diagdc )

Use ParAUHE, only: NumNodos

Use ParGloRed, only: maxres, maxele, maxramnod, disram, inrnod, incnod, suceptram, inbusm, siguem, nomnod, nomram

Implicit none

Integer i, j, k, m, rama, rama2, ind

Integer iele, ires

Integer lk, lk2

real*8 deltakmsum, deltakdiag

integer indnodm( 2*maxramnod )

real*8 bkm ( 2*maxramnod )

real*8 DiagDc ( maxres )

real*8 MatDc ( maxele )

integer iren ( maxres )

integer jcol ( maxele )


! Inicializa contador de restriciones
ires = 0

! Inicializa arreglos
iren= 0
jcol =0
matdc = 0.0
diagdc = 0.0

! Inicializa contador de elementos diferentes de cero
iele = 0

   do i = 1, NumNodos

     if ( inrnod(i) .ne. 0 ) then
       ires = ires + 1

!      * Genera los elementos debidos a ramas conectadas a nodo k *
       ind = 0
       k = i

!      Inicializa variables auxiliares de elementos k-k
       deltakdiag = 0.0

       lk = incnod(k)
       do while ( lk .ne. 0 )
         m = inbusm(lk)

!        Inicializa variables auxiliares de elementos k-m
         deltakmsum = 0.0

!        derivadas de inyecciones de potencia activa con respecto
!        al angulo de voltaje
         rama = (lk+1)/2
	     if ( disram(rama) .ge. 1 ) then
           deltakmsum = deltakmsum - suceptram(rama)
	       deltakdiag = deltakdiag + suceptram(rama)

         endif
!        ----------------------------------------------
!        * Incluye la ramas en paralelo a la rama k-m *
!        ----------------------------------------------
         lk2 = siguem(lk)
         do while ( lk2 .ne. 0 )
           if ( inbusm(lk2) .ne. m ) then
             lk2 = 0
           else 
!            derivadas de inyecciones de potencia activa con respecto
!            al angulo de voltaje
             rama2 = (lk2+1)/2
		     if ( disram(rama2) .ge. 1 ) then 
               deltakmsum = deltakmsum - suceptram(rama2)            
               deltakdiag = deltakdiag + suceptram(rama2) 

             endif		   
             
             lk = lk2
             lk2 = siguem(lk)
           endif
         enddo

!        Guarda elementos asociados a nodo m (angulo de voltaje).
         if ( inrnod(m) .ne. 0 .and. inrnod(m) .ge. inrnod(i)) then
           ind = ind + 1
           Indnodm(ind) = inrnod(m)
	       bkm(ind) = deltakmsum 
         endif


         lk = siguem(lk)
       enddo

!      Elemento de la diagonal k-esima (angulo de voltaje)
	   diagDc(ires) = deltakdiag 
       
       if ( deltakdiag .eq. 0 ) then
           write ( *, * ) " Hey ", i,  nomnod(i)
       endif

     
!      Ordena los elementos en orden ascendente de indice de columna
       if ( ind .gt. 0 ) then
         Call OrdVecEnt_r ( ind, Indnodm, bkm )
       endif

!      Guarda elementos en los arreglos de la matriz de coeficientes
	   iren(inrnod(i)) = iele + 1
       do j = 1, ind
	     if ( dabs ( bkm(j) ) .ne. 0.0 ) then
	       iele = iele + 1 
           MatDc (iele ) = bkm(j)
	       jcol(iele ) = indnodm(j)
	     endif
	   enddo

     endif

   enddo 

! Guarda informacion en renglon numres + 1
iren(ires+1) = iele + 1



return

end
    

!
!   *******************************************************
!   Preparar informacion del configurador de red electrica            
!   *******************************************************
    Subroutine ConfiguraRedElectrica

Use ParAUHE, only: maxnod, NumNodos
Use ParGloRed, only: maxeleybus, numisl, incnod, inbusm, siguem, islnod


Implicit none
 
!   -------------------------------------------------
!   * Inicializa arreglos de conectividad de la red *
!   -------------------------------------------------
    incnod = 0; inbusm = 0; siguem = 0

!   ------------------------------------------------------
!   * Forma arreglos de conectividad de la red electrica *
!   ------------------------------------------------------
    call conectred 

!   -----------------------------------------------------
!   * Configura la red electrica para identificar islas *
!   * electricas                                        *
!   -----------------------------------------------------
    call topolo ( numisl, Numnodos, islnod )

    return
    end
!
!   **************************************************************
!   *                                                            *
!   * INSTITUTO DE INVESTIGACIONES ELECTRICAS                    *
!   * GERENCIA DE ANALISIS DE REDES                              *
!   *                                                            *
!   * OBJETIVO:                                                  *
!   *                                                            *
!   * - GENERA INFORMACION DE CONECTIVIDAD DE RAMA K-M           *
!   *                                                            *
!   * ENTRADA:                                                   *
!   *                                                            *
!   * - BUSM:  Indice del nodo adyacente.                        *
!   *                                                            *
!   * SALIDA:                                                    *
!   *                                                            *
!   * - SIGUEM: Apunta a siguiente elemento de bus.              *
!   * - INBUSM: Nodos a los que esta conectado el bus.           *
!   *                                                            *
!   *                                                            *
!   * ENTRADA/SALIDA:                                            *
!   *                                                            *
!   * - PRXPOS: Proxima posicion.                                *
!   * - INBUSK: Inicio de informacion de nodo 'k'.               *
!   *                                                            *
!   *                                            ENERO DEL 2009  *
!   *                                                            *
!   **************************************************************
!
    Subroutine conectram ( Prxpos, Inbusk, Busm )
!
    Use ParAUHE, only: Bmensaje
    Use ParGloRed, only: maxeleybus, siguem, inbusm

    Implicit none

    integer ierror, banbit
!
    integer antes, guarda, rowk
!
    integer busm, inbusk, prxpos
!
    logical sigue

    character*7 letb
!
    sigue = .true.
    guarda = inbusk
    rowk  = inbusk
    antes = inbusk
    do while ( sigue )
      if ( rowk .eq. 0 ) then
!        ----------------------------------------
!        * No existen buses conectados al bus k *
!        ----------------------------------------
         prxpos = prxpos + 1
		 if ( prxpos .gt. maxeleybus ) go to 1000
         if ( antes .ne. rowk ) then
!           ----------------------------------------------
!           * Existen otros buses ya conectados al bus k *
!           * mueve la informacion para agregar el nuevo *
!           ----------------------------------------------
            siguem (prxpos) = siguem (antes)
            siguem (antes ) = prxpos
         else
!          ---------------------------------------------------------
!          * Actualiza el vector de conectividad co el apuntador hea
!          ---------------------------------------------------------
           siguem (prxpos) = guarda
           inbusk = prxpos
         endif
!        -------------------------------------
!        * Guarda el bus que esta conectado  *
!        -------------------------------------
         inbusm (prxpos) = busm
         sigue = .false.
      else
!        -----------------------------------------------
!        * Existen buses conectados al bus en cuestion *
!        -----------------------------------------------
         if ( busm .lt. inbusm(rowk) ) then
!           ------------------------------------------------------
!           * Actualiza nueva posicion disponible para encadenar *
!           ------------------------------------------------------
            prxpos = prxpos + 1
	        if ( prxpos .gt. maxeleybus ) go to 1000
            if ( antes .ne. rowk ) then
               siguem (prxpos) = siguem (antes)
               siguem (antes ) = prxpos
            else
               siguem (prxpos) = guarda
               inbusk = prxpos
            endif
            inbusm (prxpos) = busm
            sigue         = .false.
         else
!          -------------------------------------------------------
!          * Del encadenamiento ya existente saca nueva posicion *
!          * para recorrer la informacion de los vectores        *
!          *               siguem     y    inbusm                *
!          -------------------------------------------------------
           antes = rowk
           rowk  = siguem (rowk)
         endif
      endif

    enddo

go to 1500

1000 continue

     write ( letb, 100 )  maxeleybus

     BMensaje = '!!! FOP 62 El numero maximo de "Elementos de Ybus" es '//trim(letb)//' y es rebasado'&
                 //' este depende del numero de ramas y nodos electricos'	            
     ierror = 62
	 BanBit = 1
	 call  Mensaje_AuSeg ( ierror, BanBit, Bmensaje ) 


1500 continue


100 format ( I7 )

    return
    end
!
!   **************************************************************
!   *                                                            *
!   * INSTITUTO DE INVESTIGACIONES ELECTRICAS                    *
!   * GERENCIA DE ANALISIS DE REDES                              *
!   *                                                            *
!   * OBJETIVO:                                                  *
!   *                                                            *
!   * - RUTINA QUE FORMA LA CONECTIVIDAD DE LA RED ELECTRICA     *
!   *                                                            *
!   * ENTRADA:                                                   *
!   *                                                            *
!   * - NumRam:      Numero de ramas de la red electrica.         *
!   * - OriRam(i)   Nodo origen de rama i.                       *
!   * - DesRam(i)   Nodo destino de rama i.                      *
!   *                                                            *
!   * SALIDA:                                                    *
!   *                                                            *
!   *   INCNOD(I):   Inicia conectividad de nodo i.              *
!   *   SIGUEM(K):   Apunta a siguiente elemento conectado al    *
!   *                nodo i.                                     *
!   *   INBUSM(K):   Indice de nodo adyacente m.                 *
!   *                                                            *
!   *                                                            *
!   *                                             ENERO DEL 2009 *
!   *                                                            *
!   **************************************************************
!
    Subroutine Conectred 
!
!
Use ParGloRed, only: numram, oriram, incnod, desram, disnod
!
Implicit none
!
    integer i, prxpos
!
    prxpos = 0
    do i = 1, numram
!
!     -----------------------------------------------------
!     * Genera informacion de conectividad de nodo origen *
!     -----------------------------------------------------
      call conectram ( prxpos, incnod(oriram(i)), desram(i) )
!
!     ------------------------------------------------------
!     * Genera informacion de conectividad de nodo destino *
!     ------------------------------------------------------
      call conectram ( prxpos, incnod(desram(i)), oriram(i) )

    enddo
!
    return
    end
!
!   ***************************************************************
!   *                                                             *
!   * INSTITUTO DE INVESTIGACIONES ELECTRICAS                     *
!   * GERENCIA DE ANALISIS DE REDES                               *
!   *                                                             *
!   * OBJETIVO:                                                   *
!   *                                                             *
!   *   - CONFIGURA LA RED ELECTRICA PARA IDENTIFICAR SI EXISTEN  *
!   *     ISLAS ELECTRICAS                                        *
!   *                                                             *
!   *   + ENTRADAS:                                               *
!   *                                                             *
!   *     NBUS ------> Numero de buses o nodos.                   *
!   *     NumRam ----> Numero de ramas (lineas + transformadores).*
!   *     INCNOD(I)--> Inicia conectividad de nodo i.             *
!   *     SIGUEM(K)--> Apunta a siguiente elemento conectado al   *
!   *                  nodo i.                                    *
!   *     INBUSM(K)--> Indice de nodo adyacente m.                *
!   *                                                             *
!   *   + SALIDAS:                                                *
!   *                                                             *
!   *     NARBOL ------> Numero de islas.                         *
!   *     GRAFIC(I)----> Isla a la que pertenece el nodo i.       *
!   *                                                             *
!   *                                                             *
!   *                                                             *
!   *                                               MAYO DEL 2000 *
!   *                                                             *
!   ***************************************************************
!
    Subroutine Topolo ( narbol, nbus, grafic )

Use ParAUHE, only: maxnod
Use ParGloRed, only: incnod, numram, disram, siguem, inbusm

Implicit none

!
    integer i, m, num, otro, rowk
! 
    integer narbol, nbus
!
    integer valbus
!
    integer grafic
!
    logical sigue
!
    dimension grafic ( maxnod )
    dimension valbus ( maxnod )
!
!   -------------------------------------------------------------
!   * No existen graficas identificadas ,es decir hay 0 islas   *
!   -------------------------------------------------------------
    grafic = 0; valbus = 0
    
    narbol = 0
    otro  = 0
!
!   ---------------------------------------------------
!   * Para todos los buses identifica isla topologica *
!   ---------------------------------------------------
    do i = 1, nbus
       if ( i == 1230 ) then
          continue
       endif
!     ------------------------------------------------
!     * Pregunta si el bus tiene ya isla asignada    *
!     ------------------------------------------------
      if ( grafic(i) .eq. 0 ) then
!       -----------------------------------------------
!       * No esta identificado, asigna isla secuencial*
!       -----------------------------------------------
        sigue    = .true.
        narbol   = narbol+1
        grafic(i)= narbol
!       -----------------------------------------------
!       * Asocia isla a los buses conectados al bus i *
!       -----------------------------------------------
        rowk = incnod(i)
        do while ( sigue )
          if ( rowk .le.  0 ) then
!           -----------------------------------------
!           * Si ya terminaste de asignar isla para.*
!           -----------------------------------------
            if ( otro .eq. 0 ) then
              sigue = .false.
            else
!             --------------------------------------
!             * Todavia no he terminado de asignar *
!             --------------------------------------
              rowk  = incnod( valbus(otro) )
              otro  = otro-1
            endif
          else
!           ---------------------------------------------
!           * Apunta al numero de rama conectada al bus *
!           ---------------------------------------------
            num = (rowk+1)/2
            if ( num .gt. numram .or. disram(num) .eq. 0 ) then
!             ----------------------------------------------
!             * Rama invalida o desactivada obten otro bus *
!             ----------------------------------------------
              rowk = siguem(rowk)
            else
!             ------------------------------------------------
!             * Estoy al otro lado de la rama num, con bus m *
!             ------------------------------------------------
              m   = inbusm(rowk)
              if ( grafic(m) .eq. 0 ) then
!               --------------------------------------------
!               * Al bus m le asignare la isla del bus i   *
!               --------------------------------------------
                grafic(m)   = narbol
                otro        = otro+1
                valbus(otro) = m
              endif
              rowk = siguem(rowk)
            endif
          endif

        enddo

      endif

    enddo

    return
    end
    

!       **************************************************************
!       *                                                            *
!       * INSTITUTO DE INVESTIGACIONES ELECTRICAS                    *
!       * DEPARTAMENTO DE ANALISIS DE REDES                          *
!       * DIVISION DE SISTEMAS DE CONTROL                            *
!       *                                                            *
!       * OBJETIVO:                                                  *
!       *                                                            *
!       * - ESTA SUBRUTINA FACTORIZA UNA MATRIZ A DISPERSA,          *
!       *   SIMETRICA, DEFINIDA POSITIVA Y ADEMAS ESTA EMPAQUETADA.  *
!       *                                                            *
!       *                                              MAYO DE 1996  *
!       *          (A) = (UT)(D)(U)                                  *
!       *                                                            *
!       *    DEFINICION DE VARIABLES:                                *
!       *                                                            *
!       *      IA,JA   ESTRUCTURA DE LA MATRIZ (A) DADA EN FORMA     *
!       *              RR(U)U.                                       *
!       *      AN,ADMAT   ELEMENTOS DE LA MATRIZ (A) DADA EN FORMA   *
!       *              RR(U)U.                                       *
!       *      N       ORDEN DE LAS MATRICES (A) Y (U).              *
!       *      MAXRES     ORDEN MAXIMO DE LAS MATRICES (A) Y (U).       *
!       *      MAXELE     NUMERO MAXIMO DE ELEMENTOS NO NULOS DENTRO DE *
!       *              DE LA MATRIZ (A) EN FORMA RR(U)O.             *
!       *      MAXELE   NUMERO MAXIMO DE ELEMENTOS NO NULOS DENTRO DE *
!       *              DE LA MATRIZ (U) EN FORMA RR(U)O.             *
!       *      IUM,JUM   ESTRUCTURA DE LA MATRIZ RESULTANTE (U) EN   *
!       *              FORMA RR(U)O. (IUM) ES TAMBIEN USADO COMO     *
!       *              ARREGLO DE CAMBIO MULTIPLE.                   *
!       *      UNMAT      ELEMENTOS NO NULOS DE LA MATRIZ (U) DADA   *
!       *              EN FORMA RR(U)O.                              *
!       *      DIMAT      INVERSA DE LA MATRIZ DIAGONAL.             *
!       *                                                            *
!       *      FALFAC  INDICA FALLA EN LA FACTORIZACION              *
!       *                                                            *
!       *      DIAGCER ELEMENTO DIAGONAL CERO DURANTE EL PROCESO     *
!       *              DE FACTORIZACION                              *
!       *                                                            *
!       **************************************************************

        SUBROUTINE FACRIZ ( NE, IA, JA, AN, ADMAT, IUM, JUM, UNMAT, &
                            DIMAT, FALFAC, DIAGCER )

        Use ParGloRed

        IMPLICIT NONE

        INTEGER NE

        INTEGER DIAGCER

        INTEGER IA, JA

        INTEGER IUMT, JUT

        INTEGER IUM, JUM

        REAL*8 ADMAT, AN

        REAL*8 DIMAT, UNMAT

        LOGICAL FALFAC

!       ------------------------
!       * ARREGLOS DE ENTRADA: *
!       ------------------------
        DIMENSION         ADMAT(MAXRES)
        DIMENSION         AN(MAXELE)
        DIMENSION         IA(MAXRES)
        DIMENSION         JA(MAXELE)

!       ------------------------
!       * ARREGLOS DE TARBAJO: *
!       ------------------------
        DIMENSION         IUMT(MAXRES)
        DIMENSION         JUT(MAXELE)

!       -----------------------
!       * ARREGLOS DE SALIDA: *
!       -----------------------
        DIMENSION         IUM(MAXRES)
        DIMENSION         JUM(MAXELE)
        DIMENSION         UNMAT(MAXELE)
        DIMENSION         DIMAT(MAXRES)

        ! Inicializa arreglos de trabajo y de salida
        ium= 0; jum=0; unmat=0.0; dimat=0.0; iumt = 0; jut = 0
        
!       ---------------------------------------------------------
!       * SE REALIZA LA FACTORIZACION SIMBOLICA DE LA MATRIZ A. *
!       ---------------------------------------------------------
        CALL FACSIM ( NE, IA, JA, IUM, JUM )

!       ----------------------------------------------------------------
!       * PUESTO QUE LA ESTRUCTURA DE LA MATRIZ 'U' RESULTANTE DE LA   *
!       * FACTORIZACION SIMBOLICA ES RR(U)U ( NO-ORDENADA) Y LA        * 
!       * FACTORIZACION NUMERICA REQUIERE QUE SEA RR(U)O, SE REORDENA. *
!       ----------------------------------------------------------------
        CALL TRANSP ( NE, IUM, JUM, IUMT, JUT )

        CALL TRANSP ( NE, IUMT, JUT, IUM, JUM )

!       --------------------------------------------------------
!       * SE REALIZA LA FACTORIZACION NUMERICA DE LA MATRIZ A. *
!       --------------------------------------------------------
        CALL FACNUM ( NE, IA, JA, AN, ADMAT, IUM, JUM, UNMAT, DIMAT, &
                     FALFAC, DIAGCER )

        RETURN

        END
 

!       **************************************************************
!       *                                                            *
!       * INSTITUTO DE INVESTIGACIONES ELECTRICAS                    *
!       * DEPARTAMENTO DE ANALISIS DE REDES                          *
!       * DIVISION DE SISTEMAS DE CONTROL                            *
!       *                                                            *
!       * OBJETIVO:                                                  *
!       *                                                            *
!       * - ESTA SUBRUTINA REALIZA LA FACTORIZACION SIMBOLICA DE LA  *
!       *   MATRIZ A.                                                *
!       *                                                            *
!       *                                              MAYO DE 1996  *
!       **************************************************************

        SUBROUTINE FACSIM ( NE, IA, JA, IUM, JUM )

        Use ParGloRed

        IMPLICIT NONE

        INTEGER I, IAA, IAB, IUA, IUB, J, JJ, JP, JPI, JPP

        INTEGER L, LAST, LH, MIN, NH, NM

        INTEGER IP

        INTEGER NE

        INTEGER IA, JA

        INTEGER IUM, JUM

        LOGICAL CONV

!       ------------------------
!       * ARREGLOS DE ENTRADA: *
!       ------------------------
        DIMENSION         IA(MAXRES)
        DIMENSION         JA(MAXELE)

!       -----------------------
!       * ARREGLOS DE SALIDA: *
!       -----------------------
        DIMENSION         IUM(MAXRES)
        DIMENSION         JUM(MAXELE)

!       ------------------------
!       * ARREGLOS DE TRABAJO: *
!       ------------------------
       DIMENSION         IP(MAXRES)

        NM = NE - 1
        NH = NE + 1
!       -----------------------------------------------
!       * INCICIALIZA APUNTADOR A INICIO DE RENGLON Y *
!       * EL VECTOR DE LISTAS ENCADENADAS DE COLUMNAS *
!       -----------------------------------------------
        IUM = 0; IP = 0; JUM=0

        JP = 1
!       -----------------------------------------------------
!       * RECORRE LOS PRIMEROS N-1 RENGLONES DE LA MATRIZ A *
!       -----------------------------------------------------
        DO I = 1, NM
          JPI=JP
          JPP=NE+JP-I
          MIN=NH
          IAA=IA(I)
          IAB=IA(I+1)-1
!         --------------------------------------------------------------
!         * PARA LOS ELEMENTOS DIFERENTES DE CERO DEL RENGLON I GUARDA *
!         * EN JUM LOS ELEMENTOS DE JA ( LOCALIDADES YA EXISTENTES )   *
!         --------------------------------------------------------------
          IF ( IAB .GE. IAA ) THEN
            DO J = IAA, IAB
              JJ = JA(J)
              JUM(JP) = JJ
              JP = JP+1
              IF ( JJ .LT. MIN ) THEN
                MIN = JJ
              END IF
              IUM(JJ) = I
            ENDDO
          END IF
          LAST = IP(I)
!         -------------------------------------------------------------
!         * VERIFICA SI HAY ELEMENTOS DIFERENTES DE CERO EN COLUMNA I *
!         -------------------------------------------------------------
          IF ( LAST .NE. 0 ) THEN
            L = LAST
            L = IP(L)
            CONV = .FALSE.
!           -------------------------------------------------------
!           * RECORRE LOS ELEMENTOS DE RENGLON L QN MATRIZ U Y SE *
!           * GENERAN LAS LOCALIDADES NUEVAS EN CASO NECESARIO    *
!           -------------------------------------------------------
            DO WHILE ( .NOT. CONV )
              LH = L + 1
              IUA = IUM(L)
              IUB = IUM(LH) - 1
              IF ( LH .EQ. I ) THEN
                IUB = JPI - 1
              END IF
              IUM(I) = I
              DO J = IUA, IUB
                JJ = JUM(J)
                IF ( IUM(JJ) .NE. I ) THEN
                  JUM(JP) = JJ
                  JP = JP + 1
                  IUM(JJ) = I
                  IF ( JJ .LT. MIN ) THEN
                    MIN = JJ
                  END IF
                END IF
              ENDDO
              IF ( JP .NE. JPP ) THEN
                IF ( L .NE. LAST ) THEN
                  L = IP(L)
                ELSE
                  CONV = .TRUE.
                END IF
              ELSE
                CONV = .TRUE.
              ENDIF
            ENDDO
          END IF
!         -------------------------------------------------------
!         * SE GENERAN LAS LISTAS ENCADENADAS DE RENGLONES PARA *
!         * COLUMNAS I, L O I, MIN                              *
!         -------------------------------------------------------
          IF ( JP .EQ. JPP ) THEN
            L = IP(MIN)
            IF ( L .NE. 0 ) THEN
              IP(I) = IP(L)
              IP(L) = I
            ELSE
              IP(MIN) = I
              IP(I) = I
            END IF
          END IF
          IF ( JP .NE. JPP ) THEN
            IF ( MIN .NE. NH ) THEN
              L = IP(MIN)
              IF ( L .NE. 0 ) THEN
                IP(I) = IP(L)
                IP(L) = I
              ELSE
                IP(MIN) = I
                IP(I) = I
              END IF
            END IF
          ENDIF
          IUM(I) = JPI

        ENDDO
        IUM(NE) = JP
        IUM(NH) = JP

        RETURN

        END

!       **************************************************************
!       *                                                            *
!       * INSTITUTO DE INVESTIGACIONES ELECTRICAS                    *
!       * DEPARTAMENTO DE ANALISIS DE REDES                          *
!       * DIVISION DE SISTEMAS DE CONTROL                            *
!       *                                                            *
!       * OBJETIVO:                                                  *
!       *                                                            *
!       * - ESTA SUBRUTINA TRANSPONE LA MATRIZ 'U' REPRESENTADA      *
!       *   EN FORMA RR(U)U.                                         *
!       *                                                            *
!       *                                              MAYO DE 1996  *
!       **************************************************************

        SUBROUTINE TRANSP ( NE, IUM, JUM, IUMT, JUT )

        Use ParGlored

        IMPLICIT NONE

        INTEGER I, IAA, IAB, J, JP, K, NH

        INTEGER NE

        INTEGER IUM, JUM

        INTEGER IUMT, JUT

!       ------------------------
!       * ARREGLOS DE ENTRADA: *
!       ------------------------
        DIMENSION         IUM(MAXRES)
        DIMENSION         JUM(MAXELE)

!       -----------------------
!       * ARREGLOS DE SALIDA: *
!       -----------------------
        DIMENSION         IUMT(MAXRES)
        DIMENSION         JUT(MAXELE)


        NH = NE + 1
!       --------------------------------------------
!       * INICIALIZA APUNTADOR A INICIO DE RENGLON *
!       --------------------------------------------
        DO I = 2, NH
          IUMT(I) = 0
        ENDDO
!       -------------------------------------------------------------
!       * RECORRE TODOS LOS ELEMENTOS DIFERENTES DE CERO Y GURDA EN *
!       * EN LA POSICION J + 2 DE IUMT EL NUMERO DE ELEMENTOS DE    *
!       * COLUMNA J ORIGINAL                                        *
!       -------------------------------------------------------------
        IAB = IUM(NH) - 1
        DO I = 1, IAB
          J = JUM(I) + 2
          IF ( J .LE. NH ) THEN
            IUMT(J) = IUMT(J) + 1
          END IF
        ENDDO
        IUMT(1) = 1
        IUMT(2) = 1
!       -----------------------------------------------------------
!       * GUARDA EL ACUMULADO DEL NUMERO DE ELEMENTOS DEL RENGLON *
!       -----------------------------------------------------------
        IF ( NE .NE. 1 ) THEN
          DO I = 3, NH
            IUMT(I) = IUMT(I) + IUMT(I-1)
          ENDDO
        END IF
!       --------------------------------------------------------------
!       * GENERA APUNTADORES A INICIO DE RENGLON E INDICE DE COLUMNA *
!       --------------------------------------------------------------
        DO I = 1, NE
          IAA = IUM(I)
          IAB = IUM(I+1) - 1
          IF ( IAB .GE. IAA ) THEN
            DO JP = IAA, IAB
              J = JUM(JP) + 1
              K = IUMT(J)
              JUT(K) = I
              IUMT(J) = K + 1
            ENDDO
          END IF
        ENDDO

        RETURN

    END
    
!       **************************************************************
!       *                                                            *
!       * INSTITUTO DE INVESTIGACIONES ELECTRICAS                    *
!       * DEPARTAMENTO DE ANALISIS DE REDES                          *
!       * DIVISION DE SISTEMAS DE CONTROL                            *
!       *                                                            *
!       * OBJETIVO:                                                  *
!       *                                                            *
!       * - ESTA SUBRUTINA REALIZA LA FACTORIZACION NUMERICA DE LA   *
!       *   MATRIZ A.                                                *
!       *                                                            *
!       *                                              MAYO DE 1996  *
!       **************************************************************

        SUBROUTINE FACNUM ( NE, IA, JA, AN, ADMAT, IUM, JUM, UNMAT, &
                            DIMAT, FALFAC, DIAGCER )
        
        Use ParGloRed
        
        IMPLICIT NONE

        INTEGER I, IAA, IAB, IH, IUA, IUB, IUC, IUD

        INTEGER J, JJ, L, LAST, LN

        INTEGER NE

        INTEGER DIAGCER

        INTEGER IA, IUM, JA, JUM

        INTEGER IP, IUP

        REAL*8 UM

        REAL*8 ADMAT, AN

        REAL*8 DIMAT, UNMAT

        LOGICAL CONV, FALFAC

!       ------------------------
!       * ARREGLOS DE ENTRADA: *
!       ------------------------
        DIMENSION         ADMAT(MAXRES)
        DIMENSION         AN(MAXELE)
        DIMENSION         IA(MAXRES)
        DIMENSION         IUM(MAXRES)
        DIMENSION         JA(MAXELE)
        DIMENSION         JUM(MAXELE)

!       -----------------------
!       * ARREGLOS DE SALIDA: *
!       -----------------------
        DIMENSION         DIMAT(MAXRES)
        DIMENSION         UNMAT(MAXELE)

!       ------------------------
!       * ARREGLOS DE TRABAJO: *
!       ------------------------
        DIMENSION         IP(MAXRES)
        DIMENSION         IUP(MAXRES)


        FALFAC = .FALSE.
        DIAGCER = 0
        CONV = .FALSE.
!       -------------------------------------------
!       * INICIALIZA VECTOR DE LISTAS ENCADENADAS *
!       * DE RENGLONES POR COLUMNA                *
!       -------------------------------------------
        DO J = 1, NE
           IP(J) = 0
        ENDDO

        DO I = 1, NE
          IH = I + 1
          IUA = IUM(I)
          IUB = IUM(IH) - 1
!         -----------------------------------------------------------------------
!         * SI HAY ELEMENTOS DIFERENTES DE CERO FUERA DE LA DIAGONAL EN         *
!         * RENGLON I INICIALIZA UIJ = AIJ, SE GUARDA TEMPORALMENTE EN DIMAT(J) *
!         -----------------------------------------------------------------------
          IF ( IUB .GE. IUA ) THEN
            DO J = IUA, IUB
              DIMAT(JUM(J)) = 0.0
            ENDDO
            IAA = IA(I)
            IAB = IA(IH) - 1
            IF ( IAB .GE. IAA ) THEN
              DO J = IAA, IAB
                DIMAT(JA(J)) = AN(J)
              ENDDO
            END IF
          END IF
!         -------------------------------------------
!         * GUARDA EL ELEMENTO DIAGONAL DE MATRIZ A *
!         -------------------------------------------
          DIMAT(I) = ADMAT(I)
          LAST = IP(I)
          IF ( LAST .NE. 0 ) THEN
            LN = IP(LAST)
            L = LN
            CONV = .FALSE.
!           ----------------------------------------------------------
!           * RECORRE LOS RENGLONES ( L ) DE COLUMNA I Y ACUMULA LOS *
!           * TERMINOS DEBIDO A RENGLON L ( L < I )                  *
!           ----------------------------------------------------------
            DO WHILE ( .NOT. CONV )
              LN = IP(L)
              IUC = IUP(L)
              IUD = IUM(L+1) - 1
              UM = UNMAT(IUC)*DIMAT(L)
              DO J = IUC, IUD
                JJ = JUM(J)
                DIMAT(JJ) = DIMAT(JJ) - UNMAT(J)*UM
              ENDDO
              UNMAT(IUC) = UM
              IUP(L) = IUC + 1
!             ---------------------------------------------------
!             * FORMA LISTAS ENCADENADAS A RENGLON DE COLUMNA L *
!             ---------------------------------------------------
              IF ( IUC .NE. IUD ) THEN
                J = JUM(IUC+1)
                JJ = IP(J)
                IF ( JJ .NE. 0 ) THEN
                  IP(L) = IP(JJ)
                  IP(JJ) = L
                ELSE
                  IP(J) = L
                  IP(L) = L
                END IF
              END IF
              IF ( L .NE. LAST ) THEN
                L = LN
              ELSE
                CONV = .TRUE.
              END IF
            ENDDO
          END IF

!         -------------------------------------------------------------
!         * SI DIMAT(I) ES CERO SE DETIENE PROCESO, ELEMENTO DIAGONAL *
!         * NULO DURANTE LA FACTORIZACION.                            *
!         -------------------------------------------------------------
          IF ( ABS(DIMAT(I)) .LE. 1.0E-20 ) THEN
            FALFAC = .TRUE.
            WRITE ( *, * ) ' ELEMENTO CERO: ', I
            DIAGCER = I
            RETURN
          END IF
!         -------------------------------------------------------------
!         * FORMA LISTAS ENCADENADAS A RENGLON DE COLUMNA I Y GUARDA  *
!         * EN UNMAT, LOS FACTORES ACUMULADOS TEMPORALMENTE EN DIMAT  *
!         -------------------------------------------------------------
          DIMAT(I) = 1.0/DIMAT(I)
          IF ( IUB .GE. IUA ) THEN
            DO J = IUA, IUB
              UNMAT(J) = DIMAT(JUM(J))
            ENDDO
            J = JUM(IUA)
            JJ = IP(J)
            IF ( JJ .NE. 0 ) THEN
              IP(I) = IP(JJ)
              IP(JJ) = I
            ELSE
              IP(J) = I
              IP(I) = I
            END IF
          END IF
          IUP(I) = IUA

        ENDDO

        RETURN

        END

!       **************************************************************
!       *                                                            *
!       * INSTITUTO DE INVESTIGACIONES ELECTRICAS                    *
!       * DEPARTAMENTO DE ANALISIS DE REDES                          *
!       * DIVISION DE SISTEMAS DE CONTROL                            *
!       *                                                            *
!       * OBJETIVO:                                                  *
!       *                                                            *
!       * - ESTA SUBRUTINA SOLUCIONA UN SISTEMA FACTORIZADO          *
!       *   DEL TIPO:                                                *
!       *                                                            *
!       *      (UT)(D)(U)X=B                                         *
!       *                                                            *
!       *   EN DONDE LAS MATRIZ (U) ESTA EN FORMA RR(U)O.            *
!       *                                                            *
!       *   DEFINICION DE VARIABLES:                                 *
!       *                                                            *
!       *      N       ORDEN DE LA MATRIZ (U).                       *
!       *      MAXRES     ORDEN MAXIMO DE LA MATRIZ (U).                *
!       *      MAXELE   NUMERO MAXIMO DE ELEMENTOS NO NULOS DENTRO DE *
!       *              DE LA MATRIZ (U) EN FORMA RR(U)O.             *
!       *      IUM,JUM   ESTRUCTURA DE LA MATRIZ RESULTANTE (U) EN   *
!       *              FORMA RR(U)O.                                 *
!       *      UNMAT      ELEMENTOS NO NULOS DE LA MATRIZ (U) DADA   *
!       *              EN FORMA RR(U)O.                              *
!       *      DIMAT      INVERSA DE LA MATRIZ DIAGONAL.             *
!       *      B       VECTOR DE LADOS DERECHOS.                     *
!       *      X       VECTOR SOLUCION.                              *
!       *                                                            *
!       *                                              MAYO DE 1996  *
!       **************************************************************

        SUBROUTINE SOLFAC ( NE, IUM, JUM, UNMAT, DIMAT, B, X )

        Use ParGloRed

        IMPLICIT NONE

        INTEGER I, IAP, IUA, IUB, K, NM

        INTEGER NE

        INTEGER IUM, JUM

        REAL*8 XX

        REAL*8 B, DIMAT, UNMAT

        REAL*8 X

!       ------------------------
!       * ARREGLOS DE ENTRADA: *
!       ------------------------
        DIMENSION         B(MAXRES)
        DIMENSION         DIMAT(MAXRES)
        DIMENSION         IUM(MAXRES)
        DIMENSION         JUM(MAXELE)
        DIMENSION         UNMAT(MAXELE)

!       -----------------------
!       * ARREGLOS DE SALIDA: *
!       -----------------------
        DIMENSION X(MAXRES)

!       -------------------------------
!       * SUSTITUCION HACIA ADELANTE. *
!       -------------------------------
        NM = NE - 1
        DO I = 1, NE
          X(I) = B(I)
        ENDDO
        DO K = 1, NM
          IUA = IUM(K)
          IUB = IUM(K+1) - 1
          XX = X(K)
          IF ( IUB .GE. IUA ) THEN
            DO I = IUA, IUB
              IAP = JUM(I)
              X(IAP) = X(IAP) - UNMAT(I)*XX
            ENDDO
          END IF
          X(K) = XX*DIMAT(K)
        ENDDO

!       ----------------------------
!       * SUSTITUCION HACIA ATRAS. *
!       ----------------------------
        X(NE) = X(NE)*DIMAT(NE)
        K = NM
        DO WHILE ( K .GT. 0 ) 
          IUA = IUM(K)
          IUB = IUM(K+1) - 1
          IF ( IUB .GE. IUA ) THEN
            XX = X(K)
            DO I = IUA, IUB
              IAP = JUM(I)
              XX = XX - UNMAT(I)*X(IAP)
            ENDDO
            X(K) = XX
          END IF
          K = K - 1
        ENDDO

        RETURN

        END      