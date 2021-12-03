!  ********************************************************************
!  Subrutina que envia mensaje de error del programa de flujos optimos
!  ********************************************************************

Subroutine Mensaje_AuSeg ( ierror, ibanbit, BMensaje )

implicit none

integer ierror, ibanbit

integer ini, ifin, imax, idesp

integer ipos, ich, ibl, idir, msj, jpos

integer Inicia_Noblanco, Inicia_blanco

character BMensaje*255

character*42 NomtablaB

character*75 Letaux

character*1 blanco

logical sigue

!  Inicializa blanco
   Blanco = ' '

   NomtablaB = 'Bitacora Flujos Optimos de Potencia Activa'
   ini = 1
   idesp = 5 ! despazamiento del mensaje a partir del segundo renglon
   ifin = 255 ! tamaño de la cadena
   imax = 75  ! magnitud del renglon

!  ...................................................
!  Escribe en bitacora si el tipo de error lo permite
!  :::::::::::::::::::::::::::::::::::::::::::::::::::
   if ( ibanbit .eq. 1 .or. ibanbit .eq. 2 ) then
     Call Mensaje_Bitacora ( ierror, BMensaje )
     call gebita2( BMensaje )
   endif

   if ( ibanbit .ne. 2 ) then
    !  ..............................................................
    !  Escribe en pantalla en uno o varios renglones de acuerdo a la
    !  magnitud del mensaje
    !  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

       sigue = .true.
       msj = 0
       do while ( sigue .and. msj .le. 5 )
         msj = msj + 1
         ipos = min ( (ini + imax - 1 ) , ifin )
	     idir = 1
         ich = Inicia_Noblanco ( ini, ipos, idir, BMensaje )
         if ( ich .ne. 0 ) then
           if ( BMensaje(ipos:ipos) .eq. blanco ) then
             call AcomMensaje ( msj, idesp, ini, ipos, BMensaje, Letaux ) 
             call EscriPanta ( Letaux )

             ini = ipos + 1
           else
             jpos = min ( ipos+1, ifin)
             if ( BMensaje(jpos:jpos) .eq. blanco ) then
               call AcomMensaje ( msj, idesp, ini, ipos, BMensaje, Letaux ) 
               call EscriPanta ( Letaux )

               ini = ipos + 2
             else
    !          Encuentra el primer blanco atras de ipos
               idir = - 1
               ibl = Inicia_blanco ( ini, ipos, idir, BMensaje ) 
               call AcomMensaje ( msj, idesp, ini, ibl, BMensaje, Letaux ) 
               call EscriPanta ( Letaux  )

		       ini = ibl + 1
             endif
	       endif
         else
           sigue = .false.
	     endif
         if ( msj .eq. 1 ) then
           imax = imax - idesp
         endif
       enddo
   end if
   
!  .................................
!  Verifica si el error es de falla
!  :::::::::::::::::::::::::::::::::
if ( ierror .ge. 50 ) then
    BMensaje = '             '
    Call EscriPanta ( BMensaje )

    BMensaje = '      Error Fatal de función para escribir mensajes'
    Call EscriPanta ( BMensaje )
    write ( *, * ) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
endif

Return
end

!  ***********************************************************************
!  Acomoda mensaje de error dependiendo si es el primer renglon o uno de 
!  los siguientes renglones
!  ***********************************************************************

Subroutine AcomMensaje ( msj, idesp, ini, ifin, BMensaje, Letaux )

implicit none

integer i, j

integer msj, idesp, ini, ifin

Character BMensaje*255

Character*(*) Letaux


! Llena de blancos el letrero final
  Letaux = ' '

! Si es el primer mensaje, no hay despalzamiento de inicio de mensaje
  if ( msj .eq. 1 ) then
    j = 0
    do i = ini, ifin
      j = j + 1
	  letaux(j:j) = BMensaje(i:i)
	enddo
  else
! Si no es el primer mensaje, hay un despalzamiento de inicio de mensaje
    j = idesp
    do i = ini, ifin
      j = j + 1
	  letaux(j:j) = BMensaje(i:i)
	enddo
  endif


Return
end


!***********************************************************************
! Encuentra el primer "blanco" en la direccion incremental ( ndir = 1)
! direccion decreciente ( ndir ne 1 )
!***********************************************************************
Integer Function Inicia_blanco ( ini, ifin, idir, Letrero )

Implicit none

integer ini, ifin, idir

integer j, ind

character Letrero*(*), blanco*1

   blanco = ' '

   ind = 0
!  Busca el primer blanco en la direccion incremental
   if ( idir .eq. 1 ) then
     j = ini 
     do while ( j .le. ifin .and. ind .eq. 0 )
       if ( Letrero(j:j) .eq. blanco ) then
	     ind = j
	   endif
	   j = j + 1
     enddo
   else
!    Busca el primer blanco en la direccion decreciente
     j = ifin
     do while ( j .ge. ini .and. ind .eq. 0 )
       if ( Letrero(j:j) .eq. blanco ) then
	     ind = j
	   endif
	   j = j - 1
     enddo
   endif

   Inicia_blanco = ind

Return

End Function

!***********************************************************************
! Encuentra el primer Noblanco en la direccion incremental ( ndir = 1)
! direccion decreciente ( ndir ne 1 )
!***********************************************************************
Integer Function Inicia_Noblanco ( ini, ifin, idir, Letrero )

Implicit none

integer ini, ifin, idir

integer j, ind

character Letrero*(*), blanco*1

   blanco = ' '

   ind = 0
   if ( idir .eq. 1 ) then
     j = ini 
     do while ( j .le. ifin .and. ind .eq. 0 )
       if ( Letrero(j:j) .ne. blanco ) then
	     ind = j
	   endif
	   j = j + 1
     enddo
   else
     j = ifin 
     do while ( j .ge. ini .and. ind .eq. 0 )
       if ( Letrero(j:j) .ne. blanco ) then
	     ind = j
	   endif
	   j = j - 1
     enddo
   endif

   Inicia_Noblanco = ind

Return

End Function

! **********************************
! >> Escribe mensaje a pantalla >>
! **********************************
Subroutine EscriPanta ( BMensaje )

Implicit none

!Character*(*) BMensaje*255

Character*(*) BMensaje

   write ( *, * ) Trim ( BMensaje )

Return
End

!  ********************************************************************
!  Subrutina que envia mensaje de error del programa de flujos optimos
!  a bitacora
!  ********************************************************************

Subroutine Mensaje_Bitacora ( ierror, Bmensaje )

implicit none

integer ierror

integer ini, ifin, imax, idesp

integer ipos, ich, ibl, idir, msj, jpos

integer Inicia_Noblanco, Inicia_blanco

character BMensaje*255

character*42 NomtablaB

character*85 Letaux

character*1 blanco

logical sigue

!  Inicializa blanco
   Blanco = ' '

   NomtablaB = 'Bitacora Flujos Optimos de Potencia Activa'
   ini = 1
   if ( ierror .ge. 50 ) then
     idesp = 4 ! despazamiento del mensaje a partir del segundo renglon
   else
     idesp = 4 ! despazamiento del mensaje a partir del segundo renglon
   endif
   ifin = 255 ! tamaño de la cadena
   imax = 75  ! magnitud del renglon

!  ..............................................................
!  Escribe en bitacora en uno o varios renglones de acuerdo a la
!  magnitud del mensaje
!  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

   sigue = .true.
   msj = 0
   do while ( sigue .and. msj .le. 5 )
     msj = msj + 1
     ipos = min ( (ini + imax - 1 ) , ifin )
	 idir = 1
     ich = Inicia_Noblanco ( ini, ipos, idir, BMensaje )
     if ( ich .ne. 0 ) then
       if ( BMensaje(ipos:ipos) .eq. blanco ) then
         call AcomMensaje ( msj, idesp, ini, ipos, BMensaje, Letaux ) 
         call EscribeBitacora ( Letaux )

         ini = ipos + 1
       else
         jpos = min (ipos+1, ifin)
         if ( BMensaje(jpos:jpos) .eq. blanco ) then
           call AcomMensaje ( msj, idesp, ini, ipos, BMensaje, Letaux ) 
           call EscribeBitacora ( Letaux )

           ini = ipos + 2
         else
!          Encuentra el primer blanco atras de ipos
           idir = - 1
           ibl = Inicia_blanco ( ini, ipos, idir, BMensaje ) 
           call AcomMensaje ( msj, idesp, ini, ibl, BMensaje, Letaux ) 
           call EscribeBitacora ( Letaux )

		   ini = ibl + 1
         endif
	   endif
     else
       sigue = .false.
	 endif
     if ( msj .eq. 1 ) then
       imax = imax - idesp
     endif
   enddo


Return
end

! **********************************
! >> Escribe mensaje a bitacora  >>
! **********************************
Subroutine EscribeBitacora ( BMensaje )

Implicit none

Character*(*) BMensaje

   write ( 444, * ) Trim ( BMensaje )

Return
End

!***********************************************************************
! Encuentra el primer "char" en la direccion incremental ( idir = 1)
! direccion decreciente ( idir ne 1 )
!***********************************************************************
Integer Function Inicia_char ( ini, ifin, idir, Char, Letrero )

Implicit none

integer ini, ifin, idir

integer j, ind

character Letrero*(*), Char*1

   ind = 0
!  Busca el primer char en la direccion incremental
   if ( idir .eq. 1 ) then
     j = ini 
     do while ( j .le. ifin .and. ind .eq. 0 )
       if ( Letrero(j:j) .eq. char ) then
	     ind = j
	   endif
	   j = j + 1
     enddo
   else
!    Busca el primer char en la direccion decreciente
     j = ifin
     do while ( j .ge. ini .and. ind .eq. 0 )
       if ( Letrero(j:j) .eq. char ) then
	     ind = j
	   endif
	   j = j - 1
     enddo
   endif

   Inicia_char = ind

Return

End Function

!  ----------------------------------------------------
!  * Salta "j" campos del registro en cadena "letaux" *
!  * donde el separador es la coma(,)                 *
!  ----------------------------------------------------
Subroutine SaltaCapos ( j, ipos, letaux )

Implicit None

   Integer ini, ipos, idir, ifin

   Integer i, j

   Integer Inicia_char

   Character letaux*(*)

   character*1 let

   idir = 1
   ifin = 250


   do i = 1, j
!       * Desecha la informacion del siguiente campo VS
  	    let = ","
        ini = ipos + 1
        ipos = Inicia_char ( ini, ifin, idir, let, letaux )
   enddo

return

end


! *************************************************************
! Subrutina que ordena en forma ascendente un vector entero
! *************************************************************
  subroutine OrdVecEnt ( nele, VecEnt, VecRea )
   
!!Use IMSLF90
!!Use NUMERICAL_LIBRARIES

Implicit none

    integer nele, i, k
 
    integer VecEnt ( nele )

    integer VecSal ( nele )

    integer Orden ( nele )

    real*8 VecRea ( nele )

    real*8 VecAux ( nele )


! Se inicializa el vector del orden
    Do k = 1 , nele
      Orden ( k ) = k
	  Vecaux ( k ) = VecRea ( k )
    End do

!!    Call  SVIGP ( nele , VecEnt , VecSal, Orden )
    Call  SVIGP_A ( nele , VecEnt , VecSal, Orden )

!   Se acomodan los datos del vector real en el orden final

    do i = 1, nele
      VecEnt(i) = VecSal(i)
      Vecrea(i) = Vecaux( orden(i) )
	enddo

Return

end


! *************************************************************
! Subrutina que ordena en forma ascendente un vector entero
! y compacta cuando se repiten indices
! *************************************************************
  subroutine OrdyComVecEnt ( nele, VecEnt, VecRea )
   
!!Use IMSLF90

!!Use NUMERICAL_LIBRARIES

Implicit none

    integer nele, i, k, ne
 
    integer VecEnt ( nele  )

	integer VecBan ( nele  )

    real*8 VecRea ( nele   )

    real*8 VecAux ( nele   )
    
    logical ultimo


!   Ordena en orden ascendente
    call OrdVecEnt ( nele, VecEnt, VecRea )

!   Se compacta informacion cuando hay indices repetidos
    k = 0
	i = 1
	VecBan = 1
	VecAux = VecRea
    do while ( i .le. nele - 1 )
	  k = i + 1; ultimo = .false.
	  do while ( VecEnt(i) .eq. VecEnt(k) .and. .not. ultimo )
          VecAux(i) = VecAux(i) + VecAux(k)
		  VecBan(k) = 0
		  k = k + 1
		  if ( k .eq. nele + 1 ) then
		     ultimo = .true.; k = nele
		  endif
	  enddo
	  i = max ( k, i + 1 )
	enddo

!  Se guarda en arreglos de salida informacion compactada
   ne = 0
   do i = 1, nele
      if ( VecBan(i) .ne. 0 ) then
	    ne = ne + 1
        Vecrea(ne) = VecAux(i)
		VecEnt(ne) = VecEnt(i)
	  endif
   enddo
    
!  Limpia arreglos sin uso
   do i = ne+1, nele
     Vecrea(i) = 0.0
	 Vecent(i) = 0
   enddo


!  Actualiza numero de elementos
   nele = ne

   
Return

end

! Subrutina que ordena en forma ascendente un vector de enteros

Subroutine SVIGP_A ( nele , VecEnt , VecSal, Orden )

Implicit none

Integer nele

Integer i, pos

Integer VecEnt ( nele )

Integer VecSal ( nele )

Integer Orden ( nele )

Integer vector ( nele )

! guarda vector de entrada en vector de trabajo
vector = vecent

do i = 1, nele

  call menor ( nele, vector, pos )
  vecsal (i) =  vector ( pos )
  orden  (i) = pos
  vector(pos) = 9999999

enddo


return

end

! Subrutina que encuentra el menor entre una lista de enteros

Subroutine menor ( nele, set, loc ) 

Implicit none

integer nele

integer i, loc, small

integer set ( nele )

loc = 1
small = set(1)

do i = 2, nele
  if (abs(set(i)) .le. small) then
    small = set(i)
    loc = i
  endif
enddo

return

end

    
SUBROUTINE FechaEjecucion ( Fecha_Ej )
!******************************************************************************
!  Objetivo:                                                                  *
!      		Generar Fecha y hora de la ejecuacion                             *
!                                                                             *
!  Registro de revisiones:                                                    *
!                                                                             *
!      Fecha         Programador       Descripcion de codigo                  *
!      ====      =================     =====================                  *
!    30/11/11   Isaías Guillén Moya       Codigo original                    *
!******************************************************************************
IMPLICIT NONE
!      
CHARACTER (LEN = 12 ) REAL_CLOCK (3)
CHARACTER*4  ANO
CHARACTER*2  MES
CHARACTER*2  DIA
CHARACTER*2  HOR
CHARACTER*2  MIN
CHARACTER*2  SEG
CHARACTER*3  MSE
CHARACTER*10  AnoMesDia
CHARACTER*8  Hora
CHARACTER*19  Fecha_ej
INTEGER DATE_TIME (8)
!
CALL DATE_AND_TIME ( REAL_CLOCK(1),REAL_CLOCK(2),REAL_CLOCK(3),DATE_TIME)
!	  
WRITE(ANO,1350)DATE_TIME(1) ! ANO
WRITE(MES,1351)DATE_TIME(2) ! MES
WRITE(DIA,1351)DATE_TIME(3) ! DIA
WRITE(HOR,1351)DATE_TIME(5) ! HORA
WRITE(MIN,1351)DATE_TIME(6) ! MINUTOS
WRITE(SEG,1351)DATE_TIME(7) ! SEGUNDOS
WRITE(MSE,1352)DATE_TIME(8) ! MILESIMAS DE SEGUNDO
!
AnoMesDia = ANO //'/'//MES//'/'//DIA!
Hora = HOR //':'//MIN//':'//SEG !//!':'//MSE
Fecha_Ej = AnoMesDia//' '//Hora !//' '//' '
!     
1350  FORMAT ( I4.4 )
1351  FORMAT ( I2.2 )
1352  FORMAT ( I3.3 )
END SUBROUTINE FechaEjecucion

! Subroutina que para el proceso    
Subroutine ParaProceso ( letent )

use ParAUHE, only: NomEjecu

implicit none

integer ibanbit, ierror

character BMensaje*255, letent*255

character fecha_Ej*19

ibanbit = 1
ierror = 0
Call FechaEjecucion (fecha_Ej)
bmensaje = fecha_Ej//' '//NomEjecu// '001 '//trim(letent)
Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
write(*,*) '1, 1'
write(998,*) '1, 1'
!   Se ecribe resultado de semaforos
call EscSemaforosError 
letent = trim ( BMensaje )
stop

return

end    
    
!***********************************************************************
! Elimina blancos de una cadena al principio y al final
!***********************************************************************
Subroutine Elimina_blancos ( letrero, dim )

Implicit none

integer i, k, dim

character*dim Letrero, letaux

character*1 blanco

! Guarda los no blancos en un arreglo auxiliar
blanco = ' '
letaux = ' '
k = 0
do i = 1, dim
   if ( letrero(i:i) .ne. blanco ) then
      k = k + 1
      letaux(k:k) = letrero(i:i)
   endif
enddo

! Guarda arreglo auxiliar en letrero de entrada/salida
letrero = ' '
do i = 1, k
     letrero(i:i) = letaux(i:i)
enddo
   
Return
end
    
! Subroutina que para el proceso    
Subroutine EnviaMensajeError ( letent )

use ParAUHE, only: NomEjecu

implicit none

integer ibanbit, ierror

character BMensaje*255, letent*255

character fecha_Ej*19

ibanbit = 1
ierror = 0
Call FechaEjecucion (fecha_Ej)
bmensaje = fecha_Ej//' '//NomEjecu// '001 '//trim(letent)
Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )

return

end