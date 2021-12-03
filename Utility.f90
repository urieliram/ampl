! ***************************************************
! Quita comas
! ***************************************************
Subroutine QuitaComas ( letrero, dim )

Implicit none

integer dim 

character*dim  letrero, letaux

character*1 comilla

integer i, icuenta, imax

letaux = " "
icuenta = 0
comilla = '"'
imax = len_trim(letrero)
do i = 1, imax
    if ( letrero(i:i) .ne. comilla .and. letrero(i:i) .ne. " ") then
        icuenta = icuenta + 1
        letaux(icuenta:icuenta) = letrero(i:i)
    endif
enddo

letrero = trim(letaux)
    
Return
    
end 

    
! ***************************************************
! calcula la conductancia y suceptancia de una línea
! ***************************************************
Subroutine CalculaAdmitancia ( r, x, g, b )

Use ParAUHE

Implicit none

real r, x, aux, g, b

aux = r*r + x*x
if ( abs(aux) .eq. 0.0 ) x = 0.0001
aux = r*r + x*x
g = r/aux
b = x/aux
g = g*(100/base)
b = b*(100/base)

Return
    
end 
    
! *************************************************************
! Subrutina que ordena en forma ascendente un vector entero
!    VecEnt: Vector de entrada/salida a ordenar tipo entero
!    VecRea: Vector acompañante tipo real
! *************************************************************
subroutine OrdVecEnt_r ( nele, VecEnt, VecRea )
   
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
!    VecEnt: Vector de entrada a ordenar
!    Vectora, Vectorb : Vectores acompañantes tipo entero
! *************************************************************
subroutine OrdVecEnt_ii ( nele, VecEnt, Vectora, Vectorb )
   
Implicit none

    integer nele, i, k
 
    integer VecEnt ( nele )

    integer VecSal ( nele )

    integer Orden ( nele )

    integer Vectora ( nele )
    
    integer Vectorb ( nele )

    integer VecAuxa ( nele )

    integer VecAuxb ( nele )

! Se inicializa el vector del orden
    Do k = 1 , nele
      Orden ( k ) = k
	  Vecauxa ( k ) = Vectora ( k )
      Vecauxb ( k ) = Vectorb ( k )

    End do

    Call  SVIGP_A ( nele , VecEnt , VecSal, Orden )

!   Se acomodan los datos del vector real en el orden final

    do i = 1, nele
      VecEnt(i) = VecSal(i)
      Vectora(i) = Vecauxa( orden(i) )
      Vectorb(i) = Vecauxb( orden(i) )
	enddo

Return

end
    
! **********************************************
! Busca indice del nodo en lista de subsistemas
! **********************************************
integer Function BuscaSubsistema ( letsis )

Use ParAUHE, only: numsis, nomsis

Implicit none

integer i, iban

character(*) letsis

   iban = 0
   i = 1
   do while ( i .le. numsis .and. iban .eq. 0 )
       if ( trim(letsis) .eq. trim(nomsis(i)) ) then
          iban = i
       endif
       i = i + 1
    enddo

    BuscaSubsistema = iban
    
Return
    
END Function
    
! ****************************************
! Busca indice del nodo en lista de nodos
! ****************************************
integer Function BuscaNodo ( letnod )

Use ParAUHE, only: NumNodos
Use ParGloRed, only: nomnod

Implicit none

integer i, iban

character(*) letnod

   iban = 0
   i = 1
   do while ( i .le. Numnodos .and. iban .eq. 0 )
       if ( trim(letnod) .eq. trim(nomnod(i)) ) then
          iban = i
       endif
       i = i + 1
    enddo

    BuscaNodo = iban
    
    Return
    
END Function
    
! ************************************************************
! Busca indice del grupo de rama en lista de grupos de ramas
! ************************************************************
integer Function BuscaGrupoRamas ( letgru )

Use ParGloRed, only: numgruram, nomgruram

Implicit none

integer i, iban

character(*) letgru

   iban = 0
   i = 1
   do while ( i .le. numgruram .and. iban .eq. 0 )
       if ( trim(letgru) .eq. trim(nomgruram(i)) ) then
          iban = i
       endif
       i = i + 1
    enddo

    BuscaGrupoRamas = iban
    
    Return
    
END Function
    
! ******************************************
! Busca indice de la rama en lista de ramas
! *******************************************
integer Function BuscaRama ( letram )

Use ParGloRed, only: numram, nomram

Implicit none

integer i, iban

character(*) letram

   iban = 0
   i = 1
   do while ( i .le. numram .and. iban .eq. 0 )
       if ( trim(letram) .eq. trim(nomram(i)) ) then
          iban = i
       endif
       i = i + 1
   enddo

   BuscaRama = iban
    
   Return
    
    END Function 

! ******************************************
! Convierte tipo de nodo
! *******************************************
Subroutine ConvierteNodo ( tipo, ltip )

Use ParGloRed, only: 

Implicit none

integer tipo

character*1 ltip

   select case ( tipo  )
   case ( 1 )
      ltip = "G"
   case ( 2 )
      ltip = "C"
   case ( 3 )
      ltip = "A"
   case ( 4 )
      ltip = "R"
   case default
      ltip = "X"
   end select
    
Return
    
END 
    
! ****************************************
! Busca indice de la zona de carga
! ****************************************
integer Function BuscaZonaCarga ( letzon )

Use ParGloRed, only: numzoncar, nomzoncar

Implicit none

integer i

character(*) letzon

   BuscaZonaCarga = 0
   i = 1
   do while ( i .le. NumZonCar )
       if ( trim(letzon) .eq. trim(nomzoncar(i)) ) then
          BuscaZonaCarga  = i
          exit
       endif
       i = i + 1
    enddo

    
    Return
    
END Function
    
! ****************************************
! Busca indice de la zona de carga
! ****************************************
integer Function BuscaIndZonaCarga ( indz )

Use ParGloRed, only: numzoncar, indzoncarems

Implicit none

integer i, indz

   BuscaIndZonaCarga = 0
   i = 1
   do while ( i .le. NumZonCar )
       if ( indz .eq. indzoncarems(i) ) then
          BuscaIndZonaCarga  = i
          exit
       endif
       i = i + 1
    enddo

    
    Return
    
END Function
    
    
! ****************************************
! Busca indice del area 
! ****************************************
integer Function BuscaArea ( letare )

Use ParGloRed, only: nomarea, numarea

Implicit none

integer i

character(*) letare

   BuscaArea = 0
   i = 1
   do while ( i .le. numarea )
       if ( trim(letare) .eq. trim(nomarea(i)) ) then
          BuscaArea  = i
          exit
       endif
       i = i + 1
    enddo

    
    Return
    
END Function
    
        