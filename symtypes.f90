!
      MODULE symtypes
!
!  Establishes parameters for referencing intrinsic types by symbolic names.
!
!  Written By:  Ike Patterson                         On:  13 May 98
!
!  Last Modified On:  12 Jun 98
!
!  Allow no implicit declarations
!
      implicit none
!
!  Integers
!
      integer, parameter :: IM = selected_int_kind(1)
      integer, parameter :: IS = selected_int_kind(4)
      integer, parameter :: IL = selected_int_kind(9)
      integer, parameter :: ILL = selected_int_kind(10)
!
!  Reals
!
      integer, parameter :: RS = kind(1.0)
      integer, parameter :: RD = kind(1.0d0)
!
!  Complex
!
      integer, parameter :: CS = kind((1.0,1.0))
      integer, parameter :: CD = kind((1.0d0,1.0d0))
!
!  Logical (default size only)
!
      integer, parameter :: LD = kind(.true.)
!
      END MODULE symtypes