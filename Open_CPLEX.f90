! ---------------------------------------------------------------------
! Abre el ambiente para el software de optimizacion del cplex         *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio del 2015                                                      *
! ---------------------------------------------------------------------

 Subroutine openCPLEX

 Use ParAUHE

 Use symtypes

! Use cplex_ifaces, only: CPXopenCPLEX !Windows (comentarizar para Linux)

 implicit none
 integer                 CPXopenCPLEX !Linux (comentarizar para Windows)
                   
 integer(IL), parameter :: NULL = 0

! environment and LP variable pointers
!
 integer(IL) ibanbit, ierror
 integer(ILL) status
 CHARACTER fecha_Ej*19

 data status  / 0 /

 ibanbit = 1
 ierror = 0

! Initialize the CPLEX environment

 enb = CPXopenCPLEX (status)

!  If an error occurs, the status value indicates the reason for
!   failure.

!if ( env .eq. NULL ) then
if ( enb .eq. NULL ) then
    write (*,*) ' Could not open CPLEX environment'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
endif

return

end





! ---------------------------------------------------------------------
! Cierra el ambiente para el software de optimizacion del cplex       *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Julio del 2015                                                      *
! ---------------------------------------------------------------------

 Subroutine closeCPLEX

 Use ParAUHE

 Use symtypes

! Use cplex_ifaces, only: CPXcloseCPLEX !Windows (comentarizar para Linux)

 implicit none

 integer                 CPXcloseCPLEX !Linux (comentarizar para Windows)
                   

! environment and LP variable pointers
!
 integer status, ibanbit, ierror
 CHARACTER fecha_Ej*19

 data status  / 0 /

 ibanbit = 1
 ierror = 0

! Free up the CPLEX environment, if necessary
!
 status = CPXcloseCPLEX (enb)
!
! Note that CPXcloseCPLEX produces no output,
if ( status .ne. 0 ) then
    write (*,*) ' Could not close CPLEX environment.'
    Call FechaEjecucion (fecha_Ej)
    bmensaje = fecha_Ej//' '//NomEjecu//'101 TERMINACION ERROR FATAL CPLEX'
    Call Mensaje_AuSeg ( ierror, ibanbit, BMensaje )
    write(*,*) '1'
!   Se ecribe resultado de semaforos
    call EscSemaforosError
!   algoritmo no termina bien
    call SalidaError
    stop
endif

return

end
