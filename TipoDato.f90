
! ---------------------------------------------------------------------
! Modulo de declaracion tipos de datos AuSeg                          *
!                                                                     *
! Instituto de investigaciones Electricas                             *
! Gerencia de analisis de redes                                       *
! Division de sistemas electricos                                     *
!                                                                     *
! Enero del 2009                                                      *
! ---------------------------------------------------------------------
!

Module TipoDato

! use parametros_Termo

implicit none

!  incluye los parametros de la red eléctrica
include 'C:\AUHE\Parametros\param.par'  !Windows (comentarizar para Linux)

!include 'param.par'   !Linux (comentarizar para Windows)

!implicit none

   Type, public:: ConectaClase
        Integer::num ! Numero de elementos conectados al nodo
        Integer::indice (maxramnod) ! Indice del elemento conectado
        Integer::tipele (maxramnod) ! Tipo de elemento (1:generador, 2:sincrono, 3:cevs)

   End Type ConectaClase


   Type(ConectaClase)::ElemNodo(maxnod)

   Type(ConectaClase)::ElemContingencia(maxcontg)




   Type, public:: RamasEnlace
        Integer::num ! Numero de ramas que pertenecen al enlace
        Integer::indice(maxramenl) ! Indice de la rama a la que pertenece el grupo
        Integer::signo(maxramenl) ! Sentido de la rama a la que pertenece el grupo

   End Type RamasEnlace

   Type(RamasEnlace)::RamEnlace(maxenl)

   Type(RamasEnlace)::EnlGrupo(maxgruenl)

   Type, public:: RamasGrupo
        Integer::num ! Numero de ramas que pertenecen al grupo de ramas
        Integer::indice(maxramenl) ! Indice de la rama a la que pertenece el grupo ( en lista de ramas )
        Integer::signo(maxramenl) ! Sentido de la rama a la que pertenece el grupo
        Integer::ramres(maxramenl) ! Indice de la rama a la que pertenece el grupo ( en lista de ramas restringidas )

   End Type RamasGrupo

   Type(RamasGrupo)::RamGrurar(maxgrurar)



end module

