#include "Unused.h90"

!==============================================================================!
  module Discretize_Mod
!----------------------------------[Modules]-----------------------------------!
  use In_Out_Mod
  use Dense_Mod
  use Sparse_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   A suite of routines for solver demonstration                               !
!==============================================================================!

  !---------------------!
  !                     !
  !   Discretize type   !
  !                     !
  !---------------------!
  type Discretize_Type
    contains
      procedure :: On_Sparse_Matrix
  end type

  ! Introduce the singleton object
  type(Discretize_Type) :: Discretize

  contains
#   include "Discretize_Mod/On_Sparse_Matrix.f90"

  end module
