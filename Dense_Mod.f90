#include "Assert.h90"

!==============================================================================!
  module Dense_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
  use Grid_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   Dense (full) matrix type                                                   !
!==============================================================================!

  !----------------!
  !                !
  !   Dense Type   !
  !                !
  !----------------!
  type, extends(Matrix_Type) :: Dense_Type
    integer           :: bw        ! band width
    real, allocatable :: val(:,:)  ! value

    contains
      procedure :: Dense_Allocate
      procedure :: Dense_Deallocate

  end type

  contains

#   include "Dense_Mod/Allocate.f90"
#   include "Dense_Mod/Deallocate.f90"

  end module
