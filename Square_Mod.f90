!==============================================================================!
  module Square_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Grid_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   Square (full) matrix type                                                  !
!==============================================================================!

  !-----------------!
  !                 !
  !   Square Type   !
  !                 !
  !-----------------!
  type Square_Type
    integer              :: n = 0     ! matrix dimension
    integer              :: nonzeros  ! number of nonzero entries
    real,    allocatable :: val(:,:)  ! value
  end type

  contains

  include 'Square_Mod/Allocate.f90'
  include 'Square_Mod/Deallocate.f90'

  end module
