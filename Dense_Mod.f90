!==============================================================================!
  module Dense_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
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
  type Dense_Type
    integer              :: n = 0     ! matrix dimension
    integer              :: nonzeros  ! number of nonzero entries
    integer              :: bw        ! band width
    real,    allocatable :: val(:,:)  ! value

    contains
      procedure :: Dense_Allocate
      procedure :: Dense_Deallocate

  end type

  contains

  include 'Dense_Mod/Allocate.f90'
  include 'Dense_Mod/Deallocate.f90'

  end module
