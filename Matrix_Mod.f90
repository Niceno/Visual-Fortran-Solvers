!==============================================================================!
  module Matrix_Mod
!------------------------------------------------------------------------------!
  use Grid_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   Mother of all matrices                                                     !
!==============================================================================!

  !-----------------!
  !                 !
  !   Matrix Type   !
  !                 !
  !-----------------!
  type, public :: Matrix_Type

   type(Grid_Type), pointer :: pnt_grid  ! pointer to grid

    integer      :: n = 0        ! matrix dimension
    integer      :: nonzeros     ! number of nonzero entries
    character(4) :: text_u = ""  ! text to put in the upper righ corner
    character(4) :: text_l = ""  ! text to put in the lower left corent
  end type

  end module
