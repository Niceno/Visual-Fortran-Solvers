!==============================================================================!
  module Grid_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   Defines grid (which is ludicrously simple)                                 !
!==============================================================================!

  !---------------!
  !               !
  !   Grid Type   !
  !               !
  !---------------!
  type Grid_Type
    real    :: lx, ly, lz  ! domain sizes in x, y and z directions
    integer :: nx, ny, nz  ! domain resolutions in x, y and z directions
  end type

  end module
