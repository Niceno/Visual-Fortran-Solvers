#include "Assert.h90"

!==============================================================================!
  module Grid_Mod
!------------------------------------------------------------------------------!
  use Assert_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   Defines a (ludicrously simple) grid                                        !
!==============================================================================!

  !---------------!
  !               !
  !   Grid Type   !
  !               !
  !---------------!
  type Grid_Type

    integer :: n_cells
    integer :: n_bnd_cells
    integer :: n_faces

    integer, allocatable :: faces_c(:,:)

    integer :: nx, ny, nz  ! domain resolution in x, y and z direction
    real    :: lx, ly, lz  ! domain size in x, y and z direction
    real    :: dx, dy, dz  ! cell size in x, y and z direction

    contains
      procedure :: Create_Grid
      procedure :: Cell_Number
      procedure :: Cells_I_J_K
      procedure :: Save_Vtk_Debug

  end type

  contains
#   include "Grid_Mod/Create_Grid.f90"
#   include "Grid_Mod/Cell_Number.f90"
#   include "Grid_Mod/Cells_I_J_K.f90"
#   include "Grid_Mod/Save_Vtk_Debug.f90"

  end module
