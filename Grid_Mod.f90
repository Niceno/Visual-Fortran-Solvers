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

  !-----------------------------!
  !                             !
  !   Boundary condition type   !
  !                             !
  !-----------------------------!
  type Bc_Type

    ! Types on all sides (N will be for Neumann, D for Dirichlet)
    character(1) :: west_t
    character(1) :: east_t
    character(1) :: south_t
    character(1) :: north_t
    character(1) :: bottom_t
    character(1) :: top_t

    ! Values on all sides
    real :: west_v
    real :: east_v
    real :: south_v
    real :: north_v
    real :: bottom_v
    real :: top_v

  end type

  !---------------!
  !               !
  !   Grid Type   !
  !               !
  !---------------!
  type Grid_Type

    integer :: n_cells
    integer :: n_bnd_cells
    integer :: n_faces

    integer :: nx, ny, nz  ! domain resolution in x, y and z direction
    real    :: lx, ly, lz  ! domain size in x, y and z direction

    integer, allocatable :: faces_c(:,:)
    integer, allocatable :: cells_c(:,:)
    integer, allocatable :: cells_n_cells(:)

    real, allocatable :: xn(:), yn(:), zn(:)
    real, allocatable :: xc(:), yc(:), zc(:)
    real, allocatable :: dx(:), dy(:), dz(:)

    type(Bc_Type) :: bc  ! boundary conditions

    contains
      procedure :: Create_Grid
      procedure :: Cell_Number
      procedure :: Cells_I_J_K
      procedure :: Destroy_Grid
      procedure :: Load_Grid
      procedure :: Save_Vtk_Scalar
      procedure :: Save_Vtk_Vector

  end type

  contains
#   include "Grid_Mod/Create_Grid.f90"
#   include "Grid_Mod/Cell_Number.f90"
#   include "Grid_Mod/Cells_I_J_K.f90"
#   include "Grid_Mod/Destroy_Grid.f90"
#   include "Grid_Mod/Load_Grid.f90"
#   include "Grid_Mod/Save_Vtk_Scalar.f90"
#   include "Grid_Mod/Save_Vtk_Vector.f90"

  end module
