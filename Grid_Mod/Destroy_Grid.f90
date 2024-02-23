!==============================================================================!
  subroutine Destroy_Grid(Grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Grid_Type)    :: Grid
!-----------------------------------[Locals]-----------------------------------!
  integer           :: s, c, c1, c2, e, n, t, i, j, k
  real, allocatable :: visited(:)
!==============================================================================!

  Grid % n_bnd_cells = 0
  Grid % n_cells     = 0
  Grid % n_faces     = 0

  !-----------!
  !           !
  !   Nodes   !
  !           !
  !-----------!

  ! De-allocate memory for node coordinates
  deallocate(Grid % xn)
  deallocate(Grid % yn)
  deallocate(Grid % zn)

  !---------------!
  !               !
  !   Cells (1)   !
  !               !
  !---------------!

  ! De-allocate memory for cell coordinates
  deallocate(Grid % xc)
  deallocate(Grid % yc)
  deallocate(Grid % zc)

  ! Allocate memory for cells to cells connectivity
  deallocate(Grid % cells_n_cells)
  deallocate(Grid % cells_c)

  !-----------!
  !           !
  !   Faces   !  (some of them are also boundary cells, in fact)
  !           !
  !-----------!

  deallocate(Grid % faces_c)
  deallocate(Grid % dx)
  deallocate(Grid % dy)
  deallocate(Grid % dz)

  end subroutine

