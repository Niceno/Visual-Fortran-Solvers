!==============================================================================!
  subroutine Cells_I_J_K(Grid, c, i, j, k)
!------------------------------------------------------------------------------!
!>  Returns the cell's indices i, j and k based on the cell number.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Grid_Type)     :: Grid
  integer, intent(in)  :: c
  integer, intent(out) :: i, j, k
!==============================================================================!

  i = mod(c-1, Grid % nx) + 1
  j = mod(c-i, Grid % nx * Grid % ny) / Grid % nx + 1
  k = (c - i - (j-1) * Grid % nx) / (Grid % nx * Grid % ny) + 1

  end subroutine
