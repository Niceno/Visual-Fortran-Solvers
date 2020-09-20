!==============================================================================!
  subroutine Demo_Mod_Compress_Decompress
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: col, bw
!==============================================================================!

  ! Full matrix
  integer, parameter    :: n = 7
  real, dimension(n, n) :: f_matrix
  data (f_matrix(1,col), col=1,n) / 44., -4., -3.,  0., -1.,  0.,  0. /
  data (f_matrix(2,col), col=1,n) / -4., 44., -4., -3.,  0., -1.,  0. /
  data (f_matrix(3,col), col=1,n) / -3., -4., 44., -4., -3.,  0., -1. /
  data (f_matrix(4,col), col=1,n) /  0., -3., -4., 44., -4., -3.,  0. /
  data (f_matrix(5,col), col=1,n) / -1.,  0., -3., -4., 44., -4., -3. /
  data (f_matrix(6,col), col=1,n) /  0., -1.,  0., -3., -4., 44., -4. /
  data (f_matrix(7,col), col=1,n) /  0.,  0., -1.,  0., -3., -4., 44. /

  ! Compressed matrix
  type(Matrix_Type) :: c_matrix
  real, allocatable :: e_matrix(:,:)
!------------------------------------------------------------------------------!

  call In_Out_Mod_Print_Matrix("Original matrix f_matrix", f_matrix)

  ! Compress matrix "f_matrix" and store it in "c_matrix"
  call Matrix_Mod_Compress(c_matrix, f_matrix)

  call In_Out_Mod_Print_Matrix_Compressed("c_matrix:", c_matrix)

  ! Expand matrix "ac2" and store it in "e_matrix"
  call Matrix_Mod_Expand(e_matrix, c_matrix, bw)

  call In_Out_Mod_Print_Matrix("Epanded matrix c_matrix:", e_matrix)

  end subroutine
