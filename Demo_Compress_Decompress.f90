!==============================================================================!
  subroutine Demo_Compress_Decompress 
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Interfaces]---------------------------------!
  include "Backward_Substitution.int"
  include "Forward_Substitution.int"
  include "Print_Matrix.int"               
  include "Print_Vector.int"               
  include "Cholesky_Factorization.int"
  include "Cholesky_Factorization_Compressed.int"
  include "Gaussian_Elimination.int"
  include "Matrix_Matrix_Multiply.int"
  include "Matrix_Vector_Multiply.int"
  include "Transpose_Matrix.int" 
  include "Compress_Matrix.int"               
  include "Expand_Matrix.int"               
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col, choice, i
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
  type(Matrix)      :: c_matrix
  real, allocatable :: a3(:,:)
!------------------------------------------------------------------------------!

  call Print_Matrix("Original matrix f_matrix", f_matrix)

  ! Compress matrix "f_matrix" and store it in "c_matrix"
  call Compress_Matrix(c_matrix, f_matrix)

  call Print_Vector("c_matrix % val:", c_matrix % val)

  write(*,*) "c_matrix % row = "
  do i=1, size(c_matrix % row)
    write(*,'(2I4)'), i, c_matrix % row(i)
  end do 
  write(*,*) "c_matrix % dia = "
  do i=1, size(c_matrix % dia)
    write(*,'(2I4)'), i, c_matrix % dia(i)
  end do 
  write(*,*) "c_matrix % col = "
  do i=1, size(c_matrix % col)
    write(*,'(2I4)'), i, c_matrix % col(i)
  end do 
  write(*,*) "c_matrix % mir = "
  do i=1, size(c_matrix % mir)
    write(*,'(2I4)'), i, c_matrix % mir(i)
  end do 

  ! Expand matrix "ac2" and store it in "a3"
  call Expand_Matrix(a3, c_matrix)

  call Print_Matrix("Epanded matrix c_matrix:", a3)

  ! Perform Cholesky factorization on the matrix to fin the lower one
  call Cholesky_Factorization_Compressed(c_matrix, f_matrix)
  call Print_Vector("c_matrix % val:", c_matrix % val)

  end subroutine Demo_Compress_Decompress 
