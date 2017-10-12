!==============================================================================!
  program Driver
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  include "Backward_Substitution.int"
  include "Forward_Substitution.int"
  include "Print_Matrix.int"               
  include "Print_Vector.int"               
  include "Cholesky_Factorization.int"
  include "Gaussian_Elimination.int"
  include "Matrix_Matrix_Multiply.int"
  include "Matrix_Vector_Multiply.int"
  include "Transpose_Matrix.int" 
!------------------------------------------------------------------------------!
  integer :: row, col, choice
!==============================================================================!

!-----------------------------------+
! Matrix 1 for Gaussian elimination !
!-----------------------------------!
  integer, parameter      :: n1 = 4
  real, dimension(n1, n1) :: a1, g1
  real, dimension(n1)     :: b1, x1, y1
  data (a1(1,col), col=1,n1) /  1.0,  2.0,  1.0, -1.0 /  ! =--> row 1
  data (a1(2,col), col=1,n1) /  3.0,  2.0,  4.0,  4.0 /  ! =--> row 2
  data (a1(3,col), col=1,n1) /  4.0,  4.0,  3.0,  4.0 /  ! =--> row 3
  data (a1(4,col), col=1,n1) /  2.0,  0.0,  1.0,  5.0 /  ! =--> row 4
  data (b1(row),   row=1,n1) /  5.0, 16.0, 22.0, 15.0 /

!-------------------------------------+
! Matrix 2 for Cholesky factorization !
!-------------------------------------!
  integer, parameter      :: n2 = 7
  real, dimension(n2, n2) :: a2, l2, u2, p2
  real, dimension(n2)     :: b2, x2, y2
  data (a2(1,col), col=1,n2) / 44., -4., -3., -2., -1.,  0.,  0. /  ! =--> row 1
  data (a2(2,col), col=1,n2) / -4., 44., -4., -3., -2., -1.,  0. /  ! =--> row 2
  data (a2(3,col), col=1,n2) / -3., -4., 44., -4., -3., -2., -1. /  ! =--> row 3
  data (a2(4,col), col=1,n2) / -2., -3., -4., 44., -4., -3., -2. /  ! =--> row 4
  data (a2(5,col), col=1,n2) / -1., -2., -3., -4., 44., -4., -3. /  ! =--> row 5
  data (a2(6,col), col=1,n2) /  0., -1., -2., -3., -4., 44., -4. /  ! =--> row 6
  data (a2(7,col), col=1,n2) /  0.,  0., -1., -2., -3., -4., 44. /  ! =--> row 7
  data (b2(row),   row=1,n2) /  1.,  2.,  3.,  4.,  3.,  2.,  1. /
!------------------------------------------------------------------------------!

  write(*,*) '============================'
  write(*,*) 'Select a case to demonstrate'
  write(*,*) '1 - Gaussian elimination'
  write(*,*) '2 - Cholesky factorization'
  write(*,*) '----------------------------'
  read(*,*) choice

  !-------------------------------------!
  !   Demonstrate Gussian elimination   !
  !-------------------------------------!
  if(choice == 1) then

    ! Just print original matrix
    call Print_Matrix("Original matrix a1:", a1)

    ! Perform gauissian elimination on matrix and r.h.s. vector
    call Gaussian_Elimination(g1, b1, a1)
    call Print_Matrix("Matrix g1 after elimination:", g1)
    call Print_Vector("R.h.s vector b1 after elimination:", b1)

    ! Perform backward substitution
    call Backward_Substitution(x1, g1, b1)
    call Print_Vector("Solution vector x1: ", x1)

    ! Multiply original matrix with solution vector to check result
    call Matrix_Vector_Multiply(y1, a1, x1)
    call Print_Vector("Vector y1 should recover the source term:", y1)
  end if

  !----------------------------------------!
  !   Demonstrate Cholesky Factorization   !
  !----------------------------------------!
  if(choice == 2) then
  l2 = 0

    ! Just print original matrix
    call Print_Matrix("Original matrix a1:", a2)

    ! Perform Cholesky factorization on the matrix to fin the lower one
    call Cholesky_Factorization(l2, a2)
    call Print_Matrix("Matrix l2 after Cholesky factorization", l2)

    ! Transpose the lower matrix to get the upper
    call Transpose_Matrix(u2, l2)
    call Print_Matrix("Matrix u2 after transpose:", u2)

    ! Multiply lower and upper to check
    call Matrix_Matrix_Multiply(p2, l2, u2)
    call Print_Matrix("Matrix p2 after multiplication of l2 and u2:", p2)

    ! Compute y by forward substitution
    call Forward_Substitution(y2, l2, b2)
    call Print_Vector("Vector y2 after forward substitution:", y2) 

    ! Compute x by backward substitution
    call Backward_Substitution(x2, u2, y2)
    call Print_Vector("Vector x2 after forward substitution:", x2) 

    ! Multiply original matrix with solution vector to check result
    call Matrix_Vector_Multiply(y2, a2, x2)
    call Print_Vector("Vector y2 should recover the source term:", y2)
  end if

  end program Driver
