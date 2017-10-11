!======================================================================!
  program Driver
!----------------------------------------------------------------------!
  implicit none
!----------------------------------------------------------------------!
  include "Backward_Substitution.int"
  include "Forward_Substitution.int"
  include "Print_Matrix.int"               
  include "Print_Vector.int"               
  include "Cholesky_Factorization.int"
  include "Gaussian_Elimination.int"
  include "Matrix_Matrix_Multiply.int"
  include "Matrix_Vector_Multiply.int"
  include "Transpose_Matrix.int" 
!----------------------------------------------------------------------!
  integer :: row, col, choice
!----------------------------------------------------------------------!

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
  data (x1(row),   row=1,n1) /  0.0,  0.0,  0.0,  0.0 /
  data (y1(row),   row=1,n1) /  0.0,  0.0,  0.0,  0.0 /

!-------------------------------------+
! Matrix 2 for Cholesky factorization !
!-------------------------------------!
  integer, parameter      :: n2 = 3
  real, dimension(n2, n2) :: a2, l2, u2, p2
  real, dimension(n2)     :: b2, x2, y2
  data (a2(1,col), col=1,n2) / 60.0, 30.0, 20.0 /  ! =--> row 1
  data (a2(2,col), col=1,n2) / 30.0, 20.0, 15.0 /  ! =--> row 2
  data (a2(3,col), col=1,n2) / 20.0, 15.0, 12.0 /  ! =--> row 3
  data (b2(row),   row=1,n2) /  5.0, 16.0, 15.0 /

!-----------------------------------+
! Matrix 3 for Forward substitution !
!-----------------------------------!
  integer, parameter      :: n3 = 3
  real, dimension(n3, n3) :: a3
  real, dimension(n3)     :: b3, y3
  data (a3(1,col), col=1,n3) / 6.0, 0.0, 0.0 /  ! =--> row 1
  data (a3(2,col), col=1,n3) / 3.0, 2.0, 0.0 /  ! =--> row 2
  data (a3(3,col), col=1,n3) / 4.0, 2.0, 1.0 /  ! =--> row 3
  data (b3(row),   row=1,n3) / 6.0, 5.0, 7.0 /

!------------------------------------+
! Matrix 4 for Backward substitution !
!------------------------------------!
  integer, parameter      :: n4 = 3
  real, dimension(n4, n4) :: a4
  real, dimension(n4)     :: b4, y4
  data (a4(1,col), col=1,n4) / 4.0, 2.0, 1.0 /  ! =--> row 1
  data (a4(2,col), col=1,n4) / 0.0, 3.0, 2.0 /  ! =--> row 2
  data (a4(3,col), col=1,n4) / 0.0, 0.0, 6.0 /  ! =--> row 3
  data (b4(row),   row=1,n4) / 7.0, 5.0, 6.0 /
!----------------------------------------------------------------------!

  write(*,*) '============================'
  write(*,*) 'Select a case to demonstrate'
  write(*,*) '1 - Gaussian elimination'
  write(*,*) '2 - Cholesky factorization'
  write(*,*) '3 - Backward substitution'
  write(*,*) '4 - Forward substitution'
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

  !---------------------------------------!
  !   Demonstrate Backward Substitution   !
  !---------------------------------------!
  if(choice == 3) then
    call Print_Matrix("Matrix a3:", a3)
    call Forward_Substitution(y3, a3, b3)
    call Print_Vector("Vector y3", y3) 
  end if
 
  !--------------------------------------!
  !   Demonstrate Forward Substitution   !
  !--------------------------------------!
  if(choice == 4) then
    call Print_Matrix("Matrix a4:", a4)
    call Backward_Substitution(y4, a4, b4)
    call Print_Vector("Vector y4:", y4) 
  end if
 
  end program Driver
