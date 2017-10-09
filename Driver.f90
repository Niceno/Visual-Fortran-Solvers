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
  include "Transpose_Matrix.int" 
!----------------------------------------------------------------------!
  integer :: row, col
!----------------------------------------------------------------------!

!-----------------------------------+
! Matrix 1 for Gaussian elimination !
!-----------------------------------!
  integer, parameter      :: n1 = 4
  real, dimension(n1, n1) :: a1
  data (a1(1,col), col=1,n1) /  6.0, -2.0,  2.0,  4.0 /  ! =--> row 1
  data (a1(2,col), col=1,n1) / 12.0, -8.0,  6.0, 10.0 /  ! =--> row 2
  data (a1(3,col), col=1,n1) /  3.0,-13.0,  9.0,  3.0 /  ! =--> row 3
  data (a1(4,col), col=1,n1) / -6.0,  4.0,  1.0,-18.0 /  ! =--> row 4

!-------------------------------------+
! Matrix 2 for Cholesky factorization !
!-------------------------------------!
  integer, parameter      :: n2 = 3
  real, dimension(n2, n2) :: a2, l2, u2, p2
  data (a2(1,col), col=1,n2) / 60.0, 30.0, 20.0 /  ! =--> row 1
  data (a2(2,col), col=1,n2) / 30.0, 20.0, 15.0 /  ! =--> row 2
  data (a2(3,col), col=1,n2) / 20.0, 15.0, 12.0 /  ! =--> row 3

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

  !-------------------------------------!
  !   Demonstrate Gussian elimination   !
  !-------------------------------------!
  write(*,*) "Original matrix a1:"
  call Print_Matrix(a1)

  call Gaussian_Elimination(a1)

  write(*,*) "Matrix a1 after elimination:"
  call Print_Matrix(a1)

  !----------------------------------------!
  !   Demonstrate Cholesky Factorization   !
  !----------------------------------------!
  l2 = 0
  call Cholesky_Factorization(l2, a2)

  write(*,*) "Matrix l2 after Cholesky factorization:"
  call Print_Matrix(l2)

  call Transpose_Matrix(u2, l2)

  write(*,*) "Matrix u2 after inversion:"
  call Print_Matrix(u2)

  call Matrix_Matrix_Multiply(p2, l2, u2)

  write(*,*) "Matrix p2 after multiplication:"
  call Print_Matrix(p2)

  !---------------------------------------!
  !   Demonstrate Backward Substitution   !
  !---------------------------------------!
  write(*,*) "Matrix a3"                          
  call Print_Matrix(a3)

  call Forward_Substitution(y3, a3, b3)

  call Print_Vector(y3) 
 
  !--------------------------------------!
  !   Demonstrate Forward Substitution   !
  !--------------------------------------!
  write(*,*) "Matrix a4"                          
  call Print_Matrix(a4)

  call Backward_Substitution(y4, a4, b4)

  call Print_Vector(y4) 
 
  end program Driver
