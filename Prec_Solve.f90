!==============================================================================!
  subroutine Prec_Solve(N, NB, A, D, x, b) 
!------------------------------------------------------------------------------!
! Solves the preconditioning system [D]{x}={b}                                 !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer      :: N, NB
  type(Matrix) :: A
  type(Matrix) :: D
  real         :: x(-NB:N), b(N)
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k
  real    :: sum1
!==============================================================================!
           
  ! Forward substitutionn
  do i=1,N
    sum1=b(i)
    do j=A % row(i),A % dia(i)-1  ! only the lower triangular
      k = A % col(j)             
      sum1 = sum1- A % val(j)*x(k)  
    end do
    x(i) = sum1 * D % val(D % dia(i))         ! BUG ?
  end do

  do i=1,N
    x(i) = x(i) / ( D % val(D % dia(i)) )
  end do

  ! Backward substitution
  do i=N,1,-1
    sum1=x(i)
    do j = A % dia(i)+1, A % row(i+1)-1 ! upper triangular 
      k = A % col(j)                  
      sum1 = sum1 - A % val(j)*x(k)      
    end do
    x(i) = sum1* D % val(D % dia(i))               ! BUG ?
  end do

  end subroutine Prec_Solve
