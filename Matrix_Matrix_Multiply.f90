!======================================================================!
  subroutine Matrix_Matrix_Multiply(c, a, b, n)
!----------------------------------------------------------------------!
  implicit none
!----------------------------------------------------------------------!
  integer              :: n
  real, dimension(n,n) :: c
  real, dimension(n,n) :: a
  real, dimension(n,n) :: b
!----------------------------------------------------------------------!
  integer :: i, j, k
!----------------------------------------------------------------------!

  do i = 1, n
    do j = 1, n
      c(i,j) = 0
      do k = 1, n
        c(i,j) = c(i,j) + a(i,k) * b(k,j)
      end do
    end do
  end do

  end subroutine Matrix_Matrix_Multiply
