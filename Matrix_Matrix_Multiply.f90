!======================================================================!
  subroutine Matrix_Matrix_Multiply(c, a, b)
!----------------------------------------------------------------------!
  implicit none
!----------------------------------------------------------------------!
  real, dimension(:,:) :: c
  real, dimension(:,:) :: a
  real, dimension(:,:) :: b
!----------------------------------------------------------------------!
  integer :: i, j, k, n
!----------------------------------------------------------------------!

  n = size(a, 1)  ! some checks would be possible

  do i = 1, n
    do j = 1, n
      c(i,j) = 0
      do k = 1, n
        c(i,j) = c(i,j) + a(i,k) * b(k,j)
      end do
    end do
  end do

  end subroutine Matrix_Matrix_Multiply
