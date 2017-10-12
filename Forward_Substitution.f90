!==============================================================================!
  subroutine Forward_Substitution(x, f, b)
!------------------------------------------------------------------------------!
!   Performs forward substitution using only lower trinangular matrix.         !
!   It is the same as Forward_Substitution_L                                   !
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  real, dimension(:)   :: x
  real, dimension(:,:) :: f
  real, dimension(:)   :: b
!------------------------------------------------------------------------------!
  integer :: i, j, n
  real    :: sum
!==============================================================================!

  n = size(f,1)  ! some checks would be possible

  do i=1,n
    sum = b(i)
    do j=1,i-1
      sum = sum - f(i,j)*x(j)  ! straightforward for compressed row format
    end do
    x(i) = sum / f(i,i)
  end do

  end subroutine Forward_Substitution
