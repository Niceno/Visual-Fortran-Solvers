!==============================================================================!
  subroutine Backward_Substitution(x, f, b)
!------------------------------------------------------------------------------!
!   Performs forward substitution using only upper trinangular matrix.         !
!   It is the same as Backward_Substitution_U                                  !
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

  do i=n,1,-1
    sum = b(i)
    do j=i+1,n
      sum = sum - f(i,j)*x(j)  ! straighforward for compressed row format
    end do
    x(i) = sum/f(i,i)
  end do

  end subroutine Backward_Substitution
