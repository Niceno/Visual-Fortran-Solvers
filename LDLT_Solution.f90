!==============================================================================!
  subroutine LDLT_Solution(x, f, b)
!------------------------------------------------------------------------------!
!   Performs forward substitution on a full matrix.                            !
!   It will address only elements in lower trinangular part though.            !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:)   :: x
  real, dimension(:,:) :: f
  real, dimension(:)   :: b
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n
  real    :: sum
!==============================================================================!

  n = size(f,1)  ! some checks would be possible

  ! Forward substitutions
  do i=1,n
    sum = b(i)
    do j=1,i-1
      sum = sum - f(i,j)*x(j)  ! straightforward for compressed row format
    end do
    x(i) = sum
  end do

  ! Treat the diagonal term
  do i=1,n
    x(i) = x(i) / f(i,i)
  end do

  ! Backward substitution
  do i=n,1,-1
    sum = b(i)
    do j=i+1,n
      sum = sum - f(i,j)*x(j)  ! straighforward for compressed row format
    end do
    x(i) = sum 
  end do

  end subroutine LDLT_Solution
