!==============================================================================!
  subroutine Gaussian_Elimination(g, b, a)
!------------------------------------------------------------------------------!
!   Performs Gaussian elimination on the given matrix "a" and source term "b". !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:,:) :: g
  real, dimension(:)   :: b
  real, dimension(:,:) :: a
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, n
  real    :: mult
!==============================================================================!

  n = size(a,1)  ! some checks would be possible

  ! Copy the matrix first
  do i=1,n
    do j=1,n
      g(i,j) = a(i,j)
    end do
  end do

  ! Make elimination for resulting matrix
  do k=1,n-1
    do i=k+1,n      
      mult = g(i,k)/g(k,k)       
      g(i,k) = 0.0
      do j=k+1,n    
        g(i,j) = g(i,j) - mult*g(k,j)      
      end do
      b(i) = b(i) - mult*b(k)
    end do
  end do

  end subroutine Gaussian_Elimination
