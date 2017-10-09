!======================================================================!
  subroutine Gaussian_Elimination(a)
!----------------------------------------------------------------------!
  implicit none
!----------------------------------------------------------------------!
  real, dimension(:,:) :: a
!----------------------------------------------------------------------!
  integer :: i, j, k, n
  real    :: xmult
!----------------------------------------------------------------------!

  n = size(a,1)  ! some checks would be possible

  do k=1,n-1
    do i=k+1,n      
      xmult = a(i,k)/a(k,k)       
      a(i,k) = 0.0
      do j=k+1,n    
        a(i,j) = a(i,j) - xmult*a(k,j)      
      end do
    end do
  end do

  end subroutine Gaussian_Elimination
