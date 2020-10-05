!==============================================================================!
  subroutine Solvers_Mod_Gauss_Elimination(g, b, a, bw)
!------------------------------------------------------------------------------!
!   Performs Gaussian elimination on the given matrix "a" and source term "b". !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Square_Type)  :: g
  real, dimension(:) :: b
  type(Square_Type)  :: a
  integer            :: bw  ! band width
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, n
  real    :: mult
!==============================================================================!

  n = a % n  ! some checks would be possible

  ! Copy the matrix first
  do i = 1, n
    do j = 1, n
      g % val(i,j) = a % val(i,j)
    end do
  end do

  ! Make elimination for resulting matrix
  do k = 1, n-1
    do i = k+1, min(k+bw,n)
      mult = g % val(i,k) / g % val(k,k)
      g % val(i,k) = 0.0
      do j = k+1, min(k+bw,n)
        g % val(i,j) = g % val(i,j) - mult*g % val(k,j)
      end do
      b(i) = b(i) - mult*b(k)
    end do
  end do

  end subroutine
