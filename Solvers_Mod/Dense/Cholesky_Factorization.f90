!==============================================================================!
  subroutine Solvers_Mod_Dense_Cholesky_Factorization(L, A)
!------------------------------------------------------------------------------!
!>  Computes Cholesky decomposition on square (full) matrices.
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Cholesky                                                     !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type) :: L  !! factorized matrix
  type(Dense_Type) :: A
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, k, s, n, bw
  real    :: sum
!==============================================================================!

  print *, '# Factorizing dense matrix with Cholesky method'

  ! Take some aliases
  n  = A % n
  bw = A % bw

  ! Initialize the values
  L % val(:,:) = 0.0

  ! Perform the factorization
  do k = 1, n
    sum = A % val(k,k)
    do s = max(1, k - bw), k - 1
      sum = sum - L % val(k,s)**2
    end do
    L % val(k,k) = sqrt(sum)
    do i = k + 1, min(k + bw, n)
      sum = A % val(i,k)
      do s = max(1, k - bw, i - bw), k - 1
        sum = sum - L % val(i,s)*L % val(k,s)
      end do
      L % val(i,k) = sum / L % val(k,k)
    end do
  end do

  call IO % Plot_Snippet(__FILE__, 28, 41)

  end subroutine
