!==============================================================================!
  subroutine Solvers_Mod_Dense_Cholesky_Factorization(LL, A)
!------------------------------------------------------------------------------!
!>  Computes Cholesky decomposition on square (full) matrices.
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Cholesky                                                     !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type) :: LL  !! factorized matrix
  type(Dense_Type) :: A
!-----------------------------------[Locals]-----------------------------------!
  integer          :: i, k, m, n, bw
  real             :: sum
!==============================================================================!

  print *, '# Factorizing square (full) matrix with Cholesky method'

  ! Take some aliases
  n  = A % n
  bw = A % bw

  ! Perform the factorization
  do k = 1, n
    sum = A % val(k,k)
    do m = max(1, k - bw), k - 1
      sum = sum - LL % val(k,m)**2  ! straightforward for sparse matrix
    end do
    LL % val(k,k) = sqrt(sum)
    do i = k + 1, min(k + bw, n)
      sum = A % val(i,k)
      do m = max(1, k - bw, i - bw), k - 1
        sum = sum - LL % val(m,i)*LL % val(m,k)  ! straighforward for sparse
      end do
      LL % val(k,i) = sum / LL % val(k,k)
      LL % val(i,k) = sum / LL % val(k,k)  ! make it full
    end do
  end do

  call IO % Plot_Snippet(__FILE__, 25, 39)

  end subroutine
