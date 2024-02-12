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

    sum = 0.0
    do s = max(1, k - bw), k - 1
      sum = sum + L % val(k,s)**2
      call IO % Plot_Dense("factorization", L, B=A, src1=(/k,s,GREEN/))
    end do
    L % val(k,k) = sqrt(A % val(k,k) - sum)
    call IO % Plot_Dense("factorization", L, B=A, targ=(/k,k,PINK2/))

    do i = k + 1, min(k + bw, n)
      sum = 0.0
      do s = max(1, k - bw, i - bw), k - 1
        sum = sum + L % val(i,s)*L % val(k,s)
        call IO % Plot_Dense("factorization", L, B=A, src1=(/i,s,GREEN2/), src2=(/k,s,GREEN/))
      end do
      L % val(i,k) = (A % val(i,k) - sum) / L % val(k,k)
      call IO % Plot_Dense("factorization", L, B=A, targ=(/i,k,PINK2/))
    end do

  end do

  call IO % Plot_Snippet(__FILE__, 28, 48)

  end subroutine
