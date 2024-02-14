!==============================================================================!
  subroutine Solvers_Mod_Dense_Cholesky_Factorization(L, A)
!------------------------------------------------------------------------------!
!>  Computes Cholesky factorization on square (full) matrices.
!------------------------------------------------------------------------------!
!   Cholesky factorization in full, looks like this:                           !
!                                                                              !
!         | L11                 | | L11 L12 L13 L14 L15 |                      !
!         | L21 L22             | |     L22 L23 L24 L25 |                      !
!   LL' = | L31 L32 L33         | |         L33 L34 L35 |                      !
!         | L41 L42 L43 L44     | |             L44 L45 |                      !
!         | L51 L52 L53 L54 L55 | |                 L55 |                      !
!                                                                              !
!   But given that LL's is symmetric, only one L is stored:                    !
!                                                                              !
!              | L11                 |                                         !
!     stored   | L21 L22             |                                         !
!   LL'      = | L31 L32 L33         |                                         !
!              | L41 L42 L43 L44     |                                         !
!              | L51 L52 L53 L54 U55 |                                         !
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

  !-------------------------------!
  !   Perform the factorization   !
  !-------------------------------!
  do k = 1, n  ! <-A

    ! Work out (and store) diagonal term
    sum = 0.0
    do s = max(1, k - bw), k - 1
      sum = sum + L % val(k,s)**2
      if(k.eq.n/2) call IO % Plot_Dense("chol_dens", L, B=A, src1=(/k,s,GREEN/))
    end do
    L % val(k,k) = sqrt(A % val(k,k) - sum)
    if(k.eq.n/2) call IO % Plot_Dense("chol_dens", L, B=A, targ=(/k,k,PINK2/))

    ! Work out (and store) the L
    do i = k + 1, min(k + bw, n)
      sum = 0.0
      do s = max(1, k - bw, i - bw), k - 1
        sum = sum + L % val(i,s)*L % val(k,s)
        if(k.eq.n/2) call IO % Plot_Dense("chol_dens", L, B=A, src1=(/i,s,GREEN2/), src2=(/k,s,GREEN/))
      end do
      L % val(i,k) = (A % val(i,k) - sum) / L % val(k,k)
      if(k.eq.n/2) call IO % Plot_Dense("chol_dens", L, B=A, targ=(/i,k,PINK2/))
    end do

  end do  ! A->

  call IO % Plot_Snippet(__FILE__, '<-A', 'A->')

  end subroutine
