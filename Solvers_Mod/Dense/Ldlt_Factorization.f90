!==============================================================================!
  subroutine Solvers_Mod_Dense_Ldlt_Factorization(LD, A)
!------------------------------------------------------------------------------!
!>  Computes LDL' factorization on square (full) matrices.
!------------------------------------------------------------------------------!
!   LDL' decomposition in full, looks like this:                               !
!                                                                              !
!   LDL =                                                                      !
!    |  1                  | | D11                 | |  1  L12 L13 L14 L15 |   !
!    | L21  1              | |     D22             | |      1  L23 L24 L25 |   !
!    | L31 L32  1          | |         D33         | |          1  L34 L35 |   !
!    | L41 L42 L43  1      | |             D44     | |              1  L45 |   !
!    | L51 L52 L53 L54  1  | |                 D55 | |                  1  |   !
!                                                                              !
!   But given that L's diagonal is equal to one, it doesn't have to be stored. !
!                                                                              !
!               | D11                 |                                        !
!      stored   | L21 D22             |                                        !
!   LDL       = | L31 L32 D33         |                                        !
!               | L41 L42 L43 D44     |                                        !
!               | L51 L52 L53 L54 D55 |                                        !
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Ldlt_Solver                                                  !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type) :: LD  !! factorized matrix (only L and D are stored)
  type(Dense_Type) :: A   !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, k, s, n, bw
  real    :: sum
!==============================================================================!

  print *, '# Factorizing square (full) matrix with LDL'' method'

  ! Take some aliases
  n  = A % n
  bw = A % bw

  ! Initialize the values
  LD % val(:,:) = 0.0

  !-------------------------------!
  !   Perform the factorization   !
  !-------------------------------!
  do k = 1, n  ! <-A

    ! Work out the diagonal D
    sum = 0.0
    do s = max(1, k - bw), k - 1
      sum = sum + LD % val(k,s)**2 * LD % val(s,s)
      call IO % Plot_Dense("factorization", LD, B=A, src1=(/k,s,GREEN/), src2=(/s,s,GREEN2/))
    end do
    LD % val(k,k) = A % val(k,k) - sum
    call IO % Plot_Dense("factorization", LD, B=A, targ=(/k,k,PINK2/))

    ! Work out (and store) the L
    do i = k + 1, min(k + bw, n)
      sum = 0.0
      do s = max(1, k - bw, i - bw), k - 1
        sum = sum + LD % val(i,s) * LD % val(k,s) * LD % val(s,s)
        call IO % Plot_Dense("factorization", LD, B=A, src1=(/i,s,GREEN/), src2=(/k,s,GREEN2/), src3=(/s,s,GREEN4/))
      end do
      LD % val(i,k) = (A % val(i,k) - sum) / LD % val(k,k)
      call IO % Plot_Dense("factorization", LD, B=A, targ=(/i,k,PINK2/))
    end do
  end do       ! A->

  call IO % Plot_Snippet(__FILE__, '<-A', 'A->')

  end subroutine
