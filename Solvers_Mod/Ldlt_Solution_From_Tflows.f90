!==============================================================================!
  subroutine Solvers_Mod_Ldlt_Solution_From_Tflows(n, nb, A, F, x, b)
!------------------------------------------------------------------------------!
!   Solves the preconditioning system [F]{x}={b}                               !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt_From_Tflows                                  !
!------------------------------------------------------------------------------!
!   Important to note here:                                                    !
!                                                                              !
!   1. In the forward substitution part:                                       !
!      do i = 1, n                                                             !
!        sum = b(i)                                                            !
!        do ij = A % row(i), A % dia(i) - 1  ! only the lower triangular       !
!          j = A % col(ij)                   ! fetch the column                !
!          ! In the original algorithm, F entries would be used here, (see in  !
!          ! Ldlt_Solution_Dense.f90).  But it doesn't matter, because in the  !
!          ! line after "end do", we multiply the sum with diagonal entry      !
!          ! which actually holds the reciprocal of the diagonal from LDL'     !
!          sum = sum - A % val(ij) * x(j)                                      !
!        end do                                                                !
!        x(i) = sum * F % val(F % dia(i))    ! F holds reciprocal of diagonal  !
!      end do                                                                  !
!                                                                              !
!   2. The same trick is used in the backward substitution.                    !
!                                                                              !
!   3. I can't help but think that there is an error here:                     !
!      do i = 1, n                                                             !
!        x(i) = x(i) / ( F % val(F % dia(i)) )                                 !
!      end do                                                                  !
!                                                                              !
!      Since F holds the reciprocal of the diagonal, x should be multiplied    !
!      with it, not divided :-(                                                !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer           :: n, nb
  type(Sparse_Type) :: A
  type(Sparse_Type) :: F
  real              :: x(-nb:n), b(n)
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, ij, j
  real    :: sum
!==============================================================================!

  ! Forward substitutionn
  do i = 1, n
    sum = b(i)
    do ij = A % row(i), A % dia(i) - 1  ! only the lower triangular
      j = A % col(ij)                   ! fetch the column
      sum = sum - A % val(ij) * x(j)
    end do
    x(i) = sum * F % val(F % dia(i))
  end do

  do i = 1, n
    x(i) = x(i) / ( F % val(F % dia(i)) )
  end do

  ! Backward substitution
  do i = n, 1, -1
    sum = x(i)
    do ij = A % dia(i) + 1, A % row(i+1) - 1  ! upper triangular
      j = A % col(ij)                         ! fetch the column
      sum = sum - A % val(ij) * x(j)
    end do
    x(i) = sum * F % val(F % dia(i))
  end do

  end subroutine
