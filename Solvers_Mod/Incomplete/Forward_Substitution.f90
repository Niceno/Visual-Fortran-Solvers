!==============================================================================!
  subroutine Solvers_Mod_Sparse_Forward_Substitution(x, L, b, d_one, d_only)
!------------------------------------------------------------------------------!
!>  Performs forward substitution using a sparse matrix.
!   It will address only elements in lower trinangular part though.            !
!------------------------------------------------------------------------------!
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Cholesky                                          !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x       !! resulting vector
  type(Sparse_Type)  :: L       !! factorized matrix, should be U in the caller
  real, dimension(:) :: b       !! right hand side vector
  logical,  optional :: d_one   !! diagonal is 1, good for LU decomposition
  logical,  optional :: d_only  !! solve using diagonal only, used in LDL'
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, ij, n
  real    :: sum
  logical :: diagonal_one  = .false.
  logical :: diagonal_only = .false.
!==============================================================================!

  ! Take some aliases
  n = L % n      ! some checks would be possible

  ! Treat the optional arguments
  diagonal_one = .false.  ! keep this, compiler issue
  if(present(d_one)) diagonal_one = d_one

  diagonal_only = .false.  ! keep this, compiler issue
  if(present(d_only)) diagonal_only = d_only

  !------------------------------------------------------------------!
  !   Here we seek non-trivial solutions with forward substitution   !
  !------------------------------------------------------------------!
  if(.not. diagonal_only) then

    ! Here, i > j, therfore it is a lower matrix
    if(.not. diagonal_one) then
      do i = 1, n  ! <-A
        sum = b(i)
        call IO % Plot_Sparse_System("spar_forw", L, x, b, srcb=(/i/))
        do ij = L % row(i), L % dia(i) - 1  ! up to diagonal
          j = L % col(ij)                   ! j < i; hence L
          Assert(j < i)
          sum = sum - L % val(ij) * x(j)
          call IO % Plot_Sparse_System("spar_forw", L, x, b, srca=(/i,j/), srcx=(/j/))
        end do
        x(i) = sum / L % val( L % dia(i) )
        call IO % Plot_Sparse_System("spar_forw", L, x, b, tarx=(/i/), srca=(/i,i/))
      end do       ! A->
      call IO % Plot_Snippet(__FILE__, '<-A', 'A->')

    ! Diagonal is equal to 1, good for LU and LDL' methods
    else
      do i = 1, n  ! <-B
        sum = b(i)
        call IO % Plot_Sparse_System("spar_forw", L, x, b, srcb=(/i/))
        do ij = L % row(i), L % dia(i) - 1  ! up to diagonal
          j = L % col(ij)                   ! j < i; hence L
          Assert(j < i)
          sum = sum - L % val(ij) * x(j)
          call IO % Plot_Sparse_System("spar_forw", L, x, b, srca=(/i,j/), srcx=(/j/))
        end do
        x(i) = sum
        call IO % Plot_Sparse_System("spar_forw", L, x, b, tarx=(/i/))
      end do       ! B->
      call IO % Plot_Snippet(__FILE__, '<-B', 'B->')

    end if

  !-------------------------------------------------!
  !   Trivial solutions with forward substitution   !
  !    as used in ssecond step of LDL' solution     !
  !-------------------------------------------------!
  else
    do i = 1, n  ! <-C
      x(i) = b(i) / L % val(L % dia(i))
      call IO % Plot_Sparse_System("spar_forw", L, x, b, tarx=(/i/), srca=(/i,i/), srcb=(/i/))
    end do       ! C->
    call IO % Plot_Snippet(__FILE__, '<-C', 'C->')

  end if

  end subroutine
