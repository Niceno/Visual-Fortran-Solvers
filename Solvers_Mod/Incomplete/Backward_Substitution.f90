!==============================================================================!
  subroutine Solvers_Mod_Sparse_Backward_Substitution(x, U, b, t)
!------------------------------------------------------------------------------!
!>  Performs backward substitution using a sparse matrix.
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Cholesky                                          !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x  !! resulting vector
  type(Sparse_Type)  :: U  !! factorized matrix, should be U in the caller
  real, dimension(:) :: b  !! right hand side vector
  logical,  optional :: t  !! use transposed matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, ij, n
  real    :: sum
  logical :: transposed = .false.
!==============================================================================!

  n = U % n      ! some checks would be possible

  transposed = .false.           ! for some reason, this must be repeated
  if(present(t)) transposed = t

  ! Here, j > i, therfore it is a U matrix
  if(.not. transposed) then
    do i = n, 1, -1
      sum = b(i)
      do ij = U % dia(i) + 1, U % row(i + 1) - 1
        j = U % col(ij)
        sum = sum - U % val(ij) * x(j)
      end do
      x(i) = sum / U % val( U % dia(i) )
    end do

  ! Matrix is transposed, useful for Cholesky factorization which uses only L
  else
    do i = n, 1, -1
      sum = b(i)
      do ij = U % dia(i) + 1, U % row(i + 1) - 1
        j = U % col(ij)
        sum = sum - U % val(U % mir(ij)) * x(j)
      end do
      x(i) = sum / U % val( U % dia(i) )
    end do

  end if

  end subroutine
