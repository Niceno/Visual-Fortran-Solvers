!==============================================================================!
  subroutine Solvers_Mod_Sparse_Backward_Substitution(x, U, b, t, d_one)
!------------------------------------------------------------------------------!
!>  Performs backward substitution using a sparse matrix.
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Cholesky                                          !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x      !! resulting vector
  type(Sparse_Type)  :: U      !! factorized matrix, should be U in the caller
  real, dimension(:) :: b      !! right hand side vector
  logical,  optional :: t      !! use transposed matrix
  logical,  optional :: d_one  !! diagonal is 1, good for LU and LDL'
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, ij, n
  real    :: sum
  logical :: transposed = .false.
  logical :: diagonal_one = .false.
!==============================================================================!

  ! Take some aliases
  n = U % n      ! some checks would be possible


  ! Treat the optional arguments
  transposed = .false.           ! for some reason, this must be repeated
  if(present(t)) transposed = t

  diagonal_one = .false.  ! keep this, compiler issue
  if(present(d_one)) diagonal_one = d_one

  !---------------------------------------------------------------------!
  !   Here, j > i, therfore it is an U matrix, all regular, use as is   !
  !---------------------------------------------------------------------!
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

    ! Diagonal is not equal to 1
    if(.not. diagonal_one) then
      do i = n, 1, -1
        sum = b(i)
        do ij = U % dia(i) + 1, U % row(i + 1) - 1
          j = U % col(ij)
          sum = sum - U % val(U % mir(ij)) * x(j)
        end do
        x(i) = sum / U % val( U % dia(i) )
      end do
      call IO % Plot_Snippet(__FILE__, 52, 59)

    ! Diagonal is equal to 1, good for LU and LDL' methods
    else
      do i = n, 1, -1
        sum = b(i)
        do ij = U % dia(i) + 1, U % row(i + 1) - 1
          j = U % col(ij)
          sum = sum - U % val(U % mir(ij)) * x(j)
        end do
        x(i) = sum
      end do
      call IO % Plot_Snippet(__FILE__, 64, 71)

    end if

  end if

  end subroutine
