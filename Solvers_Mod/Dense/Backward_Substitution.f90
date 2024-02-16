!==============================================================================!
  subroutine Solvers_Mod_Dense_Backward_Substitution(x, U, b, t, d_one)
!------------------------------------------------------------------------------!
!>  Performs backward substitution on a square (full) matrix.                  !
!>  It will address only elements in upper trinangular part.                   !
!------------------------------------------------------------------------------!
!   Note:                                                                      !
!                                                                              !
!   * It is called called by:                                                  !
!     - Solvers_Mod_Cholesky                                                   !
!     - Solvers_Mod_Gauss                                                      !
!     - Solvers_Mod_Lu                                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  real, dimension(:) :: x      !! resulting vector
  type(Dense_Type)   :: U      !! factorized matrix, should be U in the caller
  real, dimension(:) :: b      !! right hand side vector
  logical,  optional :: t      !! use transposed matrix
  logical,  optional :: d_one  !! diagonal is 1, good for LU and LDL'
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, n, bw
  real    :: sum
  logical :: transposed   = .false.
  logical :: diagonal_one = .false.
!==============================================================================!

  ! Take some aliases
  n  = U % n
  bw = U % bw

  ! Treat the optional arguments
  transposed = .false.  ! keep this, compiler issue
  if(present(t)) transposed = t

  diagonal_one = .false.  ! keep this, compiler issue
  if(present(d_one)) diagonal_one = d_one

  !---------------------------------------------------------------------!
  !   Here, j > i, therfore it is an U matrix, all regular, use as is   !
  !---------------------------------------------------------------------!
  if(.not. transposed) then
    do i = n, 1, -1  ! <-A
      sum = b(i)
      call IO % Plot_Dense_System("dens_back", U, x, b, srcb=(/i/))
      do j = i+1, min(i + bw, n)
        sum = sum - U % val(i,j) * x(j)
        call IO % Plot_Dense_System("dens_back", U, x, b, srca=(/i,j/), srcx=(/j/))
      end do
      x(i) = sum / U % val(i,i)
      call IO % Plot_Dense_System("dens_back", U, x, b, tarx=(/i/), srca=(/i,i/))
    end do           ! A->
    call IO % Plot_Snippet(__FILE__, '<-A', 'A->')

  !------------------------------------------------------------------------!
  !   Matrix is transposed, useful for factorizations which store only L   !
  !------------------------------------------------------------------------!
  else

    ! Diagonal is not equal to 1 (called from Cholesky method)
    if(.not. diagonal_one) then
      do i = n, 1, -1  ! <-B
        sum = b(i)
        call IO % Plot_Dense_System("dens_back", U, x, b, srcb=(/i/))
        do j = i+1, min(i + bw, n)
          sum = sum - U % val(j,i) * x(j)
          call IO % Plot_Dense_System("dens_back", U, x, b, srca=(/j,i/), srcx=(/j/))
        end do
        x(i) = sum / U % val(i,i)
        call IO % Plot_Dense_System("dens_back", U, x, b, tarx=(/i/), srca=(/i,i/))
      end do           ! B->
      call IO % Plot_Snippet(__FILE__, '<-B', 'B->')

    ! Diagonal is equal to 1 (called from LU and LDL' methods)
    else
      do i = n, 1, -1  ! <-C
        sum = b(i)
        call IO % Plot_Dense_System("dens_back", U, x, b, srcb=(/i/))
        do j = i+1, min(i + bw, n)
          sum = sum - U % val(j,i) * x(j)
          call IO % Plot_Dense_System("dens_back", U, x, b, srca=(/j,i/), srcx=(/j/))
        end do
        x(i) = sum
        call IO % Plot_Dense_System("dens_back", U, x, b, tarx=(/i/))
      end do           ! C->
      call IO % Plot_Snippet(__FILE__, '<-C', 'C->')

    end if

  end if

  end subroutine
