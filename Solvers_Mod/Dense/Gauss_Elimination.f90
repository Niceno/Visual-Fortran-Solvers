!==============================================================================!
  subroutine Solvers_Mod_Gauss_Elimination(U, b, A)
!------------------------------------------------------------------------------!
!>  Performs Gaussian elimination on the given matrix "A" and source term "b".
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type)   :: U  !! factorize upper triangular matrix
  real, dimension(:) :: b
  type(Dense_Type)   :: A  !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, bw, n
  real    :: mult
!==============================================================================!

  ! Take some aliases
  n  = A % n
  bw = A % bw

  ! Copy the matrix first
  do i = 1, n  ! <-A
    do j = 1, n
      U % val(i,j) = A % val(i,j)
    end do
  end do       ! A->

  ! Make elimination for resulting matrix
  do k = 1, n - 1  ! <-A
    do i = k + 1, min(k + bw, n)
      mult = U % val(i,k) / U % val(k,k)
      U % val(i,k) = 0.0
      call IO % Plot_Dense("factorization", U, B=A, targ=(/i,k,PINK2/), src1=(/k,k,CYAN/))
      do j = k + 1, min(k + bw, n)
        U % val(i,j) = U % val(i,j) - mult * U % val(k,j)
        call IO % Plot_Dense("factorization", U, B=A, targ=(/i,j,PINK2/), src1=(/k,j,CYAN/))
      end do
      b(i) = b(i) - mult * b(k)
    end do
  end do           ! A->

  call IO % Plot_Snippet(__FILE__, '<-A', 'A->')

  end subroutine
