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
  integer :: i, j, k
  real    :: mult
!==============================================================================!

  ! Copy the matrix first
  do i = 1, A % n
    do j = 1, A % n
      U % val(i,j) = A % val(i,j)
    end do
  end do

  ! Make elimination for resulting matrix
  do k = 1, A % n - 1
    do i = k + 1, min(k + A % bw, A % n)
      mult = U % val(i,k) / U % val(k,k)
      U % val(i,k) = 0.0
      call IO % Plot_Dense("factorization", U, ijk=(/i,j,k/), targ=(/i,k/), src1=(/k,k/))
      do j = k + 1, min(k + A % bw, A % n)
        U % val(i,j) = U % val(i,j) - mult * U % val(k,j)
        call IO % Plot_Dense("factorization", U, ijk=(/i,j,k/), targ=(/i,j/), src1=(/k,j/))
      end do
      b(i) = b(i) - mult*b(k)
    end do
  end do

  call IO % Plot_Snippet(__FILE__, 24, 35)

  end subroutine
