!==============================================================================!
  subroutine Solvers_Mod_Gauss_Elimination(G, b, A)
!------------------------------------------------------------------------------!
!>  Performs Gaussian elimination on the given matrix "A" and source term "b".
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type)   :: G   ! upper triangular which remains
  real, dimension(:) :: b
  type(Dense_Type)   :: A
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k
  real    :: mult
!==============================================================================!

  ! Copy the matrix first
  do i = 1, A % n
    do j = 1, A % n
      G % val(i,j) = A % val(i,j)
    end do
  end do

  ! Make elimination for resulting matrix
  do k = 1, A % n - 1
    do i = k + 1, min(k + A % bw, A % n)
      mult = G % val(i,k) / G % val(k,k)
      G % val(i,k) = 0.0
      do j = k + 1, min(k + A % bw, A % n)
        G % val(i,j) = G % val(i,j) - mult * G % val(k,j)
      end do
      b(i) = b(i) - mult*b(k)
    end do
  end do

  end subroutine
