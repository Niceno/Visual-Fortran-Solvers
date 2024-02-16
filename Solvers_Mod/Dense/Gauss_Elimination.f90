!==============================================================================!
  subroutine Solvers_Mod_Gauss_Elimination(U, b, A)
!------------------------------------------------------------------------------!
!>  Performs Gaussian elimination on the given matrix "A" and source term "b".
!------------------------------------------------------------------------------!
!   After the Gaussinal elimination, the matrix looks and is stored like this: !
!                                                                              !
!       | L11 L12 L13 L14 L15 |                                                !
!       |     L22 L23 L24 L25 |                                                !
!   U = |         L33 L34 L35 |                                                !
!       |             L44 L45 |                                                !
!       |                 L55 |                                                !
!                                                                              !
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

  print *, '# Factorizing dense matrix with Gaussian elimination'

  ! Take some aliases
  n  = A % n
  bw = A % bw

  ! Set the type of the matrix (in a sense)
  U % text_u ="U"
  U % text_l ="L=0"

  !---------------------------------------------------------------------!
  !   Initialize the values by copying the original matrix to U first   !
  !---------------------------------------------------------------------!
  do i = 1, n  ! <-A
    do j = 1, n
      U % val(i,j) = A % val(i,j)
    end do
  end do       ! A->

  !--------------------------------------------------------------------------!
  !   Perform the factorization (elimination here) on the resulting matrix   !
  !--------------------------------------------------------------------------!
  do k = 1, n - 1  ! <-A
    do i = k + 1, min(k + bw, n)
      mult = U % val(i,k) / U % val(k,k)
      U % val(i,k) = 0.0
      call IO % Plot_Dense("dens_gauss", U, B=A, targ=(/i,k/), src1=(/k,k/))
      do j = k + 1, min(k + bw, n)
        U % val(i,j) = U % val(i,j) - mult * U % val(k,j)
        call IO % Plot_Dense("dens_gauss", U, B=A, targ=(/i,j/), src1=(/k,j/))
      end do
      b(i) = b(i) - mult * b(k)
    end do
  end do           ! A->

  call IO % Plot_Snippet(__FILE__, '<-A', 'A->')

  end subroutine
