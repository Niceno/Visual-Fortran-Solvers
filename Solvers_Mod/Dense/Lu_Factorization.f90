!==============================================================================!
  subroutine Solvers_Mod_Dense_Lu_Factorization(F, G, A)
!------------------------------------------------------------------------------!
!   Performs LU decomposition of the give matrix "A" and stores L and U in     !
!   "F" and "G".  This subroutine was derived from Gaussian_Elimination.       !
!                                                                              !
!  Called by:                                                                  !
!  - Solvers_Mod_Lu                                                            !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type) :: F   !! matrix L
  type(Dense_Type) :: G   !! matrix U
  type(Dense_Type) :: A
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k
  real    :: mult
!==============================================================================!

  ! Initialize L (F) and U (G)
  do i = 1, A % n
    do j = 1, A % n
      F % val(i,j) = 0.0
      G % val(i,j) = A % val(i,j)
    end do
  end do
  do i = 1, A % n
    F % val(i,i) = 1.0
  end do

  ! Make elimination for resulting matrix
  do k = 1, A % n - 1
    do i = k + 1, min(k + A % bw, A % n)
      mult = G % val(i,k) / G % val(k,k)
      F % val(i,k) = mult  ! store multiplier in L
      G % val(i,k) = 0.0   ! set lower part of U to 0
      do j = k + 1, min(k + A % bw, A % n)
        G % val(i,j) = G % val(i,j) - mult * G % val(k,j)
      end do
    end do
  end do

  end subroutine
