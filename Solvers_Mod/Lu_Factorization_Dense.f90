!==============================================================================!
  subroutine Solvers_Mod_Lu_Factorization_Dense(f, G, A, bw)
!------------------------------------------------------------------------------!
!   Performs LU decomposition of the give matrix "A" and stores L and U in     !
!   "f" and "G".  This subroutine was derived from Gaussian_Elimination.       !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type) :: f   ! matrix L
  type(Dense_Type) :: G   ! matrix U
  type(Dense_Type) :: A
  integer          :: bw  ! band width
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, n
  real    :: mult
!==============================================================================!

  n = A % n  ! some checks would be possible

  ! Initialize L (f) and U (G)
  do i = 1, n
    do j = 1, n
      f % val(i,j) = 0.0
      G % val(i,j) = A % val(i,j)
    end do
  end do
  do i = 1, n
    f % val(i,i) = 1.0
  end do

  ! Make elimination for resulting matrix
  do k = 1, n-1
    do i = k+1, min(k+bw,n)
      mult = G % val(i,k) / G % val(k,k)
      f % val(i,k) = mult  ! store multiplier in L
      G % val(i,k) = 0.0   ! set lower part of U to 0
      do j = k+1, min(k+bw,n)
        G % val(i,j) = G % val(i,j) - mult * G % val(k,j)
      end do
    end do
  end do

  end subroutine
