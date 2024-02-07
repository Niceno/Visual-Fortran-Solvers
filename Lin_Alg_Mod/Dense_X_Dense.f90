!==============================================================================!
  subroutine Lin_Alg_Mod_Dense_X_Dense(C, A, B)
!------------------------------------------------------------------------------!
!>  Multiplies two dense (full) matrices.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type) :: C  !! result matrix
  type(Dense_Type) :: A  !! operand matrix
  type(Dense_Type) :: B  !! operand matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k
!==============================================================================!

  Assert(A % n .eq. B % n)
  Assert(A % n .eq. C % n)

  do i = 1, A % n
    do j = 1, A % n
      C % val(i,j) = 0
      do k = 1, A % n
        C % val(i,j) = C % val(i,j) + A % val(i,k) * B % val(k,j)
      end do
    end do
  end do

  end subroutine
