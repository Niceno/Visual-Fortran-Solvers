!==============================================================================!
  subroutine Lin_Alg_Mod_Dense_T(B, A)
!------------------------------------------------------------------------------!
!>  Transposes A dense (full) matrix.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type) :: B  !! resulting matrix
  type(Dense_Type) :: A  !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j
!==============================================================================!

  Assert(A % n .eq. B % n)

  do i = 1, A % n
    do j = 1, A % n
      B % val(j, i) = A % val(i, j)
    end do
  end do

  end subroutine
