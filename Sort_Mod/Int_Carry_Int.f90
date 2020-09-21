!==============================================================================!
  recursive subroutine Sort_Mod_Int_Carry_Int(a, b)
!------------------------------------------------------------------------------!
!   Quick sort one integer array and carry another integer arral along         !
!                                                                              !
!   Adapted from: https://gist.github.com/1AdAstra1  (good work Olga)          !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer :: a(:)
  integer :: b(:)
!-----------------------------------[Locals]-----------------------------------!
  integer :: x, at
  integer :: bt
  integer :: i, j, n
!------------------------------------------------------------------------------!

  n = size(a, 1)
  x = a( (1+n) / 2 )
  i = 1
  j = n
  
  do
    do while (a(i) < x)
      i = i + 1
    end do
    do while (x < a(j))
      j = j - 1
    end do
    if (i >= j) exit

    ! Swap values in a and b
    at   = a(i);  bt   = b(i)
    a(i) = a(j);  b(i) = b(j)
    a(j) = at;    b(j) = bt

    i = i + 1
    j = j - 1
  end do

  if (1 < i - 1) call Sort_Mod_Int_Carry_Int(a(1:i-1), b(1:i-1))
  if (j + 1 < n) call Sort_Mod_Int_Carry_Int(a(j+1:n), b(j+1:n))

  end subroutine 
