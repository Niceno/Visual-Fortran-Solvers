!==============================================================================!
  subroutine Sparse_Deallocate(A)
!------------------------------------------------------------------------------!
!>  Deallocates memory occupied by type Sparse (matrix).
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Sparse_Type) :: A  !! parent class
!------------------------------------------------------------------------------!

  A % n        = 0
  A % nonzeros = 0
  deallocate(A % val)    ! value
  deallocate(A % col)    ! beginning of each row
  deallocate(A % row)    ! column positions
  deallocate(A % dia)    ! diagonal positions
  deallocate(A % mir)    ! position of the mirror entry

  end subroutine
