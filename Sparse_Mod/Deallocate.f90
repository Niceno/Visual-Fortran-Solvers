!==============================================================================!
  subroutine Sparse_Mod_Deallocate(a)
!------------------------------------------------------------------------------!
!   Deallocates memory occupied by type Sparse (matrix)                        !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: a
!------------------------------------------------------------------------------!

  a % n        = 0
  a % nonzeros = 0
  deallocate(a % val)    ! value
  deallocate(a % col)    ! beginning of each row
  deallocate(a % row)    ! column positions
  deallocate(a % dia)    ! diagonal positions
  deallocate(a % mir)    ! position of the mirror entry

  end subroutine
