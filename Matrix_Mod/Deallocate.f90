!==============================================================================!
  subroutine Matrix_Mod_Deallocate(a)
!------------------------------------------------------------------------------!
!   Deallocates memory occupied by type Matrix                                 !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Matrix_Type) :: a
!------------------------------------------------------------------------------!

  a % n        = 0
  a % nonzeros = 0
  deallocate(a % val)    ! value
  deallocate(a % col)    ! beginning of each row
  deallocate(a % row)    ! column positions
  deallocate(a % dia)    ! diagonal positions
  deallocate(a % mir)    ! position of the mirror entry

  end subroutine
