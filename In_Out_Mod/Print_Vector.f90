!==============================================================================!
  subroutine Print_Vector(IO, message, vector)
!------------------------------------------------------------------------------!
!   Prints vector out.                                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type) :: IO
  character(len=*)   :: message
  real, dimension(:) :: vector
!-----------------------------------[Locals]-----------------------------------!
  integer      :: row
  character(6) :: item
!==============================================================================!

  if(size(vector) > 64) return

  print *, message

  do row = 1, size(vector)
    write(item(1:6), '(f6.1)') vector(row)
    call Foul % Formatted_Write(item, 'green', forward='yes')
  end do

  end subroutine
