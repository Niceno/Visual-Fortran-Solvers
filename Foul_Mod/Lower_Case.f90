!==============================================================================!
  subroutine Lower_Case(string)
!------------------------------------------------------------------------------!
!   Helper function
!   Converts the supplied string to lower case
!------------------------------------------------------------------------------!
!   Input:
!   string: Variable-length character string that is to be converted
!
!   Output:
!   string: String converted to lower case; conversion is done in place
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  character(*), intent(inout) :: string
!------------------------------------------------------------------------------!
  integer :: i, character_code
!==============================================================================!

  do i = 1, len_trim(string)
    character_code = ichar(string(i : i))

    if (character_code >= 65 .and. character_code <= 90) then
      string(i : i) = char(character_code + 32)
    end if
  end do

  end subroutine

