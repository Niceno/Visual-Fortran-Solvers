!==============================================================================!
  function Integer_To_String(Foul, integer_value, string_length)
!------------------------------------------------------------------------------!
!   Converts the supplied integer to a character string
!------------------------------------------------------------------------------!
!   Input:
!   integer_value: Value to be converted
!   string_length: Minimal length of resulting string; result will be 
!                  right-adjusted if desired length is greater than actual
!                  length
!
!   Output:
!   Return value: String resulting from conversion
!------------------------------------------------------------------------------!
!   1 + floor(log10(real(abs(integer_value)))) =
!   number of digits needed to represent integer_value
!
!   int(sign(1, -integer_value) + 1) / 2) =
!   0, if integer_value is positive
!   1, if integer_value is negative
!   (used to make room for sign)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Foul_Type)    :: Foul
  integer, intent(in) :: integer_value, string_length
!-----------------------------------[Locals]-----------------------------------!
  character(max(1 + floor(log10(real(abs(integer_value))))    &
                  + int((sign(1, -integer_value) + 1) / 2),   &
                    string_length, 1)) :: Integer_To_String
  character(16) :: string_buffer
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Foul)
!==============================================================================!

  write(string_buffer, '(I16)') integer_value
  string_buffer = adjustl(string_buffer)

  if (len_trim(string_buffer) < len(Integer_To_String)) then
    Integer_To_String = repeat(' ',  len(Integer_To_String)       &
                                   - len_trim(string_buffer)) //  &
                        trim(string_buffer)
  else
    Integer_To_String = trim(string_buffer)
  end if

  end function

