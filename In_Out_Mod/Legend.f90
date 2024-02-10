!==============================================================================!
  subroutine Print_Legend(IO, row, max_val)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)  :: IO
  integer, intent(in) :: row
  real,    intent(in) :: max_val
!==============================================================================!

  if(row .eq. 1) then
    call Foul % Formatted_Write('  ', 'red',  &
                        '  ', 'red background_red', forward='no')
    print '(a3,1es11.3)', '  >', SCALE(1) * max_val
  else if(row .eq. 2) then
    call Foul % Formatted_Write('  ', 'yellow',  &
                        '  ', 'yellow background_yellow', forward='no')
    print '(a3,1es11.3)', '  >', SCALE(2) * max_val
  else if(row .eq. 3) then
    call Foul % Formatted_Write('  ', 'green',  &
                        '  ', 'green background_green', forward='no')
    print '(a3,1es11.3)', '  >', SCALE(3) * max_val
  else if(row .eq. 4) then
    call Foul % Formatted_Write('  ', 'blue',  &
                        '  ', 'blue background_blue', forward='no')
    print '(a3,1es11.3)', '  >', SCALE(4) * max_val
  else if(row .eq. 5) then
    call Foul % Formatted_Write('  ', 'magenta',  &
                        '  ', 'magenta background_magenta', forward='no')
    print '(a3,1es11.3)', '  =', 0.0
  else
    print *, ""
  end if

  end subroutine

