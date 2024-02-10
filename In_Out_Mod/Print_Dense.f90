!==============================================================================!
  subroutine Print_Dense(IO, message, A)
!------------------------------------------------------------------------------!
!>  Prints a dense matrix out.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)           :: IO       !! parent class
  character(len=*), intent(in) :: message  !! message to print along
  type(Dense_Type), intent(in) :: A        !! matrix to print
!-----------------------------------[Locals]-----------------------------------!
  integer      :: row, col  ! row used to be "i", col used to be "j"
  character(2) :: item
  real         :: max_val
!==============================================================================!

  if(A % n > 64) return

  print *, message

  ! Find maximum entry in the system matrix
  max_val = 0
  max_val = maxval(A % val(:,:))

  ! Print the matrix
  do row = 1, A % n
    do col = 1, A % n
      write(item(1:2), '(a2)') '  '

      if(      abs(A % val(row, col)) > SCALE(1) * max_val) then
        call Foul % Formatted_Write(item, 'red background_red',          &
                                    forward='no')
      else if( abs(A % val(row, col)) > SCALE(2) * max_val) then
        call Foul % Formatted_Write(item, 'yellow background_yellow',    &
                                    forward='no')
      else if( abs(A % val(row, col)) > SCALE(3) * max_val) then
        call Foul % Formatted_Write(item, 'green background_green',      &
                                    forward='no')
      else if( abs(A % val(row, col)) > SCALE(4) * max_val) then
        call Foul % Formatted_Write(item, 'blue background_blue',        &
                                    forward='no')
      else
        call Foul % Formatted_Write(item, 'magenta background_magenta',  &
                                    forward='no')
      end if

    end do

    ! Print legend too
    call IO % Print_Legend(row, max_val)

  end do

  end subroutine
