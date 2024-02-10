!==============================================================================!
  subroutine Print_Sparse(IO, message, A)
!------------------------------------------------------------------------------!
!>  Prints compressed matrix out.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type) :: IO       !! parent class
  character(len=*)   :: message  !! message to print along
  type(Sparse_Type)  :: A        !! matrix to print
!-----------------------------------[Locals]-----------------------------------!
  integer      :: row, col, pos  ! row used to be "i", col used to be "j"
  logical      :: found
  character(2) :: item
  real         :: max_val
!==============================================================================!

  if(A % n > 64) return

  print *, message

  ! Find maximum entry in the system matrix
  max_val = 0
  max_val = maxval(A % val)

  do row = 1, A % n
    do col = 1, A % n
      found = .false.

      ! Look for position row, col in the compressed
      ! A and print if you have found it
      do pos = A % row(row), A % row(row + 1) - 1

        write(item(1:2), '(a2)') '  '

        if( A % col(pos) == col ) then

          if(      abs(A % val(pos)) > SCALE(1) * max_val) then
            call Foul % Formatted_Write(item, 'red background_red',          &
                                        forward='no')
          else if( abs(A % val(pos)) > SCALE(2) * max_val) then
            call Foul % Formatted_Write(item, 'yellow background_yellow',    &
                                        forward='no')
          else if( abs(A % val(pos)) > SCALE(3) * max_val) then
            call Foul % Formatted_Write(item, 'green background_green',      &
                                        forward='no')
          else if( abs(A % val(pos)) > SCALE(4) * max_val) then
            call Foul % Formatted_Write(item, 'blue background_blue',        &
                                        forward='no')
          else
            call Foul % Formatted_Write(item, 'magenta background_magenta',  &
                                        forward='no')
          end if

          found = .true.
        end if
      end do

      ! If you haven't found it, print something else
      if( .not. found ) then
        call Foul % Formatted_Write(item, 'black', forward='no')
      end if
    end do

    ! Print legend too
    call IO % Print_Legend(row, max_val)

  end do

  end subroutine
