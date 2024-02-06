!==============================================================================!
  subroutine In_Out_Mod_Print_Dense(message, full)
!------------------------------------------------------------------------------!
!   Prints full matrix out.                                                    !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  character(len=*) :: message
  type(Dense_Type) :: full
!-----------------------------------[Locals]-----------------------------------!
  integer      :: row, col  ! row used to be "i", col used to be "j"
  character(2) :: item
  real         :: max_val
!==============================================================================!

  if(full % n > 64) return

  print *, message

  ! Find maximum entry in the system matrix
  max_val = 0
  max_val = maxval(full % val(:,:))

  ! Print the matrix
  do row = 1, full % n
    do col = 1, full % n
      write(item(1:2), '(a2)') '  '

      if(      abs(full % val(row, col)) > SCALE(1) * max_val) then
        call Foul_Mod_Write(item, 'red background_red', forward='no')
      else if( abs(full % val(row, col)) > SCALE(2) * max_val) then
        call Foul_Mod_Write(item, 'yellow background_yellow', forward='no')
      else if( abs(full % val(row, col)) > SCALE(3) * max_val) then
        call Foul_Mod_Write(item, 'green background_green', forward='no')
      else if( abs(full % val(row, col)) > SCALE(4) * max_val) then
        call Foul_Mod_Write(item, 'blue background_blue', forward='no')
      else
        call Foul_Mod_Write(item, 'magenta background_magenta', forward='no')
      end if

    end do

    ! Print legend too
    call In_Out_Mod_Legend(row, max_val)

  end do

  end subroutine
