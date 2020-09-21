!==============================================================================!
  subroutine In_Out_Mod_Print_Matrix(message, full)
!------------------------------------------------------------------------------!
!   Prints full matrix out.                                                    !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  character(len=*)     :: message
  real, dimension(:,:) :: full
!-----------------------------------[Locals]-----------------------------------!
  integer      :: row, col  ! row used to be "i", col used to be "j"
  character(2) :: item
  real         :: max_val
!==============================================================================!

  if(size(full,1) > 64) return

  print *, message

  ! Find maximum entry in the system matrix
  max_val = 0
  max_val = maxval(full)

  ! Print the matrix
  do row = 1, size(full, 1)
    do col = 1, size(full, 2)
      write(item(1:2), '(a2)') '  '

      if( abs(full(row, col)) > 0.5 * max_val) then
        call Foul_Mod_Write(item, 'red background_red', forward='no')
      else if( abs(full(row, col)) > 0.01 * max_val) then
        call Foul_Mod_Write(item, 'yellow background_yellow', forward='no')
      else if( abs(full(row, col)) > 0.001 * max_val) then
        call Foul_Mod_Write(item, 'green background_green', forward='no')
      else if( abs(full(row, col)) > TINY * max_val) then
        call Foul_Mod_Write(item, 'blue background_blue', forward='no')
      else
        call Foul_Mod_Write(item, 'magenta background_magenta', forward='no')
      end if

    end do
    print *, ""
  end do

  end subroutine
