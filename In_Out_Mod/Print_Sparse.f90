!==============================================================================!
  subroutine In_Out_Mod_Print_Sparse(message, sparse)
!------------------------------------------------------------------------------!
!   Prints compressed matrix out.                                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  character(len=*)  :: message
  type(Sparse_Type) :: sparse
!-----------------------------------[Locals]-----------------------------------!
  integer      :: row, col, pos  ! row used to be "i", col used to be "j"
  logical      :: found
  character(2) :: item
  real         :: max_val
!==============================================================================!

  if(sparse % n > 64) return

  print *, message

  ! Find maximum entry in the system matrix
  max_val = 0
  max_val = maxval(sparse % val)

  do row = 1, sparse % n
    do col = 1, sparse % n
      found = .false.

      ! Look for position row, col in the compressed
      ! sparse and print if you have found it
      do pos = sparse % row(row), sparse % row(row + 1) - 1

        write(item(1:2), '(a2)') '  '

        if( sparse % col(pos) == col ) then

          if(      abs(sparse % val(pos)) > SCALE(1) * max_val) then
            call Foul_Mod_Write(item, 'red background_red', forward='no')
          else if( abs(sparse % val(pos)) > SCALE(2) * max_val) then
            call Foul_Mod_Write(item, 'yellow background_yellow', forward='no')
          else if( abs(sparse % val(pos)) > SCALE(3) * max_val) then
            call Foul_Mod_Write(item, 'green background_green', forward='no')
          else if( abs(sparse % val(pos)) > SCALE(4) * max_val) then
            call Foul_Mod_Write(item, 'blue background_blue', forward='no')
          else
            call Foul_Mod_Write(item, 'magenta background_magenta', forward='no')
          end if

          found = .true.
        end if
      end do

      ! If you haven't found it, print something else
      if( .not. found ) then
        call Foul_Mod_Write(item, 'black', forward='no')
      end if
    end do

    ! Print legend too
    call In_Out_Mod_Legend(row, max_val)

  end do

  end subroutine
