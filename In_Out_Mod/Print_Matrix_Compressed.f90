!==============================================================================!
  subroutine In_Out_Mod_Print_Matrix_Compressed(message, sparse)
!------------------------------------------------------------------------------!
!   Prints compressed matrix out.                                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  character(len=*)  :: message
  type(Matrix_Type) :: sparse
!-----------------------------------[Locals]-----------------------------------!
  integer      :: row, col, pos  ! row used to be "i", col used to be "j"
  logical      :: found
  character(2) :: item
!==============================================================================!

  if(sparse % n > 64) return

  print *, message

  do row = 1, sparse % n
    do col = 1, sparse % n
      found = .false.

      ! Look for position row, col in the compressed
      ! sparse and print if you have found it
      do pos = sparse % row(row), sparse % row(row + 1) - 1

        write(item(1:2), '(a2)') '  '

        if( sparse % col(pos) == col ) then

          ! Diagonal terms in red
          if(row .eq. col) then
            call Foul_Mod_Write(item, 'red background_red', forward='no')

          ! Off-diagonal terms
          else
            call Foul_Mod_Write(item, 'green background_green', forward='no')
          end if

          found = .true.
        end if
      end do

      ! If you haven't found it, print something else
      if( .not. found ) then
        call Foul_Mod_Write(item, 'black', forward='no')
      end if
    end do
    print *, ''
  end do

  end subroutine
