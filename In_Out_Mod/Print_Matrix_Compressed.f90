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
  integer :: row, col, pos  ! row used to be "i", col used to be "j"
  logical :: found
!==============================================================================!

  if(sparse % n > 64) return

  write(*,*) message

  do row = 1, sparse % n
    do col = 1, sparse % n
      found = .false.

      ! Look for position row, col in the compressed
      ! sparse and print if you have found it
      do pos = sparse % row(row), sparse % row(row + 1) - 1
        if( sparse % col(pos) == col ) then
          write(*,"(f6.1)",advance="no") sparse % val(pos)
          found = .true.
        end if
      end do

      ! If you haven't found it, print something else
      if( .not. found ) then
        write(*,"(a6)",advance="no") "   .   "
      end if
    end do
    write(*,*) ""
  end do

  end subroutine
