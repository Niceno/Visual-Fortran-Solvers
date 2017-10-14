!==============================================================================!
  subroutine Print_Matrix_Compressed(message, c_matrix)
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  character(len=*) :: message
  type(Matrix)     :: c_matrix
!------------------------------------------------------------------------------!
  integer :: row, col, pos  ! row used to be "i", col used to be "j"
  logical :: found
!==============================================================================!

  write(*,*) message

  do row = 1, c_matrix % n
    do col = 1, c_matrix % n
      found = .false.

      ! Look for position row, col in the compressed
      ! c_matrix and print if you have found it
      do pos = c_matrix % row(row), c_matrix % row(row + 1) - 1
        if( c_matrix % col(pos) == col ) then
          write(*,"(f8.3)",advance="no") c_matrix % val(pos)
          found = .true.
        end if
      end do

      ! If you haven't found it, print something else
      if( .not. found ) then
        write(*,"(a8)",advance="no") "    .    "     
      end if
    end do       
    write(*,*) ""
  end do           

  end subroutine Print_Matrix_Compressed
