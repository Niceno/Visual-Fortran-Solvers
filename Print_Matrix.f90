!==============================================================================!
  subroutine Print_Matrix(message, matrix)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  character(len=*)          :: message
  real,      dimension(:,:) :: matrix
!------------------------------------------------------------------------------!
  integer :: row, col  ! row used to be "i", col used to be "j"
!==============================================================================!

  write(*,*) message

  do row = 1, size(matrix, 1)
    do col = 1, size(matrix, 2)
      write(*,"(f8.3)",advance="no") matrix(row,col)
    end do       
    write(*,*) ""
  end do           

  end subroutine Print_Matrix
