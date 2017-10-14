!==============================================================================!
  subroutine Print_Matrix(message, f_matrix)
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  character(len=*)          :: message
  real,      dimension(:,:) :: f_matrix
!------------------------------------------------------------------------------!
  integer :: row, col  ! row used to be "i", col used to be "j"
!==============================================================================!

  write(*,*) message

  do row = 1, size(f_matrix, 1)
    do col = 1, size(f_matrix, 2)
      write(*,"(f8.3)",advance="no") f_matrix(row,col)
    end do       
    write(*,*) ""
  end do           

  end subroutine Print_Matrix
