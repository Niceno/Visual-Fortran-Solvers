!==============================================================================!
  subroutine In_Out_Mod_Print_Matrix(message, f_matrix)
!------------------------------------------------------------------------------!
!   Prints full matrix out.                                                    !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  character(len=*)          :: message
  real,      dimension(:,:) :: f_matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col  ! row used to be "i", col used to be "j"
!==============================================================================!

  if(size(f_matrix,1) > 64) return

  write(*,*) message

  do row = 1, size(f_matrix, 1)
    do col = 1, size(f_matrix, 2)
      write(*,"(f6.1)",advance="no") f_matrix(row,col)
    end do
    write(*,*) ""
  end do

  end subroutine
