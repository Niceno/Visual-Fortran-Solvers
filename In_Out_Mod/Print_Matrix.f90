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
  integer :: row, col  ! row used to be "i", col used to be "j"
!==============================================================================!

  if(size(full,1) > 64) return

  write(*,*) message

  do row = 1, size(full, 1)
    do col = 1, size(full, 2)
      write(*,"(f6.1)",advance="no") full(row,col)
    end do
    write(*,*) ""
  end do

  end subroutine
