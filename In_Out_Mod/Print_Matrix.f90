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
  character(6) :: item
!==============================================================================!

  if(size(full,1) > 64) return

  print *, message

  do row = 1, size(full, 1)
    do col = 1, size(full, 2)
      write(item(1:6), '(f6.1)') full(row,col)

      ! Diagonal terms in red
      if(row .eq. col) then
        call write_formatted(item, 'red', forward='no')

      ! Off-diagonal terms
      else
        if(abs(full(row,col)) < TINY) then
          call write_formatted(item, 'blue', forward='no')
        else
          call write_formatted(item, 'green', forward='no')
        end if
      end if

    end do
    print *, ""
  end do

  end subroutine
