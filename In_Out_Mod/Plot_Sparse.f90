!==============================================================================!
  subroutine Plot_Sparse(IO, name_out, A)
!------------------------------------------------------------------------------!
!   Prints compressed matrix out.                                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)            :: IO        !! parent class
  character(len=*),  intent(in) :: name_out  !! output file name
  type(Sparse_Type), intent(in) :: A         !! matrix to plit
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col, pos  ! row used to be "i", col used to be "j"
  logical :: found
  real    :: max_val, min_val, max_abs
!==============================================================================!

  if(A % n > 216) return  ! bigger than this, you can't see

  call Foul % Formatted_Write(' # Plotting the matrix:          ',  &
                              'white',                              &
                               name_out//'.fig',                    &
                              'bright red');
  open(9, file=name_out//'.fig')

  ! Write the header out
  call IO % Plot_Header(9)

  ! Followed by the compound
  write(9, '(3i2, 2i9)') 6, 0, 0, A % n * CM, A % n * CM

  ! Find maximum entry in the system matrix
  max_val = maxval(A % val)
  min_val = minval(A % val)
  max_abs = max(abs(min_val), abs(max_val))

  do row = 1, A % n
    do pos = A % row(row), A % row(row + 1) - 1
      col = A % col(pos)
      call IO % Plot_Circle(9, row, col, A % val(pos), min_val, max_val, 50)
    end do
  end do

  end subroutine
