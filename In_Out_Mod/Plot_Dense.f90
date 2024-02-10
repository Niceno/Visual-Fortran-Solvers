!==============================================================================!
  subroutine Plot_Dense(IO, name_out, A)
!------------------------------------------------------------------------------!
!>  Plots the dense matrix A out in the Xfig file format.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)           :: IO        !! parent class
  character(len=*), intent(in) :: name_out  !! output file name
  type(Dense_Type), intent(in) :: A         !! matrix to plit
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col  ! row used to be "plot_x", col used to be "plot_y"
  integer :: plot_x, plot_y
  real    :: max_val, min_val, max_abs
!==============================================================================!

  if(A % n > 216) return  ! bigger than this, you can't see

  print '(a,a)', " # Plotting the matrix: ", name_out
  open(9, file=name_out)

  ! Write the header out
  call IO % Plot_Header(9)

  ! Find maximum entry in the system matrix
  min_val = minval(A % val(:,:))
  max_val = maxval(A % val(:,:))
  max_abs = max(abs(min_val), abs(max_val))

  ! Print the matrix, only the upper part
  do row = 1, A % n
    do col = 1, A % n
      call IO % Plot_Circle(9, row, col, A % val(row, col), min_val, max_val)
    end do
  end do

  close(9)

  end subroutine
