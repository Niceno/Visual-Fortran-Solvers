!==============================================================================!
  subroutine Plot_Dense(IO, name_out, A, ijk)
!------------------------------------------------------------------------------!
!>  Plots the dense matrix A out in the Xfig file format.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)           :: IO        !! parent class
  character(len=*), intent(in) :: name_out  !! output file name
  type(Dense_Type), intent(in) :: A         !! matrix to plit
  integer,          optional   :: ijk(3)
!-----------------------------------[Locals]-----------------------------------!
  integer        :: row, col
  integer        :: plot_x, plot_y
  integer, save  :: cnt = 0
  real           :: max_val, min_val, max_abs
  character(6)   :: frame = '_00000'
  character(512) :: full_name = ''
!==============================================================================!

  if(A % n > 216) return  ! bigger than this, you can't see

  ! Set the name
  if(.not. present(ijk)) then
    full_name = name_out//'.fig'
  else
    cnt = cnt + 1
    write(frame(2:6), '(i5.5)') cnt
    full_name = name_out//frame//'.fig'
  end if
  open(9, file=trim(full_name))

  call Foul % Formatted_Write(' # Plotting the matrix:          ',  &
                              'white',                              &
                               trim(full_name),                     &
                              'bright red');

  ! Write the header out
  call IO % Plot_Header(9)

  ! Followed by the compound
  write(9, '(3i2, 2i9)') 6, 0, 0, A % n * XFIG_CM, A % n * XFIG_CM

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
