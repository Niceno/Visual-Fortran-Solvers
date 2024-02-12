!==============================================================================!
  subroutine Plot_Dense(IO, name_out, A, B, targ, src1, src2, src3)
!------------------------------------------------------------------------------!
!>  Plots the dense matrix A out in the Xfig file format.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)                     :: IO        !! parent class
  character(len=*), intent(in)           :: name_out  !! output file name
  type(Dense_Type), intent(in)           :: A         !! matrix to plit
  type(Dense_Type), intent(in), optional :: B         !! background matrix
  integer,          intent(in), optional :: targ(3)   !! row, col, color
  integer,          intent(in), optional :: src1(3)
  integer,          intent(in), optional :: src2(3)
  integer,          intent(in), optional :: src3(3)
!-----------------------------------[Locals]-----------------------------------!
  integer        :: row, col
  integer        :: plot_x, plot_y
  integer, save  :: cnt = 0
  real           :: max_val, min_val
  character(6)   :: frame = '_00000'
  character(512) :: full_name = ''
!==============================================================================!

  !------------------------------------------------!
  !   Exit if matrix is too big to be visualized   !
  !------------------------------------------------!
  if(A % n > 216) return  ! bigger than this, you can't see

  !------------------!
  !   Set the name   !
  !------------------!
  if(present(targ) .or.  &
     present(src1) .or. present(src2) .or. present(src3)) then
    cnt = cnt + 1
    write(frame(2:6), '(i5.5)') cnt
    full_name = name_out//frame//'.fig'
  else
    full_name = name_out//'.fig'
  end if
  open(9, file=trim(full_name))

  call Foul % Formatted_Write(' # Plotting the matrix:          ',  &
                              'white',                              &
                               trim(full_name),                     &
                              'bright red');

  !--------------------------!
  !   Write the header out   !
  !--------------------------!
  call IO % Plot_Header(9)

  !------------------------------!
  !   Followed by the compound   !
  !------------------------------!
  write(9, '(3i2, 2i9)') 6, 0, 0, A % n * CM, A % n * CM

  !----------------------------!
  !   Do the actual plotting   !
  !----------------------------!

  ! This is just to deserve a "bounding box", to avoid jittering amimations
  call IO % Plot_Box(9,     1,     1, WHITE, 999)
  call IO % Plot_Box(9, A % n, A % n, WHITE, 999)

  ! Plot the background matrix, if specified
  if(present(B)) then

    min_val = minval(B % val(:,:))
    max_val = maxval(B % val(:,:))

    do row = 1, B % n
      do col = 1, B % n
        call IO % Plot_Square(9, row, col, B % val(row, col), min_val, max_val, 60)
      end do
    end do
  end if

  min_val = minval(A % val(:,:))
  max_val = maxval(A % val(:,:))

  ! Browse through matrix and plot it :-)
  do row = 1, A % n
    do col = 1, A % n

      ! This is normal, scaled circle for a matrix value
      call IO % Plot_Circle(9, row, col, A % val(row, col), min_val, max_val, 50)

      ! If target is present
      if(present(targ)) then
        if(row .eq. targ(1) .and. col .eq. targ(2)) then
          call IO % Plot_Box(9, row, col, targ(3), 55)
        end if
      end if

      ! If source
      if(present(src1)) then
        if(row .eq. src1(1) .and. col .eq. src1(2)) then
          call IO % Plot_Box(9, row, col, src1(3), 51)
        end if
      end if
      if(present(src2)) then
        if(row .eq. src2(1) .and. col .eq. src2(2)) then
          call IO % Plot_Box(9, row, col, src2(3), 52)
        end if
      end if
      if(present(src3)) then
        if(row .eq. src3(1) .and. col .eq. src3(2)) then
          call IO % Plot_Box(9, row, col, src3(3), 53)
        end if
      end if
    end do
  end do

  close(9)

  end subroutine
