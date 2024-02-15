!==============================================================================!
  subroutine Plot_Dense(IO, name_out, A, B, targ, src1, src2, src3)
!------------------------------------------------------------------------------!
!>  Plots the dense matrix A out in the Xfig file format.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)                     :: IO        !! parent class
  character(len=*), intent(in)           :: name_out  !! output file name
  type(Dense_Type), intent(in)           :: A         !! matrix to plot
  type(Dense_Type), intent(in), optional :: B         !! background matrix
  integer,          intent(in), optional :: targ(3)   !! row, col, color
  integer,          intent(in), optional :: src1(3)
  integer,          intent(in), optional :: src2(3)
  integer,          intent(in), optional :: src3(3)
!-----------------------------------[Locals]-----------------------------------!
  integer        :: row, col, n
  integer, save  :: cnt = 0
  real           :: max_v, min_v
  character(6)   :: frame = '_00000'
  character(512) :: full_name = ''
!==============================================================================!

  ! Take an alias
  n = A % n

  !------------------------------------------------!
  !                                                !
  !   Exit if matrix is too big to be visualized   !
  !                                                !
  !------------------------------------------------!
  if(n > 216) return  ! bigger than this, you can't see

  !------------------!
  !                  !
  !   Set the name   !
  !                  !
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
  !                          !
  !   Write the header out   !
  !                          !
  !--------------------------!
  call IO % Plot_Header(9)

  !------------------------------!
  !                              !
  !   Followed by the compound   !
  !                              !
  !------------------------------!
  write(9, '(3i2, 2i9)') 6, 0, 0, n * CM, n * CM

  !----------------------------!
  !                            !
  !   Do the actual plotting   !
  !                            !
  !----------------------------!

  ! This is just to deserve a "bounding box", to avoid jittering amimations
  call IO % Plot_Box(9, 1, 1, WHITE, 999)
  call IO % Plot_Box(9, n, n, WHITE, 999)
  call IO % Plot_Bracket_Left (9, (/1,n/), 1, 40)
  call IO % Plot_Bracket_Right(9, (/1,n/), n, 40)

  !----------------------------------------------!
  !   Plot the background matrix, if specified   !
  !----------------------------------------------!
  if(present(B)) then

    min_v = minval(B % val(:,:))
    max_v = maxval(B % val(:,:))

    do row = 1, B % n
      do col = 1, B % n
        call IO % Plot_Square(9, row, col, B % val(row, col), min_v, max_v, 60)
      end do
    end do
  end if

  min_v = minval(A % val(:,:))
  max_v = maxval(A % val(:,:))

  !-------------------------------------------!
  !   Browse through matrix and plot it :-)   !
  !-------------------------------------------!
  do row = 1, n
    do col = 1, n

      ! This is normal, scaled circle for a matrix value
      call IO % Plot_Circle(9, row, col, A % val(row, col), min_v, max_v, 50)

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
