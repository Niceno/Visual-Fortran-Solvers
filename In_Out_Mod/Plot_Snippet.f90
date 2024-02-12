
!==============================================================================!
  subroutine Plot_Snippet(IO, inp_file_name, lf, ll)
!------------------------------------------------------------------------------!
!>  Plots snippet of a Fortran code in to the Xfig file.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)       :: IO             !! parent class
  character(*), intent(in) :: inp_file_name  !! source file name
  integer,      intent(in) :: lf, ll         !! first and last line to plot
!-----------------------------------[Locals]-----------------------------------!
  character (80) :: line
  character(512) :: out_file_name
  integer        :: i, n, iostat, lc, pos, wc
!==============================================================================!

  ! Store the length of the input file name
  n = len(inp_file_name)

  call Foul % Formatted_Write(' # Processing the file:          ',  &
                              'white',                              &
                              inp_file_name,                        &
                              'green');

  out_file_name = ''
  out_file_name = inp_file_name(1:n-4)

  call Foul % Formatted_Write(' # Plotting the code snippet to: ',  &
                              'white',                              &
                              trim(out_file_name)//'.fig',          &
                              'bright red');

  ! Open input and output files
  open(8, file=     inp_file_name,          action='read')
  open(9, file=trim(out_file_name)//'.fig', action='write')

  ! Start the Xfig file by plotting the header
   call IO % Plot_Header(9)

  ! Process the source file
  lc = 0  ! reset the line counter
  wc = 0  ! reset the wrote counter

  ! Read the whole file
  do
    read(8, '(a)', iostat=iostat) line

    if (iostat /= 0) exit  ! exit the loop if end of file or error
    lc = lc + 1

    ! If in desired range, plot them
    if(lc .ge. lf .and. lc .le. ll) then

      pos = index(line, 'IO')
      if(pos .eq. 0) then  ! skip lines which write something
        wc = wc + 1
        write(9, '(a)',  advance='no') '4 0 0 50 -1 12 22 0.0000 4 450 450 '
        write(9, '(i9)', advance='no') CM / 2
        write(9, '(i9)', advance='no') wc * CM - CM / 4
        write(9, '(a)',  advance='no') trim(line)
        write(9, '(a)')                '\001'
      end if
    end if
  end do

  end subroutine
