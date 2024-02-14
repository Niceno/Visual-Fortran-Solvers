
!==============================================================================!
  subroutine Plot_Snippet(IO, inp_file_name, str_start, str_end)
!------------------------------------------------------------------------------!
!>  Plots snippet of a Fortran code in to the Xfig file.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)       :: IO             !! parent class
  character(*), intent(in) :: inp_file_name  !! source file name
  character(3), intent(in) :: str_start      !! string start, usually '<-A'
  character(3), intent(in) :: str_end        !! string end,   usually 'A->'
!-----------------------------------[Locals]-----------------------------------!
  character (80) :: read_line  ! line as read from the file
  character (80) :: proc_line  ! processed line
  character(512) :: out_file_name
  integer        :: n, iostat, pos, pos_io, pos_com, lc
  logical        :: processing, last_line, not_comment, out_exists
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(IO)
!==============================================================================!

  ! Print some useful message, be transparent, tell what you are doing
  call Foul % Formatted_Write(' # Processing the file:          ',  &
                              'white',                              &
                              inp_file_name,                        &
                              'green');

  !------------------------------!
  !   Set the output file name   !
  !------------------------------!
  n = len(inp_file_name)
  out_file_name = ''
  out_file_name = inp_file_name(1:n-4)

  ! Print another useful message
  call Foul % Formatted_Write(' # Plotting the code snippet to: ',  &
                              'white',                              &
                              trim(out_file_name)//'.fig',          &
                              'bright red');

  !---------------------------------!
  !   Open input and output files   !
  !---------------------------------!
  open(8, file=     inp_file_name,          action='read')

  ! Check if the output file exists
  out_exists = .false.
  inquire(file=trim(out_file_name)//'.fig', exist=out_exists)
  if(.not. out_exists) then
    open(9, file=trim(out_file_name)//'.fig', action='write')
  else
    open(9, file=trim(out_file_name)//'.fig', action   = 'write',   &
                                              position = 'append',  &
                                              status   = 'old')
  end if

  ! Start the Xfig file by plotting the header
  if(.not. out_exists) then
     call IO % Plot_Header(9)
  end if

  ! Reset the line counter
  lc = 0

  ! Initialize flags for processing
  processing = .false.
  last_line  = .false.

  !---------------------------------------------------------!
  !                                                         !
  !   Browse through the whole file and read line by line   !
  !                                                         !
  !---------------------------------------------------------!
  do
    read(8, '(a)', iostat=iostat) read_line

    ! Exit the loop if end of file or error
    if (iostat /= 0) exit

    !--------------------------------------------------------------!
    !   Check if symbols to start and end processing are present   !
    !--------------------------------------------------------------!
    if(index(read_line, 'IO') .eq. 0) then  ! avoid IO lines as starts/ends
      if(.not. processing) then
        pos = index(read_line, str_start)
        if(pos .ne. 0) processing = .true.
      else
        pos = index(read_line, str_end)
        if(pos .ne. 0) last_line = .true.
      end if
    end if

    !------------------------------------------!
    !   If in the processing mode, go for it   !
    !------------------------------------------!
    if(processing .or. last_line) then

      pos = index(read_line, 'IO')  ! check if the line makes calls to this

      not_comment = .true.             ! check it the whole line is a comment
      proc_line = adjustl(trim(read_line))
      if(proc_line(1:1) .eq. '!') not_comment = .false.

      ! Skip lines which write something or are comment lines
      if(pos .eq. 0 .and. not_comment) then
        lc = lc + 1

        ! Also remove trailing comments, sometimes they are stupid
        pos = index(read_line, '!')
        if(pos .eq. 0) then
          proc_line = read_line
        else
          proc_line = read_line(1:pos-1)
        end if

        ! Write the processed (worked) line out
        write(9, '(a)',  advance='no') '4 0 0 50 -1 12 22 0.0000 4 450 450 '
        write(9, '(i9)', advance='no') CM / 2
        write(9, '(i9)', advance='no') lc * CM - CM / 4
        write(9, '(a)',  advance='no') trim(proc_line)
        write(9, '(a)')                '\001'
      end if

      ! You encountered the last line, time to stop (for now)
      if(last_line) then
        processing = .false.
        last_line  = .false.
      end if

    end if

  end do

  end subroutine
