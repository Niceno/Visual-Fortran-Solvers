!==============================================================================!
  subroutine Plot_Text(IO, fu, text, row, col, depth, fsize)
!------------------------------------------------------------------------------!
!>  Plots a text (single character) in matrix coordinates in given depth.
!>  Color is predefined to medium gray though, just like matrix brackets.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)                 :: IO        !! parent class
  integer,      intent(in)           :: fu        !! file unit
  character(*), intent(in)           :: text      !! text to plot
  integer,      intent(in)           :: row, col  !! row and column
  integer,      intent(in)           :: depth     !! depth
  integer,      intent(in), optional :: fsize     !! font size
!-----------------------------------[Locals]-----------------------------------!
  integer :: plot_x, plot_y, d
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(IO)
!==============================================================================!

  plot_x = (col) * CM
  plot_y = (row) * CM + CM
  d      = CM / 2      ! delta

  write(fu,'(a)',  advance='no') ' 4'         !  1 text
  write(fu,'(a)',  advance='no') ' 1'         !  2 justification (1 is center)
  write(fu,'(i3)', advance='no') GRAY         !  3 color (the same as brackets)
  write(fu,'(i4)', advance='no') depth        !  4 layer
  write(fu,'(a)',  advance='no') ' -1'        !  5 pen style (not used)
  write(fu,'(a)',  advance='no') ' 16'        !  6 font:
                                              !      0 is Times-Roman
                                              !      1 is Times-Italic
                                              !      2 is Times-Bold
                                              !      3 is Times-Bold-Italic
                                              !     12 is Courier
                                              !     13 is Courier-Oblique
                                              !     14 is Courier-Bold
                                              !     15 is Courier-Bold-Oblique
                                              !     16 is Helvetica
                                              !     17 is Helvetica-Oblique
                                              !     18 is Helvetica-Bold
  if(present(fsize)) then
    write(fu,'(i4)', advance='no') fsize      !  7 font size
  else
    write(fu,'(a)',  advance='no') ' 36'      !  7 font size
  end if
  write(fu,'(a)',  advance='no') ' 0.0000'    !  8 angle
  write(fu,'(a)',  advance='no') ' 4'         !  9 font flags (whatever it is)
  write(fu,'(i4)', advance='no') CM           ! 10 height
  write(fu,'(i4)', advance='no') CM           ! 11 length
  write(fu,'(i9)', advance='no') plot_x+d     ! 12 x coordinate
  write(fu,'(i9)', advance='no') plot_y       ! 13 y coordinate
  write(fu,'(a)',  advance='no') ' '//trim(text)
  write(fu,'(a)')                '\001'

  end subroutine
