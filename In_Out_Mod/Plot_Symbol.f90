!==============================================================================!
  subroutine Plot_Symbol(IO, fu, symbol, row, col, depth)
!------------------------------------------------------------------------------!
!>  Plots a symbol (single character) in matrix coordinates in given depth.
!>  Color is predefined to medium gray though, just like matrix brackets.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)       :: IO        !! parent class
  integer,      intent(in) :: fu        !! file unit
  character(*), intent(in) :: symbol
  integer,      intent(in) :: row, col  !! row and column
  integer,      intent(in) :: depth     !! depth
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
  write(fu,'(a)',  advance='no') ' 16'        !  6 font: 16 is Helvetica
                                              !          17 is Helvetica-Oblique
                                              !          18 is Helvetica-Bold
  write(fu,'(a)',  advance='no') ' 36'        !  7 font size
  write(fu,'(a)',  advance='no') ' 0.0000'    !  8 angle
  write(fu,'(a)',  advance='no') ' 4'         !  9 font flags (whatever it is)
  write(fu,'(i4)', advance='no') CM           ! 10 height
  write(fu,'(i4)', advance='no') CM           ! 11 length
  write(fu,'(i9)', advance='no') plot_x+d     ! 12 x coordinate
  write(fu,'(i9)', advance='no') plot_y       ! 13 y coordinate
  write(fu,'(a2)', advance='no') ' '//trim(symbol)
  write(fu,'(a)')                '\001'

  end subroutine
