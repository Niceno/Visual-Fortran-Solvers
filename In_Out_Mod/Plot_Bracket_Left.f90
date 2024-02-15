!==============================================================================!
  subroutine Plot_Bracket_Left(IO, fu, rows, col, depth)
!------------------------------------------------------------------------------!
!>  Plots box in to the Xfig file.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)  :: IO       !! parent class
  integer, intent(in) :: fu       !! file unit
  integer, intent(in) :: rows(2)  !! starting and ending row
  integer, intent(in) :: col      !! column
  integer, intent(in) :: depth    !! layer
!-----------------------------------[Locals]-----------------------------------!
  integer :: plot_x, plot_y1, plot_yn, d
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(IO)
!==============================================================================!

  plot_x  = (col)     * CM
  plot_y1 = (rows(1)) * CM
  plot_yn = (rows(2)) * CM + CM
  d       = CM / 4      ! delta

  write(fu,'(a)',     advance='no') ' 2'                !  1 polyline
  write(fu,'(a)',     advance='no') ' 1'                !  2 it is a polyline
  write(fu,'(a)',     advance='no') ' 0'                !  3 line style
  write(fu,'(a)',     advance='no') ' 4'                !  4 line thickness
  write(fu,'(i3)',    advance='no')  49                 !  5 line color
  write(fu,'(i3)',    advance='no')  49                 !  6 fill color
  write(fu,'(i4)',    advance='no') depth               !  7 layer
  write(fu,'(a)',     advance='no') ' -1'               !  8 pen style, unused
  write(fu,'(a)',     advance='no') ' -1'               !  9 fill intensity
  write(fu,'(a)',     advance='no') ' 0.000'            ! 10
  write(fu,'(a)',     advance='no') ' 1'                ! 11 join style
  write(fu,'(a)',     advance='no') ' 0'                ! 12 cap style
  write(fu,'(a)',     advance='no') ' -1'               ! 13 radius
  write(fu,'(a)',     advance='no') ' 0'                ! 14 forward arrow
  write(fu,'(a)',     advance='no') ' 0'                ! 15 backwar arrow
  write(fu,'(a)')                   ' 4'                ! 16 number of points
  write(fu,'(a)',     advance='no') achar(9)            !    separator
  write(fu,'(i9,i9)', advance='no') plot_x+d, plot_y1-d ! 17 point 1
  write(fu,'(i9,i9)', advance='no') plot_x-d, plot_y1-d ! 18 point 2
  write(fu,'(i9,i9)', advance='no') plot_x-d, plot_yn+d ! 19 point 3
  write(fu,'(i9,i9)', advance='no') plot_x+d, plot_yn+d ! 20 point 4
  write(fu,*)

  end subroutine
