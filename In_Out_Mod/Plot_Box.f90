!==============================================================================!
  subroutine Plot_Box(IO, fu, row, col, color, depth)
!------------------------------------------------------------------------------!
!>  Plots box in to the Xfig file.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)  :: IO        !! parent class
  integer, intent(in) :: fu        !! file unit
  integer, intent(in) :: row, col  !! row and columnt
  integer, intent(in) :: color     !! color
  integer, intent(in) :: depth     !! color
!-----------------------------------[Locals]-----------------------------------!
  integer :: plot_x, plot_y, d
!==============================================================================!

  plot_x = (col) * CM
  plot_y = (row) * CM
  d      = CM          ! delta

  write(9,'(a)',     advance='no') ' 2'                !  1 polyline
  write(9,'(a)',     advance='no') ' 2'                !  2 defined as a box
  write(9,'(a)',     advance='no') ' 0'                !  3 line style
  write(9,'(a)',     advance='no') ' 0'                !  4 line thickness
  write(9,'(i3)',    advance='no') color               !  5 line color
  write(9,'(i3)',    advance='no') color               !  6 fill color
  write(9,'(i4)',    advance='no') depth               !  7 layer
  write(9,'(a)',     advance='no') ' -1'               !  8 pen style, unused
  write(9,'(a)',     advance='no') ' 20'               !  9 fill intensity
  write(9,'(a)',     advance='no') ' 0.000'            ! 10
  write(9,'(a)',     advance='no') ' 1'                ! 11 join style
  write(9,'(a)',     advance='no') ' 0'                ! 12 cap style
  write(9,'(a)',     advance='no') ' -1'               ! 13 radius
  write(9,'(a)',     advance='no') ' 0'                ! 14 forward arrow
  write(9,'(a)',     advance='no') ' 0'                ! 15 backwar arrow
  write(9,'(a)')                   ' 5'                ! 16 number of points
  write(9,'(a)',     advance='no') achar(9)            !    separator
  write(9,'(i9,i9)', advance='no') plot_x,   plot_y    ! 17 point 1
  write(9,'(i9,i9)', advance='no') plot_x+d, plot_y    ! 18 point 2
  write(9,'(i9,i9)', advance='no') plot_x+d, plot_y+d  ! 19 point 3
  write(9,'(i9,i9)', advance='no') plot_x,   plot_y+d  ! 20 point 4
  write(9,'(i9,i9)', advance='no') plot_x,   plot_y    ! 21 point 5, closing
  write(9,*)

  end subroutine
