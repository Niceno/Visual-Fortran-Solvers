!==============================================================================!
  subroutine Plot_Square(IO, fu, row, col, val, minv, maxv, depth)
!------------------------------------------------------------------------------!
!>  Plots matrix entry as a gray square in to the Xfig file.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)  :: IO        !! parent class
  integer, intent(in) :: fu        !! file unit
  integer, intent(in) :: row, col  !! row and columnt
  real,    intent(in) :: val       !! value to plot
  real,    intent(in) :: minv      !! maximum absolute value in the matrix
  real,    intent(in) :: maxv      !! maximum absolute value in the matrix
  integer, intent(in) :: depth     !! depth
!-----------------------------------[Locals]-----------------------------------!
  integer :: plot_x, plot_y, d, color
  real    :: maxa
!==============================================================================!

  maxa = max(abs(minv), abs(maxv))

  ! Do not scale by size
  d = CM / 2

  ! Scaling by color, but use only shades of green
  if(val > 0) then
    color = 48
    if(IO % scale_by_color) color = 40 + 8 * (sqrt( val) / sqrt(maxa))
    Assert(color .ge. 40)
    Assert(color .le. 48)
!   if(IO % scale_by_color) color = 60 + 8 * (      val  /      maxa)
  else
    color = 48
    if(IO % scale_by_color) color = 40 + 8 * (sqrt(-val) / sqrt(maxa))
    Assert(color .ge. 40)
    Assert(color .le. 48)
!   if(IO % scale_by_color) color = 60 + 8 * (      val  /      maxa)
  end if

  ! Perform actual plotting of a square
  if(abs(val) > 1.0e-15) then
    plot_x = CM/2 + col * CM
    plot_y = CM/2 + row * CM
    write(fu,'(a)',     advance='no') ' 2'                !  1 polyline
    write(fu,'(a)',     advance='no') ' 2'                !  2 defined as a box
    write(fu,'(a)',     advance='no') ' 0'                !  3 line style
    write(fu,'(a)',     advance='no') ' 0'                !  4 line thickness
    if(val > 0.0) then
      write(fu,'(i3)',  advance='no') color               !  5 line color
      write(fu,'(i3)',  advance='no') color               !  6 fill color
    else
      write(fu,'(i3)',  advance='no') color               !  5 line color
      write(fu,'(i3)',  advance='no') color               !  6 fill color
    end if
    write(fu,'(i4)',    advance='no') depth               !  7 layer
    write(fu,'(a)',     advance='no') ' -1'               !  8 pen style, unused
    write(fu,'(a)',     advance='no') ' 20'               !  9 fill intensity
    write(fu,'(a)',     advance='no') ' 0.000'            ! 10
    write(fu,'(a)',     advance='no') ' 1'                ! 11 join style
    write(fu,'(a)',     advance='no') ' 0'                ! 12 cap style
    write(fu,'(a)',     advance='no') ' -1'               ! 13 radius
    write(fu,'(a)',     advance='no') ' 0'                ! 14 forward arrow
    write(fu,'(a)',     advance='no') ' 0'                ! 15 backward arrow
    write(fu,'(a)')                   ' 5'                ! 16 number of points
    write(fu,'(a)',     advance='no') achar(9)            !    separator
    write(fu,'(i9,i9)', advance='no') plot_x-d, plot_y-d  ! 17 point 1
    write(fu,'(i9,i9)', advance='no') plot_x+d, plot_y-d  ! 18 point 2
    write(fu,'(i9,i9)', advance='no') plot_x+d, plot_y+d  ! 19 point 3
    write(fu,'(i9,i9)', advance='no') plot_x-d, plot_y+d  ! 20 point 4
    write(fu,'(i9,i9)', advance='no') plot_x-d, plot_y-d  ! 21 point 5, closing
    write(fu,*)
  end if

  end subroutine
