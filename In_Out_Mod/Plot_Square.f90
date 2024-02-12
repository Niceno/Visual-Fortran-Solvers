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
    write(9,'(a)',     advance='no') ' 2'                !  1 polyline
    write(9,'(a)',     advance='no') ' 2'                !  2 defined as a box
    write(9,'(a)',     advance='no') ' 0'                !  3 line style
    write(9,'(a)',     advance='no') ' 0'                !  4 line thickness
    if(val > 0.0) then
      write(9,'(i3)',  advance='no') color               !  5 line color
      write(9,'(i3)',  advance='no') color               !  6 fill color
    else
      write(9,'(i3)',  advance='no') color               !  5 line color
      write(9,'(i3)',  advance='no') color               !  6 fill color
    end if
    write(9,'(i4)',    advance='no') depth               !  7 layer
    write(9,'(a)',     advance='no') ' -1'               !  8 pen style, unused
    write(9,'(a)',     advance='no') ' 20'               !  9 fill intensity
    write(9,'(a)',     advance='no') ' 0.000'            ! 10
    write(9,'(a)',     advance='no') ' 1'                ! 11 join style
    write(9,'(a)',     advance='no') ' 0'                ! 12 cap style
    write(9,'(a)',     advance='no') ' -1'               ! 13 radius
    write(9,'(a)',     advance='no') ' 0'                ! 14 forward arrow
    write(9,'(a)',     advance='no') ' 0'                ! 15 backward arrow
    write(9,'(a)')                   ' 5'                ! 16 number of points
    write(9,'(a)',     advance='no') achar(9)            !    separator
    write(9,'(i9,i9)', advance='no') plot_x-d, plot_y-d  ! 17 point 1
    write(9,'(i9,i9)', advance='no') plot_x+d, plot_y-d  ! 18 point 2
    write(9,'(i9,i9)', advance='no') plot_x+d, plot_y+d  ! 19 point 3
    write(9,'(i9,i9)', advance='no') plot_x-d, plot_y+d  ! 20 point 4
    write(9,'(i9,i9)', advance='no') plot_x-d, plot_y-d  ! 21 point 5, closing
    write(9,*)
  end if

  end subroutine
