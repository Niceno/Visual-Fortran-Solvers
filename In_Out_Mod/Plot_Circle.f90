!==============================================================================!
  subroutine Plot_Circle(IO, fu, row, col, val, minv, maxv, depth)
!------------------------------------------------------------------------------!
!>  Plots matrix entry as a circle in to the Xfig file.
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
  integer :: plot_x, plot_y, rad, color
  real    :: maxa
!==============================================================================!

  maxa = max(abs(minv), abs(maxv))

  ! Scaling by circle size
  rad = 135
  if(IO % scale_by_size) then
    rad = 15 + 150 * ( sqrt(abs(val)) /  sqrt(maxa))  ! quadratic scaling
!   rad = 50 + 100 * (      abs(val)  /       maxa)   ! linear scaling
!   rad = 50 + 100 * ( sqrt(abs(val)) /  sqrt(maxa))  ! quadratic scaling
!   rad = 10 + 140 * (      abs(val)  /       maxa)   ! linear scaling
!   rad = 10 + 140 * ( sqrt(abs(val)) /  sqrt(maxa))  ! quadratic scaling
  end if

  ! Scaling by color
  if(val > 0) then
    color = 58
    if(IO % scale_by_color) color = 50 + 8 * (sqrt( val) / sqrt( maxv))
!   if(IO % scale_by_color) color = 50 + 8 * (      val  /       maxv)
  else
    color = 78
    if(IO % scale_by_color) color = 70 + 8 * (sqrt(-val) / sqrt(-minv))
!   if(IO % scale_by_color) color = 70 + 8 * (      val  /      -minv)
  end if

  ! Perform actual plotting of a circle
  if(abs(val) > 1.0e-15) then
    plot_x = CM/2 + col * CM
    plot_y = CM/2 + row * CM
    write(9,'(a)',     advance='no') ' 1'            !  1 ellipse
    write(9,'(a)',     advance='no') ' 3'            !  2 define with radii
    write(9,'(a)',     advance='no') ' 0'            !  3 line style
    write(9,'(a)',     advance='no') ' 1'            !  4 line thickness
    if(val > 0.0) then
      write(9,'(i3)',  advance='no') RED             !  5 line color
      write(9,'(i3)',  advance='no') color           !  6 fill color
    else
      write(9,'(i3)',  advance='no') BLUE            !  5 line color
      write(9,'(i3)',  advance='no') color           !  6 fill color
    end if
    write(9,'(i4)',    advance='no') depth           !  7 layer
    write(9,'(a)',     advance='no') ' -1'           !  8 pen style, unused
    write(9,'(a)',     advance='no') ' 20'           !  9 fill intensity
    write(9,'(a)',     advance='no') ' 0.000'        ! 10
    write(9,'(a)',     advance='no') ' 1'            ! 11
    write(9,'(a)',     advance='no') ' 0.000'        ! 12
    write(9,'(i9,i9)', advance='no') plot_x, plot_y  ! 13-14 coordinates
    write(9,'(i4,i4)', advance='no') rad, rad        ! 15-16 radii
    write(9,'(i9,i9)', advance='no') plot_x, plot_y  ! 17-18
    write(9,'(i9,i9)', advance='no') plot_x, plot_y  ! 19-20
    write(9,*)
  end if

  end subroutine
