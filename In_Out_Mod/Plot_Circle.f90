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
  if(IO % scale_by_size .and. abs(val) > TINY) then
    rad = int(15 + 150 * ( sqrt(abs(val)) /  sqrt(maxa))) ! quadratic scaling
!   rad = int(50 + 100 * (      abs(val)  /       maxa) ) ! linear scaling
!   rad = int(50 + 100 * ( sqrt(abs(val)) /  sqrt(maxa))) ! quadratic scaling
!   rad = int(10 + 140 * (      abs(val)  /       maxa) ) ! linear scaling
!   rad = int(10 + 140 * ( sqrt(abs(val)) /  sqrt(maxa))) ! quadratic scaling
  end if

  ! Scaling by color
  if(val > TINY) then
    color = 58
    if(IO % scale_by_color) color = int(50 + 8 * (sqrt( val) / sqrt( maxv)))
!   if(IO % scale_by_color) color = int(50 + 8 * (      val  /       maxv)
  else if(val < -TINY) then
    color = 78
    if(IO % scale_by_color) color = int(70 + 8 * (sqrt(-val) / sqrt(-minv)))
!   if(IO % scale_by_color) color = int(70 + 8 * (      val  /      -minv)
  else
    color = WHITE
  end if

  ! Perform actual plotting of a circle
  if(abs(val) > 1.0e-15) then
    plot_x = CM_HALF + col * CM
    plot_y = CM_HALF + row * CM
    write(fu,'(a,es12.5)')  '# ', val
    write(fu,'(a)',     advance='no') ' 1'            !  1 ellipse
    write(fu,'(a)',     advance='no') ' 3'            !  2 define with radii
    write(fu,'(a)',     advance='no') ' 0'            !  3 line style
    write(fu,'(a)',     advance='no') ' 1'            !  4 line thickness
    if(val > 0.0) then
      write(fu,'(i3)',  advance='no') RED             !  5 line color
      write(fu,'(i3)',  advance='no') color           !  6 fill color
    else
      write(fu,'(i3)',  advance='no') BLUE            !  5 line color
      write(fu,'(i3)',  advance='no') color           !  6 fill color
    end if
    write(fu,'(i4)',    advance='no') depth           !  7 layer
    write(fu,'(a)',     advance='no') ' -1'           !  8 pen style, unused
    write(fu,'(a)',     advance='no') ' 20'           !  9 fill intensity
    write(fu,'(a)',     advance='no') ' 0.000'        ! 10
    write(fu,'(a)',     advance='no') ' 1'            ! 11
    write(fu,'(a)',     advance='no') ' 0.000'        ! 12
    write(fu,'(i9,i9)', advance='no') plot_x, plot_y  ! 13-14 coordinates
    write(fu,'(i4,i4)', advance='no') rad, rad        ! 15-16 radii
    write(fu,'(i9,i9)', advance='no') plot_x, plot_y  ! 17-18
    write(fu,'(i9,i9)', advance='no') plot_x, plot_y  ! 19-20
    write(fu,*)
  end if

  end subroutine
