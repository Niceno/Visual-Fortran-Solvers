!==============================================================================!
  subroutine Plot_Ring(IO, fu, row, col, color)
!------------------------------------------------------------------------------!
!>  Plots ring in to the Xfig file.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)  :: IO        !! parent class
  integer, intent(in) :: fu        !! file unit
  integer, intent(in) :: row, col  !! row and columnt
  integer, intent(in) :: color     !! color
!-----------------------------------[Locals]-----------------------------------!
  integer :: plot_x, plot_y, rad
!==============================================================================!

  ! Set radius
  rad = 180

  ! Perform actual plotting of a ring

  plot_x = CM/2 + col * CM
  plot_y = CM/2 + row * CM
  write(9,'(a)',     advance='no') ' 1'            !  1 ellipse
  write(9,'(a)',     advance='no') ' 3'            !  2 define with radii
  write(9,'(a)',     advance='no') ' 0'            !  3 line style
  write(9,'(a)',     advance='no') ' 4'            !  4 line thickness
  write(9,'(i3)',    advance='no') color           !  5 line color
  write(9,'(i3)',    advance='no') color           !  6 fill color
  write(9,'(a)',     advance='no') ' 50'           !  7 layer
  write(9,'(a)',     advance='no') ' -1'           !  8 pen style, unused
  write(9,'(a)',     advance='no') ' -1'           !  9 fill intensity
  write(9,'(a)',     advance='no') ' 0.000'        ! 10
  write(9,'(a)',     advance='no') ' 1'            ! 11
  write(9,'(a)',     advance='no') ' 0.000'        ! 12
  write(9,'(i9,i9)', advance='no') plot_x, plot_y  ! 13-14 coordinates
  write(9,'(i4,i4)', advance='no') rad, rad        ! 15-16 radii
  write(9,'(i9,i9)', advance='no') plot_x, plot_y  ! 17-18
  write(9,'(i9,i9)', advance='no') plot_x, plot_y  ! 19-20
  write(9,*)

  end subroutine
