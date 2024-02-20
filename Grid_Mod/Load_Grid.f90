!==============================================================================!
  subroutine Load_Grid(Grid, grid_name)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Grid_Type)         :: Grid
  character(*), intent(in) :: grid_name
!-----------------------------------[Locals]-----------------------------------!
  integer       :: nx, ny, nz
  real          :: lx, ly, lz
  character(80) :: dummy
  character(1)  :: bc_t  ! boundary condition type
  real          :: bc_v  ! boundary condition value
  integer       :: file_unit
  logical       :: file_exists
!==============================================================================!

  !-----------------------------------------!
  !   First check if the grid file exists   !
  !-----------------------------------------!
  inquire(file = grid_name, exist = file_exists)

  !------------------------!
  !   File doesn't exist   !
  !------------------------!
  if(.not. file_exists) then

    print *, "# File "//grid_name//" doesn't exist, setting the default values"
    lx =  1.0;  ly =  1.0;  lz =  1.0
    nx = 10;    ny = 10;    nz = 10

    Grid % bc % west_t   = 'D';  Grid % bc % west_v   = -1.0
    Grid % bc % east_t   = 'D';  Grid % bc % east_v   = +1.0
    Grid % bc % south_t  = 'N';  Grid % bc % south_v  =  0.0
    Grid % bc % north_t  = 'N';  Grid % bc % north_v  =  0.0
    Grid % bc % bottom_t = 'N';  Grid % bc % bottom_v =  0.0
    Grid % bc % top_t    = 'N';  Grid % bc % top_v    =  0.0

  !-----------------!
  !   File exists   !
  !-----------------!
  else

    print *, "# Reading the file "//grid_name
    open(newunit = file_unit, file = grid_name, action = 'read')

    !--------------------------------!
    !   Dimensions and resolutions   !
    !--------------------------------!

    read(file_unit, *) dummy, lx
    read(file_unit, *) dummy, ly
    read(file_unit, *) dummy, lz
    read(file_unit, *) dummy, nx
    read(file_unit, *) dummy, ny
    read(file_unit, *) dummy, nz

    !-------------------------!
    !   Boundary conditions   !
    !-------------------------!

    ! West
    read(file_unit, *) dummy, bc_t, bc_v
    Assert(bc_t .eq. 'D' .or. bc_t .eq. 'N')
    Grid % bc % west_t = bc_t
    Grid % bc % west_v = bc_v

    ! East
    read(file_unit, *) dummy, bc_t, bc_v
    Assert(bc_t .eq. 'D' .or. bc_t .eq. 'N')
    Grid % bc % east_t = bc_t
    Grid % bc % east_v = bc_v

    ! South
    read(file_unit, *) dummy, bc_t, bc_v
    Assert(bc_t .eq. 'D' .or. bc_t .eq. 'N')
    Grid % bc % south_t = bc_t
    Grid % bc % south_v = bc_v

    ! North
    read(file_unit, *) dummy, bc_t, bc_v
    Assert(bc_t .eq. 'D' .or. bc_t .eq. 'N')
    Grid % bc % north_t = bc_t
    Grid % bc % north_v = bc_v

    ! Bottom
    read(file_unit, *) dummy, bc_t, bc_v
    Assert(bc_t .eq. 'D' .or. bc_t .eq. 'N')
    Grid % bc % bottom_t = bc_t
    Grid % bc % bottom_v = bc_v

    ! Top
    read(file_unit, *) dummy, bc_t, bc_v
    Assert(bc_t .eq. 'D' .or. bc_t .eq. 'N')
    Grid % bc % top_t = bc_t
    Grid % bc % top_v = bc_v

    close(file_unit)
  end if

  !-----------------------------------!
  !                                   !
  !   Create the computational grid   !
  !                                   !
  !-----------------------------------!
  call Grid % Create_Grid(lx, ly, lz, nx, ny, nz)

  end subroutine
