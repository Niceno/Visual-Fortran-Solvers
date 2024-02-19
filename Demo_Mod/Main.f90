!==============================================================================!
  subroutine Demo_Mod_Main
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type)   :: Grid
  type(Dense_Type)  :: Ad
  type(Sparse_Type) :: As
  real, allocatable :: x(:)
  real, allocatable :: b(:)
  character(80)     :: dummy
  character(32)     :: arg = ''              ! command line argument
  integer           :: choice, f_in, n_iter, file_unit, test
  integer           :: nx, ny, nz
  real              :: lx, ly, lz
  character(1)      :: bc_t  ! boundary condition type
  real              :: bc_v  ! boundary condition value
  real              :: res
  logical           :: file_exists
!==============================================================================!

  !-------------------------------------!
  !   Read init.dat file if it exists   !
  !-------------------------------------!

  ! First check if the init file exists
  inquire(file  = 'init.dat',  &
          exist = file_exists)

  ! File doesn't exist
  if(.not. file_exists) then

    print *, "# File 'init.dat' doesn't exist, setting the default values"
    lx =  1.0
    ly =  1.0
    lz =  1.0
    nx = 10
    ny = 10
    nz = 10

    f_in   =  1
    n_iter = 10
    res    =  1.0e-15

  ! File exists; open it and read the parameters
  else

    print *, "# Reading the file 'init.dat'"
    open(newunit = file_unit,   &
         file    = 'init.dat',  &
         action  = 'read')

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

    read(file_unit, *) dummy, f_in
    read(file_unit, *) dummy, n_iter
    read(file_unit, *) dummy, res
  end if

  !-----------------------------------!
  !   Create the computational grid   !
  !-----------------------------------!
  call Grid % Create_Grid(lx, ly, lz, nx, ny, nz)

  !------------------------------------------------------!
  !   Check if command line argument has been supplied   !
  !------------------------------------------------------!
  call get_command_argument(1, arg)
  test = -1
  if(arg .ne. '') read(arg,*) test

  !------------------------!
  !   Open the main menu   !
  !------------------------!
1 print *, '#=========================================================='
  call Foul % Formatted_Write(' # ', 'default',  &
                       'Select a Case to Demonstrate', 'bright yellow');
  print *, '#----------------------------------------------------------'
  print *, '#  0 - Exit'
  print *, '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  call Foul % Formatted_Write(' # ', 'default',  &
                           'Section 1 - Direct Solvers', 'bright cyan');
  print *, '# 11 - Gaussian elimination'
  print *, '# 12 - Cholesky solver'
  print *, '# 13 - LDL'' solver'
  print *, '# 14 - LU solver (based on Gauss)'
  print *, '# 15 - LU solver (based on Doolittle)'
  print *, '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  call Foul % Formatted_Write(' # ', 'default',  &
                       'Section 2 - Incomplete Solvers', 'bright cyan');
  print *, '# 21 - Incomplete Cholesky solver'
  print *, '# 22 - Incomplete LDL'' solver'
  print *, '# 23 - Incomplete LU solver (based on Gauss)'
  print *, '# 24 - Incomplete LU solver (based on Doolittle)'
  print *, '# 25 - T-Flows'' bare-bones LDL'' solver'
  print *, '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  call Foul % Formatted_Write(' # ', 'default',  &
                        'Section 3 - Iterative Solvers', 'bright cyan');
  print *, '# 31 - CG solver without preconditioning'
  print *, '# 32 - CG solver with diagonal preconditioning'
  print *, '# 33 - CG solver with T-Flows preconditioning'
  print *, '# 34 - CG solver with LDL'' preconditioning'
  print *, '# 35 - CG solver with Cholesky preconditioning'
  print *, '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  call Foul % Formatted_Write(' # ', 'default',  &
                         'Section 4 - Additional Tests', 'bright cyan');
  print *, '# 41 - Compressed matrices'
  print *, '# 42 - Preconditioning matrix'
  print *, '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  call Foul % Formatted_Write(' # ', 'default',  &
                         'Section 5 - Various Settings', 'bright cyan');
  write(*,'(a46,3i4)') '# 51 - Change grid resolution, currently at: ',  &
                       Grid % nx, Grid % ny, Grid % nz
  write(*,'(a46,1i4)') '# 52 - Change fill-in level, currently at:   ',  &
                       f_in
  write(*,'(a46,1i4)') '# 53 - Change num iterations, currently at:  ',  &
                       n_iter
  write(*,'(a46,1es13.4)') '# 54 - Change target residual, currently at: ',  &
                       res
  if(IO % scale_by_color) then
    print *, '# 55 - Do not scale by color'
  else
    print *, '# 55 - Scale by color'
  end if
  if(IO % scale_by_size) then
    print *, '# 56 - Do not scale by size'
  else
    print *, '# 56 - Scale by size'
  end if
  print *, '#----------------------------------------------------------'

  if(test .eq. -1) then
    read *, choice
  else
    choice = test
  end if

  !---------------------------------------------!
  !   Perform some action based on user input   !
  !   (This is the only place where I use the   !
  !    indentation of 4.  It is black swan!)    !
  !---------------------------------------------!
  select case(choice)
    case(0)
      return

    case(11)
        call Solvers_Mod_Gauss(Grid, Ad, x, b)
    case(12)
        call Solvers_Mod_Cholesky(Grid, Ad, x, b)
    case(13)
        call Solvers_Mod_Ldlt(Grid, Ad, x, b)
    case(14)
        call Solvers_Mod_Lu(Grid, Ad, x, b, GAUSS)
    case(15)
        call Solvers_Mod_Lu(Grid, Ad, x, b, DOOLITTLE)

    case(21)
        call Solvers_Mod_Incomplete_Cholesky(Grid, As, x, b, f_in)
    case(22)
        call Solvers_Mod_Incomplete_Ldlt(Grid, As, x, b, f_in)
    case(23)
        call Solvers_Mod_Incomplete_Lu(Grid, As, x, b, f_in, GAUSS)
    case(24)
        call Solvers_Mod_Incomplete_Lu(Grid, As, x, b, f_in, DOOLITTLE)
    case(25)
        call Solvers_Mod_Incomplete_Ldlt_From_Tflows(Grid, As, x, b)

    case(31)
        call Solvers_Mod_Cg_No_Prec(Grid, As, x, b, n_iter, res)
    case(32)
        call Solvers_Mod_Cg_Diag_Prec(Grid, As, x, b, n_iter, res)
    case(33)
        call Solvers_Mod_Cg_Tflows_Prec(Grid, As, x, b, n_iter, res)
    case(34)
        call Solvers_Mod_Cg_Ldlt_Prec(Grid, As, x, b, n_iter, res, f_in)
    case(35)
        call Solvers_Mod_Cg_Cholesky_Prec(Grid, As, x, b, n_iter, res, f_in)

    case(41)
        call Demo_Mod_Compress_Decompress
    case(42)
        call Demo_Mod_Fill_In(f_in, Grid)

    case(51)
        print *, "# Enter the desired resolution: "
        read *, nx, ny, nz
        call Grid % Create_Grid(lx, ly, lz, nx, ny, nz)
    case(52)
        print *, "# Enter the desired fill-in level: "
        read *, f_in
    case(53)
        print *, "# Enter the desired number of iterations: "
        read *, n_iter
    case(54)
        print *, "# Enter the desired target residual: "
        read *, res
    case(55)
        IO % scale_by_color = .not. IO % scale_by_color
    case(56)
        IO % scale_by_size = .not. IO % scale_by_size

    case default
        print *, "Invalid choice"

  end select

  if(test .eq. -1) goto 1

  end subroutine
