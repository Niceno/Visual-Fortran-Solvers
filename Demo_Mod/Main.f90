!==============================================================================!
  subroutine Demo_Mod_Main
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type)   :: grid
  type(Dense_Type)  :: Ad
  type(Sparse_Type) :: As
  real, allocatable :: x(:)
  real, allocatable :: b(:)
  character(80)     :: dummy
  character(32)     :: arg = ''              ! command line argument
  integer           :: choice, f_in, n_iter, file_unit, test
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
    grid % lx =  1.0
    grid % ly =  1.0
    grid % lz =  1.0
    grid % nx = 10
    grid % ny = 10
    grid % nz = 10

    f_in   =  1
    n_iter = 10
    res    =  1.0e-15

  ! File exists; open it and read the parameters
  else

    print *, "# Reading the file 'init.dat'"
    open(newunit = file_unit,   &
         file    = 'init.dat',  &
         action  = 'read')

    read(file_unit, *) dummy, grid % lx
    read(file_unit, *) dummy, grid % ly
    read(file_unit, *) dummy, grid % lz
    read(file_unit, *) dummy, grid % nx
    read(file_unit, *) dummy, grid % ny
    read(file_unit, *) dummy, grid % nz

    read(file_unit, *) dummy, f_in
    read(file_unit, *) dummy, n_iter
    read(file_unit, *) dummy, res
  end if

  !------------------------------------------------------!
  !   Check if command line argument has been supplied   !
  !------------------------------------------------------!
  call get_command_argument(1, arg)
  test = -1
  if(arg .ne. '') read(arg,*) test

  ! Open the main menu
1 print *, '#=========================================================='
  print *, '# Select a case to demonstrate'
  print *, '#----------------------------------------------------------'
  print *, '#  0 - Exit'
  print *, '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  print *, '#  1 - Gaussian elimination'
  print *, '#  2 - Cholesky solver'
  print *, '#  3 - LDL'' solver'
  print *, '#  4 - LU solver'
  print *, '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  print *, '#  5 - Incomplete Cholesky solver'
  print *, '#  6 - Incomplete LDL'' solver'
  print *, '#  7 - Bare-bones LDL'' solver from T-Flows'
  print *, '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  print *, '#  8 - CG solver without preconditioning'
  print *, '#  9 - CG solver with diagonal preconditioning'
  print *, '# 10 - CG solver with T-Flows preconditioning'
  print *, '# 11 - CG solver with LDL'' preconditioning'
  print *, '# 12 - CG solver with Cholesky preconditioning'
  print *, '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  print *, '# 13 - Compressed matrices'
  print *, '# 14 - Preconditioning matrix'
  print *, '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,'(a46,3i4)') '# 15 - Change grid resolution, currently at: ',  &
                       grid % nx, grid % ny, grid % nz
  write(*,'(a46,1i4)') '# 16 - Change fill-in level, currently at:   ',  &
                       f_in
  write(*,'(a46,1i4)') '# 17 - Change num iterations, currently at:  ',  &
                       n_iter
  write(*,'(a46,1es13.4)') '# 18 - Change target residual, currently at: ',  &
                       res
  print *, '#----------------------------------------------------------'

  if(test .eq. -1) then
    read *, choice
  else
    choice = test
  end if

  if(choice ==  0) return

  if(choice ==  1) call Solvers_Mod_Gauss   (grid, Ad, x, b)
  if(choice ==  2) call Solvers_Mod_Cholesky(grid, Ad, x, b)
  if(choice ==  3) call Solvers_Mod_Ldlt    (grid, Ad, x, b)
  if(choice ==  4) call Solvers_Mod_Lu      (grid, Ad, x, b)

  if(choice ==  5) call Solvers_Mod_Incomplete_Cholesky        (grid, As, x, b, f_in)
  if(choice ==  6) call Solvers_Mod_Incomplete_Ldlt            (grid, As, x, b, f_in)
  if(choice ==  7) call Solvers_Mod_Incomplete_Ldlt_From_Tflows(grid, As, x, b)

  if(choice ==  8) call Solvers_Mod_Cg_No_Prec      (grid, As, x, b, n_iter, res)
  if(choice ==  9) call Solvers_Mod_Cg_Diag_Prec    (grid, As, x, b, n_iter, res)
  if(choice == 10) call Solvers_Mod_Cg_Tflows_Prec  (grid, As, x, b, n_iter, res)
  if(choice == 11) call Solvers_Mod_Cg_Ldlt_Prec    (grid, As, x, b, n_iter, res, f_in)
  if(choice == 12) call Solvers_Mod_Cg_Cholesky_Prec(grid, As, x, b, n_iter, res, f_in)

  if(choice == 13) call Demo_Mod_Compress_Decompress
  if(choice == 14) call Demo_Mod_Fill_In(f_in, grid)

  if(choice == 15) then
    print *, '# Enter the desired resolution: '
    read *, grid % nx, grid % ny, grid % nz
  end if
  if(choice == 16) then
    print *, '# Enter the desired fill-in level: '
    read *, f_in
  end if
  if(choice == 17) then
    print *, '# Enter the desired number of iterations: '
    read *, n_iter
  end if
  if(choice == 18) then
    print *, '# Enter the desired target residual: '
    read *, res
  end if

  if(test .eq. -1) goto 1

  end subroutine
