!==============================================================================!
  subroutine Demo_Mod_Main
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type) :: grid
  character(80)   :: dummy
  integer         :: choice, f_in, n_iter
  integer         :: file_unit
  real            :: res
  logical         :: file_exists
!==============================================================================!

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

  ! Open the main menu
1 write(*,*) '#=========================================================='
  write(*,*) '# Select a case to demonstrate'
  write(*,*) '#----------------------------------------------------------'
  write(*,*) '#  0 - Exit'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  1 - Gaussian elimination'
  write(*,*) '#  2 - Cholesky solver'
  write(*,*) '#  3 - LDL^T solver'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  4 - Incomplete Cholesky solver'
  write(*,*) '#  5 - Incomplete LDL^T solver'
  write(*,*) '#  6 - Bare-bones LDL^T solver from T-Flows'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  7 - CG solver'
  write(*,*) '#  8 - Preconditioned CG solver'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  9 - Compressed matrices'
  write(*,*) '# 10 - Preconditioning matrix'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,'(a46,3i4)') '# 11 - Change grid resolution, currently at: ',  &
                       grid % nx, grid % ny, grid % nz
  write(*,'(a46,1i4)') '# 12 - Change fill-in level, currently at:   ',  &
                       f_in
  write(*,'(a46,1i4)') '# 13 - Change num iterations, currently at:  ',  &
                       n_iter
  write(*,'(a46,1es13.4)') '# 14 - Change target residual, currently at: ',  &
                       res
  write(*,*) '#----------------------------------------------------------'
  read(*,*) choice

  if(choice ==  0) return

  if(choice ==  1) call Solvers_Mod_Gauss   (grid)
  if(choice ==  2) call Solvers_Mod_Cholesky(grid)
  if(choice ==  3) call Solvers_Mod_Ldlt    (grid)

  if(choice ==  4) call Solvers_Mod_Incomplete_Cholesky        (grid, f_in)
  if(choice ==  5) call Solvers_Mod_Incomplete_Ldlt            (grid, f_in)
  if(choice ==  6) call Solvers_Mod_Incomplete_Ldlt_From_Tflows(grid)

  if(choice ==  7) call Solvers_Mod_Cg               (grid, n_iter, res)
  if(choice ==  8) call Solvers_Mod_Preconditioned_Cg(grid, n_iter, res, f_in)

  if(choice ==  9) call Demo_Mod_Compress_Decompress
  if(choice == 10) call Demo_Mod_Fill_In(f_in, grid)

  if(choice == 11) then
    write(*,*) '# Enter the desired resolution: '
    read(*,*) grid % nx, grid % ny, grid % nz
  end if
  if(choice == 12) then
    write(*,*) '# Enter the desired fill-in level: '
    read(*,*) f_in
  end if
  if(choice == 13) then
    write(*,*) '# Enter the desired number of iterations: '
    read(*,*) n_iter
  end if
  if(choice == 14) then
    write(*,*) '# Enter the desired target residual: '
    read(*,*) res
  end if

  goto 1

  end subroutine
