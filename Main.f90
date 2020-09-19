!==============================================================================!
  program Main
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Globals_Mod
  use Demo_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: choice, fill_in_level
!==============================================================================!

  fill_in_level = 1

1 write(*,*) '#===================================================='
  write(*,*) '# Select a case to demonstrate'
  write(*,*) '#----------------------------------------------------'
  write(*,*) '#  0 - Exit'
  write(*,'(a46,1i2)') '#  1 - Change fill-in level, currently at:   ',  &
                       fill_in_level
  write(*,'(a46,3i3)') '#  2 - Change grid resolution, currently at: ',  &
                       nx, ny, nz
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  3 - Gaussian elimination'
  write(*,*) '#  4 - Cholesky solver'
  write(*,*) '#  5 - LDL^T solver'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  6 - Compressed matrices'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  7 - Preconditioning matrix'
  write(*,*) '#  8 - Incomplete Cholesky solver'
  write(*,*) '#  9 - Incomplete LDLT solver'
  write(*,*) '# 10 - Bare-bones LDL^T solver from T-FlowS' 
  write(*,*) '#----------------------------------------------------'
  read(*,*) choice

  if(choice ==  0) return
  if(choice ==  1) then
    write(*,*) '# Enter the desired fill-in level: '
    read(*,*) fill_in_level
  end if
  if(choice ==  2) then
    write(*,*) '# Enter the desired resolution: '   
    read(*,*) nx, ny, nz
  end if
  if(choice ==  3) call Demo_Mod_Gauss_Solver
  if(choice ==  4) call Demo_Mod_Cholesky_Solver
  if(choice ==  5) call Demo_Mod_Ldlt_Solver
  if(choice ==  6) call Demo_Mod_Compress_Decompress
  if(choice ==  7) call Demo_Mod_Fill_In(fill_in_level)
  if(choice ==  8) call Demo_Mod_Incomplete_Cholesky_Solver(fill_in_level)
  if(choice ==  9) call Demo_Mod_Incomplete_Ldlt_Solver(fill_in_level)
  if(choice == 10) call Demo_Mod_Ldlt_Solver_From_Tflows

  goto 1

  end program Main
