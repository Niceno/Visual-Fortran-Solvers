!==============================================================================!
  program Main
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
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
  write(*,'(a47,i2)') '#  1 - Change fill-in level, currently it is: ', fill_in_level
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  2 - Gaussian elimination'
  write(*,*) '#  3 - Cholesky solver'
  write(*,*) '#  4 - LDL^T Solver'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  5 - Compressed matrices'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  6 - Preconditioning matrix'
  write(*,*) '#  7 - Incomplete Cholesky Solver'
  write(*,*) '#  8 - Incomplete LDLT Solver'
  write(*,*) '#  9 - LDL^T Solver from T-FlowS' 
  write(*,*) '#----------------------------------------------------'
  read(*,*) choice

  if(choice ==  0) return
  if(choice ==  1) then
    write(*,*) '# Enter the desired fill-in level: '
    read(*,*) fill_in_level
  end if
  if(choice ==  2) call Demo_Gauss_Solver
  if(choice ==  3) call Demo_Cholesky_Solver
  if(choice ==  4) call Demo_LDLT_Solver
  if(choice ==  5) call Demo_Compress_Decompress
  if(choice ==  6) call Demo_Fill_In(fill_in_level)
  if(choice ==  7) call Demo_Incomplete_Cholesky_Solver(fill_in_level)
  if(choice ==  8) call Demo_Incomplete_LDLT_Solver(fill_in_level)
  if(choice ==  9) call Demo_LDLT_Solver_From_Tflows

  goto 1
 
  end program Main
