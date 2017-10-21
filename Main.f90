!==============================================================================!
  program Main
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: choice
!==============================================================================!

1 write(*,*) '#===================================================='
  write(*,*) '# Select a case to demonstrate'
  write(*,*) '#----------------------------------------------------'
  write(*,*) '#  0 - Exit'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  1 - Compressed matrices'
  write(*,*) '#  2 - Preconditioning matrix'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  3 - Gaussian elimination'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  4 - Cholesky solver'
  write(*,*) '#  5 - Incomplete Cholesky Solver'
  write(*,*) '#  6 - Incomplete Cholesky Solver with level 1 fill-in'
  write(*,*) '#  7 - Incomplete Cholesky Solver with level 2 fill-in'
  write(*,*) '#  8 - Incomplete Cholesky Solver with level 3 fill-in'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '#  9 - LDL^T Solver'
  write(*,*) '# 10 - Incomplete LDLT Solver'
  write(*,*) '# 11 - Incomplete LDLT Solver with level 1 fill-in'
  write(*,*) '# 12 - Incomplete LDLT Solver with level 2 fill-in'
  write(*,*) '# 13 - Incomplete LDLT Solver with level 3 fill-in'
  write(*,*) '# - - - - - - - - - - - - - - - - - - - - - - - - - -'
  write(*,*) '# 14 - LDL^T Solver from T-FlowS' 
  write(*,*) '#----------------------------------------------------'
  read(*,*) choice

  if(choice ==  0) return
  if(choice ==  1) call Demo_Compress_Decompress
  if(choice ==  2) call Demo_Fill_In
  if(choice ==  3) call Demo_Gauss_Solver
  if(choice ==  4) call Demo_Cholesky_Solver
  if(choice ==  5) call Demo_Incomplete_Cholesky_Solver(0)
  if(choice ==  6) call Demo_Incomplete_Cholesky_Solver(1)
  if(choice ==  7) call Demo_Incomplete_Cholesky_Solver(2)
  if(choice ==  8) call Demo_Incomplete_Cholesky_Solver(3)
  if(choice ==  9) call Demo_LDLT_Solver
  if(choice == 10) call Demo_Incomplete_LDLT_Solver(0)
  if(choice == 11) call Demo_Incomplete_LDLT_Solver(1)
  if(choice == 12) call Demo_Incomplete_LDLT_Solver(2)
  if(choice == 13) call Demo_Incomplete_LDLT_Solver(3)
  if(choice == 14) call Demo_LDLT_Solver_From_Tflows

  goto 1
 
  end program Main
