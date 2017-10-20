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
  write(*,*) '# 0 - Exit'
  write(*,*) '# 1 - Compressed matrices'
  write(*,*) '# 2 - Gaussian elimination'
  write(*,*) '# 3 - Cholesky factorization'
  write(*,*) '# 4 - LDL^T Solver'
  write(*,*) '# 5 - Incomplete Cholesky Solver'
  write(*,*) '# 6 - Incomplete Cholesky Solver with level 1 fill-in'
  write(*,*) '# 7 - Incomplete Cholesky Solver with level 2 fill-in'
  write(*,*) '# 8 - Incomplete Cholesky Solver with level 3 fill-in'
  write(*,*) '# 9 - LDL^T Solver from T-FlowS' 
  write(*,*) '#----------------------------------------------------'
  read(*,*) choice

  ! Exit
  if(choice == 0) then
    return

  ! Demonstrate Compress and Decompress
  else if(choice == 1) then
    call Demo_Compress_Decompress

  ! Demonstrate Gussian elimination
  else if(choice == 2) then
    call Demo_Gauss_Solver

  ! Demonstrate Cholesky Factorization
  else if(choice == 3) then
    call Demo_Cholesky_Solver

  ! Demonstrate Cholesky Factorization
  else if(choice == 4) then
    call Demo_LDLT_Solver

  ! Demonstrate Incomplete Cholesky Solver
  else if(choice == 5) then
    call Demo_Incomplete_Cholesky_Solver(0)

  ! Demonstrate Incomplete Cholesky Solver with level 1 fill in
  else if(choice == 6) then
    call Demo_Incomplete_Cholesky_Solver(1)

  ! Demonstrate Incomplete Cholesky Solver with level 2 fill in
  else if(choice == 7) then
    call Demo_Incomplete_Cholesky_Solver(2)

  ! Demonstrate Incomplete Cholesky Solver with level 3 fill in
  else if(choice == 8) then
    call Demo_Incomplete_Cholesky_Solver(3)

  ! Demonstrate LDL^T Solver from T-FlowS
  else if(choice == 9) then
    call Demo_LDLT_Solver_From_Tflows

  end if

  goto 1
 
  end program Main
