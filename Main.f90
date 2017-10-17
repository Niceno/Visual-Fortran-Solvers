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

  write(*,*) '=============================='
  write(*,*) 'Select a case to demonstrate'
  write(*,*) '1 - Compressed matrices'
  write(*,*) '2 - Gaussian elimination'
  write(*,*) '3 - Cholesky factorization'
  write(*,*) '4 - Incomplete Cholesky Solver'
  write(*,*) '------------------------------'
  read(*,*) choice

  ! Demonstrate Compress and Decompress
  if(choice == 1) then
    call Demo_Compress_Decompress

  ! Demonstrate Gussian elimination
  else if(choice == 2) then
    call Demo_Gauss_Solver

  ! Demonstrate Cholesky Factorization
  else if(choice == 3) then
    call Demo_Cholesky_Solver

  ! Demonstrate Incomplete Cholesky Solver
  else if(choice == 4) then
    call Demo_Incomplete_Cholesky_Solver

  end if

  end program Main
