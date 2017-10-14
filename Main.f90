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

  write(*,*) '============================'
  write(*,*) 'Select a case to demonstrate'
  write(*,*) '1 - Gaussian elimination'
  write(*,*) '2 - Cholesky factorization'
  write(*,*) '3 - Compressed matrices'
  write(*,*) '----------------------------'
  read(*,*) choice

  ! Demonstrate Gussian elimination
  if(choice == 1) then
    call Demo_Gauss_Solver

  ! Demonstrate Cholesky Factorization
  else if(choice == 2) then
    call Demo_Cholesky_Solver

  ! Demonstrate Compress and Decompress
  else if(choice == 3) then
    call Demo_Compress_Decompress

  end if

  end program Main
