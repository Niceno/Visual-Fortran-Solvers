!==============================================================================!
  subroutine Demo_Mod_Main
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  type(Grid_Type) :: grid
  integer         :: choice, fill_in_level
!==============================================================================!

  grid % nx = 10
  grid % ny = 10
  grid % nz = 10
  fill_in_level = 1

1 write(*,*) '#===================================================='
  write(*,*) '# Select a case to demonstrate'
  write(*,*) '#----------------------------------------------------'
  write(*,*) '#  0 - Exit'
  write(*,'(a46,1i2)') '#  1 - Change fill-in level, currently at:   ',  &
                       fill_in_level
  write(*,'(a46,3i3)') '#  2 - Change grid resolution, currently at: ',  &
                       grid % nx, grid % ny, grid % nz
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
  write(*,*) '# 10 - Bare-bones LDL^T solver from T-Flows'
  write(*,*) '#----------------------------------------------------'
  read(*,*) choice

  if(choice ==  0) return
  if(choice ==  1) then
    write(*,*) '# Enter the desired fill-in level: '
    read(*,*) fill_in_level
  end if
  if(choice ==  2) then
    write(*,*) '# Enter the desired resolution: '
    read(*,*) grid % nx, grid % ny, grid % nz
  end if
  if(choice ==  3) call Solvers_Mod_Gauss   (grid)
  if(choice ==  4) call Solvers_Mod_Cholesky(grid)
  if(choice ==  5) call Solvers_Mod_Ldlt    (grid)
  if(choice ==  6) call Demo_Mod_Compress_Decompress
  if(choice ==  7) call Demo_Mod_Fill_In               (fill_in_level, grid)
  if(choice ==  8) call Solvers_Mod_Incomplete_Cholesky(fill_in_level, grid)
  if(choice ==  9) call Solvers_Mod_Incomplete_Ldlt    (fill_in_level, grid)
  if(choice == 10) call Solvers_Mod_Incomplete_Ldlt_From_Tflows       (grid)

  goto 1

  end subroutine
