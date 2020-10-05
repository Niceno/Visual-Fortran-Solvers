!==============================================================================!
  subroutine Solvers_Mod_Cholesky(grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
!-----------------------------------[Locals]-----------------------------------!
  real    :: time_ps, time_pe, time_ss, time_se
  integer :: bw
!==============================================================================!

  print *, '#=========================================================='
  print *, '# Solving the sytem with Cholesky decomposition'
  print *, '#----------------------------------------------------------'

  !------------------!
  !   Praparations   !
  !------------------!
  call Solvers_Mod_Prepare_System(grid)

  ! Create two full matrices from the sparse one
  call Sparse_Mod_Expand(a_square, a_sparse, bw)
  call Sparse_Mod_Expand(p_square, a_sparse, bw)
  p_square % val(:,:) = 0

  ! Just print original matrix
  call In_Out_Mod_Print_Square("a_square:", a_square)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform Cholesky factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Cholesky_Factorization_Square(p_square, a_square, bw)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Square(  &
       "p_square after Cholesky factorization", p_square)

  ! Compute y by forward substitution
  call Cpu_Time(time_ss)
  call Solvers_Mod_Forward_Substitution_Square(y, p_square, b)
  !@ call In_Out_Mod_Print_Vector("Vector y after forward substitution:", y)

  ! Compute x by backward substitution
  call Solvers_Mod_Backward_Substitution_Square(x, p_square, y)
  !@ call In_Out_Mod_Print_Vector("Solution x after backward substitution:", x)
  call Cpu_Time(time_se)

  print '(a,1es10.4)', ' # Time for matrix preparation: ', time_pe - time_ps
  print '(a,1es10.4)', ' # Time for solution:           ', time_se - time_ss
  print '(a,1es10.4)', ' # Total time:                  ',  &
                                       time_pe - time_ps + time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  call Solvers_Mod_Check_Solution_Square(a_square)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()

  end subroutine
