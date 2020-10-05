!==============================================================================!
  subroutine Solvers_Mod_Cg_Ldlt_Prec(grid, n_iter, res, f_in)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
  integer         :: n_iter
  real            :: res
  integer         :: f_in
!-----------------------------------[Locals]-----------------------------------!
  integer :: n  ! number of unknowns
  integer :: i, iter
  real    :: alpha, beta, rho_old, rho, pap
  real    :: time_ps, time_pe, time_ss, time_se
!==============================================================================!

  print *, '#=========================================================='
  print *, '# Solving the sytem with preconditioned CG method'
  print *, '#----------------------------------------------------------'

  !------------------!
  !                  !
  !   Praparations   !
  !                  !
  !------------------!
  call Solvers_Mod_Prepare_System(grid)

  call Sparse_Mod_Create_Preconditioning(p_sparse, a_sparse, f_in)
  call In_Out_Mod_Print_Sparse("Sparse p_sparse:", p_sparse)

  !------------------------!
  !                        !
  !   Actual computation   !
  !                        !
  !------------------------!
  n = a_sparse % n

  ! Perform LDL^T factorization on the matrix to find the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Ldlt_Factorization_Sparse(p_sparse, a_sparse)
  call Cpu_Time(time_pe)

  !----------------!
  !   r = b - Ax   !
  !----------------!
  call Lin_Alg_Mod_Sparse_X_Vector(ax, a_sparse, x)
  do i = 1, n
    r(i) = b(i) - ax(i)
  end do

  !---------------------!
  !   solve M * z = r   !
  !---------------------!
  call Solvers_Mod_Ldlt_Solution_Sparse(z, p_sparse, r)

  !------------------!
  !   rho = r' * z   !
  !------------------!
  call Lin_Alg_Mod_Vector_Dot_Vector(rho, r, z)

  !-----------!
  !   p = z   !
  !-----------!
  do i = 1, n
    p(i) = z(i)
  end do

  !-------------------------------!
  !                               !
  !   Browse through iterations   !
  !                               !
  !-------------------------------!
  call Cpu_Time(time_ss)
  do iter = 1, n_iter

    !------------!
    !   p = Ap   !
    !------------!
    call Lin_Alg_Mod_Sparse_X_Vector(ap, a_sparse, p)

    !-----------------------!
    !   alpha = rho / pAp   !
    !-----------------------!
    call Lin_Alg_Mod_Vector_Dot_Vector(pap, p, ap)
    alpha = rho / pap

    !---------------------!
    !   x = x + alfa p    !
    !   r = r - alfa Ap   !
    !---------------------!
    do i = 1, n
      x(i) = x(i) + alpha * p(i)
    end do
    do i = 1, n
      r(i) = r(i) - alpha * ap(i)
    end do

    !---------------------!
    !   solve M * z = r   !
    !---------------------!
    call Solvers_Mod_Ldlt_Solution_Sparse(z, p_sparse, r)

    !------------------!
    !   rho = r' * z   !
    !------------------!
    rho_old = rho
    call Lin_Alg_Mod_Vector_Dot_Vector(rho, r, z)

    print '(a,i3,a,1es10.4)', ' #', iter, '; rho = ', sqrt(rho)
    if(sqrt(rho) < res) goto 1

    !---------------------------------!
    !   p = r + (rho / rho_old) * p   !
    !---------------------------------!
    beta = rho / max(rho_old, 1.0e-12)
    do i = 1, n
      p(i) = z(i) + beta * p(i)
    end do
  end do

1 continue
  call Cpu_Time(time_se)

  !@ call In_Out_Mod_Print_Vector("Solution x:", x)

  print '(a,1es10.4)', ' # Time for matrix preparation: ', time_pe - time_ps
  print '(a,1es10.4)', ' # Time for solution:           ', time_se - time_ss
  print '(a,1es10.4)', ' # Total time:                  ',  &
                                       time_pe - time_ps + time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  print '(a,1es10.4)', " # Error:                       ", sqrt(rho)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()

  end subroutine
