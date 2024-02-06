!==============================================================================!
  subroutine Solvers_Mod_Cg_Tflows_Prec(grid, n_iter, res)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
  integer         :: n_iter
  real            :: res
!-----------------------------------[Locals]-----------------------------------!
  integer                    :: n  ! number of unknowns
  integer                    :: i, iter
  real                       :: alpha, beta, rho_old, rho, pap
  real                       :: time_ps, time_pe, time_ss, time_se
  type(Sparse_Type), pointer :: A, D
!==============================================================================!

  print *, '#=========================================================='
  print *, '# Solving the sytem with T-Flows preconditioned CG method'
  print *, '#----------------------------------------------------------'

  A => a_sparse
  D => p_sparse  ! in theory, this should be LDL, but only D is stored

  !------------------!
  !                  !
  !   Praparations   !
  !                  !
  !------------------!
  call Solvers_Mod_Prepare_System(grid)

  call Sparse_Mod_Create_Preconditioning(D, A, 0)
  call In_Out_Mod_Print_Sparse("Sparse D:", D)

  !------------------------!
  !                        !
  !   Actual computation   !
  !                        !
  !------------------------!
  n = A % n

  ! Perform LDL^T factorization on the matrix to find the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Ldlt_Factorization_From_Tflows(D, A)
  call Cpu_Time(time_pe)

  !----------------!
  !   r = b - Ax   !
  !----------------!
  call Lin_Alg_Mod_Sparse_X_Vector(q, A, x)
  do i = 1, n
    r(i) = b(i) - q(i)
  end do

  !---------------------!
  !   solve M * z = r   !
  !---------------------!
  call Solvers_Mod_Ldlt_Solution_From_Tflows(n, -1, A, D, z, r)

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
    !   q = Ap   !
    !------------!
    call Lin_Alg_Mod_Sparse_X_Vector(q, A, p)

    !-----------------------!
    !   alpha = rho / pAp   !
    !-----------------------!
    call Lin_Alg_Mod_Vector_Dot_Vector(pap, p, q)

    alpha = rho / pap

    !---------------------!
    !   x = x + alfa p    !
    !   r = r - alfa Ap   !
    !---------------------!
    do i = 1, n
      x(i) = x(i) + alpha * p(i)
    end do

    do i = 1, n
      r(i) = r(i) - alpha * q(i)
    end do

    !---------------------!
    !   solve M * z = r   !
    !---------------------!
    call Solvers_Mod_Ldlt_Solution_From_Tflows(n, -1, A, D, z, r)

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
