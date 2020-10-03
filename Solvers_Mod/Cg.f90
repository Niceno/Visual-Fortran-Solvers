!==============================================================================!
  subroutine Solvers_Mod_Cg(grid, n_iter, res)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
  integer         :: n_iter
  real            :: res
!-----------------------------------[Locals]-----------------------------------!
  integer :: n  ! number of unknowns
  integer :: i, iter
  real    :: alpha, beta, rho_old, rho, pap
  real    :: time_ps, time_pe, time_ss, time_se
!==============================================================================!

  print *, '#=========================================================='
  print *, '# Solving the sytem with CG method'
  print *, '#----------------------------------------------------------'

  !------------------!
  !                  !
  !   Praparations   !
  !                  !
  !------------------!
  call Solvers_Mod_Prepare_System(grid)

  call Cpu_Time(time_ps)
  call Cpu_Time(time_pe)

  !------------------------!
  !                        !
  !   Actual computation   !
  !                        !
  !------------------------!
  n = a_sparse % n

  !----------------!
  !   r = b - Ax   !
  !----------------!
  call Lin_Alg_Mod_Matrix_Vector_Multiply_Compressed(ax, a_sparse, x)
  r(1:n) = b(1:n) - ax(1:n)

  !------------------!
  !   rho = r' * r   !
  !------------------!
  call Lin_Alg_Mod_Vector_Vector_Dot_Product(rho, r, r)

  !-----------!
  !   p = r   !
  !-----------!
  p(1:n) = r(1:n)

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
    call Lin_Alg_Mod_Matrix_Vector_Multiply_Compressed(ap, a_sparse, p)

    !-----------------------!
    !   alpha = rho / pAp   !
    !-----------------------!
    call Lin_Alg_Mod_Vector_Vector_Dot_Product(pap, p, ap)
    alpha = rho / pap

    !---------------------!
    !   x = x + alfa p    !
    !   r = r - alfa Ap   !
    !---------------------!
    !$acc parallel loop
    do i = 1, n
      x(i) = x(i) + alpha * p(i)
    end do
    r(1:n) = r(1:n) - alpha * ap(1:n)

    !------------------!
    !   rho = r' * r   !
    !------------------!
    rho_old = rho
    call Lin_Alg_Mod_Vector_Vector_Dot_Product(rho, r, r)

    print '(a,i3,a,1es10.4)', ' #', iter, '; rho = ', sqrt(rho)
    if(sqrt(rho) < res) goto 1

    !---------------------------------!
    !   p = r + (rho / rho_old) * p   !
    !---------------------------------!
    beta = rho / max(rho_old, 1.0e-12)

    !$acc parallel loop
    do i = 1, n
      p(i) = r(i) + beta * p(i)
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
  call Solvers_Mod_Check_Solution(sparse = a_sparse)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()

  end subroutine
