!==============================================================================!
  subroutine Solvers_Mod_Preconditioned_Cg(grid, fill_in, n_iter, res)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
  integer         :: fill_in
  integer         :: n_iter
  real            :: res
!-----------------------------------[Locals]-----------------------------------!
  integer :: n  ! number of unknowns
  integer :: i
  real    :: alpha, res_old, res_new, rr, pap
  real    :: time_ps, time_pe, time_ss, time_se
!==============================================================================!

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

  !--------------------------------!
  !   Calculate initial residual   !
  !--------------------------------!
  call Lin_Alg_Mod_Vector_Vector_Dot_Product(res_old, r, r)

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
  do i = 1, n_iter

    !------------!
    !   p = Ap   !
    !------------!
    call Lin_Alg_Mod_Matrix_Vector_Multiply_Compressed(ap, a_sparse, p)

    !---------------------------!
    !   alpha = res_old / pAp   !
    !---------------------------!
    call Lin_Alg_Mod_Vector_Vector_Dot_Product(pap, p, ap)
    alpha = res_old / pap

    !---------------------!
    !   x = x + alfa p    !
    !   r = r - alfa Ap   !
    !---------------------!
    x(1:n) = x(1:n) + alpha * p(1:n)
    r(1:n) = r(1:n) - alpha * ap(1:n)

    !----------------------!
    !   res_new = r' * r   !
    !----------------------!
    call Lin_Alg_Mod_Vector_Vector_Dot_Product(res_new, r, r)

    print '(a,1es10.4)', ' # res_new = ', sqrt(res_new)

    !-------------------------------------!
    !   p = r + (res_new / res_old) * p   !
    !-------------------------------------!
    rr = res_new / max(res_old, 1.0e-12)
    p(1:n) = r(1:n) + rr * p(1:n)

    res_old = res_new
  end do
  call Cpu_Time(time_se)

  call In_Out_Mod_Print_Vector("Solution x:", x)

  print '(a,1es10.4)', ' # Time for matrix preparation: ', time_pe - time_ps
  print '(a,1es10.4)', ' # Time for solution:           ', time_se - time_ss

  !------------------------!
  !   Check the solution   !
  !------------------------!
  call Solvers_Mod_Check_Solution(sparse = a_sparse)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()

  end subroutine
