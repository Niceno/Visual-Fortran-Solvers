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

  ! Copy matrix (a_sparse), vectors (ax - b) and scalars (alpha - pap) to GPU
  !$acc enter data copyin(a_sparse)
  !$acc enter data copyin(a_sparse % row(:))
  !$acc enter data copyin(a_sparse % col(:))
  !$acc enter data copyin(a_sparse % val(:))
  !$acc enter data copyin(ax(:), ap(:), x(:), p(:), r(:), b(:))
  !$acc enter data copyin(alpha, beta, rho, rho_old, pap)

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
  !$acc  parallel loop      &
  !$acc& present(r, b, ax)
  do i = 1, n
    r(i) = b(i) - ax(i)
  end do

  !------------------!
  !   rho = r' * r   !
  !------------------!
  call Lin_Alg_Mod_Vector_Vector_Dot_Product(rho, r, r)

  !-----------!
  !   p = r   !
  !-----------!
  !$acc  parallel loop  &
  !$acc& present(p, r)
  do i = 1, n
    p(i) = r(i)
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
    call Lin_Alg_Mod_Matrix_Vector_Multiply_Compressed(ap, a_sparse, p)

    !-----------------------!
    !   alpha = rho / pAp   !
    !-----------------------!
    call Lin_Alg_Mod_Vector_Vector_Dot_Product(pap, p, ap)

    !$acc kernels present(alpha, rho, pap)
    alpha = rho / pap
    !$acc end kernels

    !---------------------!
    !   x = x + alfa p    !
    !   r = r - alfa Ap   !
    !---------------------!
    !$acc  parallel loop  &
    !$acc& present(x, alpha, p)
    do i = 1, n
      x(i) = x(i) + alpha * p(i)
    end do

    !$acc  parallel loop   &
    !$acc& present(r, alpha, ap)
    do i = 1, n
      r(i) = r(i) - alpha * ap(i)
    end do

    !------------------!
    !   rho = r' * r   !
    !------------------!
    !$acc parallel present(rho_old, rho)
    rho_old = rho
    !$acc end parallel

    call Lin_Alg_Mod_Vector_Vector_Dot_Product(rho, r, r)

    !$acc update self(rho)
    print '(a,i3,a,1es10.4)', ' #', iter, '; rho = ', sqrt(rho)
    if(sqrt(rho) < res) goto 1

    !---------------------------------!
    !   p = r + (rho / rho_old) * p   !
    !---------------------------------!
    !$acc parallel present(beta, rho, rho_old)
    beta = rho / max(rho_old, 1.0e-12)
    !$acc end parallel

    !$acc  parallel loop  &
    !$acc& present(p, r, beta)
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

  ! Clean the data from the device
  !$acc exit data delete(alpha, beta, rho, rho_old, pap)
  !$acc exit data delete(ax, ap, x, p, r, b)
  !$acc exit data delete(a_sparse % val(:))
  !$acc exit data delete(a_sparse % col(:))
  !$acc exit data delete(a_sparse % row(:))
  !$acc exit data delete(a_sparse)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()

  end subroutine
