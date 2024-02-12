!==============================================================================!
  subroutine Solvers_Mod_Cg_Cholesky_Prec(grid, A, x, b, n_iter, res, f_in)
!------------------------------------------------------------------------------!
!>  Performs Choleksy (LL') preconditioned CG solution of a linear system.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type)   :: grid     !! computational grid
  type(Sparse_Type) :: A        !! original sparse system matrix
  real, allocatable :: x(:)
  real, allocatable :: b(:)
  integer           :: n_iter   !! maximum number of iterations
  real              :: res      !! target residual
  integer           :: f_in     !! fill-in factor
!-----------------------------------[Locals]-----------------------------------!
  integer                    :: n  ! number of unknowns
  integer                    :: i, iter
  real                       :: alpha, beta, rho_old, rho, pap
  real                       :: time_ps, time_pe, time_ss, time_se
  type(Sparse_Type), pointer :: LL  ! used for Cholesky (LL') factorization
!==============================================================================!

  ! Take aliases
  LL => P_Sparse

  print *, '#============================================================'
  print *, '# Solving the sytem with Cholesky preconditioned CG method'
  print *, '#------------------------------------------------------------'

  !------------------!
  !                  !
  !   Praparations   !
  !                  !
  !------------------!
  call Discretize % On_Sparse_Matrix(grid, A, x, b)
  call Solvers_Mod_Allocate_Vectors(A % n)
  call LL % Sparse_Create_Preconditioning(A, f_in)

  !------------------------!
  !                        !
  !   Actual computation   !
  !                        !
  !------------------------!
  n = A % n

  ! Perform LDL' factorization on the matrix to find the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Sparse_Cholesky_Factorization(LL, A)
  call Cpu_Time(time_pe)

  call IO % Plot_Sparse ("sparse_ll",  LL)
  call IO % Print_Sparse("Sparse LL:", LL)

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
  call Solvers_Mod_Sparse_Forward_Substitution (y, LL, r)  ! LL y = r
  call Solvers_Mod_Sparse_Backward_Substitution(z, LL, y)  ! LL z = y

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
    call Solvers_Mod_Sparse_Forward_Substitution (y, LL, r)  ! LL y = r
    call Solvers_Mod_Sparse_Backward_Substitution(z, LL, y)  ! LL z = y

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
  print '(a,1es10.4)', " # Sqrt(rho):                   ", sqrt(rho)
  call Solvers_Mod_Check_Solution_Sparse(A, x, b)

  !-------------------------!
  !   Clean-up the memory   !
  !-------------------------!
  call Solvers_Mod_Deallocate()
  call A % Sparse_Deallocate()
  deallocate(x)
  deallocate(b)

  end subroutine
