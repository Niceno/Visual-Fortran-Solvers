!==============================================================================!
  subroutine Demo_Mod_Ldlt_Solver(grid)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Grid_Type) :: grid
!-----------------------------------[Locals]-----------------------------------!
  integer           :: n
  real, allocatable :: a_matrix(:,:), p_matrix(:,:)
  real, allocatable :: b(:), x(:), y(:), r(:)
  real              :: error, time_ps, time_pe, time_ss, time_se
  type(Matrix)      :: c_matrix
!==============================================================================!

  !------------------!
  !   Praparations   !
  !------------------!

  ! Create compressed system matrix
  call Matrix_Mod_Create_Compressed(c_matrix, grid)
  call In_Out_Mod_Print_Matrix_Compressed("Compressed c_matrix:", c_matrix)

  ! Create two full matrices from the compressed one
  call Matrix_Mod_Expand(a_matrix, c_matrix)
  call Matrix_Mod_Expand(p_matrix, c_matrix)
  p_matrix = 0

  ! Finish memory allocation
  n = c_matrix % n
  allocate (b(n))
  allocate (x(n))
  allocate (y(n))
  allocate (r(n))

  ! Fill the right hand side
  b = 0.1

  ! Just print original matrix
  call In_Out_Mod_Print_Matrix("a_matrix:", a_matrix)

  !------------------------!
  !   Actual computation   !
  !------------------------!

  ! Perform LDLT factorization on the matrix to fin the lower one
  call Cpu_Time(time_ps)
  call Solvers_Mod_Ldlt_Factorization(p_matrix, a_matrix)
  call Cpu_Time(time_pe)
  call In_Out_Mod_Print_Matrix(   &
       "p_matrix after Cholesky factorization", p_matrix)

  ! Compute x
  call Cpu_Time(time_ss)
  call Solvers_Mod_Ldlt_Solution(x, p_matrix, b)
  call Cpu_Time(time_se)
  call In_Out_Mod_Print_Vector("Solution x:", x)

  write(*,*) '# Time for matrix preparation:', time_pe - time_ps
  write(*,*) '# Time for solution:          ', time_se - time_ss

  !------------------------!
  !   Check the solition   !
  !------------------------!
  call Lin_Alg_Mod_Matrix_Vector_Multiply(y, a_matrix, x)
  call In_Out_Mod_Print_Vector("Vector y should recover the source term:", y)
  r = b - y
  call Lin_Alg_Mod_Vector_Vector_Dot_Product(error, r, r)
  write(*,*) "Error: ", sqrt(error)

  deallocate(a_matrix)
  deallocate(p_matrix)
  deallocate(b)
  deallocate(x)
  deallocate(y)
  deallocate(r)
  call Matrix_Mod_Deallocate(c_matrix)

  end subroutine
