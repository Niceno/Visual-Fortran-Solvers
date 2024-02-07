!==============================================================================!
  subroutine Solvers_Mod_Check_Solution_Sparse(A, x, b)
!------------------------------------------------------------------------------!
!   Check the solution for sparse systems                                      !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: A     !! original system matrix
  real              :: x(:)  !! final solution
  real              :: b(:)  !! right hand side, should be the original one
!-----------------------------------[Locals]-----------------------------------!
  real :: error
!==============================================================================!

  call Lin_Alg_Mod_Sparse_X_Vector(y, A, x)

  r = b - y

  call Lin_Alg_Mod_Vector_Dot_Vector(error, r, r)

  print '(A,1es10.4)', " # Error:                       ", sqrt(error)

  end subroutine
