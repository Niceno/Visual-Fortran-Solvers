!==============================================================================!
  subroutine Solvers_Mod_Check_Solution_Sparse(a)
!------------------------------------------------------------------------------!
!   Check the solution for sparse systems                                      !
!------------------------------------------------------------------------------!
  implicit none
  type(Sparse_Type) :: a
!-----------------------------------[Locals]-----------------------------------!
  real :: error
!==============================================================================!

  call Lin_Alg_Mod_Sparse_X_Vector(y, a, x)

  r = b_o - y

  call Lin_Alg_Mod_Vector_Dot_Vector(error, r, r)

  print '(a,1es10.4)', " # Error:                       ", sqrt(error)

  end subroutine
