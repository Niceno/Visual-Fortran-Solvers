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

  !$acc enter data copyin(y)
  call Lin_Alg_Mod_Sparse_X_Vector(y, a, x)
  !$acc update self(y)

  r = b_o - y

  !$acc enter data copyin(error)
  call Lin_Alg_Mod_Vector_Dot_Vector(error, r, r)
  !$acc update self(error)

  print '(a,1es10.4)', " # Error:                       ", sqrt(error)

  end subroutine
