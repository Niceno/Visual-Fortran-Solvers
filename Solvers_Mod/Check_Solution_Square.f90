!==============================================================================!
  subroutine Solvers_Mod_Check_Solution_Square(a)
!------------------------------------------------------------------------------!
!   Check the solution                                                         !
!------------------------------------------------------------------------------!
  implicit none
  type(Square_Type) :: a
!-----------------------------------[Locals]-----------------------------------!
  real :: error
!==============================================================================!

  call Lin_Alg_Mod_Square_X_Vector(y, a, x)

  r = b_o - y

  call Lin_Alg_Mod_Vector_Dot_Vector(error, r, r)

  print '(a,1es10.4)', " # Error:                       ", sqrt(error)

  end subroutine
