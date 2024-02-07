!==============================================================================!
  subroutine Solvers_Mod_Check_Solution_Dense(A, x)
!------------------------------------------------------------------------------!
!   Check the solution                                                         !
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Dense_Type) :: A
  real             :: x(:)
!-----------------------------------[Locals]-----------------------------------!
  real :: error
!==============================================================================!

  call Lin_Alg_Mod_Dense_X_Vector(y, A, x)

  r = b_o - y

  call Lin_Alg_Mod_Vector_Dot_Vector(error, r, r)

  print '(A,1es10.4)', " # Error:                       ", sqrt(error)

  end subroutine
