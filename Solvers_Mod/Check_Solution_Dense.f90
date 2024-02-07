!==============================================================================!
  subroutine Solvers_Mod_Check_Solution_Dense(A, x, b)
!------------------------------------------------------------------------------!
!>  Check the solution.
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Dense_Type) :: A     !! original system matrix
  real             :: x(:)  !! final solution
  real             :: b(:)  !! right hand side, should be the original one
!-----------------------------------[Locals]-----------------------------------!
  real :: error
!==============================================================================!

  Assert(A % n .eq. size(x))
  Assert(A % n .eq. size(b))

  call Lin_Alg_Mod_Dense_X_Vector(y, A, x)

  r = b - y

  call Lin_Alg_Mod_Vector_Dot_Vector(error, r, r)

  print '(A,1es10.4)', " # Error:                       ", sqrt(error)

  end subroutine
