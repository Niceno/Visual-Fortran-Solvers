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
  real          :: error
  character(10) :: output
!==============================================================================!

  Assert(A % n .eq. size(x))
  Assert(A % n .eq. size(b))

  call Lin_Alg_Mod_Dense_X_Vector(y, A, x)

  r = b - y

  call Lin_Alg_Mod_Vector_Dot_Vector(error, r, r)

  write(output, '(1es10.4)') sqrt(error)
  call Foul % Formatted_Write(' # Error:                       ',  &
                              'white',                             &
                              output,                              &
                              'bright yellow');
  end subroutine
