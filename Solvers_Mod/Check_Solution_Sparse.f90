!==============================================================================!
  subroutine Solvers_Mod_Check_Solution_Sparse(A, x, b)
!------------------------------------------------------------------------------!
!>  Check the solution for sparse systems.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: A     !! original system matrix
  real              :: x(:)  !! final solution
  real              :: b(:)  !! right hand side, should be the original one
!-----------------------------------[Locals]-----------------------------------!
  real          :: error
  character(10) :: output
!==============================================================================!

  Assert(A % n .eq. size(x))
  Assert(A % n .eq. size(b))

  ! Save the results for visualization
  call A % pnt_grid % Save_Vtk_Debug("solution.vtk", x)

  ! Compute y=Ax
  call Lin_Alg_Mod_Sparse_X_Vector(y, A, x)

  ! Compute r=b-y
  r = b - y

  call Lin_Alg_Mod_Vector_Dot_Vector(error, r, r)

  write(output, '(1es10.4)') sqrt(error)
  call Foul % Formatted_Write(' # Error:                       ',  &
                              'white',                             &
                              output,                              &
                              'bright yellow');
  end subroutine
