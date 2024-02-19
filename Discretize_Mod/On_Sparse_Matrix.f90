!==============================================================================!
  subroutine On_Sparse_Matrix(Discrete, Grid, A, x, b)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Discretize_Type) :: Discrete
  type(Grid_Type)        :: Grid
  type(Sparse_Type)      :: A
  real, allocatable      :: x(:)
  real, allocatable      :: b(:)
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(Discrete)
!==============================================================================!

  ! Create sparse system matrix
  call A % Sparse_Create(Grid, b)

  call IO % Plot_Sparse ("sparse_a",  A)
  call IO % Print_Sparse("Sparse A:", A)

  allocate(x(A % n))
  x(:) = 0.0

  end subroutine
