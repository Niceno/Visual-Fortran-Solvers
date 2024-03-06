!==============================================================================!
  subroutine Solvers_Mod_Allocate_Vectors(n)
!------------------------------------------------------------------------------!
!>  Allocates memory for vectors used inside the Solvers_Mod.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer, intent(in) :: n
!==============================================================================!

  Assert(n .gt. 0)

  ! These are used in direct solvers ...
  allocate(r(n))
  allocate(y(n))

  ! ... and these are needed, additionally, for iterative ones
  allocate(p(n))
  allocate(q(n))
  allocate(z(n))

  end subroutine
