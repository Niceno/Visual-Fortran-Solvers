!==============================================================================!
  subroutine Solvers_Mod_Allocate_Vectors(n)
!------------------------------------------------------------------------------!
!>  Allocates memory for vectors used inside the Solvers_Mod.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer, intent(in) :: n
!==============================================================================!

  allocate (b_o(n))  ! needed only for Gauss elimination
  allocate (r  (n))
  allocate (y  (n))

  allocate (p(n))
  allocate (q(n))
  allocate (z(n))

  end subroutine
