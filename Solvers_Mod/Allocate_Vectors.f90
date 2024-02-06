!==============================================================================!
  subroutine Solvers_Mod_Allocate_Vectors(n)
!------------------------------------------------------------------------------!
!>  Allocates memory for vectors used inside the Solvers_Mod.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer, intent(in) :: n
!==============================================================================!

  allocate (b  (n))
  allocate (b_o(n))  ! needed only for Gauss elimination
  allocate (x  (n))
  allocate (y  (n))
  allocate (r  (n))

  allocate (p(n))
  allocate (z(n))
  allocate (q(n))

  end subroutine
