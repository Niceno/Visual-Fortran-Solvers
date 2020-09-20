!==============================================================================!
  subroutine Solvers_Mod_Allocate_Vectors(n)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer :: n
!==============================================================================!

  allocate (b  (n))
  allocate (b_o(n))  ! needed only for Gauss elimination
  allocate (x  (n))
  allocate (y  (n))
  allocate (r  (n))

  allocate (p  (n))
  allocate (z  (n))
  allocate (ax (n))
  allocate (ap (n))

  end subroutine
