!==============================================================================!
  subroutine Dense_Allocate(A, n)
!------------------------------------------------------------------------------!
!   Deallocates memory occupied by Dense_Type                                  !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Dense_Type)    :: A  !! parent class
  integer, intent(in)  :: n  !! matrix dimension
!------------------------------------------------------------------------------!

  A % n = n               ! store matrix dimension
  allocate(A % val(n,n))  ! allocate matrix entries
  A % val(:,:) = 0.0      ! initial values

  end subroutine
