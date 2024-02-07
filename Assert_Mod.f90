!==============================================================================!
  module Assert_Mod
!------------------------------------------------------------------------------!
!>  This module, together with file Assert.h90, creates an assertion system.
!------------------------------------------------------------------------------!
!   The Assert.h header sets up the macro for assertions, Assert_Mod provides  !
!   the module in which the Handle_Assert subroutine is defined, and           !
!   Handle_Assert itself implements the logic for what happens when an         !
!   assertion fails. This system enhances the software's reliability and       !
!   maintainability by enabling thorough internal self-checks and clear        !
!   reporting of issues during development and debugging phases.               !
!==============================================================================!

  contains

#   include "Assert_Mod/Handle_Assert.f90"

  end module

