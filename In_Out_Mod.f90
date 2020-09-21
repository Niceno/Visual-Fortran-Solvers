!==============================================================================!
  module In_Out_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Foul_Mod
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   A suite of routines for input and output                                   !
!==============================================================================!

  real, parameter               :: TINY  = 1.0e-30
  real, parameter, dimension(4) :: SCALE = (/0.5, 0.01, 0.001, TINY/)

  contains

!==============================================================================!
  subroutine In_Out_Mod_Legend(row, max_val)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer :: row
  real    :: max_val
!==============================================================================!

  if(row .eq. 1) then
    call Foul_Mod_Write('  ', 'red',  &
                        '  ', 'red background_red', forward='no')
    print '(a3,1es11.3)', '  >', SCALE(1) * max_val
  else if(row .eq. 2) then
    call Foul_Mod_Write('  ', 'yellow',  &
                        '  ', 'yellow background_yellow', forward='no')
    print '(a3,1es11.3)', '  >', SCALE(2) * max_val
  else if(row .eq. 3) then
    call Foul_Mod_Write('  ', 'green',  &
                        '  ', 'green background_green', forward='no')
    print '(a3,1es11.3)', '  >', SCALE(3) * max_val
  else if(row .eq. 4) then
    call Foul_Mod_Write('  ', 'blue',  &
                        '  ', 'blue background_blue', forward='no')
    print '(a3,1es11.3)', '  >', SCALE(4) * max_val
  else if(row .eq. 5) then
    call Foul_Mod_Write('  ', 'magenta',  &
                        '  ', 'magenta background_magenta', forward='no')
    print '(a3,1es11.3)', '  =', 0.0
  else
    print *, ""
  end if

  end subroutine

  include 'In_Out_Mod/Load_Linear_System.f90'
  include 'In_Out_Mod/Print_Matrix_Compressed.f90'
  include 'In_Out_Mod/Print_Matrix.f90'
  include 'In_Out_Mod/Print_Vector.f90'

  end module
