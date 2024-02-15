!==============================================================================!
  subroutine Plot_Brackets(IO, fu, rows, cols, depth)
!------------------------------------------------------------------------------!
!>  Plots brackets to the Xfig file.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)  :: IO       !! parent class
  integer, intent(in) :: fu       !! file unit
  integer, intent(in) :: rows(2)  !! starting and ending row
  integer, intent(in) :: cols(2)  !! starting and ending column
  integer, intent(in) :: depth    !! layer
!==============================================================================!

  call IO % Plot_Bracket_Left (fu, rows, cols(1), depth)
  call IO % Plot_Bracket_Right(fu, rows, cols(2), depth)

  end subroutine
