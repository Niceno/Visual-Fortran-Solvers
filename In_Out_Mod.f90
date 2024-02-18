#include "Assert.h90"
#include "Unused.h90"

!==============================================================================!
  module In_Out_Mod
!------------------------------------------------------------------------------!
!   A suite of routines for input and output                                   !
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Foul_Mod
  use Dense_Mod
  use Sparse_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  !-----------------!
  !                 !
  !   In/Out type   !
  !                 !
  !-----------------!
  type In_Out_Type

    logical :: scale_by_color = .true.
    logical :: scale_by_size  = .true.

    contains
      procedure          :: Plot_Box
      procedure, private :: Plot_Bracket_Left
      procedure, private :: Plot_Bracket_Right
      procedure          :: Plot_Brackets
      procedure          :: Plot_Circle
      procedure          :: Plot_Dense
      procedure          :: Plot_Dense_System
      procedure          :: Plot_Header
      procedure          :: Plot_Ring
      procedure          :: Plot_Snippet
      procedure          :: Plot_Sparse
      procedure          :: Plot_Sparse_System
      procedure          :: Plot_Square
      procedure          :: Plot_Text
      procedure          :: Print_Legend
      procedure          :: Print_Dense
      procedure          :: Print_Sparse

  end type

  ! Singleton object for I/O
  type(In_Out_Type) :: IO

  ! Constants defined with this module
# include "In_Out_Mod/Constants.f90"

  contains

#   include "In_Out_Mod/Legend.f90"
#   include "In_Out_Mod/Load_Linear_System.f90"
#   include "In_Out_Mod/Plot_Box.f90"
#   include "In_Out_Mod/Plot_Bracket_Left.f90"
#   include "In_Out_Mod/Plot_Bracket_Right.f90"
#   include "In_Out_Mod/Plot_Brackets.f90"
#   include "In_Out_Mod/Plot_Circle.f90"
#   include "In_Out_Mod/Plot_Dense.f90"
#   include "In_Out_Mod/Plot_Dense_System.f90"
#   include "In_Out_Mod/Plot_Header.f90"
#   include "In_Out_Mod/Plot_Ring.f90"
#   include "In_Out_Mod/Plot_Snippet.f90"
#   include "In_Out_Mod/Plot_Sparse.f90"
#   include "In_Out_Mod/Plot_Sparse_System.f90"
#   include "In_Out_Mod/Plot_Square.f90"
#   include "In_Out_Mod/Plot_Text.f90"
#   include "In_Out_Mod/Print_Dense.f90"
#   include "In_Out_Mod/Print_Sparse.f90"
#   include "In_Out_Mod/Print_Vector.f90"

  end module
