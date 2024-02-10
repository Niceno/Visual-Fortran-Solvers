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

  real,   parameter               :: TINY  = 1.0e-30
  real,   parameter, dimension(4) :: SCALE = (/0.5, 0.01, 0.001, TINY/)
  integer, parameter              :: XFIG_BLACK =   0
  integer, parameter              :: XFIG_BLUE  =   1
  integer, parameter              :: XFIG_GREEN =   2
  integer, parameter              :: XFIG_CYAN  =   3
  integer, parameter              :: XFIG_RED   =   4
  integer, parameter              :: XFIG_CM    = 450

  !-----------------!
  !                 !
  !   In/Out type   !
  !                 !
  !-----------------!
  type In_Out_Type

    logical :: scale_by_color = .true.
    logical :: scale_by_size  = .true.

    contains
      procedure :: Plot_Circle
      procedure :: Plot_Dense
      procedure :: Plot_Header
      procedure :: Plot_Snippet
      procedure :: Plot_Sparse
      procedure :: Print_Legend
      procedure :: Print_Dense
      procedure :: Print_Sparse

  end type

  ! Singleton object for I/O
  type(In_Out_Type) :: IO

  contains

#   include "In_Out_Mod/Legend.f90"
#   include "In_Out_Mod/Load_Linear_System.f90"
#   include "In_Out_Mod/Plot_Circle.f90"
#   include "In_Out_Mod/Plot_Dense.f90"
#   include "In_Out_Mod/Plot_Header.f90"
#   include "In_Out_Mod/Plot_Snippet.f90"
#   include "In_Out_Mod/Plot_Sparse.f90"
#   include "In_Out_Mod/Print_Dense.f90"
#   include "In_Out_Mod/Print_Sparse.f90"
#   include "In_Out_Mod/Print_Vector.f90"

  end module
