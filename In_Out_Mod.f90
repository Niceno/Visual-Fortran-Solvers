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
  integer, parameter              :: XFIG_BLACK    =   0
  integer, parameter              :: XFIG_BLUE     =   1
  integer, parameter              :: XFIG_GREEN    =   2
  integer, parameter              :: XFIG_CYAN     =   3
  integer, parameter              :: XFIG_RED      =   4
  integer, parameter              :: XFIG_MAGENTA  =   5
  integer, parameter              :: XFIG_YELLOW   =   6
  integer, parameter              :: XFIG_WHITE    =   7
  integer, parameter              :: XFIG_BLUE4    =   8  ! darkest
  integer, parameter              :: XFIG_BLUE3    =   9  ! medium
  integer, parameter              :: XFIG_BLUE2    =  10  ! ligt, but darker than blue
  integer, parameter              :: XFIG_LTBLUE   =  11
  integer, parameter              :: XFIG_GREEN4   =  12  ! darkest
  integer, parameter              :: XFIG_GREEN3   =  13  ! medium
  integer, parameter              :: XFIG_GREEN2   =  14  ! lightest
  integer, parameter              :: XFIG_CYAN4    =  15  ! darkest
  integer, parameter              :: XFIG_CYAN3    =  16  ! medium
  integer, parameter              :: XFIG_CYAN2    =  17  ! lightest
  integer, parameter              :: XFIG_RED4     =  18  ! darkest
  integer, parameter              :: XFIG_RED3     =  19  ! medium
  integer, parameter              :: XFIG_RED2     =  20  ! lightest
  integer, parameter              :: XFIG_MAGENTA4 =  21
  integer, parameter              :: XFIG_MAGENTA3 =  22
  integer, parameter              :: XFIG_MAGENTA2 =  23
  integer, parameter              :: XFIG_BROWN4   =  24
  integer, parameter              :: XFIG_BROWN3   =  25
  integer, parameter              :: XFIG_BROWN2   =  26
  integer, parameter              :: XFIG_PINK4    =  27
  integer, parameter              :: XFIG_PINK3    =  28
  integer, parameter              :: XFIG_PINK2    =  29
  integer, parameter              :: XFIG_PINK     =  30
  integer, parameter              :: XFIG_GOLD     =  31
  integer, parameter              :: XFIG_CM       = 450

  !-----------------!
  !                 !
  !   In/Out type   !
  !                 !
  !-----------------!
  type In_Out_Type

    logical :: scale_by_color = .true.
    logical :: scale_by_size  = .true.

    contains
      procedure :: Plot_Box
      procedure :: Plot_Circle
      procedure :: Plot_Dense
      procedure :: Plot_Header
      procedure :: Plot_Ring
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
#   include "In_Out_Mod/Plot_Box.f90"
#   include "In_Out_Mod/Plot_Circle.f90"
#   include "In_Out_Mod/Plot_Dense.f90"
#   include "In_Out_Mod/Plot_Header.f90"
#   include "In_Out_Mod/Plot_Ring.f90"
#   include "In_Out_Mod/Plot_Snippet.f90"
#   include "In_Out_Mod/Plot_Sparse.f90"
#   include "In_Out_Mod/Print_Dense.f90"
#   include "In_Out_Mod/Print_Sparse.f90"
#   include "In_Out_Mod/Print_Vector.f90"

  end module
