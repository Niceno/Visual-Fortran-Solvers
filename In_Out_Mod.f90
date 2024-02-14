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

  real,   parameter               :: TINY  = 1.0e-30
  real,   parameter, dimension(4) :: SCALE = (/0.5, 0.01, 0.001, TINY/)
  integer, parameter              :: BLACK    =   0
  integer, parameter              :: BLUE     =   1
  integer, parameter              :: GREEN    =   2
  integer, parameter              :: CYAN     =   3
  integer, parameter              :: RED      =   4
  integer, parameter              :: MAGENTA  =   5
  integer, parameter              :: YELLOW   =   6
  integer, parameter              :: WHITE    =   7
  integer, parameter              :: BLUE4    =   8  ! darkest
  integer, parameter              :: BLUE3    =   9  ! medium
  integer, parameter              :: BLUE2    =  10  ! light
  integer, parameter              :: LTBLUE   =  11
  integer, parameter              :: GREEN4   =  12  ! darkest
  integer, parameter              :: GREEN3   =  13  ! medium
  integer, parameter              :: GREEN2   =  14  ! lightest
  integer, parameter              :: CYAN4    =  15  ! darkest
  integer, parameter              :: CYAN3    =  16  ! medium
  integer, parameter              :: CYAN2    =  17  ! lightest
  integer, parameter              :: RED4     =  18  ! darkest
  integer, parameter              :: RED3     =  19  ! medium
  integer, parameter              :: RED2     =  20  ! lightest
  integer, parameter              :: MAGENTA4 =  21
  integer, parameter              :: MAGENTA3 =  22
  integer, parameter              :: MAGENTA2 =  23
  integer, parameter              :: BROWN4   =  24
  integer, parameter              :: BROWN3   =  25
  integer, parameter              :: BROWN2   =  26
  integer, parameter              :: PINK4    =  27
  integer, parameter              :: PINK3    =  28
  integer, parameter              :: PINK2    =  29
  integer, parameter              :: PINK     =  30
  integer, parameter              :: GOLD     =  31
  integer, parameter              :: CM       = 450

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
      procedure :: Plot_Square
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
#   include "In_Out_Mod/Plot_Square.f90"
#   include "In_Out_Mod/Print_Dense.f90"
#   include "In_Out_Mod/Print_Sparse.f90"
#   include "In_Out_Mod/Print_Vector.f90"

  end module
