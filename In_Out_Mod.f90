!==============================================================================!
  module In_Out_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Foul_Mod
  use Dense_Mod
  use Sparse_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   A suite of routines for input and output                                   !
!==============================================================================!

  real, parameter               :: TINY  = 1.0e-30
  real, parameter, dimension(4) :: SCALE = (/0.5, 0.01, 0.001, TINY/)

  contains

  include 'In_Out_Mod/Legend.f90'
  include 'In_Out_Mod/Load_Linear_System.f90'
  include 'In_Out_Mod/Print_Sparse.f90'
  include 'In_Out_Mod/Print_Dense.f90'
  include 'In_Out_Mod/Print_Vector.f90'

  end module
