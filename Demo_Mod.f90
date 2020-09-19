!==============================================================================!
  module Demo_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Grid_Mod
  use Matrix_Mod
  use In_Out_Mod
  use Lin_Alg_Mod
  use Solvers_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   A suite of routines for solver demonstration                               !
!==============================================================================!

  contains

  include 'Demo_Mod/Compress_Decompress.f90'
  include 'Demo_Mod/Fill_In.f90'
  include 'Demo_Mod/Main.f90'

  end module
