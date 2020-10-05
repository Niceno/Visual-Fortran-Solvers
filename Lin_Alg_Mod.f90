!==============================================================================!
  module Lin_Alg_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Square_Mod  ! square (full) matrix
  use Sparse_Mod  ! sparse matrix
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   A suite of linear algebra routines                                         !
!==============================================================================!

  contains

  include 'Lin_Alg_Mod/Sparse_X_Vector.f90'
  include 'Lin_Alg_Mod/Square_T.f90'
  include 'Lin_Alg_Mod/Square_X_Square.f90'
  include 'Lin_Alg_Mod/Square_X_Vector.f90'
  include 'Lin_Alg_Mod/Vector_Dot_Vector.f90'

  end module
