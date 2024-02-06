!==============================================================================!
  module Lin_Alg_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Dense_Mod   ! square (full) matrix
  use Sparse_Mod  ! sparse matrix
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   A suite of linear algebra routines                                         !
!==============================================================================!

  contains

  include 'Lin_Alg_Mod/Sparse_X_Vector.f90'
  include 'Lin_Alg_Mod/Dense_T.f90'
  include 'Lin_Alg_Mod/Dense_X_Dense.f90'
  include 'Lin_Alg_Mod/Dense_X_Vector.f90'
  include 'Lin_Alg_Mod/Vector_Dot_Vector.f90'

  end module
