!==============================================================================!
  module Lin_Alg_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   A suite of linear algebra routines                                         !
!==============================================================================!

  contains

  include 'Lin_Alg_Mod/Matrix_Matrix_Multiply.f90'
  include 'Lin_Alg_Mod/Matrix_Vector_Multiply_Compressed.f90'
  include 'Lin_Alg_Mod/Matrix_Vector_Multiply.f90'
  include 'Lin_Alg_Mod/Transpose_Matrix.f90'
  include 'Lin_Alg_Mod/Vector_Vector_Dot_Product.f90'

  end module
