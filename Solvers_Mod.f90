!==============================================================================!
  module Solvers_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   A suite of linear solvers                                                  !
!==============================================================================!

  contains

  include 'Solvers_Mod/Backward_Substitution_Compressed.f90'
  include 'Solvers_Mod/Backward_Substitution.f90'
  include 'Solvers_Mod/Cholesky_Factorization_Compressed.f90'
  include 'Solvers_Mod/Cholesky_Factorization.f90'
  include 'Solvers_Mod/Forward_Substitution_Compressed.f90'
  include 'Solvers_Mod/Forward_Substitution.f90'
  include 'Solvers_Mod/Gaussian_Elimination.f90'
  include 'Solvers_Mod/Ldlt_Factorization_Compressed.f90'
  include 'Solvers_Mod/Ldlt_Factorization.f90'
  include 'Solvers_Mod/Ldlt_Solution_Compressed.f90'
  include 'Solvers_Mod/Ldlt_Solution.f90'
  include 'Solvers_Mod/Prec_Form.f90'
  include 'Solvers_Mod/Prec_Solve.f90'

  end module
