!==============================================================================!
  module Solvers_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use In_Out_Mod
  use Lin_Alg_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   A suite of linear solvers                                                  !
!==============================================================================!

  ! Working space matrices for incomplete solvers
  type(Matrix) :: a_sparse, p_sparse

  ! Working (full) matrices
  real, allocatable :: a_matrix(:,:), p_matrix(:,:)

  ! Working arrays
  real, allocatable :: b(:), b_o(:), x(:), y(:), r(:)

  contains

  ! Memory management
  include 'Solvers_Mod/Allocate_Vectors.f90'
  include 'Solvers_Mod/Deallocate.f90'

  ! Solver components
  include 'Solvers_Mod/Backward_Substitution_Compressed.f90'
  include 'Solvers_Mod/Backward_Substitution.f90'
  include 'Solvers_Mod/Cholesky_Factorization_Compressed.f90'
  include 'Solvers_Mod/Cholesky_Factorization.f90'
  include 'Solvers_Mod/Forward_Substitution_Compressed.f90'
  include 'Solvers_Mod/Forward_Substitution.f90'
  include 'Solvers_Mod/Gauss_Elimination.f90'
  include 'Solvers_Mod/Ldlt_Factorization_Compressed.f90'
  include 'Solvers_Mod/Ldlt_Factorization.f90'
  include 'Solvers_Mod/Ldlt_Solution_Compressed.f90'
  include 'Solvers_Mod/Ldlt_Solution.f90'
  include 'Solvers_Mod/Prec_Form.f90'
  include 'Solvers_Mod/Prec_Solve.f90'

  ! Full solvers
  include 'Solvers_Mod/Cholesky.f90'
  include 'Solvers_Mod/Ldlt.f90'
  include 'Solvers_Mod/Gauss.f90'

  ! Incomplete solvers
  include 'Solvers_Mod/Incomplete_Cholesky.f90'
  include 'Solvers_Mod/Incomplete_Ldlt.f90'
  include 'Solvers_Mod/Incomplete_Ldlt_From_Tflows.f90'

  ! Other functionality
  include 'Solvers_Mod/Check_Solution.f90'
  include 'Solvers_Mod/Prepare_System.f90'

  end module
