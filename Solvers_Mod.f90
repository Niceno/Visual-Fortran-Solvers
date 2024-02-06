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
  type(Sparse_Type), target :: a_sparse, p_sparse, q_sparse

  ! Working square (full) matrices
  type(Dense_Type), target :: a_square, p_square, q_square

  ! Working arrays for direct solvers
  real, allocatable :: b(:), b_o(:), x(:), y(:), r(:)

  ! Additional arrays for iterative solvers
  real, allocatable :: p(:), q(:), z(:)

  contains

  ! Memory management
  include 'Solvers_Mod/Allocate_Vectors.f90'
  include 'Solvers_Mod/Deallocate.f90'

  ! Solver components
  include 'Solvers_Mod/Backward_Substitution_Dense.f90'
  include 'Solvers_Mod/Backward_Substitution_Sparse.f90'
  include 'Solvers_Mod/Cholesky_Factorization_Dense.f90'
  include 'Solvers_Mod/Cholesky_Factorization_Sparse.f90'
  include 'Solvers_Mod/Forward_Substitution_Dense.f90'
  include 'Solvers_Mod/Forward_Substitution_Sparse.f90'
  include 'Solvers_Mod/Gauss_Elimination.f90'
  include 'Solvers_Mod/Ldlt_Factorization_Dense.f90'
  include 'Solvers_Mod/Ldlt_Factorization_Sparse.f90'
  include 'Solvers_Mod/Ldlt_Solution_Dense.f90'
  include 'Solvers_Mod/Ldlt_Solution_Sparse.f90'
  include 'Solvers_Mod/Ldlt_Factorization_From_Tflows.f90'
  include 'Solvers_Mod/Ldlt_Solution_From_Tflows.f90'
  include 'Solvers_Mod/Lu_Factorization_Dense.f90'

  ! Full solvers
  include 'Solvers_Mod/Cholesky.f90'
  include 'Solvers_Mod/Ldlt.f90'
  include 'Solvers_Mod/Lu.f90'
  include 'Solvers_Mod/Gauss.f90'

  ! Incomplete solvers
  include 'Solvers_Mod/Incomplete_Cholesky.f90'
  include 'Solvers_Mod/Incomplete_Ldlt.f90'
  include 'Solvers_Mod/Incomplete_Ldlt_From_Tflows.f90'

  ! Iterative solvers
  include 'Solvers_Mod/Cg.f90'
  include 'Solvers_Mod/Cg_Diag_Prec.f90'
  include 'Solvers_Mod/Cg_Tflows_Prec.f90'
  include 'Solvers_Mod/Cg_Ldlt_Prec.f90'

  ! Other functionality
  include 'Solvers_Mod/Check_Solution_Sparse.f90'
  include 'Solvers_Mod/Check_Solution_Dense.f90'
  include 'Solvers_Mod/Prepare_System.f90'

  end module
