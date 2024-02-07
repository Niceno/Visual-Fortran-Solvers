!==============================================================================!
  module Solvers_Mod
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use In_Out_Mod
  use Lin_Alg_Mod
  use Discretize_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   A suite of linear solvers                                                  !
!==============================================================================!

  ! Working space matrices for incomplete solvers
  type(Sparse_Type), target :: P_Sparse, Q_Sparse

  ! Working square (full) matrices
  type(Dense_Type), target :: P_Square, Q_Square

  ! Working arrays for direct solvers
  real, allocatable :: b_o(:), y(:), r(:)

  ! Additional arrays for iterative solvers
  real, allocatable :: p(:), q(:), z(:)

  contains

  ! Memory management
  include 'Solvers_Mod/Allocate_Vectors.f90'
  include 'Solvers_Mod/Deallocate.f90'

  ! Matrix conversion
  include "Solvers_Mod/Convert/Dense_To_Sparse.f90"
  include "Solvers_Mod/Convert/Sparse_To_Dense.f90"

  ! Dense solver components
  include 'Solvers_Mod/Dense/Backward_Substitution.f90'
  include 'Solvers_Mod/Dense/Cholesky_Factorization.f90'
  include 'Solvers_Mod/Dense/Forward_Substitution.f90'
  include 'Solvers_Mod/Dense/Gauss_Elimination.f90'
  include 'Solvers_Mod/Dense/Ldlt_Factorization.f90'
  include 'Solvers_Mod/Dense/Ldlt_Solution.f90'
  include 'Solvers_Mod/Dense/Lu_Factorization.f90'

  ! Sparse solver components
  include 'Solvers_Mod/Sparse/Ldlt_Factorization.f90'
  include 'Solvers_Mod/Sparse/Ldlt_Solution.f90'
  include 'Solvers_Mod/Sparse/Tflows_Ldlt_Factorization.f90'
  include 'Solvers_Mod/Sparse/Tflows_Ldlt_Solution.f90'

  ! Direct solvers
  include 'Solvers_Mod/Dense/Cholesky.f90'
  include 'Solvers_Mod/Dense/Gauss.f90'
  include 'Solvers_Mod/Dense/Ldlt.f90'
  include 'Solvers_Mod/Dense/Lu.f90'

  ! Incomplete solver components
  include 'Solvers_Mod/Incomplete/Backward_Substitution.f90'
  include 'Solvers_Mod/Incomplete/Forward_Substitution.f90'
  include 'Solvers_Mod/Incomplete/Cholesky_Factorization.f90'

  ! Incomplete solvers
  include 'Solvers_Mod/Incomplete/Cholesky.f90'
  include 'Solvers_Mod/Incomplete/Ldlt.f90'
  include 'Solvers_Mod/Incomplete/Ldlt_From_Tflows.f90'

  ! Iterative solvers
  include 'Solvers_Mod/Sparse/Cg_Diag_Prec.f90'
  include 'Solvers_Mod/Sparse/Cg_Ldlt_Prec.f90'
  include 'Solvers_Mod/Sparse/Cg_No_Prec.f90'
  include 'Solvers_Mod/Sparse/Cg_Tflows_Prec.f90'

  ! Other functionality
  include 'Solvers_Mod/Check_Solution_Dense.f90'
  include 'Solvers_Mod/Check_Solution_Sparse.f90'
  include 'Solvers_Mod/Prepare_System.f90'

  end module
