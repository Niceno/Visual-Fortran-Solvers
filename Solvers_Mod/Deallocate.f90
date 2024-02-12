!==============================================================================!
  subroutine Solvers_Mod_Deallocate()
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  ! These are used in direct solvers ...
  deallocate(y)
  deallocate(r)

  ! ... and these are needed, additionally, for iterative ones
  deallocate(p)
  deallocate(q)
  deallocate(z)

  if(P_Dense % n  > 0) call P_Dense  % Dense_Deallocate()
  if(P_Sparse % n > 0) call P_Sparse % Sparse_Deallocate()

  end subroutine
