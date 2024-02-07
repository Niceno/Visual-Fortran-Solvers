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

  if(P_Square % n > 0) call P_Square % Dense_Deallocate()
  if(Q_Square % n > 0) call Q_Square % Dense_Deallocate()
  if(P_Sparse % n > 0) call P_Sparse % Sparse_Deallocate()
  if(Q_Sparse % n > 0) call Q_Sparse % Sparse_Deallocate()

  end subroutine
