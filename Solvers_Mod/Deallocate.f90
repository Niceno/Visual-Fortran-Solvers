!==============================================================================!
  subroutine Solvers_Mod_Deallocate()
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  deallocate(r)
  deallocate(y)
  deallocate(x)
  deallocate(b_o)
  deallocate(b)

  deallocate(p)
  deallocate(z)
  deallocate(q)

  if(A_Square % n > 0) call A_Square % Dense_Deallocate()
  if(P_Square % n > 0) call P_Square % Dense_Deallocate()
  if(Q_Square % n > 0) call Q_Square % Dense_Deallocate()
  if(A_Sparse % n > 0) call A_Sparse % Sparse_Deallocate()
  if(P_Sparse % n > 0) call P_Sparse % Sparse_Deallocate()
  if(Q_Sparse % n > 0) call Q_Sparse % Sparse_Deallocate()

  end subroutine
