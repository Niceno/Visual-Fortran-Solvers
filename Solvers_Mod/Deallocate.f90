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
  deallocate(ax)
  deallocate(q)

  if(a_square % n > 0) call Dense_Mod_Deallocate(a_square)
  if(p_square % n > 0) call Dense_Mod_Deallocate(p_square)
  if(q_square % n > 0) call Dense_Mod_Deallocate(q_square)
  if(a_sparse % n > 0) call Sparse_Mod_Deallocate(a_sparse)
  if(p_sparse % n > 0) call Sparse_Mod_Deallocate(p_sparse)
  if(q_sparse % n > 0) call Sparse_Mod_Deallocate(q_sparse)

  end subroutine
