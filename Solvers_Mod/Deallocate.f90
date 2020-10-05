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
  deallocate(ap)

  if(a_square % n > 0) call Square_Mod_Deallocate(a_square)
  if(p_square % n > 0) call Square_Mod_Deallocate(p_square)
  if(a_sparse % n > 0) call Sparse_Mod_Deallocate(a_sparse)
  if(p_sparse % n > 0) call Sparse_Mod_Deallocate(p_sparse)

  end subroutine
