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
  deallocate(ax)
  deallocate(ap)

  if(allocated(p_matrix)) deallocate(p_matrix)
  if(allocated(a_matrix)) deallocate(a_matrix)
  if(a_sparse % n > 0) call Matrix_Mod_Deallocate(a_sparse)
  if(p_sparse % n > 0) call Matrix_Mod_Deallocate(p_sparse)

  end subroutine
