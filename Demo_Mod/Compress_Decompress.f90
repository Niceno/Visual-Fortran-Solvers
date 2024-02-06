!==============================================================================!
  subroutine Demo_Mod_Compress_Decompress
!------------------------------------------------------------------------------!
  implicit none
!-----------------------------------[Locals]-----------------------------------!
  integer :: col, bw
!==============================================================================!

  ! Full matrix
  integer, parameter    :: n = 7
  real, dimension(n, n) :: f
  data (f(1,col), col=1,n) / 44., -4., -3.,  0., -1.,  0.,  0. /
  data (f(2,col), col=1,n) / -4., 44., -4., -3.,  0., -1.,  0. /
  data (f(3,col), col=1,n) / -3., -4., 44., -4., -3.,  0., -1. /
  data (f(4,col), col=1,n) /  0., -3., -4., 44., -4., -3.,  0. /
  data (f(5,col), col=1,n) / -1.,  0., -3., -4., 44., -4., -3. /
  data (f(6,col), col=1,n) /  0., -1.,  0., -3., -4., 44., -4. /
  data (f(7,col), col=1,n) /  0.,  0., -1.,  0., -3., -4., 44. /

  ! Compressed matrix
  type(Sparse_Type) :: c
  type(Dense_Type)  :: e
!------------------------------------------------------------------------------!

  ! Compress matrix "f" and store it in "c"
  call Sparse_Mod_Compress(c, f)

  call In_Out_Mod_Print_Sparse("c:", c)

  ! Expand matrix "ac2" and store it in "e"
  call Sparse_Mod_Expand(e, c, bw)

  call In_Out_Mod_Print_Dense("Epanded matrix c:", e)

  end subroutine
