!==============================================================================!
  subroutine Demo_Mod_Compress_Decompress
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  ! Full matrix
  integer, parameter :: N = 7

  ! Compressed matrix
  type(Dense_Type)  :: Orig  ! the original matrix
  type(Sparse_Type) :: Spar  ! the sparse matrix
  type(Dense_Type)  :: Dens  ! the dense matrix
!------------------------------------------------------------------------------!

  !------------------------------------------------!
  !   Create the original compressed matrix Orig   !
  !------------------------------------------------!

  ! Allocate the memory
  call Orig % Dense_Allocate(N)

  ! Set the values
  Orig % val(1,1:N) = (/ 44., -4., -3.,  0., -1.,  0.,  0. /)
  Orig % val(2,1:N) = (/ -4., 44., -4., -3.,  0., -1.,  0. /)
  Orig % val(3,1:N) = (/ -3., -4., 44., -4., -3.,  0., -1. /)
  Orig % val(4,1:N) = (/  0., -3., -4., 44., -4., -3.,  0. /)
  Orig % val(5,1:N) = (/ -1.,  0., -3., -4., 44., -4., -3. /)
  Orig % val(6,1:N) = (/  0., -1.,  0., -3., -4., 44., -4. /)
  Orig % val(7,1:N) = (/  0.,  0., -1.,  0., -3., -4., 44. /)

  !-----------------------------------------------!
  !   Compress matrix Orig and store it in Spar   !
  !-----------------------------------------------!
  call Solvers_Mod_Convert_Dense_To_Sparse(Spar, Orig)
  call In_Out_Mod_Print_Sparse("Sparse matrix:", Spar)

  !---------------------------------------------!
  !   Expand matrix Spar and store it in Dens   !
  !---------------------------------------------!
  call Solvers_Mod_Convert_Sparse_To_Dense(Dens, Spar)
  call In_Out_Mod_Print_Dense("Dense matrix:", Dens)

  end subroutine
