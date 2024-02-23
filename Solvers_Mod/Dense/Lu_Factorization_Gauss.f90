!==============================================================================!
  subroutine Solvers_Mod_Dense_Lu_Factorization_Gauss(LU, A)
!------------------------------------------------------------------------------!
!>  Performs LU factorization the given dense matrix "A", based on the
!>  Gaussian elimination.
!------------------------------------------------------------------------------!
!   LU factorization in full, looks like this:                                 !
!                                                                              !
!        |  1                  | | U11 U12 U13 U14 U15 |                       !
!        | L21  1              | |     U22 U23 U24 U25 |                       !
!   LU = | L31 L32  1          | |         U33 U34 U35 |                       !
!        | L41 L42 L43  1      | |             U44 U45 |                       !
!        | L51 L52 L53 L54  1  | |                 U55 |                       !
!                                                                              !
!   But given that L's diagonal is equal to one, it doesn't have to be stored: !
!                                                                              !
!              | U11 U12 U13 U14 U15 |                                         !
!     stored   | L21 U22 U23 U24 U25 |                                         !
!   LU       = | L31 L32 U33 U34 U35 |                                         !
!              | L41 L42 L43 U44 U45 |                                         !
!              | L51 L52 L53 L54 U55 |                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type) :: LU  !! factorized matrix
  type(Dense_Type) :: A   !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, n, bw
  real    :: mult
!==============================================================================!

  print *, '# Factorizing dense matrix with LU Gaussian method'

  ! Take some aliases
  n  = A % n
  bw = A % bw

  ! Set the type of the matrix (in a sense)
  LU % text_u ="L"
  LU % text_l ="U"

  !----------------------------------------------------------------------!
  !   Initialize the values by copying the original matrix to LU first   !
  !----------------------------------------------------------------------!
  LU % val(:,:) = 0.0
  do i = 1, n  ! <-A
    do j = max(1, i - bw), min(i + bw, n)
      LU % val(i,j) = A % val(i,j)
    end do
  end do       ! A->

  !-------------------------------------------------------!
  !   Perform the factorization on the resulting matrix   !
  !-------------------------------------------------------!
  do k = 1, n - 1  ! <-A
    do i = k + 1, min(k + bw, n)
      Assert(i > k)  ! =--> (i,k) in L
      mult = LU % val(i,k) / LU % val(k,k)
      LU % val(i,k) = mult
      call IO % Plot_Dense("dens_lu_gauss", LU, B=A, targ=(/i,k/), src1=(/k,k/))

      do j = k + 1, min(k + bw, n)
        Assert(k < j)  ! =--> (k,j) in U
        LU % val(i,j) = LU % val(i,j) - mult * LU % val(k,j)
        call IO % Plot_Dense("dens_lu_gauss", LU, B=A, targ=(/i,j/), src1=(/k,j/))
      end do
    end do

  end do           ! A->

  call IO % Plot_Snippet(__FILE__, '<-A', 'A->')

  end subroutine
