!==============================================================================!
  subroutine Solvers_Mod_Sparse_Lu_Factorization_Gauss(LU, A)
!------------------------------------------------------------------------------!
!>  Performs LU factorization the given sparse matrix "A", based on the
!>  Gaussian elimination.
!------------------------------------------------------------------------------!
!   LU factorization in full, looks like this:                                 !
!                                                                              !
!        |  1                  | | U11 U12 U13         |                       !
!        | L21  1              | |     U22 U23 U24     |                       !
!   LU = | L31 L32  1          | |         U33 U34 U35 |                       !
!        |     L42 L43  1      | |             U44 U45 |                       !
!        |         L53 L54  1  | |                 U55 |                       !
!                                                                              !
!   But given that L's diagonal is equal to one, it doesn't have to be stored: !
!                                                                              !
!              | U11 U12 U13         |                                         !
!     stored   | L21 U22 U23 U24     |                                         !
!   LU       = | L31 L32 U33 U34 U35 |                                         !
!              |     L42 L43 U44 U45 |                                         !
!              |         L53 L54 U55 |                                         !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type)   :: LU  !! factorized matrix
  type(Sparse_Type)   :: A   !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  integer           :: i, j, k, kj, ik, ki, ij, n, j_in_k, j_in_i
  real              :: mult
  real, allocatable :: a_val_i(:)  ! stores expanded row i from matrix a
!==============================================================================!

  ! Take some aliases
  n  = A % n
  allocate(a_val_i(n));  a_val_i(:) = 0.0

  !----------------------------------------------------------------------!
  !   Initialize the values by copying the original matrix to LU first   !
  !----------------------------------------------------------------------!
  do i = 1, n  ! <-A

    ! Fill the expanded row with values from A
    a_val_i(:) = 0.0
    do ij = A % row(i), A % row(i+1) - 1
      j = A % col(ij)
      a_val_i(j) = A % val(kj)
    end do

    ! Copy the values from expanded row to LU matrix
    do ij = LU % row(i), LU % row(i+1) - 1
      j = LU % col(ij)
      LU % val(ij) = a_val_i(j)
    end do

  end do       ! A->

  !-------------------------------------------------------!
  !   Perform the factorization on the resulting matrix   !
  !-------------------------------------------------------!
  do k = 1, n - 1  ! <-A

    do ki = LU % dia(k)+1, LU % row(k+1) - 1
      i = LU % col(ki)
      Assert(i > k)      ! =--> (i,k) in L
      ik = LU % mir(ki)  ! go to L instad
      mult = LU % val(ik) / LU % val(LU % dia(k))
      LU % val(ik) = mult
      call IO % Plot_Sparse("spar_lu_gauss", LU, B=A, targ=(/i,k,YELLOW/), src1=(/k,k,GREEN/))

      do kj = LU % dia(k) + 1, LU % row(k+1) - 1  ! through columns of row k
        j_in_k = LU % col(kj)                     ! column in row k
        Assert(k < j_in_k)  ! =--> (k,j) in U
        do ij = LU % row(i), LU % row(i+1) - 1    ! through columns of row i
          j_in_i = LU % col(ij)                   ! column in row i
          if(j_in_k .eq. j_in_i) then
            LU % val(ij) = LU % val(ij) - mult * LU % val(kj)
            call IO % Plot_Sparse("spar_lu_gauss", LU, B=A, targ=(/i,j,PINK2/), src1=(/k,j,CYAN/))
          end if
        end do
      end do
    end do

  end do           ! A->

  call IO % Plot_Snippet(__FILE__, '<-A', 'A->')

  end subroutine
