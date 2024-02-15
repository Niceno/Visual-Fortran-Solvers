!==============================================================================!
  subroutine Solvers_Mod_Dense_Lu_Factorization(LU, A)
!------------------------------------------------------------------------------!
!>  Performs LU decomposition of the give matrix "A" and stores the result in
!>  matrix LU, where L's diaognal is not stored since it is equal to one.
!------------------------------------------------------------------------------!
!   This code was obtained by translating the Java code provided here:
!   https://www.geeksforgeeks.org/doolittle-algorithm-lu-decomposition/
!   using the ChatGPT.                                                         !
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
!   Called by:                                                                 !
!   - Solvers_Mod_Lu                                                           !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type), target :: LU  !! factorized LU matrices
  type(Dense_Type)         :: A   !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  type(Dense_Type), pointer :: L  ! pointer to "L"
  type(Dense_Type), pointer :: U  ! pointer to "U"
  integer                   :: i, j, k, n, bw
  real                      :: sum
!==============================================================================!

  ! Take some aliases
  n  = A % n
  bw = A % bw
  L => LU
  U => LU

  ! Initialize the values
  LU % val(:,:) = 0.0

  !-------------------------------!
  !   Perform the factorization   !
  !-------------------------------!
  do k = 1, n  ! <-A

    ! Upper triangular
    do i = k, min(n, k + bw)
      Assert(k <= i)
      sum = 0.0
      do j = max(1, k - bw, i - bw), k-1
        Assert(k > j)  ! =--> (k,j) in L
        Assert(j < i)  ! =--> (j,i) in U
        sum = sum + L % val(k,j) * U % val(j,i)
        call IO % Plot_Dense("dens_lu", LU, B=A, src1=(/k,j,GREEN/), src2=(/j,i,CYAN/))
      end do
      LU % val(k,i) = A % val(k,i) - sum
      call IO % Plot_Dense("dens_lu", LU, B=A, targ=(/k,i,PINK2/))
    end do

    ! Lower triangular
    do i = k + 1, min(n, k + bw)  ! do not start from i=k, 'cos diaginal is 1
      Assert(i >= k)
      sum = 0.0
      do j = max(1, k - bw, i - bw), k-1
        Assert(j < k)  ! =--> (j,k) in U
        Assert(i > j)  ! =--> (i,j) in L
        sum = sum + L % val(i,j) * U % val(j,k)
        call IO % Plot_Dense("dens_lu", LU, B=A, src1=(/i,j,GREEN/), src2=(/j,k,CYAN/))
      end do
      LU % val(i,k) = (A % val(i,k) - sum) / LU % val(k,k)
      call IO % Plot_Dense("dens_lu", LU, B=A, targ=(/i,k,PINK2/), src1=(/k,k,CYAN/))
    end do

  end do       ! A->

  call IO % Plot_Snippet(__FILE__, '<-A', 'A->')

  end subroutine
