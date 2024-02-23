!==============================================================================!
  subroutine Solvers_Mod_Dense_Lu_Factorization_Doolittle(LU, A)
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
  integer                   :: i, j, k, s, n, bw
  real                      :: sum
!==============================================================================!

  print *, '# Factorizing dense matrix with LU Doolittle''s method'

  ! Take some aliases
  n  = A % n
  bw = A % bw
  L => LU
  U => LU

  ! Set the type of the matrix (in a sense)
  L % text_l ="L"
  U % text_u ="U"

  !----------------------------------------------------------------------!
  !   Initialize the values by copying the original matrix to LU first   !
  !----------------------------------------------------------------------!
  LU % val(:,:) = 0.0
  do i = 1, n  ! <-A
    do j = max(1, i - bw), min(i + bw, n)
      LU % val(i,j) = A % val(i,j)
    end do
  end do       ! A->

  !-------------------------------!
  !   Perform the factorization   !
  !-------------------------------!
  do k = 1, n  ! <-A

    ! Upper triangular
    do j = k, min(n, k + bw)
      Assert(k <= j)  ! =--> (k,j) in U
      sum = 0.0
      do s = max(1, k - bw, j - bw), k-1  ! s is summation index
        Assert(k > s)  ! =--> (k,s) in L
        Assert(s < j)  ! =--> (s,j) in U
        ! Here, (k,s) is travelling horizontally (in row k)
        ! and (s,j) is travelling vertically (in column j)
        sum = sum + L % val(k,s) * U % val(s,j)
        call IO % Plot_Dense("dens_lu_doolittle", LU, B=A, src1=(/k,s/), src2=(/s,j/))
      end do
      U % val(k,j) = U % val(k,j) - sum
      call IO % Plot_Dense("dens_lu_doolittle", LU, B=A, targ=(/k,j/))
    end do

    ! Lower triangular
    do i = k + 1, min(n, k + bw)  ! do not start from i=k, 'cos diaginal is 1
      Assert(i > k)  ! =--> (i,k) in L
      sum = 0.0
      do s = max(1, k - bw, i - bw), k-1  ! s is summation index
        Assert(s < k)  ! =--> (s,k) in U
        Assert(i > s)  ! =--> (i,s) in L
        ! Here, (i,s) is travelling horizontally (in row i)
        ! and (s,k) is travelling vertically (in column k)
        sum = sum + L % val(i,s) * U % val(s,k)
        call IO % Plot_Dense("dens_lu_doolittle", LU, B=A, src1=(/i,s/), src2=(/s,k/))
      end do
      L % val(i,k) = (L % val(i,k) - sum) / L % val(k,k)
      call IO % Plot_Dense("dens_lu_doolittle", LU, B=A, targ=(/i,k/), src1=(/k,k/))
    end do

  end do       ! A->

  call IO % Plot_Snippet(__FILE__, '<-A', 'A->')

  end subroutine
