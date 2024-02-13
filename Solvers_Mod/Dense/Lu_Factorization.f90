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
!   LU decomposition in full, looks like this:                                 !
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
  type(Dense_Type) :: LU  !! factorized LU matrices
  type(Dense_Type) :: A   !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, n, bw
  real    :: sum
!==============================================================================!

  ! Take some aliases
  n  = A % n
  bw = A % bw

  ! Initialize the values
  LU % val(:,:) = 0.0

  !-------------------------------!
  !   Perform the factorization   !
  !-------------------------------!
  do k = 1, n

    ! Upper triangular
    do i = k, min(n, k + bw)
      sum = 0.0
      do j = max(1, k - bw, i - bw), min(k-1, k + bw)
        sum = sum + LU % val(k,j) * LU % val(j,i)
        call IO % Plot_Dense("factorization", LU, B=A, src1=(/k,j,GREEN/), src2=(/j,i,CYAN/))
      end do
      LU % val(k,i) = A % val(k,i) - sum
      call IO % Plot_Dense("factorization", LU, B=A, targ=(/k,i,PINK2/))
    end do

    ! Lower triangular
    do i = max(k, k - bw), min(n, k + bw)
      if(k == i) then
        ! L % val(k,k) = 1.0  ! skip this, do not store ones
      else
        sum = 0.0
        do j = max(1, k - bw, i - bw), min(k-1, k + bw)
          sum = sum + LU % val(i,j) * LU % val(j,k)
          call IO % Plot_Dense("factorization", LU, B=A, src1=(/i,j,GREEN/), src2=(/j,k,CYAN/))
        end do
        LU % val(i,k) = (A % val(i,k) - sum) / LU % val(k,k)
        call IO % Plot_Dense("factorization", LU, B=A, targ=(/i,k,PINK2/), src1=(/k,k,CYAN/))
      end if
    end do

  end do

  call IO % Plot_Snippet(__FILE__, 49, 77)

  end subroutine
