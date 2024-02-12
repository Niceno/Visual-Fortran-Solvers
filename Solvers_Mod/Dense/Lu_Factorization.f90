!==============================================================================!
  subroutine Solvers_Mod_Dense_Lu_Factorization(L, U, A)
!------------------------------------------------------------------------------!
!>  Performs LU decomposition of the give matrix "A" and stores the result in
!>  separate matrices L and U (surprise, surprise)
!------------------------------------------------------------------------------!
!   This code was obtained by translating the Java code provided here:
!   https://www.geeksforgeeks.org/doolittle-algorithm-lu-decomposition/
!   using the ChatGPT.                                                         !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Lu                                                           !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type) :: L   !! factorized L matrix
  type(Dense_Type) :: U   !! factorized U matrix
  type(Dense_Type) :: A   !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, bw, n
  real    :: sum
!==============================================================================!

  ! Take some aliases
  n  = A % n
  bw = A % bw

  ! Initialize the values
  L % val(:,:) = 0.0
  U % val(:,:) = 0.0

  ! Perform the factorization
  do k = 1, n

    ! Upper triangular
    do i = k, min(n, k + bw)
      sum = 0.0
      do j = max(1, k - bw, i - bw), min(k-1, k + bw)
        sum = sum + L % val(k,j) * U % val(j,i)
!@      call IO % Plot_Dense("factorization", U, B=A, src1=(/k,j,GREEN2/), src2=(/j,i,CYAN2/))
      end do
      U % val(k,i) = A % val(k,i) - sum
!@    call IO % Plot_Dense("factorization", U, B=A, targ=(/k,i,PINK2/))
    end do

    ! Lower triangular
    do i = max(k, k - bw), min(n, k + bw)
      if(k == i) then
        L % val(k,k) = 1.0  ! Diagonal as 1
        call IO % Plot_Dense("factorization", L, B=A, targ=(/k,k,PINK2/))
      else
        sum = 0.0
        do j = max(1, k - bw, i - bw), min(k-1, k + bw)
          sum = sum + L % val(i,j) * U % val(j,k)
          call IO % Plot_Dense("factorization", L, B=A, src1=(/i,j,GREEN2/), src2=(/j,k,CYAN2/))
        end do
        L % val(i,k) = (A % val(i,k) - sum) / U % val(k,k)
        call IO % Plot_Dense("factorization", L, B=A, targ=(/i,k,PINK2/), src1=(/k,k,CYAN2/))
      end if
    end do

  end do

  call IO % Plot_Snippet(__FILE__, 33, 62)

  end subroutine
