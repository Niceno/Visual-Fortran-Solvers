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
  integer :: k, j, i
  real    :: sum
!==============================================================================!

  do k = 1, A % n

    ! Upper triangular
    do i = k, min(A % n, k + A % bw)
      sum = 0.0
      do j = max(1, k - A % bw), min(k-1, k + A % bw)
        sum = sum + L % val(k,j) * U % val(j,i)
      end do
      U % val(k,i) = A % val(k,i) - sum
    end do

    ! Lower triangular
    do i = max(k, k - A % bw), min(A % n, k + A % bw)
      if(k == i) then
        L % val(k,k) = 1.0  ! Diagonal as 1
      else
        sum = 0.0
        do j = max(1, k - A % bw), min(k-1, k + A % bw)
          sum = sum + L % val(i,j) * U % val(j,k)
        end do
        L % val(i,k) = (A % val(i,k) - sum) / U % val(k,k)
      end if
    end do

  end do

  call IO % Plot_Snippet(__FILE__, 24, 48)

  end subroutine
