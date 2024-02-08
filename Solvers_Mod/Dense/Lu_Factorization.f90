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
  integer :: i, j, k
  real    :: sum
!==============================================================================!

  do i = 1, A % n

    ! Upper triangular
    do k = i, A % n
      sum = 0.0
      do j = 1, i-1
        sum = sum + L % val(i,j) * U % val(j,k)
      end do
      U % val(i,k) = A % val(i,k) - sum
    end do

    ! Lower triangular
    do k = i, A % n
      if(i == k) then
        L % val(i,i) = 1.0  ! Diagonal as 1
      else
        sum = 0.0
        do j = 1, i-1
          sum = sum + L % val(k,j) * U % val(j,i)
        end do
        L % val(k,i) = (A % val(k,i) - sum) / U % val(i,i)
      end if
    end do

  end do

  end subroutine