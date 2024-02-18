!==============================================================================!
  subroutine Solvers_Mod_Sparse_Lu_Factorization_Doolittle(LU, A)
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
!   Called by:                                                                 !
!   - Solvers_Mod_Lu                                                           !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type), target :: LU  !! factorized LU matrices
  type(Sparse_Type)         :: A   !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  type(Sparse_Type), pointer :: L  ! pointer to "L"
  type(Sparse_Type), pointer :: U  ! pointer to "U"
  integer                   :: i, j, k, n, ij, ik, ki, is, kj, ks, s, si, sj, sk
  real                      :: sum
  real, allocatable         :: a_val_i(:)  ! stores expanded row i from matrix a
!==============================================================================!

  print *, '# Factorizing dense matrix with LU Doolittle''s method'

  ! Take some aliases
  n  = A % n
  L => LU
  U => LU
  allocate(a_val_i(n));  a_val_i(:) = 0.0

  ! Set the type of the matrix (in a sense)
  L % text_l ="L"
  U % text_u ="U"

  !----------------------------------------------------------------------!
  !   Initialize the values by copying the original matrix to LU first   !
  !----------------------------------------------------------------------!
  LU % val(:) = 0.0
  do i = 1, n  ! <-A

    ! Fill the expanded row with values from A
    a_val_i(:) = 0.0
    do ij = A % row(i), A % row(i+1) - 1
      j = A % col(ij)
      a_val_i(j) = A % val(ij)
    end do

    ! Copy the values from expanded row to LU matrix
    do ij = LU % row(i), LU % row(i+1) - 1
      j = LU % col(ij)
      LU % val(ij) = a_val_i(j)
    end do
  end do       ! A->

  !-------------------------------!
  !   Perform the factorization   !
  !-------------------------------!
  do k = 1, n  ! <-A

    ! Upper triangular
    do kj = LU % dia(k), LU % row(k+1) - 1
      Assert(k <= j)  ! =--> (k,j) in U
      j = LU % col(kj)
      sum = 0.0
      do ks = LU % row(k), LU % dia(k) - 1  ! s is summation index
        s = LU % col(ks)
        Assert(k > s)  ! =--> (k,s) in L
        Assert(s < j)  ! =--> (s,j) in U
        ! Here, (k,s) is travelling horizontally (in row k)
        ! and (s,j) is travelling vertically (in column j)
        do sj = LU % row(s), LU % row(s+1) - 1
          if(LU % col(sj) .eq. j) sum = sum + L % val(ks) * U % val(sj)
        end do
      end do
      U % val(kj) = U % val(kj) - sum
    end do

    ! Lower triangular
    do ki = LU % dia(k) + 1, LU % row(k+1) - 1
      ik = LU % mir(ki)
      i  = LU % col(ki)
      Assert(i > k)  ! =--> (i,k) in L
      sum = 0.0
      do ks = LU % row(k), LU % dia(k) - 1  ! s is summation index
        sk = LU % mir(ks)
        s  = LU % col(ks)
        Assert(s < k)  ! =--> (s,k) in U
        Assert(i > s)  ! =--> (i,s) in L
        ! Here, (i,s) is travelling horizontally (in row i)
        ! and (s,k) is travelling vertically (in column k)
        do si = LU % row(s), LU % row(s+1) - 1
          is = LU % mir(si)
          if(LU % col(si) .eq. i) sum = sum + L % val(is) * U % val(sk)
        end do
      end do
      L % val(ik) = (L % val(ik) - sum) / L % val(LU % dia(k))
    end do

  end do

  call IO % Plot_Snippet(__FILE__, '<-A', 'A->')

  end subroutine
