!==============================================================================!
  subroutine Solvers_Mod_Sparse_Cholesky_Factorization(L, A)
!------------------------------------------------------------------------------!
!>  Computes Cholesky decomposition on sparse matrices.
!------------------------------------------------------------------------------!
!   Cholesky factorization in full, looks like this:                           !
!                                                                              !
!         | L11                 | | L11 L12             |                      !
!         | L21 L22             | |     L22 L23         |                      !
!   LL' = | L31 L32 L33         | |         L33 L34     |                      !
!         |     L42 L43 L44     | |             L44 L45 |                      !
!         |         L53 L54 L55 | |                 L55 |                      !
!                                                                              !
!   But given that LL's is symmetric, only one L is stored:                    !
!                                                                              !
!              | L11                 |                                         !
!     stored   | L21 L22             |                                         !
!   LL'      = | L31 L32 L33         |                                         !
!              |     L42 L43 L44     |                                         !
!              |         L53 L54 U55 |                                         !
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Cholesky                                          !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: L  !! factorized matrices (only L is stored)
  type(Sparse_Type) :: A  !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  integer           :: i, k, n, s, ki, ik, is, ks
  real              :: sum
  real              :: a_val_ik    ! stores value of matrix A at i,k
  real, allocatable :: l_val_i(:)  ! stores expanded row i from matrix L
!==============================================================================!

  print *, '# Factorizing sparse matrix with Cholesky method'

  ! Take some aliases
  n = A % n  ! some checks would be possible

  ! Set the type of the matrix (in a sense)
  L % text_u ="U=L'"
  L % text_l ="L"

  ! Allocate local memory
  allocate( l_val_i(n) ); l_val_i = 0.0

  !-------------------------------!
  !   Perform the factorization   !
  !-------------------------------!
  do k = 1, n  ! <-A

    ! Work out (and store) diagonal term
    sum = 0.0
    do ks = L % row(k), L % dia(k) - 1
      s = L % col(ks)
      Assert(k > s)  ! =--> (k,s) in L
      sum = sum + L % val(ks)**2
      call IO % Plot_Sparse("spar_chol", L, B=A, src1=(/k,s/))
    end do
    L % val(L % dia(k)) = sqrt(A % val(A % dia(k)) - sum)
    call IO % Plot_Sparse("spar_chol", L, B=A, targ=(/k,k/))

    ! Work out (and store) the L
    do ki = L % dia(k) + 1, L % row(k+1) - 1
      i = L % col(ki)
      Assert(i > k)  ! =--> (i,k) in L

      ! Expand row i, up to the diagonal (this will store entries L(i,s)
      do is = L % row(i), L % dia(i) - 1
        s = L % col(is)
        l_val_i(s) = L % val(is)
      end do

      ! Now when you know i, you should find A(i,k)
      a_val_ik = 0.0
      do ik = A % row(i), A % row(i+1) - 1
        if(A % col(ik) .eq. k) then
          a_val_ik = A % val(ik)  ! i > k =--> A(i,k) is L
          exit
        end if
      end do

      ! Now sum rows in i and k: L(i,s) * L(k,s)
      sum = 0.0
      do ks = L % row(k), L % dia(k) - 1
        s = L % col(ks)
        Assert(k > s)  ! =--> (k,s) in L
        Assert(i > s)  ! =--> (i,s) in L
        sum = sum + L % val(ks) * l_val_i(s)
        call IO % Plot_Sparse("spar_chol", L, B=A, src1=(/i,s/), src2=(/k,s/))
      end do

      ! Store the lower part only.  At this point you have ki (k,i) for L
      ! but ik (i,k) is defined for A, which might have a different
      ! sparsity pattern.  Hence, it is easier to use mirror of ki for L
      L % val(L % mir(ki)) = (a_val_ik - sum) / L % val(L % dia(k))
      call IO % Plot_Sparse("spar_chol", L, B=A, targ=(/i,k/))

      l_val_i(:) = 0.0
    end do

  end do       ! A->

  call IO % Plot_Snippet(__FILE__, '<-A', 'A->')

  end subroutine
