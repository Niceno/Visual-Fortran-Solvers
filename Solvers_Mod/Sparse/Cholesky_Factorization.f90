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
  type(Sparse_Type) :: L  !! factorized matrices (two in one)
  type(Sparse_Type) :: A   !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  integer           :: i, k, n, s, ki, ik, is, ks
  real              :: sum
  real              :: a_val_ik    ! stores value of matrix A at i,k
  real, allocatable :: l_val_k(:)  ! stores expanded row k from matrix L
!==============================================================================!

  print *, '# Factorizing sparse matrix with Cholesky method'

  n = A % n  ! some checks would be possible

  ! Allocate local memory
  allocate( l_val_k(n) ); l_val_k = 0.0

  !-------------------------------!
  !   Perform the factorization   !
  !-------------------------------!
  do k = 1, n

    ! Work out (and store) diagonal term
    sum = 0.0
    do ks = L % row(k), L % dia(k) - 1
      s = L % col(ks)
      sum = sum + L % val(ks)**2
      if(k.eq.n/2) call IO % Plot_Sparse("sparse", L, B=A, src1=(/k,s,GREEN/))
    end do
    L % val(L % dia(k)) = sqrt(A % val(A % dia(k)) - sum)
    if(k.eq.n/2) call IO % Plot_Sparse("sparse", L, B=A, targ=(/k,k,PINK2/))

    ! Expand row k, up to the diagonal (this will store entries L(k,s)
    do ks = L % row(k), L % dia(k) - 1
      s = L % col(ks)
      l_val_k(s) = L % val(ks)
    end do

    ! Work out (and store) the L
    do ki = L % dia(k) + 1, L % row(k+1) - 1
      i = L % col(ki)  ! i > k
      Assert(i > k)

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
      do is = L % row(i), L % dia(i) - 1
        s = L % col(is)
        sum = sum + l_val_k(s) * L % val(is)
      end do

      ! Store the lower part only.  At this point you have ki (k,i) for L
      ! but ik (i,k) is defined for A, which might have a different
      ! sparsity pattern.  Hence, it is easier to use mirror of ki for L
      L % val(L % mir(ki)) = (a_val_ik - sum) / L % val(L % dia(k))
    end do

    l_val_k(:) = 0.0

  end do

  end subroutine
