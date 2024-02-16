!==============================================================================!
  subroutine Solvers_Mod_Sparse_Ldlt_Factorization(LD, A)
!------------------------------------------------------------------------------!
!   Computes L' factorization of a sparse matrix.                            !
!------------------------------------------------------------------------------!
!   LDL' decomposition in full, looks like this:                               !
!                                                                              !
!   LDL =                                                                      !
!    |  1                  | | D11                 | |  1  L12 L13         |   !
!    | L21  1              | |     D22             | |      1  L23 L24     |   !
!    | L31 L32  1          | |         D33         | |          1  L34 L35 |   !
!    |     L42 L43  1      | |             D44     | |              1  L45 |   !
!    |         L53 L54  1  | |                 D55 | |                  1  |   !
!                                                                              !
!   But given that L's diagonal is equal to one, it doesn't have to be stored. !
!                                                                              !
!               | D11                 |                                        !
!      stored   | L21 D22             |                                        !
!   LDL       = | L31 L32 D33         |                                        !
!               |     L42 L43 D44     |                                        !
!               |         L53 L54 D55 |                                        !
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt                                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: LD  !! factorized matrices (only L and D)
  type(Sparse_Type) :: A   !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  integer           :: i, k, n, s, ki, ik, is, ks
  real              :: sum
  real              :: a_val_ik    ! stores value of matrix A at i,k
  real, allocatable :: l_val_i(:)  ! stores expanded row i from matrix L
!==============================================================================!

  print *, '# Factorizing sparse matrix with LDL'' method'

  ! Take some aliases
  n = A % n  ! some checks would be possible

  ! Set the type of the matrix (in a sense)
  LD % text_u ="U=L'"
  LD % text_l ="L,D"

  ! Allocate local memory
  allocate( l_val_i(n) ); l_val_i = 0.0

  !-------------------------------!
  !   Perform the factorization   !
  !-------------------------------!
  do k = 1, n

    ! Work out (and store) diagonal term
    sum = 0.0
    do ks = LD % row(k), LD % dia(k) - 1
      s = LD % col(ks)
      Assert(k > s)  ! =--> (k,s) in L
      sum = sum + LD % val(ks)**2 * LD % val(LD % dia(s))
      call IO % Plot_Sparse("spar_ldlt", LD, B=A, src1=(/k,s/))
    end do
    LD % val(LD % dia(k)) = A % val(A % dia(k)) - sum  ! D from L
    call IO % Plot_Sparse("spar_ldlt", LD, B=A, targ=(/k,k/))

    ! Work out (and store) the L
    do ki = LD % dia(k) + 1, LD % row(k+1) - 1
      i = LD % col(ki)
      Assert(i > k)  ! =--> (i,k) in L

      ! Expand row i, up to the diagonal (this will store entries L(i,s)
      do is = LD % row(i), LD % dia(i) - 1
        s = LD % col(is)
        l_val_i(s) = LD % val(is)
      end do

      ! Now when you know i, you should find A(i,k)
      a_val_ik = 0.0
      do ik = A % row(i), A % row(i+1) - 1
        if(A % col(ik) .eq. k) then
          a_val_ik = A % val(ik)  ! i > k =--> A(i,k) is L
          exit
        end if
      end do

      ! Now sum rows in i and k: L(i,s) * L(k,s) * L(s,s)
      sum = 0.0
      do ks = LD % row(k), LD % dia(k) - 1
        s = LD % col(ks)
        Assert(k > s)  ! =--> (k,s) in L
        Assert(i > s)  ! =--> (i,s) in L
        sum = sum + l_val_i(s) * LD % val(ks) * LD % val(LD % dia(s))
        call IO % Plot_Sparse("spar_ldlt", LD, B=A, src1=(/i,s/), src2=(/k,s/), src3=(/s,s/))
      end do

      ! Store the lower part only.  At this point you have ki (k,i) for L
      ! but ik (i,k) is defined for A, which might have a different
      ! sparsity pattern.  Hence, it is easier to use mirror of ki for L
      LD % val(LD % mir(ki)) = (a_val_ik - sum) / LD % val(LD % dia(k))
      call IO % Plot_Sparse("spar_ldlt", LD, B=A, targ=(/i,k/))

      l_val_i(:) = 0.0
    end do

  end do

  end subroutine
