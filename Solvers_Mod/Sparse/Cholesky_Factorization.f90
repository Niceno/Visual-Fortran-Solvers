!==============================================================================!
  subroutine Solvers_Mod_Sparse_Cholesky_Factorization(LL, A)
!------------------------------------------------------------------------------!
!>  Computes Cholesky decomposition on sparse matrices.
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Cholesky                                          !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: LL  !! factorized matrices (two in one)
  type(Sparse_Type) :: A   !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  integer           :: i, j, k, m, n, k_m, k_i, m_j, k_i_a
  real              :: sum
  real, allocatable :: work(:)
!==============================================================================!

  print *, '# Factorizing sparse matrix with Cholesky method'

  n = A % n  ! some checks would be possible
  allocate( work(n) ); work = 0.0

  do k = 1, n

    !--------------------!
    !   Diagonal entry   !
    !--------------------!
    sum = A % val(A % dia(k))
    do k_m = LL % row(k), LL % dia(k) - 1
      sum = sum - LL % val(k_m)**2.0
    end do
    LL % val( LL % dia(k) ) = sqrt(sum)

    !------------------------!
    !   Non-diagonal entry   !
    !------------------------!
    do k_i = LL % dia(k) + 1, LL % row(k+1) - 1
      i = LL % col(k_i)

      sum = 0.0
      do k_i_a = A % row(k), A % row(k+1) - 1
        if( A % col(k_i_a) == i ) then
          sum = A % val(k_i_a)  ! A(i,k) should be the same as A(k,i), right?
        end if
      end do

      do k_m = LL % row(k), LL % dia(k) - 1
        m = LL % col(k_m)

        ! De-compress the row
        do m_j = LL % row(m), LL % row(m+1) - 1
          j = LL % col(m_j)
          work(j) = LL % val(m_j)
        end do

        sum = sum - work(i)*work(k)

        ! Set the row back to zero
        work = 0.0
      end do

      LL % val(k_i) = sum / LL % val(LL % dia(k))
      LL % val(LL % mir(k_i)) = LL % val(k_i)
    end do
  end do

  end subroutine
