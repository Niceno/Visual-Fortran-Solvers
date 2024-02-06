!==============================================================================!
  subroutine Solvers_Mod_Cholesky_Factorization_Sparse(f, a)
!------------------------------------------------------------------------------!
!   Computes Cholesky decomposition on sparse matrices.                        !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Cholesky                                          !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: f
  type(Sparse_Type) :: a
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, m, n, k_m, k_i, m_j, k_i_a
  real    :: sum
  real, allocatable :: work(:)
!==============================================================================!

  print *, '# Factorizing sparse matrix with Cholesky method'

  n = a % n  ! some checks would be possible
  allocate( work(n) ); work = 0.0

  do k = 1, n

    !--------------------!
    !   Diagonal entry   !
    !--------------------!
    sum = a % val(a % dia(k))
    do k_m = f % row(k), f % dia(k) - 1
      sum = sum - f % val(k_m)**2.0
    end do
    f % val( f % dia(k) ) = sqrt(sum)

    !------------------------!
    !   Non-diagonal entry   !
    !------------------------!
    do k_i = f % dia(k) + 1, f % row(k+1) - 1
      i = f % col(k_i)

      sum = 0.0
      do k_i_a = a % row(k), a % row(k+1) - 1
        if( a % col(k_i_a) == i ) then
          sum = a % val(k_i_a)  ! a(i,k) should be the same as a(k,i), right?
        end if
      end do

      do k_m = f % row(k), f % dia(k) - 1
        m = f % col(k_m)

        ! De-compress the row
        do m_j = f % row(m), f % row(m+1) - 1
          j = f % col(m_j)
          work(j) = f % val(m_j)
        end do

        sum = sum - work(i)*work(k)

        ! Set the row back to zero
        work = 0.0
      end do

      f % val(k_i) = sum / f % val(f % dia(k))
      f % val(f % mir(k_i)) = f % val(k_i)
    end do
  end do

  end subroutine
