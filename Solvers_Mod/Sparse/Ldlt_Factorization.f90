!==============================================================================!
  subroutine Solvers_Mod_Sparse_Ldlt_Factorization(F, A)
!------------------------------------------------------------------------------!
!   Computes LDLT decomposition on sparse matrices.                            !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt                                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: F  !! factorized matrix
  type(Sparse_Type) :: A  !! original matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, m, n, k_m, k_i, m_j, k_i_a
  real    :: sum
  real, allocatable :: work(:)
!==============================================================================!

  print *, '# Factorizing sparse matrix with LDL'' method'

  n = A % n  ! some checks would be possible
  allocate( work(n) ); work = 0.0

  do k = 1, n

    !--------------------!
    !   Diagonal entry   !
    !--------------------!
    sum = A % val(A % dia(k))
    do k_m = F % row(k), F % dia(k) - 1
      m = F % col(k_m)
      sum = sum - F % val(k_m) * F % val(k_m) * F % val( F % dia(m) )
    end do
    F % val( F % dia(k) ) = sum       ! diagonal entry, D from LDL

    !------------------------!
    !   Non-diagonal entry   !
    !------------------------!
    do k_i = F % dia(k) + 1, F % row(k+1) - 1
      i = F % col(k_i)

      sum = 0.0
      do k_i_a = A % row(k), A % row(k+1) - 1
        if( A % col(k_i_a) == i ) then
          sum = A % val(k_i_a)  ! A(i,k) should be the same as A(k,i), right?
        end if
      end do

      do k_m = F % row(k), F % dia(k) - 1
        m = F % col(k_m)

        ! De-compress the row
        do m_j = F % row(m), F % row(m+1) - 1
          j = F % col(m_j)
          work(j) = F % val(m_j)
        end do

        sum = sum - work(i)*work(k) * F % val( F % dia(m) )

        ! Set the row back to zero
        work = 0.0
      end do

      F % val(k_i) = sum / F % val(F % dia(k))
      F % val(F % mir(k_i)) = F % val(k_i)
    end do
  end do

  end subroutine
