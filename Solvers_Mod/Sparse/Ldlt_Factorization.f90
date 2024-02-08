!==============================================================================!
  subroutine Solvers_Mod_Sparse_Ldlt_Factorization(LDL, A)
!------------------------------------------------------------------------------!
!   Computes LDL' factorization of a sparse matrix.                            !
!------------------------------------------------------------------------------!
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt                                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: LDL  !! factorized matrices (three in one)
  type(Sparse_Type) :: A    !! original matrix
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
    do k_m = LDL % row(k), LDL % dia(k) - 1
      m = LDL % col(k_m)
      sum = sum - LDL % val(k_m) * LDL % val(k_m) * LDL % val( LDL % dia(m) )
    end do
    LDL % val( LDL % dia(k) ) = sum       ! diagonal entry, D from LDL

    !------------------------!
    !   Non-diagonal entry   !
    !------------------------!
    do k_i = LDL % dia(k) + 1, LDL % row(k+1) - 1
      i = LDL % col(k_i)

      sum = 0.0
      do k_i_a = A % row(k), A % row(k+1) - 1
        if( A % col(k_i_a) == i ) then
          sum = A % val(k_i_a)  ! A(i,k) should be the same as A(k,i), right?
        end if
      end do

      do k_m = LDL % row(k), LDL % dia(k) - 1
        m = LDL % col(k_m)

        ! De-compress the row
        do m_j = LDL % row(m), LDL % row(m+1) - 1
          j = LDL % col(m_j)
          work(j) = LDL % val(m_j)
        end do

        sum = sum - work(i)*work(k) * LDL % val( LDL % dia(m) )

        ! Set the row back to zero
        work = 0.0
      end do

      LDL % val(k_i) = sum / LDL % val(LDL % dia(k))
      LDL % val(LDL % mir(k_i)) = LDL % val(k_i)
    end do
  end do

  end subroutine
