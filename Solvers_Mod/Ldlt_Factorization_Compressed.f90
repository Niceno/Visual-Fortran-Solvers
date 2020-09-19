!==============================================================================!
  subroutine Solvers_Mod_Ldlt_Factorization_Compressed(f, a)
!------------------------------------------------------------------------------!
!   Computes LDLT decomposition on compressed matrices.                        !
!                                                                              !
!   Called by:                                                                 !
!   - Solvers_Mod_Incomplete_Ldlt                                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Matrix) :: f
  type(Matrix) :: a
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, m, n, k_m, k_i, m_j, k_i_a
  real    :: sum1, sum2
  real, allocatable :: work(:)
!==============================================================================!

  n = a % n  ! some checks would be possible
  allocate( work(n) ); work = 0.0

  do k = 1, n

    !--------------------!
    !   Diagonal entry   !
    !--------------------!
    sum1 = a % val(a % dia(k))
    do k_m = f % row(k), f % dia(k) - 1
      m = f % col(k_m)
      sum1 = sum1 - (f % val(k_m)**2.0) * f % val( f % dia(m) )
    end do
    f % val( f % dia(k) ) = sum1

    !------------------------!
    !   Non-diagonal entry   !
    !------------------------!
    do k_i = f % dia(k) + 1, f % row(k+1) -1
      i = f % col(k_i)

      sum2 = 0.0
      do k_i_a = a % row(k), a % row(k+1) - 1
        if( a % col(k_i_a) == i ) then
          sum2 = a % val(k_i_a)  ! a(i,k) should be the same as a(k,i), right?
        end if
      end do

      do k_m = f % row(k), f % dia(k) - 1
        m = f % col(k_m)

        ! De-compress the row
        do m_j = f % row(m), f % row(m+1) - 1
          j = f % col(m_j)
          work(j) = f % val(m_j)
        end do

        sum2 = sum2 - work(i)*work(k) * f % val( f % dia(m) )

        ! set the row back to zero
        work = 0.0
      end do

      f % val(k_i) = sum2 / f % val(f % dia(k))
      f % val(f % mir(k_i)) = f % val(k_i)
    end do
  end do

  end subroutine
