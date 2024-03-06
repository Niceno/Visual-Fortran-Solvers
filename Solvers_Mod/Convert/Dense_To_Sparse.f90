!==============================================================================!
  subroutine Solvers_Mod_Convert_Dense_To_Sparse(Spar, Dens)
!------------------------------------------------------------------------------!
!>  Converts a dense matrix to a sparse one.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type), intent(out) :: Spar  !! resulting sparse matrix
  type(Dense_Type),  intent(in)  :: Dens  !! original dense matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col  ! row used to be "i", col used to be "j"
  integer :: row_a, col_a, pos_a, row_b, col_b, pos_b
  integer :: n, pos
  integer :: non_zeros
!==============================================================================!

  n = Dens % n

  !------------------------------------!
  !   Stored dimension of the matrix   !
  !------------------------------------!
  Spar % n = Dens % n

  !-----------------------------!
  !   Set the pointer to grid   !
  !-----------------------------!
  Spar % pnt_grid => Dens % pnt_grid

  !----------------------------------------!
  !   Count non-zero terms in the matrix   !
  !----------------------------------------!
  non_zeros = 0
  do row = 1, Dens % n
    do col = 1, Dens % n
      if( Dens % val(row,col) /= 0.0 ) then
        non_zeros = non_zeros + 1
      end if
    end do
  end do
  Spar % nonzeros = non_zeros

  !---------------------!
  !   Allocate memory   !
  !---------------------!
  allocate(Spar % row(n+1));       Spar % row = 0
  allocate(Spar % dia(n));         Spar % dia = 0
  allocate(Spar % col(non_zeros)); Spar % col = 0
  allocate(Spar % val(non_zeros)); Spar % val = 0
  allocate(Spar % mir(non_zeros)); Spar % mir = 0

  !--------------------------------------------!
  !   Form the compressed row storage matrix   !
  !--------------------------------------------!
  pos = 1
  do row = 1, Dens % n

    ! Store the start of the current row
    Spar % row(row) = pos

    do col = 1, Dens % n

      ! Take only non-zero terms
      if( Dens % val(row,col) /= 0.0 ) then
        Spar % col(pos) = col
        Spar % val(pos) = Dens % val(row, col)

        ! Store "dia" term along the way
        if(col == row) then
          Spar % dia(row) = pos
        end if

        ! Advance for one position in the compressed matrix
        pos = pos + 1
      end if
    end do
  end do

  ! Store last position as the upper boundary of the array
  Spar % row(n+1) = pos

  !-----------------------------------------------------------------!
  !   Find it's mirror (it is non_zeros * noz_zeros operation :-(   !
  !-----------------------------------------------------------------!

  ! Outer loop
  do row_a = 1, n
    do pos_a = Spar % row(row_a), Spar % row(row_a + 1) - 1
      col_a = Spar % col(pos_a)  ! at this point you have row_a and col_a

      ! Inner loop (it might probably go from 1 to row_a-1
      do row_b = 1, n
        do pos_b = Spar % row(row_b), Spar % row(row_b + 1) - 1
          col_b = Spar % col(pos_b)  ! at this point you have row_b and col_b

          if( (col_b == row_a) .and. (row_b == col_a) ) then
            Spar % mir(pos_a) = pos_b
            Spar % mir(pos_b) = pos_a
            goto 1  ! done with the inner loop, get out
          end if
        end do
      end do
1     continue
    end do
  end do

  end subroutine
