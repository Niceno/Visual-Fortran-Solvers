!==============================================================================!
  subroutine Sparse_Mod_Create_Preconditioning(c, a, f_in)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Sparse_Type) :: c  ! preconditioning matrix
  type(Sparse_Type) :: a  ! original matrix
  integer           :: f_in
!-----------------------------------[Locals]-----------------------------------!
  integer      :: non_zeros, non_zeros_tent, entry, f
  integer      :: row, col, pos, row_a, row_b, col_a, col_b, siz, pos_a, pos_b
  integer, allocatable :: rows_new(:), cols_new(:)
!==============================================================================!

  print *, 'Nonzeros in original sparse matrix: ', a % nonzeros

  allocate(rows_new(a % nonzeros * (f_in+1)))
  allocate(cols_new(a % nonzeros * (f_in+1)))

  !-----------------------------------------------!
  !   Add aditional diagonals (with duplicates)   !
  !-----------------------------------------------!
  non_zeros_tent = 0
  do row = 1, a % n

    do pos = a % row(row), a % row(row+1) - 1
      col = a % col(pos)
      non_zeros_tent = non_zeros_tent + 1
      rows_new(non_zeros_tent) = row
      cols_new(non_zeros_tent) = col

      if(pos < a % dia(row) .and. f_in > 0) then

        ! Just add, with duplicates
        do f = 1, f_in
          if(col+f .le. a % n) then
            non_zeros_tent = non_zeros_tent + 1
            rows_new(non_zeros_tent) = row
            cols_new(non_zeros_tent) = col+f
            non_zeros_tent = non_zeros_tent + 1
            cols_new(non_zeros_tent) = row
            rows_new(non_zeros_tent) = col+f
          end if
        end do
      end if
    end do
  end do

  print *, 'Tentative number of nonzeros in the expanded matrix: ', non_zeros_tent

  !--------------------------------!
  !   Take duplicate entries out   !
  !--------------------------------!

  ! Sort rows
  call Sort_Mod_2_Int(rows_new(1:non_zeros_tent),  &
                      cols_new(1:non_zeros_tent))

  ! Compress them taking the duplicates out
  non_zeros = 1
  do entry = 2, non_zeros_tent
    if( (rows_new(entry) .ne. rows_new(entry-1)) .or. &
        (cols_new(entry) .ne. cols_new(entry-1)) ) then
      non_zeros = non_zeros+1
      rows_new(non_zeros) = rows_new(entry)
      cols_new(non_zeros) = cols_new(entry)
    end if
  end do
! non_zeros = non_zeros - 1

  print *, '# Final number of nonzeros in the expanded matrix: ', non_zeros

  !---------------------------------------------!
  !   Allocate the memory for expanded matrix   !
  !---------------------------------------------!
  c % n        = a % n
  c % nonzeros = non_zeros
  allocate (c % row(c % n+1));    c % row = 0
  allocate (c % dia(c % n));      c % dia = 0
  allocate (c % col(non_zeros));  c % col = 0
  allocate (c % val(non_zeros));  c % val = 0
  allocate (c % mir(non_zeros));  c % mir = 0

  !----------------------!
  !   Form % col entry   !
  !----------------------!
  c % col = cols_new

  !----------------------!
  !   Form % row entry   !
  !----------------------!
  row = 1
  c % row(1) = 1
  do pos = 2, non_zeros
    if(rows_new(pos) .ne. rows_new(pos-1)) then
      row = row + 1
      c % row(row) = pos
    end if
  end do
  c % row(c % n + 1) = non_zeros + 1  ! wrap it up

  !---------------------------------!
  !   Find positions of diagonals   !
  !---------------------------------!
  do row = 1, c % n
    do pos = c % row(row), c % row(row + 1) - 1
      col = c % col(pos)  ! at this point you have row and col
      if(col == row) then
        c % dia(row) = pos
        goto 1
      end if
    end do
1   continue
  end do

  !----------------------!
  !   Find it's mirror   !
  !----------------------!
  do row_a = 1, c % n
    do pos_a = c % row(row_a), c % row(row_a + 1) - 1
      col_a = c % col(pos_a)  ! at this point you have row_a and col_a

      row_b = col_a
      do pos_b = c % row(row_b), c % row(row_b + 1) - 1
        col_b = c % col(pos_b)  ! at this point you have row_b and col_b

        if( (col_b == row_a) .and. (row_b == col_a) ) then
          c % mir(pos_a) = pos_b
          c % mir(pos_b) = pos_a
          goto 2  ! done with the inner loop, get out
        end if
      end do
2     continue
    end do
  end do

  end subroutine
