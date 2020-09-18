!==============================================================================!
  subroutine Matrix_Mod_Create_Preconditioning_Compressed(  &
             c_matrix, a_matrix, f_in)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Matrix) :: c_matrix  ! preconditioning matrix
  type(Matrix) :: a_matrix  ! original matrix
  integer      :: f_in
!-----------------------------------[Locals]-----------------------------------!
  integer      :: non_zeros, non_zeros_tent, entry, f, l_new, l_old
  integer      :: row, col, pos, row_a, row_b, col_a, col_b, siz, pos_a, pos_b
  integer, allocatable :: rows_new(:), cols_new(:)
!==============================================================================!

  write(*,*) 'Nonzeros in original matrix: ', a_matrix % nonzeros

  allocate(rows_new(a_matrix % nonzeros * (f_in+1)))
  allocate(cols_new(a_matrix % nonzeros * (f_in+1)))

  !-----------------------------------------------!
  !   Add aditional diagonals (with duplicates)   !
  !-----------------------------------------------!
  non_zeros_tent = 0
  do row = 1, a_matrix % n

    do pos = a_matrix % row(row), a_matrix % row(row+1) - 1
      col = a_matrix % col(pos)
      non_zeros_tent = non_zeros_tent + 1
      rows_new(non_zeros_tent) = row
      cols_new(non_zeros_tent) = col

      if(pos < a_matrix % dia(row) .and. f_in > 0) then

        ! Just add, with duplicates
        do f = 1, f_in
          if(col+f .le. a_matrix % n) then
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

  write(*,*) 'Tentative number of nonzeros in the expanded matrix: ', non_zeros_tent

  !--------------------------------!
  !   Take duplicate entries out   !
  !--------------------------------!

  ! Sort rows
  call Sort_Int_Carry_Int(rows_new, cols_new, non_zeros_tent, 2)

  ! Sort columns in each row
  l_old = 1
  do entry = 2, non_zeros_tent
    if( rows_new(entry) .ne. rows_new(entry-1) ) then
      l_new = entry
      if(l_new - l_old > 0) then
        call Sort_Int_Carry_Int(cols_new(l_old), cols_new(l_old), l_new-l_old, 1)
      end if
      l_old = l_new
    end if
  end do
  call Sort_Int_Carry_Int(cols_new(l_old), cols_new(l_old), non_zeros_tent-l_old+1, 1)

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

  write(*,*) 'Final number of nonzeros in the expanded matrix: ', non_zeros

  !---------------------------------------------!
  !   Allocate the memory for expanded matrix   !
  !---------------------------------------------!
  c_matrix % n        = a_matrix % n
  c_matrix % nonzeros = non_zeros
  allocate (c_matrix % row(c_matrix % n+1)); c_matrix % row = 0
  allocate (c_matrix % dia(c_matrix % n));   c_matrix % dia = 0
  allocate (c_matrix % col(non_zeros));      c_matrix % col = 0
  allocate (c_matrix % val(non_zeros));      c_matrix % val = 0
  allocate (c_matrix % mir(non_zeros));      c_matrix % mir = 0

  !----------------------!
  !   Form % col entry   !
  !----------------------!
  c_matrix % col = cols_new

  !----------------------!
  !   Form % row entry   !
  !----------------------!
  row = 1
  c_matrix % row(1) = 1
  do pos = 2, non_zeros
    if(rows_new(pos) .ne. rows_new(pos-1)) then
      row = row + 1
      c_matrix % row(row) = pos
    end if
  end do
  c_matrix % row(c_matrix % n + 1) = non_zeros + 1  ! wrap it up

  !-------------------------------!
  !   Sort columns in every row   !
  !-------------------------------!
  do row = 1, c_matrix % n
    pos = c_matrix % row( row )
    siz = c_matrix % row( row+1 ) - pos
    call Sort_Int_Carry_Int(c_matrix % col(pos), c_matrix % col(pos), siz, 1)
  end do

  !---------------------------------!
  !   Find positions of diagonals   !
  !---------------------------------!
  do row = 1, c_matrix % n
    do pos = c_matrix % row(row), c_matrix % row(row + 1) - 1
      col = c_matrix % col(pos)  ! at this point you have row and col
      if(col == row) then
        c_matrix % dia(row) = pos
        goto 1
      end if
    end do
1   continue
  end do

  !----------------------!
  !   Find it's mirror   !
  !----------------------!
  do row_a = 1, c_matrix % n
    do pos_a = c_matrix % row(row_a), c_matrix % row(row_a + 1) - 1
      col_a = c_matrix % col(pos_a)  ! at this point you have row_a and col_a

      row_b = col_a
      do pos_b = c_matrix % row(row_b), c_matrix % row(row_b + 1) - 1
        col_b = c_matrix % col(pos_b)  ! at this point you have row_b and col_b

        if( (col_b == row_a) .and. (row_b == col_a) ) then
          c_matrix % mir(pos_a) = pos_b
          c_matrix % mir(pos_b) = pos_a
          goto 2  ! done with the inner loop, get out
        end if
      end do
2     continue
    end do
  end do

  end subroutine
