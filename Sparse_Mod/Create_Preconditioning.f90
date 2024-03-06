!==============================================================================!
  subroutine Sparse_Create_Preconditioning(C, A, f_in)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Sparse_Type) :: C  !! preconditioning matrix
  type(Sparse_Type)  :: A  !! original matrix
  integer            :: f_in
!-----------------------------------[Locals]-----------------------------------!
  integer              :: non_zeros, non_zeros_tent, entry, f
  integer              :: row, col, pos, row_a, row_b, col_a, col_b
  integer              :: pos_a, pos_b
  integer, allocatable :: rows_new(:), cols_new(:)
!==============================================================================!

  Assert(f_in .ge. 0)

  print *, '# Dimension of the original sparse matrix: ', A % n
  print *, '# Nonzeros in original sparse matrix     : ', A % nonzeros

  allocate(rows_new(A % nonzeros * (f_in+1)))
  allocate(cols_new(A % nonzeros * (f_in+1)))

  !-----------------------------------------------!
  !   Add aditional diagonals (with duplicates)   !
  !-----------------------------------------------!
  non_zeros_tent = 0
  do row = 1, A % n

    do pos = A % row(row), A % row(row+1) - 1
      col = A % col(pos)
      non_zeros_tent = non_zeros_tent + 1
      rows_new(non_zeros_tent) = row
      cols_new(non_zeros_tent) = col

      if(pos < A % dia(row) .and. f_in > 0) then

        ! Just add, with duplicates
        do f = 1, f_in
          if(col+f .le. A % n) then
            non_zeros_tent = non_zeros_tent + 1
            rows_new(non_zeros_tent) = row
            cols_new(non_zeros_tent) = col+f
            non_zeros_tent = non_zeros_tent + 1
            rows_new(non_zeros_tent) = col+f
            cols_new(non_zeros_tent) = row
          end if
        end do
      end if
    end do
  end do

  print *, '# Members allocated in the expanded matrix: ', size(rows_new)
  print *, '# Tentative nonzeros in the expanded matrix:', non_zeros_tent

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

  print *, '# Final number of nonzeros in the expanded matrix: ', non_zeros

  !---------------------------------------------!
  !   Allocate the memory for expanded matrix   !
  !---------------------------------------------!
  C % n        = A % n
  C % nonzeros = non_zeros
  allocate(C % row(C % n+1));    C % row = 0
  allocate(C % dia(C % n));      C % dia = 0
  allocate(C % col(non_zeros));  C % col = 0
  allocate(C % val(non_zeros));  C % val = 0
  allocate(C % mir(non_zeros));  C % mir = 0

  !----------------------!
  !   Form % col entry   !
  !----------------------!
  C % col = cols_new

  !----------------------!
  !   Form % row entry   !
  !----------------------!
  row = 1
  C % row(1) = 1
  do pos = 2, non_zeros
    if(rows_new(pos) .ne. rows_new(pos-1)) then
      row = row + 1
      C % row(row) = pos
    end if
  end do
  C % row(C % n + 1) = non_zeros + 1  ! wrap it up

  !---------------------------------!
  !   Find positions of diagonals   !
  !---------------------------------!
  do row = 1, C % n
    do pos = C % row(row), C % row(row + 1) - 1
      col = C % col(pos)  ! at this point you have row and col
      if(col == row) then
        C % dia(row) = pos
        goto 1
      end if
    end do
1   continue
  end do

  !----------------------!
  !   Find it's mirror   !
  !----------------------!
  do row_a = 1, C % n
    do pos_a = C % row(row_a), C % row(row_a + 1) - 1
      col_a = C % col(pos_a)  ! at this point you have row_a and col_a

      row_b = col_a
      do pos_b = C % row(row_b), C % row(row_b + 1) - 1
        col_b = C % col(pos_b)  ! at this point you have row_b and col_b

        if( (col_b == row_a) .and. (row_b == col_a) ) then
          C % mir(pos_a) = pos_b
          C % mir(pos_b) = pos_a
          goto 2  ! done with the inner loop, get out
        end if
      end do
2     continue
    end do
  end do

  end subroutine
