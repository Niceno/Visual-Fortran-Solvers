!==============================================================================!
  subroutine Matrix_Mod_Compress(c, a)
!------------------------------------------------------------------------------!
!   Compresses matrix to the compressed row format.                            !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Matrix)         :: c
  real, dimension(:,:) :: a
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col  ! row used to be "i", col used to be "j"
  integer :: row_a, col_a, pos_a, row_b, col_b, pos_b
  integer :: n, pos
  integer :: non_zeros
!==============================================================================!

  n = size(a, 1)

  !------------------------------------!
  !   Stored dimension of the matrix   !
  !------------------------------------!
  c % n = n

  !----------------------------------------!
  !   Count non-zero terms in the matrix   !
  !----------------------------------------!
  non_zeros = 0
  do row = 1, size(a, 1)
    do col = 1, size(a, 2)
      if( a(row,col) /= 0.0 ) then
        non_zeros = non_zeros + 1
      end if
    end do
  end do
  c % nonzeros = non_zeros

  !---------------------!
  !   Allocate memory   !
  !---------------------!
  allocate (c % row(n+1));       c % row = 0
  allocate (c % dia(n));         c % dia = 0
  allocate (c % col(non_zeros)); c % col = 0
  allocate (c % val(non_zeros)); c % val = 0
  allocate (c % mir(non_zeros)); c % mir = 0

  !--------------------------------------------!
  !   Form the compressed row storage matrix   !
  !--------------------------------------------!
  pos = 1
  do row = 1, size(a, 1)

    ! Store the start of the current row
    c % row(row) = pos

    do col = 1, size(a, 2)

      ! Take only non-zero terms
      if( a(row,col) /= 0.0 ) then
        c % col(pos) = col
        c % val(pos) = a(row, col)

        ! Store "dia" term along the way
        if(col == row) then
          c % dia(row) = pos
        end if

        ! Advance for one position in the compressed matrix
        pos = pos + 1
      end if
    end do
  end do

  ! Store last position as the upper boundary of the array
  c % row(n+1) = pos

  !-----------------------------------------------------------------!
  !   Find it's mirror (it is non_zeros * noz_zeros operation :-(   !
  !-----------------------------------------------------------------!

  ! Outer loop
  do row_a = 1, n
    do pos_a = c % row(row_a), c % row(row_a + 1) - 1
      col_a = c % col(pos_a)  ! at this point you have row_a and col_a

      ! Inner loop (it might probably go from 1 to row_a-1
      do row_b = 1, n
        do pos_b = c % row(row_b), c % row(row_b + 1) - 1
          col_b = c % col(pos_b)  ! at this point you have row_b and col_b

          if( (col_b == row_a) .and. (row_b == col_a) ) then
            c % mir(pos_a) = pos_b
            c % mir(pos_b) = pos_a
            goto 1  ! done with the inner loop, get out
          end if
        end do
      end do
1     continue
    end do
  end do

  end subroutine
