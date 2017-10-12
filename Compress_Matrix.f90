!==============================================================================!
  subroutine Compress_Matrix(c, a)
!------------------------------------------------------------------------------!
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
  type(Matrix)         :: c
  real, dimension(:,:) :: a
!------------------------------------------------------------------------------!
  integer :: row, col  ! row used to be "i", col used to be "j"
  integer :: n, pos
  integer :: non_zeros
!==============================================================================!

  n = size(a, 1)

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

  end subroutine Compress_Matrix
