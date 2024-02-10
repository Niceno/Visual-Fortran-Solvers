!==============================================================================!
  subroutine Plot_Sparse(IO, name_out, A)
!------------------------------------------------------------------------------!
!   Prints compressed matrix out.                                              !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)            :: IO        !! parent class
  character(len=*),  intent(in) :: name_out  !! output file name
  type(Sparse_Type), intent(in) :: A         !! matrix to plit
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col, pos  ! row used to be "i", col used to be "j"
  logical :: found
  real    :: max_val, min_val, max_abs
!==============================================================================!

  if(A % n > 216) return  ! bigger than this, you can't see

  print '(a,a)', " # Plotting the matrix: ", name_out
  open(9, file=name_out)

  ! Write the header out
  call IO % Plot_Header(9)

  ! Find maximum entry in the system matrix
  max_val = maxval(A % val)
  min_val = minval(A % val)
  max_abs = max(abs(min_val), abs(max_val))

  do row = 1, A % n
    do col = 1, A % n
      found = .false.

      ! Look for position row, col in the compressed
      ! A and print if you have found it
      do pos = A % row(row), A % row(row + 1) - 1

        if( A % col(pos) == col ) then
          call IO % Plot_Circle(9, row, col, A % val(pos), min_val, max_val)
          found = .true.
        end if
      end do

      ! If you haven't found it, do something else
      if( .not. found ) then
        ! Well, don't do much
      end if
    end do

  end do

  end subroutine
