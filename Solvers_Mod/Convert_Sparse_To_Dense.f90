!==============================================================================!
  subroutine Solvers_Mod_Convert_Sparse_To_Dense(Dens, Spar, bw)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type),  intent(out) :: Dens  !! resulting dense matrix
  type(Sparse_Type), intent(in)  :: Spar  !! original sparse matrix
  integer,           intent(out) :: bw    !! resulting band width
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col  ! row used to be "i", col used to be "j"
  integer :: n, pos
!==============================================================================!

  n = Spar % n

  !---------------------!
  !   Allocate memory   !
  !---------------------!
  call Dens % Dense_Allocate(n)

  !------------------------------!
  !   Form the expanded matrix   !
  !------------------------------!
  bw = 0

  pos = 1
  do row = 1, n                                        ! browse through rows
    do pos = Spar % row(row), Spar % row(row + 1) - 1  ! brows through columns
      col = Spar % col(pos)                            ! take the column number
      Dens % val(row, col) = Spar % val(pos)

      bw = max(bw, abs(col-row))
    end do
  end do

  print *, '# Matrix band width = ', bw

  end subroutine
