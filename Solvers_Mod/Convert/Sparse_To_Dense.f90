!==============================================================================!
  subroutine Solvers_Mod_Convert_Sparse_To_Dense(Dens, Spar)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Dense_Type),  intent(out) :: Dens  !! resulting dense matrix
  type(Sparse_Type), intent(in)  :: Spar  !! original sparse matrix
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col  ! row used to be "i", col used to be "j"
  integer :: pos
!==============================================================================!

  !---------------------!
  !   Allocate memory   !
  !---------------------!
  call Dens % Dense_Allocate(Spar % n)

  !-----------------------------!
  !   Set the pointer to grid   !
  !-----------------------------!
  Dens % pnt_grid => Spar % pnt_grid

  !------------------------------!
  !   Form the expanded matrix   !
  !------------------------------!
  Dens % bw = 0

  pos = 1
  do row = 1, Spar % n                                 ! browse through rows
    do pos = Spar % row(row), Spar % row(row + 1) - 1  ! brows through columns
      col = Spar % col(pos)                            ! take the column number
      Dens % val(row, col) = Spar % val(pos)

      Dens % bw = max(Dens % bw, abs(col - row))
    end do
  end do

  print *, '# Matrix band width = ', Dens % bw

  end subroutine
