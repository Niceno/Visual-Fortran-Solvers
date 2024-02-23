!==============================================================================!
  subroutine Save_Vtk_Vector(Grid, file_name, phi1, phi2, phi3)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Grid_Type) :: Grid                  !! computational grid
  character(*)     :: file_name
  real             :: phi1(Grid % n_cells)  !! solution - x component
  real             :: phi2(Grid % n_cells)  !! solution - y component
  real             :: phi3(Grid % n_cells)  !! solution - z component
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, c, fu
!==============================================================================!

  ! Open file
  fu = 10
  open(unit=fu, file=file_name, status='replace', action='write')
  print '(a)', ' # Saving the results in: '//file_name

  ! Write VTK header
  write(fu,'(a)') '# vtk DataFile Version 3.0'
  write(fu,*)     ' vtk output'
  write(fu,*)     ' ASCII'
  write(fu,*)     ' DATASET RECTILINEAR_GRID'
  write(fu,*)     ' DIMENSIONS ', Grid % nx+1, ' ',  &
                                  Grid % ny+1, ' ',  &
                                  Grid % nz+1

  ! Write X coordinates
  write(fu,*) ' X_COORDINATES ', Grid % nx+1, ' float'
  do i = 0, Grid % nx
    write(fu, '(es14.5)') Grid % xn(i)
  end do

  ! Write Y coordinates
  write(fu,*) ' Y_COORDINATES ', Grid % ny+1, ' float'
  do j = 0, Grid % ny
    write(fu, '(es14.5)') Grid % yn(j)
  end do

  ! Write Z coordinates
  write(fu,*) ' Z_COORDINATES ', Grid % nz+1, ' float'
  do k = 0, Grid % nz
    write(fu, '(es14.5)') Grid % zn(k)
  end do

  ! Write cell-centered vector data
  write(fu,*) ' CELL_DATA ', Grid % n_cells
  write(fu,*) ' VECTORS solution float'
  do k = 1, Grid % nz
    do j = 1, Grid % ny
      do i = 1, Grid % nx
        c = Grid % Cell_Number(i, j, k)
        write(fu, '(3(es14.5))') phi1(c), phi2(c), phi3(c)
      end do
    end do
  end do

  ! Close file
  close(unit=fu)

  end subroutine
