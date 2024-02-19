!==============================================================================!
  subroutine Create_Grid(Grid, lx, ly, lz, nx, ny, nz)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Grid_Type)    :: Grid
  real,    intent(in) :: lx, ly, lz
  integer, intent(in) :: nx, ny, nz
!-----------------------------------[Locals]-----------------------------------!
  integer           :: s, c, c1, c2, e, n, t, i, j, k
  real, allocatable :: visited(:)
!==============================================================================!

  ! The minimum number of cells in each direction is three
  Assert(nx > 1)
  Assert(ny > 1)
  Assert(nz > 1)

  !-----------!
  !   Cells   !
  !-----------!

  Grid % n_cells = nx * ny * nz

  Grid % lx = lx
  Grid % ly = ly
  Grid % lz = lz

  Grid % nx = nx
  Grid % ny = ny
  Grid % nz = nz

  Grid % dx = lx / nx
  Grid % dy = ly / ny
  Grid % dz = lz / nz

  !-----------!
  !   Faces   !
  !-----------!

  Grid % n_faces = (nx+1) * ny * nz  &
                 + nx * (ny+1) * nz  &
                 + nx * ny * (nz+1)
  print '(a,i12,a)', ' # Grid should have ', Grid % n_faces, ' faces'

  allocate(Grid % faces_c(2, Grid % n_faces))

  s = 0

  ! Handle the domain in the west
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(i .eq. 1) then
      s = s + 1
      Grid % faces_c(1,s) =  c
      Grid % faces_c(2,s) = -s
    end if
  end do

  ! Handle the domain in the east
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(i .eq. Grid % nx) then
      s = s + 1
      Grid % faces_c(1,s) =  c
      Grid % faces_c(2,s) = -s
    end if
  end do

  ! Handle the domain in the south
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(j .eq. 1) then
      s = s + 1
      Grid % faces_c(1,s) =  c
      Grid % faces_c(2,s) = -s
    end if
  end do

  ! Handle the domain in the north
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(j .eq. Grid % ny) then
      s = s + 1
      Grid % faces_c(1,s) =  c
      Grid % faces_c(2,s) = -s
    end if
  end do

  ! Handle the domain at the bottom
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(k .eq. 1) then
      s = s + 1
      Grid % faces_c(1,s) =  c
      Grid % faces_c(2,s) = -s
    end if
  end do

  ! Handle the domain at the top
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(k .eq. Grid % nz) then
      s = s + 1
      Grid % faces_c(1,s) =  c
      Grid % faces_c(2,s) = -s
    end if
  end do

  Grid % n_bnd_cells = s
  print '(a,i12,a)', ' # Found ', s, ' faces at boundaries'

  ! Handle facs inside the domain
  do k = 1, Grid % nz
    do j = 1, Grid % ny
      do i = 1, Grid % nx

        c = Grid % Cell_Number(i, j, k)
        e = c + 1
        n = c + Grid % nx
        t = c + Grid % nx * Grid % ny

        if(i .lt. Grid % nx) then
          s = s + 1
          Grid % faces_c(1,s) = c
          Grid % faces_c(2,s) = e
        end if

        if(j .lt. Grid % ny) then
          s = s + 1
          Grid % faces_c(1,s) = c
          Grid % faces_c(2,s) = n
        end if

        if(k .lt. Grid % nz) then
          s = s + 1
          Grid % faces_c(1,s) = c
          Grid % faces_c(2,s) = t
        end if
      end do
    end do
  end do

  print '(a,i12,a)', ' # Found a total of ', s, ' faces'

  ! Check the faces_c structure
  allocate(visited(Grid % n_cells));  visited(:) = 0.0
  do s = 1, Grid % n_faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)
    if(c2 .gt. 0) then
      visited(c1) = visited(c1) + 1.0
      visited(c2) = visited(c2) + 1.0
    end if
  end do
  call Grid % Save_Vtk_Debug("visited.vtk", visited)

  end subroutine

