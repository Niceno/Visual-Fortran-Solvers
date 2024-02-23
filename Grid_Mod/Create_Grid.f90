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
  !           !
  !   Nodes   !
  !           !
  !-----------!

  ! Allocate memory for node coordinates
  allocate(Grid % xn(0:nx))
  allocate(Grid % yn(0:ny))
  allocate(Grid % zn(0:nz))

  ! Calculate node coordinates (it rarelly gets simpler than this)
  do i = 0, nx
    Grid % xn(i) = real(i) * lx / real(nx)
  end do
  do j = 0, ny
    Grid % yn(j) = real(j) * ly / real(ny)
  end do
  do k = 0, nz
    Grid % zn(k) = real(k) * ly / real(nz)
  end do

  !---------------!
  !               !
  !   Cells (1)   !
  !               !
  !---------------!

  Grid % n_cells     = nx * ny * nz
  Grid % n_bnd_cells = 2 * (ny * nz + nx * nz + nx * ny)

  Grid % lx = lx
  Grid % ly = ly
  Grid % lz = lz

  Grid % nx = nx
  Grid % ny = ny
  Grid % nz = nz

  ! Allocate memory for cell coordinates
  allocate(Grid % xc(-Grid % n_bnd_cells:Grid % n_cells))
  allocate(Grid % yc(-Grid % n_bnd_cells:Grid % n_cells))
  allocate(Grid % zc(-Grid % n_bnd_cells:Grid % n_cells))

  ! Calculate cell coordinates
  do k = 1, nz
    do j = 1, ny
      do i = 1, nx
        c = Grid % Cell_Number(i, j, k)
        Grid % xc(c) = 0.5 * (Grid % xn(i-1) + Grid % xn(i))
        Grid % yc(c) = 0.5 * (Grid % yn(j-1) + Grid % yn(j))
        Grid % zc(c) = 0.5 * (Grid % zn(k-1) + Grid % zn(k))
      end do
    end do
  end do

  ! Allocate memory for cells to cells connectivity
  allocate(Grid % cells_n_cells(Grid % n_cells))
  allocate(Grid % cells_c(6, Grid % n_cells))

  !-----------!
  !           !
  !   Faces   !  (some of them are also boundary cells, in fact)
  !           !
  !-----------!

  Grid % n_faces = (nx+1) * ny * nz  &
                 + nx * (ny+1) * nz  &
                 + nx * ny * (nz+1)
  print '(a,i12,a)', ' # Grid should have ', Grid % n_faces, ' faces'

  allocate(Grid % faces_c(2, Grid % n_faces))
  allocate(Grid % dx(Grid % n_faces))
  allocate(Grid % dy(Grid % n_faces))
  allocate(Grid % dz(Grid % n_faces))

  s = 0

  ! Handle the domain in the west
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(i .eq. 1) then
      s = s + 1
      Grid % faces_c(1,s) =  c
      Grid % faces_c(2,s) = -s
      Grid % xc(-s) = Grid % xn(0)
      Grid % yc(-s) = 0.5 * (Grid % yn(j-1) + Grid % yn(j))
      Grid % zc(-s) = 0.5 * (Grid % zn(k-1) + Grid % zn(k))
    end if
  end do

  ! Handle the domain in the east
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(i .eq. Grid % nx) then
      s = s + 1
      Grid % faces_c(1,s) =  c
      Grid % faces_c(2,s) = -s
      Grid % xc(-s) = Grid % xn(nx)
      Grid % yc(-s) = 0.5 * (Grid % yn(j-1) + Grid % yn(j))
      Grid % zc(-s) = 0.5 * (Grid % zn(k-1) + Grid % zn(k))
    end if
  end do

  ! Handle the domain in the south
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(j .eq. 1) then
      s = s + 1
      Grid % faces_c(1,s) =  c
      Grid % faces_c(2,s) = -s
      Grid % xc(-s) = 0.5 * (Grid % xn(i-1) + Grid % xn(i))
      Grid % yc(-s) = Grid % yn(0)
      Grid % zc(-s) = 0.5 * (Grid % zn(k-1) + Grid % zn(k))
    end if
  end do

  ! Handle the domain in the north
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(j .eq. Grid % ny) then
      s = s + 1
      Grid % faces_c(1,s) =  c
      Grid % faces_c(2,s) = -s
      Grid % xc(-s) = 0.5 * (Grid % xn(i-1) + Grid % xn(i))
      Grid % yc(-s) = Grid % yn(ny)
      Grid % zc(-s) = 0.5 * (Grid % zn(k-1) + Grid % zn(k))
    end if
  end do

  ! Handle the domain at the bottom
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(k .eq. 1) then
      s = s + 1
      Grid % faces_c(1,s) =  c
      Grid % faces_c(2,s) = -s
      Grid % xc(-s) = 0.5 * (Grid % xn(i-1) + Grid % xn(i))
      Grid % yc(-s) = 0.5 * (Grid % yn(j-1) + Grid % yn(j))
      Grid % zc(-s) = Grid % zn(0)
    end if
  end do

  ! Handle the domain at the top
  do c = 1, Grid % n_cells
    call Grid % Cells_I_J_K(c, i, j, k)
    if(k .eq. Grid % nz) then
      s = s + 1
      Grid % faces_c(1,s) =  c
      Grid % faces_c(2,s) = -s
      Grid % xc(-s) = 0.5 * (Grid % xn(i-1) + Grid % xn(i))
      Grid % yc(-s) = 0.5 * (Grid % yn(j-1) + Grid % yn(j))
      Grid % zc(-s) = Grid % zn(nz)
    end if
  end do

  Assert(Grid % n_bnd_cells .eq. s)
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

  ! Form dx, dy and dz
  do s = 1, Grid % n_faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)
    Grid % dx(s) = Grid % xc(c2) - Grid % xc(c1)
    Grid % dy(s) = Grid % yc(c2) - Grid % yc(c1)
    Grid % dz(s) = Grid % zc(c2) - Grid % zc(c1)
  end do

  !---------------!
  !               !
  !   Cells (2)   !
  !               !
  !---------------!
  Grid % cells_n_cells(:) = 0
  Grid % cells_c(:,:)     = 0
  do s = 1, Grid % n_bnd_cells  ! boundary faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)
    Assert(c1 .gt. 0)
    Assert(c2 .lt. 0)
    Grid % cells_n_cells(c1) = Grid % cells_n_cells(c1) + 1
    i = Grid % cells_n_cells(c1)  ! i like index, let's say
    Grid % cells_c(i,c1) = c2
  end do

  do s = Grid % n_bnd_cells + 1, Grid % n_faces  ! inside faces
    c1 = Grid % faces_c(1,s)
    c2 = Grid % faces_c(2,s)
    Assert(c1 .gt. 0)
    Assert(c2 .gt. 0)
    Grid % cells_n_cells(c1) = Grid % cells_n_cells(c1) + 1
    Grid % cells_n_cells(c2) = Grid % cells_n_cells(c2) + 1
    i = Grid % cells_n_cells(c1)  ! i like index, let's say
    j = Grid % cells_n_cells(c2)  ! j is like, following i
    Grid % cells_c(i,c1) = c2
    Grid % cells_c(j,c2) = c1
  end do

  do c = 1, Grid % n_cells
    Assert(Grid % cells_n_cells(c) .le. 6)
  end do

  !---------------------------------!
  !   Check the faces_c structure   !
  !---------------------------------!
# if VFS_DEBUG == 1
    allocate(visited(Grid % n_cells));  visited(:) = 0.0
    do s = 1, Grid % n_faces
      c1 = Grid % faces_c(1,s)
      c2 = Grid % faces_c(2,s)
      if(c2 .gt. 0) then
        visited(c1) = visited(c1) + 1.0
        visited(c2) = visited(c2) + 1.0
      end if
    end do
    call Grid % Save_Vtk_Scalar("visited.vtk", visited)
# endif

  end subroutine

