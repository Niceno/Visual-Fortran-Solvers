!==============================================================================!
  subroutine Sparse_Create(A, Grid, singular)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(Sparse_Type)  :: A     !! parent class
  type(Grid_Type)     :: Grid  !! grid on which it is created
  logical, intent(in) :: singular
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, ni, nj, nk, non_zeros
  integer :: c, w, e, s, n, b, t
  integer :: col_a, col_b, row_a, row_b, pos_a, pos_b
  real    :: dx, dy, dz, a_e, a_w, a_n, a_s, a_t, a_b, a_sum
!==============================================================================!

  if(singular) then
    print '(a)', ' # Creating a sparse singular matrix'
  else
    print '(a)', ' # Creating a sparse non-singular matrix'
  end if

  ni = Grid % nx
  nj = Grid % ny
  nk = Grid % nz

  dx = Grid % lx / Grid % nx
  dy = Grid % ly / Grid % ny
  dz = Grid % lz / Grid % nz

  !------------------------------------------!
  !   Count non-zeroes and allocate memory   !
  !------------------------------------------!
  non_zeros = 0

  ! Browse in A way in which cell number "A" will increase one by one
  do k = 1, nk
    do j = 1, nj
      do i = 1, ni

        if(k > 1) non_zeros = non_zeros + 1
        if(j > 1) non_zeros = non_zeros + 1
        if(i > 1) non_zeros = non_zeros + 1
        non_zeros = non_zeros + 1
        if(i < ni) non_zeros = non_zeros + 1
        if(j < nj) non_zeros = non_zeros + 1
        if(k < nk) non_zeros = non_zeros + 1

      end do
    end do
  end do

  print '(a,i15)', ' # Number of nonzeros: ', non_zeros
  A % n        = Grid % nx  &
               * Grid % ny  &
               * Grid % nz
  A % nonzeros = non_zeros
  allocate (A % row(Grid % n_cells+1));  A % row = 0
  allocate (A % dia(Grid % n_cells));    A % dia = 0
  allocate (A % col(non_zeros));         A % col = 0
  allocate (A % val(non_zeros));         A % val = 0
  allocate (A % mir(non_zeros));         A % mir = 0

  !--------------------------------!
  !   Form the compressed matrix   !
  !--------------------------------!
  non_zeros = 0

  ! Browse in A way in which cell number "A" will increase one by one
  do k = 1, nk
    do j = 1, nj
      do i = 1, ni
        c = Grid % Cell_Number(i, j, k)

        ! First neighbours
        e = c+1
        w = c-1
        n = c+ni
        s = c-ni
        t = c+ni*nj
        b = c-ni*nj

        a_e = (dy*dz) / dx
        a_w = (dy*dz) / dx
        a_n = (dx*dz) / dy
        a_s = (dx*dz) / dy
        a_t = (dx*dy) / dz
        a_b = (dx*dy) / dz

        if(singular) then
          if(i .eq. ni) a_e = 0.0
          if(i .eq.  1) a_w = 0.0
          if(j .eq. nj) a_n = 0.0
          if(j .eq.  1) a_s = 0.0
          if(k .eq. nk) a_t = 0.0
          if(k .eq.  1) a_b = 0.0
        else
          if(i .eq. ni) a_e = a_e * 2.0
          if(i .eq.  1) a_w = a_w * 2.0
          if(j .eq. nj) a_n = a_n * 2.0
          if(j .eq.  1) a_s = a_s * 2.0
          if(k .eq. nk) a_t = a_t * 2.0
          if(k .eq.  1) a_b = a_b * 2.0
        end if

        ! If second pass, set row index
        A % row(c) = non_zeros + 1

        !-------!
        !   B   !
        !-------!
        if(k > 1) then
          non_zeros = non_zeros + 1
          A % col(non_zeros) = b
          A % val(non_zeros) = -a_b
        end if

        !-------!
        !   S   !
        !-------!
        if(j > 1) then
          non_zeros = non_zeros + 1
          A % col(non_zeros) = s
          A % val(non_zeros) = -a_s
        end if

        !-------!
        !   W   !
        !-------!
        if(i > 1) then
          non_zeros = non_zeros + 1
          A % col(non_zeros) = w
          A % val(non_zeros) = -a_w
        end if

        !-------!
        !   C   !
        !-------!
        non_zeros = non_zeros + 1
        A % col(non_zeros) = c
        A % val(non_zeros) = a_e + a_w + a_n + a_s + a_t + a_b ! + dx * dy * dz

        !-------!
        !   E   !
        !-------!
        if(i < ni) then
          non_zeros = non_zeros + 1
          A % col(non_zeros) = e
          A % val(non_zeros) = -a_e
        end if

        !-------!
        !   N   !
        !-------!
        if(j < nj) then
          non_zeros = non_zeros + 1
          A % col(non_zeros) = n
          A % val(non_zeros) = -a_n
        end if

        !-------!
        !   T   !
        !-------!
        if(k < nk) then
          non_zeros = non_zeros + 1
          A % col(non_zeros) = t
          A % val(non_zeros) = -a_t
        end if

      end do
    end do
  end do

  A % row(ni*nj*nk+1) = non_zeros+1

  !---------------------------------!
  !   Find positions of diagonals   !
  !---------------------------------!
  do row_a = 1, Grid % n_cells
    do pos_a = A % row(row_a), A % row(row_a + 1) - 1
      col_a = A % col(pos_a)  ! at this point you have row_a and col_a
      if(col_a == row_a) then
        A % dia(row_a) = pos_a
        goto 1
      end if
    end do
1   continue
  end do

  !----------------------!
  !   Find it's mirror   !
  !----------------------!

  ! Outer loop
  do row_a = 1, Grid % n_cells
    do pos_a = A % row(row_a), A % row(row_a + 1) - 1
      col_a = A % col(pos_a)  ! at this point you have row_a and col_a

      row_b = col_a
      do pos_b = A % row(row_b), A % row(row_b + 1) - 1
        col_b = A % col(pos_b)  ! at this point you have row_b and col_b

        if( (col_b == row_a) .and. (row_b == col_a) ) then
          A % mir(pos_a) = pos_b
          A % mir(pos_b) = pos_a
          goto 2  ! done with the inner loop, get out
        end if
      end do
2     continue
    end do
  end do

  end subroutine
