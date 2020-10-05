  program Dot_Product

  integer, parameter :: n = 1000 ! vector size
  integer, parameter :: m =   15 ! iterations
  real, dimension(n) :: x, y

  ! Initialize on the host
  do i = 1, n
    x(i) = i * 0.0001;
    y(i) = i * 0.0002;
  end do

  prod = 0
  !$acc enter data copyin(x, y)

  do j = 1, m

    !$acc  parallel loop reduction(+:prod)  &
    !$acc& present(x, y)
    do i = 1, n
      prod = prod + x(i) * y(i)
    end do

  end do

  !$acc exit data delete(x, y)

  print *, 'dot product is: ', prod

  end program
