!==============================================================================!
  subroutine Plot_Dense_System(IO, name_out, A, x, b, tarx, srca, srcx, srcb)
!------------------------------------------------------------------------------!
!>  Plots the dense system of the form Ax=b in the Xfig file format.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)                     :: IO        !! parent class
  character(*),     intent(in)           :: name_out  !! output file name
  type(Dense_Type), intent(in)           :: A         !! matrix to plot
  real,             intent(in)           :: x(:)      !! solution vector
  real,             intent(in)           :: b(:)      !! righ-hand side vector
  integer,          intent(in), optional :: tarx(:)   !! row
  integer,          intent(in), optional :: srca(:)   !! source from A
  integer,          intent(in), optional :: srcx(:)   !! source from x
  integer,          intent(in), optional :: srcb(:)   !! source form b
!-----------------------------------[Locals]-----------------------------------!
  integer              :: row, col, n
  integer,        save :: cnt = 0
  real                 :: max_a, min_a
  real                 :: max_x, min_x
  real                 :: max_b, min_b
  character(6)         :: frame = '_00000'
  character(512)       :: full_name = ''
  character(512), save :: old_name = ''
!==============================================================================!

  ! Take an alias
  n = A % n

  if(present(tarx)) Assert(size(tarx) .eq. 1)
  if(present(srca)) Assert(size(srca) .eq. 2)
  if(present(srcx)) Assert(size(srcx) .eq. 1)
  if(present(srcb)) Assert(size(srcb) .eq. 1)

  !------------------------------------------------!
  !                                                !
  !   Exit if matrix is too big to be visualized   !
  !                                                !
  !------------------------------------------------!
  if(n > 216) return  ! bigger than this, you can't see

  !-----------------------------------!
  !                                   !
  !   Restart the counter if needed   !
  !                                   !
  !-----------------------------------!
  if(present(tarx) .or.  &
     present(srca) .or. present(srcx) .or. present(srcb)) then
    if(name_out .ne. old_name) then
      cnt = 0
      old_name = name_out
    end if
  end if

  !------------------!
  !                  !
  !   Set the name   !
  !                  !
  !------------------!
  if(present(tarx) .or.  &
     present(srca) .or. present(srcx) .or. present(srcb)) then
    cnt = cnt + 1
    write(frame(2:6), '(i5.5)') cnt
    full_name = name_out//frame//'.fig'
  else
    full_name = name_out//'.fig'
  end if
  open(9, file=trim(full_name))

  call Foul % Formatted_Write(' # Plotting the matrix:          ',  &
                              'white',                              &
                               trim(full_name),                     &
                              'bright red');

  !--------------------------!
  !                          !
  !   Write the header out   !
  !                          !
  !--------------------------!
  call IO % Plot_Header(9)

  !------------------------------!
  !                              !
  !   Followed by the compound   !
  !                              !
  !------------------------------!
  write(9, '(3i2, 2i9)') 6, 0, 0, n * CM, n * CM

  !----------------------------!
  !                            !
  !   Do the actual plotting   !
  !                            !
  !----------------------------!

  ! Place some invisble boxes to serve as canvas
  call IO % Plot_Box(9,   0,   0, WHITE, 999)
  call IO % Plot_Box(9, n+1, n+8, WHITE, 999)

  ! Place brackets around the matrix and the vectors
  call IO % Plot_Brackets(9, (/1,n/), (/1,  n  /), 40)
  call IO % Plot_Brackets(9, (/1,n/), (/n+3,n+3/), 40)
  call IO % Plot_Brackets(9, (/1,n/), (/n+7,n+7/), 40)
  call IO % Plot_Text    (9, '=',   n/2,   n+5,    40, fsize=36)

  call IO % Plot_Text(9, A % text_u, 1+3, n-3, 40, fsize=36)
  call IO % Plot_Text(9, A % text_l, n-3, 1+3, 40, fsize=36)

  min_a = minval(A % val(:,:))
  max_a = maxval(A % val(:,:))
  min_x = minval(x(:))
  max_x = maxval(x(:))
  min_b = minval(b(:))
  max_b = maxval(b(:))

  !-------------------------------------------!
  !   Browse through matrix and plot it :-)   !
  !-------------------------------------------!
  do row = 1, n
    do col = 1, n

      ! This is normal, scaled circle for a matrix value
      call IO % Plot_Circle(9, row, col, A % val(row, col), min_a, max_a, 50)

      ! If source
      if(present(srca)) then
        if(row .eq. srca(1) .and. col .eq. srca(2)) then
          call IO % Plot_Box(9, row, col, SOURCEA, 51)
        end if
      end if

    end do
  end do

  !-------------------------------!
  !   Plot the unknown vector x   !
  !-------------------------------!
  do row = 1, n

    ! This is normal, scaled circle for a matrix value
    call IO % Plot_Circle(9, row, n+3, x(row), min_x, max_x, 50)

    ! If target for x is present
    if(present(tarx)) then
      if(row .eq. tarx(1)) then
        call IO % Plot_Box(9, row, n+3, TARGET, 55)
      end if
    end if

    if(present(srcx)) then
      if(row .eq. srcx(1)) then
        call IO % Plot_Box(9, row, n+3, SOURCEX, 52)
      end if
    end if

  end do

  !--------------------------------!
  !   Plot the right hand side b   !
  !--------------------------------!
  do row = 1, n
    ! This is normal, scaled circle for a matrix value
    call IO % Plot_Circle(9, row, n+7, b(row), min_b, max_b, 50)

    if(present(srcb)) then
      if(row .eq. srcb(1)) then
        call IO % Plot_Box(9, row, n+7, SOURCEB, 53)
      end if
    end if

  end do

  close(9)

  end subroutine
