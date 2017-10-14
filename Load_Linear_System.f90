!==============================================================================!
  subroutine Load_Linear_System(n, a, b)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  integer           :: n
  real, allocatable :: a(:,:)
  real, allocatable :: b(:)
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col
!==============================================================================!

  ! Read the system from the file system
  open(9, file="A_b.dat")
  read(9, *) n
  allocate (a(n,n))
  do row=1,n
    read(9, *) (a(row,col), col=1,n)
  end do
  allocate (b(n))
  read(9, *) (b(row), row=1,n)
  close(9)

  end subroutine Load_Linear_System
