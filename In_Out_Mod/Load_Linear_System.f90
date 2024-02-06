!==============================================================================!
  subroutine In_Out_Mod_Load_Linear_System(name_in, n, a, b)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  character(len=*)  :: name_in
  integer           :: n
  real, allocatable :: a(:,:)
  real, allocatable :: b(:)
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col
!==============================================================================!

  ! Read the system from the file system
  open(9, file=name_in)
  read(9, *) n
  allocate (a(n,n))
  do row = 1, n
    read(9, *) (a(row,col), col=1,n)
  end do
  allocate (b(n))
  read(9, *) (b(row), row=1,n)
  close(9)

  end subroutine
