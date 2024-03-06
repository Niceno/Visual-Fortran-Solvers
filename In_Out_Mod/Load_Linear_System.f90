!==============================================================================!
  subroutine Load_Linear_System(IO, name_in, n, a, b)
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type) :: IO
  character(len=*)   :: name_in
  integer            :: n
  real, allocatable  :: a(:,:)
  real, allocatable  :: b(:)
!-----------------------------------[Locals]-----------------------------------!
  integer :: row, col
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(IO)
!==============================================================================!

  ! Read the system from the file system
  open(9, file=name_in)
  read(9, *) n
  allocate(a(n,n))
  do row = 1, n
    read(9, *) (a(row,col), col=1,n)
  end do
  allocate(b(n))
  read(9, *) (b(row), row=1,n)
  close(9)

  end subroutine
