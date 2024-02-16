!==============================================================================!
  subroutine Handle_Assert(fail, text, file, line)
!------------------------------------------------------------------------------!
!>  Process the result of an assertion check and take appropriate actions if
!>  an assertion fails. It's is called exclusivelly from the Assert macro
!>  defined in Assert.h90.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  logical      :: fail  !! flag indicating whether the assertion has failed
  character(*) :: text  !! expression of the assertion that was checked
  character(*) :: file  !! name of the file where the assertion check occurred
  integer      :: line  !! line number where the assertion check occurred
!-----------------------------------[Locals]-----------------------------------!
  character(16) :: numb
!==============================================================================!

  if(fail) then
    write(numb, '(i16)') line
    print '(7a)', '  Assert(',         text,                 &
                  ') failed in file ', file,                 &
                  ' at line ',         trim(adjustl(numb)),  &
                  '.'
    stop
  end if

  end subroutine
