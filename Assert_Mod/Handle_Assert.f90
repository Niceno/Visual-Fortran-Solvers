!==============================================================================!
  subroutine Handle_Assert(fail, text, file, line)
!------------------------------------------------------------------------------!
!>  Process the result of an assertion check and take appropriate actions if
!>  an assertion fails. It's is called exclusivelly from the Assert macro
!>  defined in Assert.h90.
!------------------------------------------------------------------------------!
!   Functionality:                                                             !
!                                                                              !
!   * Sequential Run Handling:                                                 !
!     - If the code is running in a sequential (non-parallel) environment      !
!       (checked by Sequential_Run()), it checks if the assertion failed       !
!       (fail is true).                                                        !
!     - In case of a failure, it formats and prints an error message including !
!       the assertion condition (text), file name, and line number.            !
!     - After printing the error message, it calls Global % End_Parallel and   !
!       then stops the program execution using stop.                           !
!   * Parallel Run Handling:                                                   !
!     - If the code is running in a parallel environment, the subroutine       !
!       additionally includes the processor number in the error message        !
!       (obtained by This_Proc()).                                             !
!     - The rest of the handling is similar to the sequential run, with the    !
!       program terminating upon a failed assertion.                           !
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  logical      :: fail  !! flag indicating whether the assertion has failed
  character(*) :: text  !! expression of the assertion that was checked
  character(*) :: file  !! name of the file where the assertion check occurred
  integer      :: line  !! line number where the assertion check occurred
!-----------------------------------[Locals]-----------------------------------!
  character(16) :: numb, proc
!==============================================================================!

  ! Sequential run
  if(Sequential_Run()) then
    if(fail) then
      write(numb, '(i16)') line
      print '(7a)', '  Assert(',         text,                 &
                    ') failed in file ', file,                 &
                    ' at line ',         trim(adjustl(numb)),  &
                    '.'
      call Global % End_Parallel
      stop
    end if

  ! Parallel run
  else
    if(fail) then
      write(numb, '(i16)') line
      write(proc, '(i16)') This_Proc()
      print '(9a)', '  Assert(',         text,                 &
                    ') failed in file ', file,                 &
                    ' at line ',         trim(adjustl(numb)),  &
                    ' in processor ',    trim(adjustl(proc)),  &
                    '.'
      call Global % End_Parallel
      stop
    end if

  end if

  end subroutine
