!==============================================================================!
  subroutine Plot_Header(IO, fu)
!------------------------------------------------------------------------------!
!>  Plots the Xfig file header.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)  :: IO
  integer, intent(in) :: fu
!==============================================================================!

  ! Write the header out
  write(fu,'(a)') "#FIG 3.2  Produced by xfig version 3.2.8b"
  write(fu,'(a)') "Landscape"
  write(fu,'(a)') "Center"
  write(fu,'(a)') "Metric"
  write(fu,'(a)') "A4"
  write(fu,'(a)') "100.00"
  write(fu,'(a)') "Single"
  write(fu,'(a)') "-2"
  write(fu,'(a)') "1200 2"

  ! Define custom colors too
  write(fu,'(a)') "0 50 #FFFFFF"  ! starting from close to zero
  write(fu,'(a)') "0 51 #DDDDFF"
  write(fu,'(a)') "0 52 #BBBBFF"
  write(fu,'(a)') "0 53 #9999FF"
  write(fu,'(a)') "0 54 #7777FF"
  write(fu,'(a)') "0 55 #5555FF"
  write(fu,'(a)') "0 56 #3333FF"
  write(fu,'(a)') "0 57 #1111FF"  ! minimum negative value
  write(fu,'(a)') "0 58 #1111FF"  ! just if we slip below the bottom

  write(fu,'(a)') "0 60 #FFFFFF"  ! starting from close to zero
  write(fu,'(a)') "0 61 #FFDDDD"
  write(fu,'(a)') "0 62 #FFBBBB"
  write(fu,'(a)') "0 63 #FF9999"
  write(fu,'(a)') "0 64 #FF7777"
  write(fu,'(a)') "0 65 #FF5555"
  write(fu,'(a)') "0 66 #FF3333"
  write(fu,'(a)') "0 67 #FF1111"  ! maximum positive value
  write(fu,'(a)') "0 68 #FF0000"  ! just if we go over the top

  end subroutine
