!==============================================================================!
  subroutine Plot_Header(IO, fu)
!------------------------------------------------------------------------------!
!>  Plots the Xfig file header.
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  class(In_Out_Type)  :: IO
  integer, intent(in) :: fu
!------------------------[Avoid unused parent warning]-------------------------!
  Unused(IO)
!==============================================================================!

  !--------------------------!
  !   Write the header out   !
  !--------------------------!
  write(fu,'(a)') "#FIG 3.2  Produced by xfig version 3.2.8b"
  write(fu,'(a)') "Landscape"
  write(fu,'(a)') "Center"
  write(fu,'(a)') "Metric"
  write(fu,'(a)') "A4"
  write(fu,'(a)') "100.00"
  write(fu,'(a)') "Single"
  write(fu,'(a)') "-2"
  write(fu,'(a)') "1200 2"

  !-----------------------------------!
  !     Define custom colors too      !
  !- - - - - - - - - - - - - - - - - -!
  !   If you change this, make sure   !
  !    you check Constants.h90 too    !
  !-----------------------------------!

  ! Shades of gray, used to background matrix
  write(fu,'(a)') "0 40 #F8F8F8"  ! 248 starting from close to zero
  write(fu,'(a)') "0 41 #F0F0F0"
  write(fu,'(a)') "0 42 #E8E8E8"  ! 232
  write(fu,'(a)') "0 43 #E0E0E0"
  write(fu,'(a)') "0 44 #D8D8D8"  ! 216
  write(fu,'(a)') "0 45 #D0D0D0"
  write(fu,'(a)') "0 46 #C8C8C8"  ! 200
  write(fu,'(a)') "0 47 #C0C0C0"  ! 192 maximum positive value
  write(fu,'(a)') "0 48 #B8B8B8"  ! 184 just if we go over the top

  ! One more shade of gray, medium gray, used for matrix brackets
  write(fu,'(a)') "0 49 #808080"

  ! Shades of red, from lightest to darkest
  write(fu,'(a)') "0 50 #FFFFFF"  ! starting from close to zero
  write(fu,'(a)') "0 51 #FFDDDD"
  write(fu,'(a)') "0 52 #FFBBBB"
  write(fu,'(a)') "0 53 #FF9999"
  write(fu,'(a)') "0 54 #FF7777"
  write(fu,'(a)') "0 55 #FF5555"
  write(fu,'(a)') "0 56 #FF3333"
  write(fu,'(a)') "0 57 #FF1111"  ! maximum positive value
  write(fu,'(a)') "0 58 #FF0000"  ! just if we go over the top

  ! Shades of green, from lightest to darkest
  write(fu,'(a)') "0 60 #FFFFFF"  ! starting from close to zero
  write(fu,'(a)') "0 61 #DDFFDD"
  write(fu,'(a)') "0 62 #BBFFBB"
  write(fu,'(a)') "0 63 #99FF99"
  write(fu,'(a)') "0 64 #77FF77"
  write(fu,'(a)') "0 65 #55FF55"
  write(fu,'(a)') "0 66 #33FF33"
  write(fu,'(a)') "0 67 #11FF11"  ! maximum positive value
  write(fu,'(a)') "0 68 #00FF00"  ! just if we go over the top

  ! Shades of blue, from lightest to darkest
  write(fu,'(a)') "0 70 #FFFFFF"  ! starting from close to zero
  write(fu,'(a)') "0 71 #DDDDFF"
  write(fu,'(a)') "0 72 #BBBBFF"
  write(fu,'(a)') "0 73 #9999FF"
  write(fu,'(a)') "0 74 #7777FF"
  write(fu,'(a)') "0 75 #5555FF"
  write(fu,'(a)') "0 76 #3333FF"
  write(fu,'(a)') "0 77 #1111FF"  ! minimum negative value
  write(fu,'(a)') "0 78 #0000FF"  ! just if we slip below the bottom

  end subroutine
