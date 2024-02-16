!-------------------------!
!   Xfig units for 1 cm   !
!-------------------------!
integer, parameter :: CM = 450

!-----------------!
!   Xfig colors   !
!-----------------!

! Standard Xfig colors
integer, parameter :: BLACK    =   0
integer, parameter :: BLUE     =   1
integer, parameter :: GREEN    =   2
integer, parameter :: CYAN     =   3
integer, parameter :: RED      =   4
integer, parameter :: MAGENTA  =   5
integer, parameter :: YELLOW   =   6
integer, parameter :: WHITE    =   7
integer, parameter :: BLUE4    =   8  ! darkest
integer, parameter :: BLUE3    =   9  ! medium
integer, parameter :: BLUE2    =  10  ! light
integer, parameter :: LTBLUE   =  11
integer, parameter :: GREEN4   =  12  ! darkest
integer, parameter :: GREEN3   =  13  ! medium
integer, parameter :: GREEN2   =  14  ! lightest
integer, parameter :: CYAN4    =  15  ! darkest
integer, parameter :: CYAN3    =  16  ! medium
integer, parameter :: CYAN2    =  17  ! lightest
integer, parameter :: RED4     =  18  ! darkest
integer, parameter :: RED3     =  19  ! medium
integer, parameter :: RED2     =  20  ! lightest
integer, parameter :: MAGENTA4 =  21
integer, parameter :: MAGENTA3 =  22
integer, parameter :: MAGENTA2 =  23
integer, parameter :: BROWN4   =  24
integer, parameter :: BROWN3   =  25
integer, parameter :: BROWN2   =  26
integer, parameter :: PINK4    =  27
integer, parameter :: PINK3    =  28
integer, parameter :: PINK2    =  29
integer, parameter :: PINK     =  30
integer, parameter :: GOLD     =  31

!-----------------------------------!
!       Extended Xfig colors        !
!- - - - - - - - - - - - - - - - - -!
!   If you change this, make sure   !
!    you check Constants.h90 too    !
!-----------------------------------!

! Color for brackets and symbols
integer, parameter :: GRAY = 49

! Color for the target in matrix plotting
integer, parameter :: TARGET  = 52  ! light red, pink
integer, parameter :: SOURCE1 = 61
integer, parameter :: SOURCE2 = 62
integer, parameter :: SOURCE3 = 63
integer, parameter :: SOURCEA = 71
integer, parameter :: SOURCEX = 72
integer, parameter :: SOURCEB = 73

!---------------------!
!   Real parameters   !
!---------------------!

! This is used when printing matrices
real, parameter, dimension(4) :: SCALE = (/0.5, 0.01, 0.001, 1.0e-30/)
