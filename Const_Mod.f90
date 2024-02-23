!==============================================================================!
  module Const_Mod
!------------------------------------------------------------------------------!
  implicit none
!==============================================================================!

  ! Standard string length
  integer, parameter :: VL =   4
  integer, parameter :: SL =  80  ! standard string length (like page width)
  integer, parameter :: DL = 160  ! double string length (twice the page width)

  ! Double and single precision constants definitions
  integer, parameter :: DP =   8  ! double precisions for real and long integer
  integer, parameter :: SP =   4  ! single precisions for real and short integer

  integer, parameter :: IP = sizeof(1)
  integer, parameter :: LP = IP
  integer, parameter :: RP = sizeof(1.0)

  !----------------------------------------!
  !   A few handy mathematical constants   !
  !----------------------------------------!

  ! Big and small numbers in metric system to avoid ghost numbers
  real, parameter :: YOTTA = 1.e+24  ! avoid ghost number 1.0e+24
  real, parameter :: ZETTA = 1.e+21  ! avoid ghost number 1.0e+21
  real, parameter :: EXA   = 1.e+18  ! avoid ghost number 1.0e+18
  real, parameter :: PETA  = 1.e+15  ! avoid ghost number 1.0e+15
  real, parameter :: TERA  = 1.e+12  ! avoid ghost number 1.0e+12
  real, parameter :: GIGA  = 1.e+9   ! avoid ghost number 1.0e+9
  real, parameter :: MEGA  = 1.e+6   ! avoid ghost number 1.0e+6
  real, parameter :: KILO  = 1.e+3   ! avoid ghost number 1.0e+3
  real, parameter :: MILI  = 1.e-3   ! avoid ghost number 1.0e-3
  real, parameter :: MICRO = 1.e-6   ! avoid ghost number 1.0e-6
  real, parameter :: NANO  = 1.e-9   ! avoid ghost number 1.0e-9
  real, parameter :: PICO  = 1.e-12  ! avoid ghost number 1.0e-12
  real, parameter :: FEMTO = 1.e-15  ! avoid ghost number 1.0e-15
  real, parameter :: ATTO  = 1.e-18  ! avoid ghost number 1.0e-18
  real, parameter :: ZEPTO = 1.e-21  ! avoid ghost number 1.0e-21
  real, parameter :: YOCTO = 1.e-24  ! avoid ghost number 1.0e-24

  real,    parameter :: HUGE     = PETA        ! a very big (huge) number
  real,    parameter :: TINY     = FEMTO       ! a very small (tiny) number
  integer, parameter :: HUGE_INT = 1073741824  ! big integer (this is 2^30)

  ! Euler's prime number (also the largest integer in 32 bit precision)
  integer, parameter :: EULER    = 2147483647  ! Euler's prime number 2^31 - 1

  ! Archimedes’ constant
  real, parameter :: PI = 3.14159265359  ! Archimedes constant

  ! These are often used in turbulence models
  real, parameter :: ONE_THIRD  = 1.0 / 3.0        ! avoids frequent 1.0/3.0
  real, parameter :: TWO_THIRDS = 1.0 - ONE_THIRD  ! avoids frequent 2.0/3.0
  real, parameter :: ONE_SIXTH  = ONE_THIRD * 0.5  ! avoids frequent 1.0/6.0

  end module
