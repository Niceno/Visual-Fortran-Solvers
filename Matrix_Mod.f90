!==============================================================================!
  module Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!------------------------------------------------------------------------------!
!   Matrix type                                                                !
!                                                                              !
!   Matrix is stored in compressed row format.                                 !
!   (See: http://netlib.org/linalg/html_templates/node91.html)                 !
!                                                                              !
!   Example:                                                                   !
!                                                                              !
!       c   c  .    c                                                          !
!       o   o  .    o                                                          !
!       l   l       l                                                          !
!                                                                              !
!       1   2       n                                                          !
!                                                                              !
!    [ 10   0   4   5 ]  --> row 1                                             !
!    [  2  12  -1   0 ]  --> rows store discretized control volumes            !
!    [  0   1  99   7 ]  ...                                                   !
!    [ -3  11   0  53 ]  --> row n                                             !
!                                                                              !
!   Compressed row storage of the above matrix reads:                          !
!                                                                              !
!   A % val = [  10   4   5   2  12  -1   1  99   7  -3  11  53 ]              !
!   A % col = [   1   3   4   1   2   3   2   3   4   1   2   4 ]              !
!   A % row = [   1   4   7  10 ]                                              !
!                                                                              !
!   A % dia = [   1   5   9  12 ]                                              !
!==============================================================================!
  type Matrix
    integer              :: nonzeros               ! number of nonzero entries
    real,    allocatable :: val(:)                 ! value
    integer, allocatable :: col(:)                 ! beginning of each row   
    integer, allocatable :: row(:)                 ! column positions
    integer, allocatable :: dia(:)                 ! diagonal positions 
  end type Matrix

end module Matrix_Mod
