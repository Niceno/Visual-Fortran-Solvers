!==============================================================================!
  subroutine Create_Matrix_Compressed(mat, ni, nj, nk, fill_in)
!----------------------------------[Modules]-----------------------------------!
  use Matrix_Mod
!------------------------------------------------------------------------------!
  implicit none
!---------------------------------[Arguments]----------------------------------!
  type(Matrix) :: mat
  integer      :: ni, nj, nk
  integer      :: fill_in
!---------------------------------[Interfaces]---------------------------------!
  include "Print_Matrix.int"               
  include "Print_Matrix_Compressed.int"               
  include "Print_Vector.int"               
!-----------------------------------[Locals]-----------------------------------!
  integer :: i, j, k, pass, non_zeros
  integer :: c, w, e, s, n, b, t
  integer :: sw, se, nw, ne, bw, be, tw, te, bs, bn, ts, tn
  integer :: col_a, col_b, row_a, row_b, pos_a, pos_b
!==============================================================================!

  !--------------------------------------------------------------------!
  !   Make two passes; in the first one count non-zeros and allocate   !
  !   memory, in the second one, form the compressed matrix            !
  !--------------------------------------------------------------------!
  do pass = 1, 2

    non_zeros = 0

    ! Browse in a way in which cell number "mat" will increase one by one
    do k=1,nk
      do j=1,nj
        do i=1,ni
          c = (k-1)*ni*nj + (j-1)*ni + i

          ! First neighbours
          e = c+1
          w = c-1
          n = c+ni
          s = c-ni
          t = c+ni*nj
          b = c-nj*nj 

          ! Second neighbours
          sw = s-1 
          se = s+1 
          nw = n-1 
          ne = n+1 
          bw = b-1 
          be = b+1 
          tw = t-1 
          te = t+1 
          bs = b-ni
          bn = b+ni
          ts = t-ni
          tn = t+ni
  
          ! If second pass, set row index
          if(pass == 2) then
            mat % row(c) = non_zeros + 1
          end if

          !-------!
          !   B   !
          !-------!
          if(k > 1) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = b
              mat % val(non_zeros) = -1.1  
            end if 
          end if
          
          ! BE (good with TW)
          if((fill_in > 0) .and. (k > 1) .and. (i < ni)) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = be
            end if 
          end if
 
          ! BEE (good with TWW)
          if((fill_in > 1) .and. (k > 1) .and. (i < ni-1)) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = be+1
            end if 
          end if
 
          ! BN (Good with TS)
          if((fill_in > 0) .and. (k > 1) .and. (j < nj)) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = bn
            end if 
          end if
 
          ! BNN (Good with TSS)
          if((fill_in > 1) .and. (k > 1) .and. (j < nj-1)) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = bn+ni
            end if 
          end if
 
          !-------!
          !   S   !
          !-------!
          if(j > 1) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = s
              mat % val(non_zeros) = -2.2
            end if 
          end if

          ! SE (Good with NW)
          if((fill_in > 0) .and. (j > 1) .and. (i < ni)) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = se
            end if 
          end if

          ! SEE (Good with NWW)
          if((fill_in > 1) .and. (j > 1) .and. (i < ni-1)) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = se+1
            end if 
          end if

          !-------!
          !   W   !
          !-------!
          if(i > 1) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = w
              mat % val(non_zeros) = -3.3  
            end if 
          end if
  
          !-------------
          !
          !   Central
          !
          !-------------
          non_zeros = non_zeros + 1
          if(pass == 2) then
            mat % col(non_zeros) = c
            mat % val(non_zeros) = 19.9  
            mat % dia(c) = non_zeros
          end if
  
          !-------!
          !   E   !
          !-------!
          if(i < ni) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = e
              mat % val(non_zeros) = -3.3  
            end if 
          end if
  
          ! NW (Good with SE)
          if((fill_in > 0) .and. (j < nj) .and. (i > 1)) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = nw
            end if 
          end if

          ! NWW (Good with SEE)
          if((fill_in > 1) .and. (j < nj) .and. (i > 2)) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = nw-1
            end if 
          end if

          !------
          ! North
          !------
          if(j < nj) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = n
              mat % val(non_zeros) = -2.2  
            end if 
          end if
  
          ! TS (Good with BN)
          if((fill_in > 0) .and. (k < nk) .and. (j > 1)) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = ts
            end if 
          end if

          ! TSS (Good with BNN)
          if((fill_in > 1) .and. (k < nk) .and. (j > 2)) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = ts-ni
            end if 
          end if

          ! TW (Good with BE)
          if((fill_in > 0) .and. (k < nk) .and. (i > 1)) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = tw
            end if 
          end if

          ! TWW (Good with BEE)
          if((fill_in > 1) .and. (k < nk) .and. (i > 2)) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = tw-1
            end if 
          end if

          !-------!
          !   T   !  
          !-------!
          if(k < nk) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = t
              mat % val(non_zeros) = -1.1  
            end if 
          end if

        end do        
      end do
    end do

    if(pass == 2) then
      mat % row(ni*nj*nk+1) = non_zeros+1
    end if

    if(pass == 1) then
      write(*,*) 'Number of nonzeros: ', non_zeros
      mat % n        = ni*nj*nk  
      mat % nonzeros = non_zeros
      allocate (mat % row(ni*nj*nk+1)); mat % row = 0
      allocate (mat % dia(ni*nj*nk));   mat % dia = 0
      allocate (mat % col(non_zeros));  mat % col = 0
      allocate (mat % val(non_zeros));  mat % val = 0
      allocate (mat % mir(non_zeros));  mat % mir = 0
    end if 
  end do

  !-----------------------------------------------------------------!
  !   Find it's mirror (it is non_zeros * noz_zeros operation :-(   !
  !-----------------------------------------------------------------!

  ! Outer loop
  do row_a = 1, n
    do pos_a = mat % row(row_a), mat % row(row_a + 1) - 1 
      col_a = mat % col(pos_a)  ! at this point you have row_a and col_a  
      
      ! Inner loop (it might probably go from 1 to row_a-1
      do row_b = 1, n
        do pos_b = mat % row(row_b), mat % row(row_b + 1) - 1 
          col_b = mat % col(pos_b)  ! at this point you have row_b and col_b 

          if( (col_b == row_a) .and. (row_b == col_a) ) then
            mat % mir(pos_a) = pos_b 
            mat % mir(pos_b) = pos_a 
            goto 1  ! done with the inner loop, get out
          end if
        end do
      end do       
1     continue
    end do       
  end do       

! do i=1, ni*nj*nk+1
!   write(*,*) i, mat % row(i)
! end do
! do i=1, ni*nj*nk
!   write(*,*) i, mat % dia(i)
! end do
! do i=1, non_zeros+1
!   write(*,*) i, mat % val(i)
! end do

  end subroutine Create_Matrix_Compressed
