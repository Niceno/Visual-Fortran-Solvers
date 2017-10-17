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
  integer :: i, j, k, pass, non_zeros, level
  integer :: c, w, e, s, n, b, t
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
          
          ! BE, BEE, ... (good with TW, TWW, ...)
          do level=1,fill_in
            if((fill_in > 0) .and. (k > 1) .and. (i < ni-level+1)) then
              non_zeros = non_zeros + 1
              if(pass == 2) then
                mat % col(non_zeros) = b + level
              end if 
            end if
          end do 
 
          ! BN, BNN, ... (good with TS, TSS, ...)
          do level=1,fill_in
            if((fill_in > 0) .and. (k > 1) .and. (j < nj-level+1)) then
              non_zeros = non_zeros + 1
              if(pass == 2) then
                mat % col(non_zeros) = b + ni * level
              end if 
            end if
          end do 
 
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

          ! SE, SEE, ... (good with NW, NWW, ...)
          do level=1,fill_in
            if((fill_in > 0) .and. (j > 1) .and. (i < ni-level+1)) then
              non_zeros = non_zeros + 1
              if(pass == 2) then
                mat % col(non_zeros) = s + level
              end if 
            end if
          end do 
 
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
  
          !-------------!
          !             !
          !   Central   !
          !             !
          !-------------!
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

          ! NW, NWW, ... (good with SE, SEE, ...)
          do level=1,fill_in
            if((fill_in > 0) .and. (j < nj) .and. (i > level)) then
              non_zeros = non_zeros + 1
              if(pass == 2) then
                mat % col(non_zeros) = n - level
              end if 
            end if
          end do 
  
          !-------!
          !   N   !
          !-------!
          if(j < nj) then
            non_zeros = non_zeros + 1
            if(pass == 2) then
              mat % col(non_zeros) = n
              mat % val(non_zeros) = -2.2  
            end if 
          end if
  
          ! TS, TSS, ... (good with BN, BNN, ...)
          do level=1,fill_in
            if((fill_in > 0) .and. (k < nk) .and. (j > level)) then
              non_zeros = non_zeros + 1
              if(pass == 2) then
                mat % col(non_zeros) = t - ni * level
              end if 
            end if
          end do 

          ! TW, TWW, ... (good with BE, BEE, ...)
          do level=1,fill_in
            if((fill_in > 0) .and. (k < nk) .and. (i > level)) then
              non_zeros = non_zeros + 1
              if(pass == 2) then
                mat % col(non_zeros) = t - level
              end if 
            end if
          end do 
 
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
