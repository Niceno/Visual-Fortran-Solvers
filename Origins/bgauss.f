c
c     Numerical Analysis:
c     Mathematics of Scientific Computing
c     Third Edition
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 2002
c     Copyright (c) 1996
c
c     Section 4.3 
c
c     Example of basic Gaussian elimination  
c
c
c     file: bgauss.f
c
      parameter (n = 4)
      dimension a(n,n)
      data (a(1,j),j=1,n) /6.0,-2.0,2.0,4.0/
      data (a(2,j),j=1,n) /12.0,-8.0,6.0,10.0/
      data (a(3,j),j=1,n) /3.0,-13.0,9.0,3.0/
      data (a(4,j),j=1,n) /-6.0,4.0,1.0,-18.0/
c
      print *
      print *,' Basic gaussian elimination'
      print *,' Section 4.3, Kincaid-Cheney'
      print *
c
      do 4 k=1,n-1
         do 3 i=k+1,n      
            xmult = a(i,k)/a(k,k)       
            a(i,k) = 0.0
            do 2 j=k+1,n    
               a(i,j) = a(i,j) - xmult*a(k,j)      
 2          continue
 3       continue  
 4    continue    
c
      do 6 i=1,n
         do 5 j=1,n
            print 7,i,j,a(i,j)
 5       continue
 6    continue
c
 7    format (1x,'a(',i2,',',i2,') =',e13.6)
      stop
      end 
