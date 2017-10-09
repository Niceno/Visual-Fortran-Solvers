c     
c     Numerical Analysis:
c     Mathematics of Scientific Computing
c     Third Edition
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 2002
c     Copyright (c) 1996
c
c     Section 4.2
c
c     Example of Forward Substitution
c
c
c     file: forsub.f
c
      parameter (n=3)
      dimension a(n,n),x(n),b(n)
      data (a(1,j),j=1,n) /6.0,0.0,0.0/
      data (a(2,j),j=1,n) /3.0,2.0,0.0/
      data (a(3,j),j=1,n) /4.0,2.0,1.0/
      data (b(i),i=1,n) /6.0,5.0,7.0/
c
      print *
      print *,' Forward Substitution example'
      print *,' Section 4.2, Kincaid-Cheney'
      print *
c
      do 3 i=1,n
         sum = b(i)
         do 2 j=1,i-1
            sum = sum - a(i,j)*x(j)
 2       continue
         x(i) = sum/a(i,i)
 3    continue
c
      do 4 i=1,n
         print 5,i,x(i)
 4    continue
c
 5    format (1x,'x(',i2,') =',2x,e13.6)
      stop
      end
