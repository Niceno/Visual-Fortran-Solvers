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
c     Example of Cholesky-factorization
c
c
c     file: cholsky.f
c
      parameter (n=3)
      dimension a(n,n),l(n,n)
      real l
      data (a(1,j),j=1,n) / 60.0,30.0,20.0/
      data (a(2,j),j=1,n) / 30.0,20.0,15.0/
      data (a(3,j),j=1,n) / 20.0,15.0,12.0/
c
      print *
      print *,' Cholesky-factorization'
      print *,' Section 4.2, Kincaid-Cheney'
      print *
c
      do 5 k=1,n
         sum1 = a(k,k)
         do 2 m=1,k-1
            sum1 = sum1 - l(k,m)**2.0
 2       continue
         l(k,k) = sqrt(sum1)
         do 4 i=k+1,n
            sum2 = a(i,k)
            do 3 m=1,k-1
               sum2 = sum2 - l(i,m)*l(k,m)
 3          continue
            l(i,k) = sum2/l(k,k)
 4       continue
 5    continue
c
      do 7 i=1,n
         do 6 j=1,i
            print 8,i,j,l(i,j)
 6       continue
 7    continue
c
 8    format (1x,'l(',i2,',',i2,') =',e13.6)
      stop
      end
