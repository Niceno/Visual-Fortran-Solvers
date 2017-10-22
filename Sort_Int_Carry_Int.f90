!======================================================================*
      subroutine Sort_Int_Carry_Int(X,Y,N,KFLAG)
!----------------------------------------------------------------------*
!   Sorts int. array X and makes the same changes in int. array Y.     *
!----------------------------------------------------------------------*
      DIMENSION IL(21),IU(21)
      integer X(N),Y(N),T,TT,TY,TTY
!======================================================================*
! NIST Guide to Available Math Software.                               *
! Source for module ISORT from package CMLIB.                          *
! Retrieved from CAMSUN on Mon Feb 23 10:44:31 1998.                   *
!                                                                      *
! Slightly modified by Bojan Niceno on the same day                    *
!----------------------------------------------------------------------*
!***BEGIN PROLOGUE  ISORT
!***DATE WRITTEN   761118   (YYMMDD)
!***REVISION DATE  820801   (YYMMDD)
!***CATEGORY NO.  N6A2A
!***KEYWORDS  QUICKSORT,SINGLETON QUICKSORT,SORT,SORTING
!***AUTHOR  JONES, R. E., (SNLA)
!           KAHANER, D. K., (NBS)
!           WISNIEWSKI, J. A., (SNLA)
!***PURPOSE  ISORT sorts integer array X and optionally makes the same
!            interchanges in integer array Y.  The array X may be
!            sorted in increasing order or decreasing order.  A
!            slightly modified QUICKSORT algorithm is used.
!***DESCRIPTION
!
!     Written by Rondall E Jones
!     Modified by John A. Wisniewski to use the Singleton QUICKSORT
!     algorithm. Date 18 November 1976.
!
!     Further modified by David K. Kahaner
!     NATIONAL BUREAU OF STANDARDS
!     August, 1981
!
!     Abstract
!         ISORT sorts integer array X and optionally makes the same
!         interchanges in integer array Y.  The array X may be sorted in
!         INCREASING order or DECREASING order.  A slightly modified
!         QUICKSORT algorithm is used.
!
!     Reference
!         Singleton,R.C., Algorithm 347, An Efficient Algorithm For
!         Sorting With Minimal Storage, CACM,12(3),1969,185-7.
!
!     Description of Parameters
!         X - integer array of values to be sorted
!         Y - integer array to be (optionally) carried along
!         N - number of values in integer array X to be sorted
!     KFLAG - control parameter
!           = 2 means sort X in INCREASING order and carry Y along.
!           = 1 means sort X in INCREASING order (ignoring Y)
!           =-1 means sort X in DECREASING order (ignoring Y)
!           =-2 means sort X in DECREASING order and carry Y along.
!***REFERENCES  SINGLETON, R. C., ALGORITHM 347, AN EFFICIENT
!                 ALGORITHM FOR SORTING WITH MINIMAL STORAGE, CACM,
!                 VOL. 12, NO. 3, 1969, PP. 185-187.
!***ROUTINES CALLED  XERROR
!***end PROLOGUE  ISORT
!----------------------------------------------------------------------*
!***FIRST EXECUTABLE STATEMENT  ISORT
      NN = N
      IF (NN.GE.1) GO TO 10
      WRITE(*,*) 'ISORT: THE NUMBER OF VALUES TO BE SORTED WAS NOT POSITIVE.'
      RETURN
   10 KK = IABS(KFLAG)
      IF ((KK.EQ.1).OR.(KK.EQ.2)) GO TO 15
      WRITE(*,*) 'ISORT: THE SORT CONTROL parameter, K, WAS NOT 2, 1, -1, OR -2.'
      RETURN
!
! ALTER ARRAY X TO GET DECREASING ORDER IF NEEDED
!
   15 IF (KFLAG.GE.1) GO TO 30
      DO 20 I=1,NN
      X(I) = -X(I)
   20 CONTINUE
   30 if(KK==1) goto 100
      if(KK==2) goto 200 
!
! SORT X ONLY
!
  100 CONTINUE
      M=1
      I=1
      J=NN
      R=.375
  110 IF (I .EQ. J) GO TO 155
      IF (R .GT. .5898437) GO TO 120
      R=R+3.90625E-2
      GO TO 125
  120 R=R-.21875
  125 K=I
!                                  SELECT A CENTRAL ELEMENT OF THE
!                                  ARRAY AND SAVE IT IN LOCATION T
      IJ = I + IFIX (FLOAT (J-I) * R)
      T=X(IJ)
!                                  IF FIRST ELEMENT OF ARRAY IS GREATER
!                                  THAN T, INTERCHANGE WITH T
      IF (X(I) .LE. T) GO TO 130
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
  130 L=J
!                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
!                                  T, INTERCHANGE WITH T
      IF (X(J) .GE. T) GO TO 140
      X(IJ)=X(J)
      X(J)=T
      T=X(IJ)
!                                  IF FIRST ELEMENT OF ARRAY IS GREATER
!                                  THAN T, INTERCHANGE WITH T
      IF (X(I) .LE. T) GO TO 140
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
      GO TO 140
  135 TT=X(L)
      X(L)=X(K)
      X(K)=TT
!                                  FIND AN ELEMENT IN THE SECOND HALF OF
!                                  THE ARRAY WHICH IS SMALLER THAN T
  140 L=L-1
      IF (X(L) .GT. T) GO TO 140
!                                  FIND AN ELEMENT IN THE FIRST HALF OF
!                                  THE ARRAY WHICH IS GREATER THAN T
  145 K=K+1
      IF (X(K) .LT. T) GO TO 145
!                                  INTERCHANGE THESE ELEMENTS
      IF (K .LE. L) GO TO 135
!                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
!                                  THE ARRAY YET TO BE SORTED
      IF (L-I .LE. J-K) GO TO 150
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GO TO 160
  150 IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GO TO 160
!                                  BEGIN AGAIN ON ANOTHER PORTION OF
!                                  THE UNSORTED ARRAY
  155 M=M-1
      IF (M .EQ. 0) GO TO 300
      I=IL(M)
      J=IU(M)
  160 IF (J-I .GE. 1) GO TO 125
      IF (I .EQ. 1) GO TO 110
      I=I-1
  165 I=I+1
      IF (I .EQ. J) GO TO 155
      T=X(I+1)
      IF (X(I) .LE. T) GO TO 165
      K=I
  170 X(K+1)=X(K)
      K=K-1
      IF (T .LT. X(K)) GO TO 170
      X(K+1)=T
      GO TO 165
!
! SORT X AND CARRY Y ALONG
!
  200 CONTINUE
      M=1
      I=1
      J=NN
      R=.375
  210 IF (I .EQ. J) GO TO 255
      IF (R .GT. .5898437) GO TO 220
      R=R+3.90625E-2
      GO TO 225
  220 R=R-.21875
  225 K=I
!                                  SELECT A CENTRAL ELEMENT OF THE
!                                  ARRAY AND SAVE IT IN LOCATION T
      IJ = I + IFIX (FLOAT (J-I) *R)
      T=X(IJ)
      TY= Y(IJ)
!                                  IF FIRST ELEMENT OF ARRAY IS GREATER
!                                  THAN T, INTERCHANGE WITH T
      IF (X(I) .LE. T) GO TO 230
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
       Y(IJ)= Y(I)
       Y(I)=TY
      TY= Y(IJ)
  230 L=J
!                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
!                                  T, INTERCHANGE WITH T
      IF (X(J) .GE. T) GO TO 240
      X(IJ)=X(J)
      X(J)=T
      T=X(IJ)
       Y(IJ)= Y(J)
       Y(J)=TY
      TY= Y(IJ)
!                                  IF FIRST ELEMENT OF ARRAY IS GREATER
!                                  THAN T, INTERCHANGE WITH T
      IF (X(I) .LE. T) GO TO 240
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
       Y(IJ)= Y(I)
       Y(I)=TY
      TY= Y(IJ)
      GO TO 240
  235 TT=X(L)
      X(L)=X(K)
      X(K)=TT
      TTY= Y(L)
       Y(L)= Y(K)
       Y(K)=TTY
!                                  FIND AN ELEMENT IN THE SECOND HALF OF
!                                  THE ARRAY WHICH IS SMALLER THAN T
  240 L=L-1
      IF (X(L) .GT. T) GO TO 240
!                                  FIND AN ELEMENT IN THE FIRST HALF OF
!                                  THE ARRAY WHICH IS GREATER THAN T
  245 K=K+1
      IF (X(K) .LT. T) GO TO 245
!                                  INTERCHANGE THESE ELEMENTS
      IF (K .LE. L) GO TO 235
!                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
!                                  THE ARRAY YET TO BE SORTED
      IF (L-I .LE. J-K) GO TO 250
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GO TO 260
  250 IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GO TO 260
!                                  BEGIN AGAIN ON ANOTHER PORTION OF
!                                  THE UNSORTED ARRAY
  255 M=M-1
      IF (M .EQ. 0) GO TO 300
      I=IL(M)
      J=IU(M)
  260 IF (J-I .GE. 1) GO TO 225
      IF (I .EQ. 1) GO TO 210
      I=I-1
  265 I=I+1
      IF (I .EQ. J) GO TO 255
      T=X(I+1)
      TY= Y(I+1)
      IF (X(I) .LE. T) GO TO 265
      K=I
  270 X(K+1)=X(K)
       Y(K+1)= Y(K)
      K=K-1
      IF (T .LT. X(K)) GO TO 270
      X(K+1)=T
       Y(K+1)=TY
      GO TO 265
!
! CLEAN UP
!
  300 IF (KFLAG.GE.1) RETURN
      DO 310 I=1,NN
      X(I) = -X(I)
  310 CONTINUE
      RETURN
      end subroutine
