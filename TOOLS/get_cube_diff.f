      PROGRAM GETCUBEDIFF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This program reads two CUBE files, evaluates the largest element 
C     in each and creates a file of the differences. It is used when 
C     the two files are the density of a VB function and the density of 
C     the GAMESS HF function created by the $DENSCUBE directive in
C     GAMESS/VB2000.
C
C     Created: 28 March, 2016 - Brian J Duke
C
C     Compile: gfortran -o get_cube_diff.x get_cube_diff.f
C
C     Useage: ./get_cube_diff.x < data
C
C     where data is something like:-
C
C      40                        - No of CUBE points (I3)
C     h2-3-dens-gms.cube         - Name of GAMESS HF function CUBE
C     h2-3-dens-tot.cube         - Name of VB function CUBE
C     h2-3-dens-diff.cube        - Output file of difference CUBE
C
C     The differences are output to the terminal. Example:-
C
C     Size of CUBE: 40
C      0.29194 IS LARGEST VALUE IN h2-3-dens-gms.cube 
C      0.29673 IS LARGEST VALUE IN h2-3-dens-tot.cube   
C      0.00942 IS LARGEST VALUE IN h2-3-dens-diff.cube 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INTEGER N,I,J,M,LENFIL1,LENFIL2,LENFIL3
      DOUBLE PRECISION A(100),B(100),C(100),AA,BB,CC
      CHARACTER*64 FILE1,FILE2,FILE3,LINE,INPFILE,OUTFILE
C
      READ(5,'(I3)') N 
      WRITE(6,'("Size of CUBE:",I3)') N 
      READ(5,'(A64)') FILE1
      READ(5,'(A64)') FILE2
      READ(5,'(A64)') FILE3
C     Get filename of cube 1.
      DO I=1,64
        IF(FILE1(I:I).EQ.' ') THEN
          LENFIL1=I-1
          GOTO 1
        ENDIF
      ENDDO
 1    CONTINUE
      INPFILE=FILE1(1:LENFIL1)
C     WRITE(6,'(A64)') INPFILE
      OPEN(UNIT=7,FILE=INPFILE,STATUS='OLD',
     1  ACCESS='SEQUENTIAL',FORM='FORMATTED')
C     Get filename of cube 2.
      DO I=1,64
        IF(FILE2(I:I).EQ.' ') THEN
          LENFIL2=I-1
          GOTO 2
        ENDIF
      ENDDO
 2    CONTINUE
      INPFILE=FILE2(1:LENFIL2)
C     WRITE(6,'(A64)') INPFILE
      OPEN(UNIT=8,FILE=INPFILE,STATUS='OLD',
     1  ACCESS='SEQUENTIAL',FORM='FORMATTED')
C
C     Get filename of diff o/p.
      DO I=1,64
        IF(FILE3(I:I).EQ.' ') THEN
          LENFIL3=I-1
          GOTO 3
        ENDIF
      ENDDO
 3    CONTINUE
      OUTFILE=FILE3(1:LENFIL3)
C     WRITE(6,'(A64)') OUTFILE
      OPEN(UNIT=10,FILE=OUTFILE,STATUS='UNKNOWN',
     1  ACCESS='SEQUENTIAL',FORM='FORMATTED')
C
      DO I=1,9
        READ(7,'(A64)') LINE
        WRITE(10,'(A64)') LINE
        READ(8,'(A64)') LINE
      ENDDO
      M = N*N
      AA = -1.0D3
      BB = -1.0D3
      cc = -1.0D3
      DO I=1,M
        READ(7,'(6E13.5)') (A(J),J=1,N)
        READ(8,'(6E13.5)') (B(J),J=1,N)
        DO J=1,N
          C(J) = B(J) - A(J)
          IF (A(J).GT.AA) AA = A(J)
          IF (B(J).GT.BB) BB = B(J)
          IF (C(J).GT.CC) CC = C(J)
        ENDDO
        WRITE(10,'(6E13.5)') (C(J),J=1,N)
      ENDDO
      WRITE(6,100 ) AA,FILE1
      WRITE(6,100 ) BB,FILE2
      WRITE(6,100 ) CC,FILE3
 100  FORMAT(F8.5," IS LARGEST VALUE IN ",A64)
      END      

