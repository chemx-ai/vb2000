      PROGRAM GETBAS
C
C     This little piece of code reads a VB2000 basis set file and
C     outputs a file of the basis set in the form used by Gamess.
C     Individual atom basis sets can then be cut and pasted into a
C     Gamess input file.
C
C     Compile with:-
C
C     gfortran -o get-gms-bas.exe get-gms-bas.f
C
C     Useage is:-
C
C     get-gms-bas.exe < D95 > D95.bas
C
C     for the example of the D95 basis set which is not defined in
C     Gamess, yet used frequently in VB2000.
C
      REAL ONE
      REAL*8 A,B
      INTEGER I,J,K
      CHARACTER*80 RECORD
      CHARACTER*1 C
      ONE = 1.00
 100  READ(*,'(A80)',END=2) RECORD
      IF(RECORD(1:1).eq."-") WRITE(*,'(A80)') RECORD
 3    READ(*,'(A80)',END=2) RECORD
      IF(RECORD(1:4).eq."****") GOTO 1
      READ(RECORD,*) I, C, J
      WRITE(*,'(1X,A,I6,F6.2)') C,J,ONE
      DO I=1,J
      READ(*,'(A80)') RECORD
      READ(RECORD,*) K,A,B
      WRITE(*,'(I2,1X,2D18.10)') K,A,B
      ENDDO
      GOTO 3
 1    WRITE(*,'(A80)') RECORD
      GOTO 100
 2    CONTINUE
      END

