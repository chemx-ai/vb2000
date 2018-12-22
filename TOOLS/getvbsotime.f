      PROGRAM GETVBTIME
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Program to sum VB2000 times for VB2000 stand-alone test set.
C     The output is the time for each case, followed by the total
C     time.
C
C     Compile with:-
C     gfortran -o getvbsotime.x getvbtime.f
C
C     Before running in the directory of the output files, type:-
C     ls *.out > outs.list
C
C     Put getvbtime.x on your path and then run as:-
C     getvbsotime.x
C     or run with full path to getvbsotime.x, e.g
C     ./getvbsotime.x  or
C     ../getvbsotime.x or whatever
C
C     Author: Brian J. Duke.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      CHARACTER*4 PREF
      CHARACTER*40 FSTR,RECORD
      INTEGER I,J,IOUT,IOVBOUT,ILIST,LENFIL
      DOUBLE PRECISION T, TOT, TIME
C
C     To read and total the "TOTAL CPU TIME" from this data:-
C CPU TIME FOR MACROITERATION       14.330
C TOTAL CPU TIME                    14.900
C
C
      PREF=".out"
C     PREF=".log"
C     Alter PREF if files are *.log
C
      IOUT = 6
      IOVBOUT = 5
      TOT = 0.0D00
      J = 0
      OPEN(UNIT=ILIST,FILE="outs.list",STATUS='OLD',ERR=7,
     &  ACCESS='SEQUENTIAL',FORM='FORMATTED')
 8      TIME = 0.0D00
        READ(ILIST,12,END=4) RECORD
        DO I=1,40
          IF(RECORD(I:I).EQ.'.') THEN
            LENFIL=I-1
            GOTO 9
          ENDIF
        ENDDO
 9      FSTR=RECORD(1:LENFIL)//PREF
C       WRITE(IOUT,12) FSTR
        OPEN(UNIT=IOVBOUT,FILE=FSTR,STATUS='OLD',
     1    ACCESS='SEQUENTIAL',FORM='FORMATTED')
 1      READ(IOVBOUT,11,END=3) RECORD
        IF(RECORD(1:27).NE."CPU TIME FOR MACROITERATION") GOTO 1
        READ(IOVBOUT,'(1X,A40)') RECORD
        READ(RECORD(31:40),'(F10.3)') T
        TIME = T + TIME
        GOTO 1
 3      WRITE(IOUT,'(A20,F10.3," secs")') FSTR,TIME
        TOT = TOT + TIME
        J = J + 1
        CLOSE(IOVBOUT)
 2      GOTO 8
 7    WRITE(IOUT,'("File outs.list not found!")')
      STOP
 4    WRITE(IOUT,'("TOTAL TIME",F10.3," secs FOR",I3," FILES")') TOT,J
 11   FORMAT(1X,A40)
 12   FORMAT(A40)
      END
