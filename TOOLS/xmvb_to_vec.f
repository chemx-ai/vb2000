      PROGRAM XMVBTOVEC
C======================================================================
C
C     xmvb_to_vec.f is a small Fortran program that creates a GAMESS(US)
C     $VEC file of valence bond orbitals from a XMVB *.orb file. This
C     has several uses:-
C
C     1) Along with the GAMESS *.log file, this *.vec file has be used
C        by MacMolPlt to display the orbitals.
C     2) The *.vec file can be used by VB2000 as initial guess orbitals.
C
C     The program reads the *.orb and *.xmo files from a XMVB run, and
C     creates a *.vec file.
C
C     Author: Brian Duke, July/August 2014.
C
C======================================================================
      IMPLICIT NONE
      INTEGER I,J,K,L,I1,I2,NORB,NB,ID,MAX,MIN,MODJ
      INTEGER IN,IP,IXMO,IOUT,LENFIL
      INTEGER IORB(200),ORB(200)
      DOUBLE PRECISION X(200),VEC(200)
      CHARACTER*64 FSTR,ORBFILE,VECFILE,XMOFILE
      CHARACTER*80 XMOLINE
      IOUT=6
      IN=5
      IP=7
      IXMO=8
C     Get filename of job.
      CALL GETARG(1,FSTR)
      DO I=1,64
      IF(FSTR(I:I).EQ.' ') THEN
         LENFIL=I-1
         GOTO 1
      ENDIF
      ENDDO
 1    ORBFILE=FSTR(1:LENFIL)//'.orb'
      OPEN(UNIT=IN,FILE=ORBFILE,STATUS='OLD',
     1  ACCESS='SEQUENTIAL',FORM='FORMATTED')
C
      VECFILE=FSTR(1:LENFIL)//'.vec'
      OPEN(UNIT=IP,FILE=VECFILE,STATUS='UNKNOWN',
     1  ACCESS='SEQUENTIAL',FORM='FORMATTED')
C
C     Need to get NB = No. of basis functions
C     Open *.xmo file
      XMOFILE=FSTR(1:LENFIL)//'.xmo'
      OPEN(UNIT=IXMO,FILE=XMOFILE,STATUS='UNKNOWN',
     1  ACCESS='SEQUENTIAL',FORM='FORMATTED')
C
 3    READ(IXMO,'(A80)') XMOLINE
      IF(XMOLINE(12:35).NE."CARTESIAN GAUSSIAN BASIS") GOTO 3
      READ(XMOLINE(49:52),'(I4)') NB
C
      I1=1
C     The loops below are in case the first line, format 25(1X,I3)
C     extends over more than one line
 102  I2=I1+24 
      IF(I2.GT.200) THEN
        WRITE(IOUT,'("TOO MANY VBOs")')
        STOP
      ENDIF
      READ(IN,'(25(1X,I3))') (IORB(I),I=I1,I2)
      DO I=I1,I2
        IF(IORB(I).EQ.0) THEN
          NORB=I-1
C         Finding the first zero tels us how many VBOs we have.
C         I do not see any other way of finding it.
          GOTO 100
        ENDIF
      END DO
      I1=I1+25
      GOTO 102
 100  CONTINUE
C     WRITE(IOUT,'(" No of orbitals found:",I3)') NORB
      WRITE(IP,'("VB orbitals from XMVB ",A64)') ORBFILE
      WRITE(IP,'(" $VEC")')
      DO I=1,NORB
        READ(IN,'(15X,I3,10X,I3)') K,L
C       WRITE(IOUT,'(15X,I3,10X,I3)') K,L
        IF(K.NE.I) GOTO 200
        IF(L.NE.IORB(I)) GOTO 300
        READ(IN,900) (X(J),ORB(J),J=1,L)
        DO J=1,100
          VEC(J)=0.0D0
        ENDDO
        DO J=1,L
          VEC(ORB(J))=X(J)
        ENDDO
        ID = 0
        MAX = 0
 101    MIN = MAX+1
        MAX = MAX+5
        ID = ID+1
        IF (MAX .GT. NB) MAX = NB
        MODJ=MOD(I,100)
        WRITE (IP,901) MODJ,ID,(VEC(J),J = MIN,MAX)
        IF (MAX .LT. NB) GO TO 101
      ENDDO
      WRITE(IP,'(" $END")')
      STOP
 200  WRITE(IOUT,'(" ORBITAL ERROR FOR ORBITAL   ",2I3)') I,K
      STOP
 300  WRITE(IOUT,'(" NAO ERROR FOR ORBITAL & NAO ",3I3)') I,L,IORB(I)
      STOP
 900  FORMAT(4(F13.10,I4,2X))
 901  FORMAT(I2,I3,1P,5E15.8)
      END

