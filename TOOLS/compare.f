      PROGRAM MAIN
C======================================================================
C
C     Program compare.f to compare different VBOs from different 
C     $VEC files obtained from either GAMESS *,dat files or VB2000
C     *.vec files obtained by $PLTORB or $VECONLY. A *.ovl file of 
C     overlap integrals over the basis set is also required, All 
C     the VEC files must be for the same molecule at the same 
C     geometry with the same basis set, but using different VB
C     methods.
C
C     Written by Brian J. Duke - December 2013/January 2014.
C
C======================================================================
C
      IMPLICIT NONE
      INTEGER I,J,K,L,II,I1,I2,NBASIS,NORB,MORB,NVEC,NBA2
      INTEGER LENFIL,INP,IS,IVEC,IOUT
      INTEGER IMIN,IMAX,IC,MODJ,MODIC,JJ,ICC,NSTM
      INTEGER MAXBAS,MAXVBO,MAXVEC,MAXDIA,MAXV2
      PARAMETER (MAXBAS=726,MAXVBO=100,MAXVEC=10)
      PARAMETER (MAXDIA=MAXBAS*(MAXBAS+1)/2)
      PARAMETER (MAXV2=MAXVEC*(MAXVEC-1)/2)
      INTEGER IV1(MAXV2),IV2(MAXV2),J1
      DOUBLE PRECISION S(MAXBAS,MAXBAS),C(MAXVEC,MAXBAS,MAXVBO),
     &SUM(MAXVBO,MAXVEC,MAXVEC),SS(MAXDIA)
      DOUBLE PRECISION ZERO
      CHARACTER*64 FSTR,INPFILE,SFILE,VECFILE,OUTFILE
      CHARACTER*5 LINE,VEC
      DATA ZERO/0.0D0/
      DATA VEC/" $VEC"/
C
      INP = 5
      IOUT = 6
      IS = 7
      IVEC = 8
C
C     Get filename of job.
      CALL GETARG(1,FSTR)
      DO I=1,64
      IF(FSTR(I:I).EQ.' ') THEN
         LENFIL=I-1
         GOTO 1
      ENDIF
      ENDDO
 1    CONTINUE
C
C     Open Input file
      INPFILE=FSTR(1:LENFIL)//'.inp'
      OPEN(UNIT=INP,FILE=INPFILE,STATUS='OLD',
     1  ACCESS='SEQUENTIAL',FORM='FORMATTED')
C
C     Open Overlap file
      SFILE=FSTR(1:LENFIL)//'.ovl'
      OPEN(UNIT=IS,FILE=SFILE,STATUS='OLD',
     1  ACCESS='SEQUENTIAL',FORM='FORMATTED')
C
C     Open VEC file
      VECFILE=FSTR(1:LENFIL)//'.vec'
      OPEN(UNIT=IVEC,FILE=VECFILE,STATUS='OLD',
     1  ACCESS='SEQUENTIAL',FORM='FORMATTED')
C
C     Open Output file
      OUTFILE=FSTR(1:LENFIL)//'.out'
      OPEN(UNIT=IOUT,FILE=OUTFILE,STATUS='UNKNOWN',
     1  ACCESS='SEQUENTIAL',FORM='FORMATTED')
C
      WRITE(IOUT,901)
      WRITE(IOUT,900)
      WRITE(IOUT,901)
 900  FORMAT(" COMPARE PROGRAM FOR COMPARING VBOs BY OVERLAP",//)
 901  FORMAT(" =============================================",/)
C     WRITE(IOUT,'(" INPUT FILES ARE: ",A30)') FSTR(1:LENFIL)
      WRITE(IOUT,'(" INPUT FILES ARE: ",A30)') FSTR
C
C     Read input file
C     NBASIS = Size of basis set.
C     NORB = No of VBOs in each VEC file
C     NVEC = No of VEC files to read
      READ(INP,'(4I3)') NBASIS,NORB,NVEC,MORB
      IF(MORB.EQ.0) MORB = 1
      WRITE(IOUT,'(/," SIZE OF BASIS     =",I3)') NBASIS
      WRITE(IOUT,'(  " NO. OF VEC GROUPS =",I3)') NVEC
      WRITE(IOUT,'(  " RANGE OF ORBITALS =",I3," TO",I3,//)') MORB,NORB
C
C     Read Overlap file
      NBA2 = NBASIS*(NBASIS+1)/2
      READ(IS,9101) (SS(I),I=1,NBA2)
      CLOSE(IS)
      J=0
      DO K=1,NBASIS
        DO L=1,K
          J=J+1
          S(K,L) = SS(J)
          IF(K.NE.L) S(L,K)=SS(J)
        ENDDO
      ENDDO
C     DO K=1,NBASIS
C       WRITE(IOUT,'(10F10.6)') (S(K,L),L=1,NBASIS)
C     ENDDO
C
C     Read VEC files - in one composite file
C     Taken from GAMESS(US) code
      NSTM = 0
      DO K=1,NVEC
  220   READ(IVEC,'(A5)') LINE
C       WRITE(IOUT,'(" ST ",A5)') LINE
        IF(LINE.NE.VEC) GOTO 220
        DO 280 J = 1,NORB
          IMAX = 0
          IC = 0
  240     CONTINUE
            IMIN = IMAX+1
            IMAX = IMAX+5
            IC = IC+1
            IF(IMAX .GT. NBASIS) IMAX = NBASIS
            READ(IVEC,9040,END=300,ERR=300) JJ,ICC,
     &        (C(K,I,J),I=IMIN+NSTM,IMAX+NSTM)
C           WRITE(IOUT,9041) JJ,ICC,
C    &        (C(K,I,J),I=IMIN+NSTM,IMAX+NSTM)
            MODJ  = MOD(J ,100 )
            MODIC = MOD(IC,1000)
            IF(JJ.EQ.MODJ . AND.  ICC.EQ.MODIC) GO TO 260
               WRITE(IOUT,9060) J,MODJ,IC,JJ,ICC
               STOP
  260       CONTINUE
          IF(IMAX .LT. NBASIS) GO TO 240
  280   CONTINUE
      END DO
C
      I2=NVEC-1
      J1=0
      DO I=1,I2
        I1=I+1
        DO II=I1,NVEC
          J1=J1+1
          IV1(J1)=I
          IV2(J1)=II
        ENDDO
      ENDDO
      DO J=MORB,NORB
        DO I=1,I2
          I1=I+1
          DO II=I1,NVEC
            SUM(J,I,II)=ZERO
            DO K=1,NBASIS
            DO L=1,NBASIS
              SUM(J,I,II)=SUM(J,I,II) + C(I,L,J)* C(II,K,J) * S(L,K)
            ENDDO
            ENDDO
C           WRITE(IOUT,9102) J,I,II,SUM(J,I,II)
          ENDDO
        ENDDO
      ENDDO
      WRITE(IOUT,9103) (IV1(K),IV2(K),K=1,J1)      
      DO J=MORB,NORB
        WRITE(IOUT,9104) J, (ABS(SUM(J,IV1(K),IV2(K))),K=1,J1)
      ENDDO
      STOP
  300 CONTINUE
      WRITE(IOUT,9100) J,IC
      STOP
 9040 FORMAT(I2,I3,5E15.8)
 9041 FORMAT(" LINE ",I2,I3,5E15.8)
 9060 FORMAT(1X,'*** ERROR IN READMO:   PROBLEM READING ORBITALS!'/
     *       1X,'POSSIBLY A DAMAGED OR MANGLED ORBITAL INPUT GROUP?'/
     *       1X,'ERROR OCCURED AT ORBITAL=',I6,' (MODULUS 100=',I4,'),'/
     *       1X,'         ITS LINE NUMBER=',I6/
     *       1X,'DATA READ FROM INPUT WAS ORBITAL=',I6,' LINE=',I6)
 9100 FORMAT(' *** ERROR: PREMATURE END OF ORBITAL INPUT ENCOUNTERED'/
     *       '  (READMO)  LOOKING FOR ORBITAL',I4,' LINE',I4)
 9101 FORMAT(5F12.6)
C9102 FORMAT(" VBO ",I2," VEC SETS",I2," AND",I2," OVERLAP = ",F12.6)
 9103 FORMAT(" VBOs ", 15(2X,I2," -",I2,2X))
 9104 FORMAT(2X,I2,2X,15F10.6)
      END

