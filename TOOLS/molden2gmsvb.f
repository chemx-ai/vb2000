      PROGRAM MOL2GMS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     This program is one of many programs and scripts that creates a
C     GAMESS input file from a Molden file, this allowing easy transfer
C     from other QC programs to GAMESS.
C
C     The key difference of this program is that it generates a GAMESS
C     input file for running VB2000 in GAMESS. This file also contains
C     the HF eigenvalues as well as the eigenvectors.
C
C     Usage:
C       Compile: gfortran -o molden2gmsvb.x molden2gmsvb.f
C       Usage  = molden2gmsvb.x file
C         will use: file.molden  - Molden file
C         and:      file inp     - Gamess output file
C       Unit 6  = Normal output file
C       Unit 9  = Molden file
C       Unit 10 = Gamess output file
C
C       You need to edit the $CONTRL block in the created file. UNITS
C       will be created correctly from the Molden file, but you should
C       ensure that SCFTYP, ISPHER, MULT and ICHARG are appropriate for
C       the input file you would have created for your VB2000 run, if
C       you were not getting the data including MOs from a Molden file.
C
C       $GUESS guess=SKIP $END ensures that the initial guess for HF is 
C       skipped and not read from the *VEC group. To also skip 
C       calculating the HF, you need $SKIPGMSF in the VB2000 data.
C
C       You need to construct almost all of the VB2000 group that is
C       in the new inp file. The HF eigenvalues are in a new directive
C       group:-
C                 $EIGENVALUES
C                 N                     - no of eigenvalies
C                 Eigenvalues           - 5 per line FORMAT 5F16.8
C                 $END 
C       in the $VB2000 group. The eigenvalues are in a standard $VEC
C       group
C
C       Author: Brian J. Duke, May 2016.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INTEGER LENLINE,LENFIL
      INTEGER I,J,LEN
      LOGICAL BOHR
      CHARACTER*15 HEADER
      CHARACTER*64 MOLDENDAT,GMSOUT,FSTR
      CHARACTER*80 BUFF,TITLE,TITLEGMS
C
C     Variables for storing geometry
      DOUBLE PRECISION XYZ(3,100),ATMCHG(100)
      INTEGER NATNUM(100),NATOM,NELECTRON,N
      CHARACTER*2 ATMS,ATMSYM(92)
      CHARACTER*4 UNIT
C
C     Variables for basis set
      INTEGER II,NPR
      CHARACTER*1 LETT
      CHARACTER*4 LINE
      DOUBLE PRECISION C,E
C
C     Variables for MOs
      INTEGER NSUB,NSPIN,NIN,NEQ,NCCC,NEIG,NELE,K1,K,IEND,NBASIS
     &  ,LPOS,LTEST,MAX,MIN,MODJ,ID
      INTEGER MAXBAS,MAXBAS2
      PARAMETER (MAXBAS=726,MAXBAS2=MAXBAS*MAXBAS)
C     726 is maximum basis set in VB2000
      LOGICAL LGOT
      DOUBLE PRECISION X,EIG(MAXBAS),W(MAXBAS2)
C
      DATA ATMSYM/' H','He','Li','Be',' B',' C',' N',' O',' F','Ne',
     1            'Na','Mg','Al','Si',' P',' S','Cl','Ar',
     2            ' K','Ca','Sc','Ti',' V','Cr','Mn','Fe','Co',
     3            'Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr','Rb',
     4 'Sr',' Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In',
     5 'Sn','Sb','Te',' I','Xe','Cs','Ba','La','Ce','Pr','Nd','Pm',
     6 'Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta',
     7 ' W','Re','Os','Ir','Pt','Au','Hg','Tl','Pb','Bi','Po','At',
     8 'Rn','Fr','Ra','Ac','Th','Pa',' U'/
C
C     Initiate
      BOHR=.FALSE.
      DO I=25,80
        TITLE(I:I)=' '
      ENDDO
      TITLE(1:24)="molden2gamess conversion"
      WRITE(6,'(" ------------------------------------------------",/)')
C
C     Get filename of job and open files
      CALL GETARG(1,FSTR)
      DO I=1,64
        IF(FSTR(I:I).EQ.' ') THEN
          LENFIL=I-1
          GOTO 30
        ENDIF
      ENDDO
 30   CONTINUE
C
C     Code below allows one to test a lot of molden files by
C     running a script like:-
C     #!/bin/csh 
C     rm runall.log
C     foreach file ( *.molden)
C     molden2gmsvb.x $file >>& runall.log
C     end
C     The "*.molden" from the argument is striped off below. 
C
      IF (FSTR(LENFIL-5:LENFIL).EQ."molden") THEN
        LENFIL=LENFIL-7
        CALL FLUSH(6)
      ENDIF
C
 29   MOLDENDAT=FSTR(1:LENFIL)//'.molden'
      OPEN(UNIT=9,FILE=MOLDENDAT,STATUS='OLD')
      GMSOUT=FSTR(1:LENFIL)//'.inp'
      OPEN(UNIT=10,FILE=GMSOUT,STATUS='UNKNOWN')
      WRITE(6,'(" MOLDEN FILE: ",A64)') MOLDENDAT
      WRITE(6,'(" GAMESS FILE: ",A64)') GMSOUT
      CALL FLUSH(6)
C
C     Check whether molden file is [5D]
 1    READ(9,'(A80)',ERR=108, END=112) BUFF
      IF(BUFF(1:4).EQ."[5D]".OR.BUFF(2:5).EQ."[5D]") THEN
        WRITE(6,'(" READMOLDEN CAN NOT BE USED WITH A"
     &   " SPHERICAL HARMONIC MOLDEN FILE")')
        WRITE(6,
     &   '(" USE molden2aim.f TO CONVERT FILE FROM 5D TO 6D.")')
        CALL ABORT1("MOLDENER04")
      ENDIF
      GOTO 1
C
C     Check Header
 112  REWIND 9
      READ(9,'(A80)',ERR=102, END=102) BUFF
      READ(BUFF(1:15),'(A15)') HEADER
      CALL CONVCASE(HEADER,15,1)
      IF(HEADER.NE."[MOLDEN FORMAT]") THEN
        READ(BUFF(2:16),'(A15)') HEADER
        CALL CONVCASE(HEADER,15,1)
      END IF
      IF(HEADER.NE."[MOLDEN FORMAT]") GOTO 106
C
C     Is there a Title block? This is non-standard, but useful.
 2    READ(9,'(A80)',ERR=108, END=36) BUFF
      CALL FLUSH(6)
      CALL CONVCASE(BUFF,80,1)
      IF(BUFF(2:6).eq."TITLE".OR.BUFF(3:7).eq."TITLE") THEN
        WRITE(6,'(/," Title:")')
        READ(9,'(A80)',ERR=108, END=108) TITLE
        TITLEGMS = TITLE
        IF(TITLE(78:80).NE."   ") WRITE(6,1002)
        WRITE(6,'(A80)') TITLE
 35     READ(9,'(A80)',ERR=108, END=108) TITLE
        BUFF = TITLE
        LEN = LENLINE(BUFF)
        CALL CONVCASE(BUFF,80,1)
        DO I=1,LEN-4
          IF (BUFF(I:I+4).EQ."ATOMS") GOTO 33
        ENDDO
        WRITE(6,'(A80)') TITLE
C       CALL FLUSH(6)
        GOTO 35
      ELSE
        GOTO 2
      ENDIF
C
C     Search for [Atoms ..
 36   WRITE(6,'(/)')
 33   REWIND 9
 3    READ(9,'(A80)',ERR=108, END=108) BUFF
      CALL CONVCASE(BUFF,80,1)
      LEN = LENLINE(BUFF)
      DO I=1,LEN-5
C     Check for [ATOMS not ATOMS due to unofficial [N_ATOMS] block
        IF (BUFF(I:I+5).EQ."[ATOMS") THEN
          J = I
          GOTO 4
        ENDIF
      ENDDO
      GOTO 3
 4    DO I=J+5,LEN-1
        IF(BUFF(I:I+1).EQ."AU") THEN
          BOHR=.TRUE.
          GOTO 5
        ENDIF
        IF(BUFF(I:I+1).EQ."AN") GOTO 5
      ENDDO
C
C     Get Geometry
C
 5    NATOM=0
      NELECTRON=0
      IF(BOHR) THEN
        WRITE(6, '(" Coordinates are in BOHR",/)')
      ELSE
        WRITE(6, '(" Coordinates are in Angstom",/)')
      ENDIF
 6    READ(9,'(A80)',ERR=105, END=105) BUFF
      IF (BUFF(1:1).EQ."[".OR.BUFF(2:2).EQ."[") GOTO 7
      NATOM = NATOM + 1
      READ(BUFF(4:80),*) N,NATNUM(NATOM)
     &     ,(XYZ(J,NATOM),J=1,3)
      I = NATOM
      ATMCHG(I)=NATNUM(I)
      WRITE(6,1000) ATMSYM(NATNUM(I)),ATMCHG(I),(XYZ(J,I),J=1,3)
      NELECTRON = NELECTRON + NATNUM(NATOM)
      GOTO 6
C
 7    WRITE(6,
     & '(/," GEOMETRY, BASIS AND HF ORBITALS READ FROM MOLDEN FILE",/)')
C
C     We now have enough to output top of GAMESS input file
      IF(BOHR) THEN
        UNIT='BOHR'
      ELSE
        UNIT='ANGS'
      ENDIF
      WRITE(10,'(" $CONTRL ISPHER=XX SCFTYP=XX MULT=XX")')
      WRITE(10,'("  UNITS=",A4," COORD=UNIQUE VBTYP=VB2000 $END")') UNIT
      WRITE(10,'(" $GUESS guess=SKIP $END")')
      WRITE(10,'(" $DATA")')
        WRITE(10,'(A80)') TITLEGMS
	WRITE(10,'("C1")')
C       CALL FLUSH(10)
C
C     Check for Molden basis set.
C
 34   IF (BUFF(2:4).EQ."GTO") GOTO 8
      IF (BUFF(3:5).EQ."GTO") GOTO 8
      READ(9,'(A80)',ERR=102, END=102) BUFF
      GOTO 34
 8    DO I=1,NATOM
        WRITE(10,1000) ATMSYM(NATNUM(I)),ATMCHG(I),(XYZ(J,I),J=1,3)
        READ(9,'(A80)',ERR=102,END=102) BUFF
        LEN = LENLINE(BUFF)
        READ(BUFF(1:LEN),*) II
C       II should be atom number
C       Now read  line like "s  6  1.0"
 9      READ(9,'(A80)',ERR=102,END=102) BUFF
        LEN = LENLINE(BUFF)
        IF (LEN.EQ.0) GOTO 11
        J = 0
 10     J = J + 1
        READ(BUFF(J:J),'(A1)',END=101) LETT
        IF(LETT.EQ." ") GOTO 10 
        CALL CONVCASE(LETT,1,1)
C       WRITE(6,'(" LETT = ",A1)') LETT
C       LETT " " - end of atom basis
        IF(LETT.NE."S".AND.LETT.NE."P".AND.LETT.NE."D") GOTO 106
        NPR = 0
        READ(BUFF(J+1:LEN),*) NPR
        IF(NPR.EQ.0) CALL ABORT1("NO NPR    ")
C       WRITE(6,'(A1,8X,I2)') LETT,NPR
        WRITE(10,'(A1,8X,I2)') LETT,NPR
        DO II=1,NPR
          READ(9,'(A80)',ERR=102, END=102) BUFF
C         WRITE(6,'("BUFF3 ",A80)') BUFF
          LEN = LENLINE(BUFF)
          READ(BUFF(1:LEN),*) C,E
C         WRITE(6,'(I5,F15.7,F15.10)') II,C,E
          WRITE(10,'(I5,F15.7,F15.10)') II,C,E
        ENDDO
        GOTO 9 
 11     WRITE(10,'("")')
      ENDDO
C
C     Now complete output of GAMESS $DATA block
      WRITE(10,'(" $END")')
C
C     Output rough VB2000 block
      WRITE(10,'(" $VB2000")')
      WRITE(10,'("#! VB  PRINTALL DIIS")')
      WRITE(10,'("  ")')
      WRITE(10,'(" $SKIPGMSHF")')
      WRITE(10,'("  ")')
      WRITE(10,'(" $END")')
C     Create and output $VEC block
      NEQ = 0
      J = 1
 28   READ(9,'(A80)') BUFF
      LEN = LENLINE(BUFF)
      IF(BUFF(1:4).EQ."[MO]") GOTO 12
      IF(LEN.GT.4.AND.BUFF(2:5).EQ."[MO]") GOTO 12
      GOTO 28 
 12   READ(9,'(A80)') BUFF
      LEN = LENLINE(BUFF)
      CALL LFIND(BUFF,LEN,"Sym=",4,LGOT,LPOS)
      IF (LGOT) THEN
        NEQ = NEQ + 1
        GOTO 12
      ENDIF
      CALL LFIND(BUFF,LEN,"Ene=",4,LGOT,LPOS)
      IF (LGOT) THEN
        I = LPOS+1
        READ(BUFF(I:LEN),*) EIG(J)
        NEQ = NEQ + 1
        NEIG = NEQ
        GOTO 12
      ENDIF
      CALL LFIND(BUFF,LEN,"Spin=",5,LGOT,LPOS)
      IF (LGOT) THEN
        NEQ = NEQ + 1
        NIN = NEQ
        GOTO 12
      ENDIF
      CALL LFIND(BUFF,LEN,"Occup=",6,LGOT,LPOS)
      IF (LGOT) THEN
        NEQ = NEQ + 1
        GOTO 12
      ENDIF
      GOTO 24
 25   READ(9,'(A80)') BUFF
      LEN = LENLINE(BUFF)
 24   DO II= 2,LEN
        IF(BUFF(II:II).EQ."=") GOTO 22
      ENDDO
      READ(BUFF(1:LEN),*,ERR=102) I, X
      W(I) = X
      GOTO 25 
 22   NBASIS = I
C     Got size of basis from 1st block, now use it.
      IEND = 1
      DO J=2,NBASIS
        DO II= 1,NEQ
          IF (II.EQ.1.AND.J.EQ.2) GOTO 27 
          IF (II.NE.NEIG) READ(9,'(A80)',END=26) BUFF
          IF(II.EQ.NEIG) THEN
            READ(9,*,END=26) LINE,EIG(J)
          ENDIF
          IF(II.EQ.NIN) THEN
            LEN = LENLINE(BUFF)
            CALL CONVCASE(BUFF,80,1)
C             SKIP OUT IF NALPHA < NBASIS IN UHF CASE
            DO K1=4,LEN-3
              IF(BUFF(K1:K1+3).EQ."BETA") GOTO 26
            ENDDO
          ENDIF
 27       CONTINUE
        ENDDO
        DO K=1,NBASIS
          READ(9,*,END=26) K1,X
C         WRITE(6,'(I5,F20.6)') K1,X
C     CALL FLUSH(6)
          I = I + 1
          W(I) = X
        ENDDO
        IEND = IEND + 1
      ENDDO
 26   IF(IEND.LT.NBASIS) THEN
C       FILL IN WITH ZEROS TO NBASIS. THIS MAY NEED TO BE REMOVED.
        DO K=IEND+1,NBASIS
          EIG(K)=0.0D0
          DO J=1,NBASIS
            I = I + 1
            W(I) = 0.0D0
          ENDDO
        ENDDO
      ENDIF
      WRITE(10,'("  ")')
      WRITE(10,'(" $EIGENVALUE")')
      WRITE(10,'(I3)') NBASIS
      WRITE(10,1003) (EIG(J),J=1,NBASIS)
      WRITE(10,'(" $END")')
      WRITE(10,'(" $VEC")')
      NCCC=NBASIS
      DO J = 1,NCCC
        ID = 0
        MAX = 0
 23     MIN = MAX+1
        MAX = MAX+5
        ID = ID+1
        IF (MAX .GT. NBASIS) MAX = NBASIS
        MODJ=MOD(J,100)
        WRITE (10,1001) MODJ,ID,(W(I+(J-1)*NBASIS),I = MIN,MAX)
        IF (MAX .LT. NBASIS) GO TO 23
      ENDDO
      WRITE(10,'(" $END")')
      WRITE(6,'(" GAMESS INPUT FILE WRITTEN.",/)')
      STOP
C
C     Error exits
 108  WRITE(6,'(A80)') BUFF
      CALL ABORT1("MOLDENER01")
 102  WRITE(6,*)
     &  "WRONG FORMAT IN MOLDEN FILE. PLEASE CHECK THIS LINE"
      WRITE(6,'(A80)') BUFF
      CALL ABORT1("MOLDENER02")
 105  WRITE(6,'(" GTOs NOT FOUND IN MOLDEN FILE")')
      CALL ABORT1("MOLDENER03")
 101  WRITE(6,'(" END OF FILE")')
      CALL ABORT1("MOLDENER04")
 106  WRITE(6,'(" READMOLDEN FILE CORRUPTED")')
      STOP
 1000 FORMAT(1X,A2,3X,F5.1,3F16.8)
 1001 FORMAT(I2,I3,1P,5E15.8)
 1002 FORMAT(" IF THIS RUNS FAILS SHORTEN TITLE LINE IN MOLDEN FILE",/
     & " OR SPLIT IT TO LESS THAN 78 CHARACTERS PER LINE.",/)
 1003 FORMAT(5E16.8)
      END
      SUBROUTINE ABORT1(MESSAGE)
C
C     Taken fron VB2000 and simplified for here
C
      CHARACTER*10 MESSAGE
      WRITE(6,'(" PROGRAM STOPPED AT ",A10, 
     &  ". PLEASE CHECK YOUR INPUT!!",/)') MESSAGE(1:10)
      STOP
      END
      SUBROUTINE CONVCASE(STRING,LEN,ICASE)
C
C     Taken fron VB2000
C
      IMPLICIT NONE
      INTEGER ICASE,LEN,I,IC
      CHARACTER*(*) STRING
      CHARACTER*26 UCASE,LCASE
      DATA UCASE /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LCASE /'abcdefghijklmnopqrstuvwxyz'/
C
C     ICASE = 1 lower --> upper
C     ICASE = 2 upper --> lower
      SELECT CASE (ICASE)
      CASE(1)
      DO I=1,LEN
         IC = INDEX(LCASE,STRING(I:I))
         IF (IC.GT.0) STRING(I:I) = UCASE(IC:IC)
      END DO
      CASE(2)
      DO I=1,LEN
         IC = INDEX(UCASE,STRING(I:I))
         IF (IC.GT.0) STRING(I:I) = LCASE(IC:IC)
      END DO
      END SELECT
      RETURN
      END
      INTEGER FUNCTION LENLINE(CHARS)
C
C     Taken fron VB2000
C
      IMPLICIT NONE
      CHARACTER*(*) CHARS
      INTEGER J,K
      DO J=LEN(CHARS),1,-1
        K = ICHAR(CHARS(J:J))
        IF (K.GT.32.AND.K.LE.126) GOTO 1
      ENDDO
      J = 0
 1    LENLINE = J
      RETURN
      END
      SUBROUTINE LFIND(BUFF,LEN1,GET,LEN2,L,IPOS)
      IMPLICIT NONE
      LOGICAL L
      CHARACTER*(*) BUFF,GET
      INTEGER LEN1,LEN2,I,J,K,IPOS
      L=.FALSE.
      K = LEN1 - LEN2 + 1
      DO I=1,K
        J = I+LEN2-1
        IF(BUFF(I:J).EQ.GET(1:LEN2)) THEN
          L=.TRUE.
          IPOS=J
          GOTO 1
        END IF
      END DO
 1    RETURN
      END

