CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     A simple program for converting basis set format (Gaussian to Hondo)
C
C     Jiabo Li, May 20,2000
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      Character*80 A, bfsym
      dimension expx(100),cx(100),dx(100)
  10  nbf=0
  20  read(*,*,END=200)A
      nbf=nbf+1
      write(*,'(A4,I2)')A,0
  30  read(*,*)A
C
C     TEST
C   
      if(A(1:4).EQ."****") THEN
      WRITE(*,'(A4)')A
      GOTO 10
      ELSE
      BACKSPACE(5)
      END IF
      read(*,*)A,ngf,b
C     write(*,*)"XXXXX",A,ngf,b
      IF(A(1:2).eq.'SP'.OR.A(2:3).eq.'SP') THEN
      write(*,'(I6,5X,A5,I6)')nbf,'S',ngf
        DO i=1,ngf
        read(*,*)exp,c
        write(*,100)I,exp,c
        end do
        DO i=1,ngf
        Backspace(5)
        end do
      write(*,'(I6,5X,A5,I6)')nbf,'P',ngf
        DO i=1,ngf
        read(*,*)exp,c,c
        write(*,100)I,exp,c
        end do
      Else
      write(*,'(I6,5X,A5,I6)')nbf,A,ngf
        DO i=1,ngf
        read(*,*)exp,c
        write(*,100)I,exp,c
        end do
      END IF
      GOTO 30
 100  FORMAT(I5,F15.7,F15.10)
 200  STOP
      END  
