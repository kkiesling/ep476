      PROGRAM tconv
      IMPLICIT NONE

c     This little program demonstrates type conversion from integer
c     to real and among reals of different kind.

      INTEGER, PARAMETER :: rknd=SELECTED_REAL_KIND(13,100)
      INTEGER :: ii
      REAL(rknd) :: pi=ACOS(-1._rknd)
      REAL(rknd), DIMENSION(0:10) :: cresult1,cresult2,cresult3

      DO ii=0,10
        cresult1(ii)=ii*(pi/10)
        cresult2(ii)=REAL(ii,rknd)*(pi/10._rknd)
        cresult3(ii)=pi*(ii/10)
      ENDDO

      cresult1=COS(cresult1)
      cresult2=COS(cresult2)
      cresult3=COS(cresult3)

      DO ii=0,10
        WRITE(*,*) "ii outside, no explicit conversion ",cresult1(ii)
        WRITE(*,*) "ii outside, with explicit conversion ",cresult2(ii)
        WRITE(*,*) "ii inside ",cresult3(ii)
      ENDDO

      END PROGRAM tconv
