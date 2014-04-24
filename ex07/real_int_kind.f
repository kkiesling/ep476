c-----------------------------------------------------------------------
c     This program defines variables of various kind values and
c     prints the same rational number at the different levels of
c     precision for the reals and the largest number possible for
c     the integers.
c
c     We should note that what results for specific kind values is
c     implementation-dependent.
c-----------------------------------------------------------------------

      PROGRAM real_int_kind
      IMPLICIT NONE

      REAL(KIND=4)  :: r4
      REAL(KIND=8)  :: r8
      REAL(KIND=10)  :: r10
      REAL(KIND=16)  :: r16
      INTEGER(KIND=2)  :: i2
      INTEGER(4)  :: i4               !   writing "KIND=" is optional
      INTEGER(8)  :: i8               !   in all of these.

c-----------------------------------------------------------------------
c     The Fortran 90 intrinsic function SELECTED_INT_KIND(R) returns the
c     kind-value for the smallest representation that holds integers
c     ranging from -10**R to +10**R
c
c     What results here is implementation-independent.
c-----------------------------------------------------------------------

      INTEGER(KIND=SELECTED_INT_KIND(1))  :: iv1
      INTEGER(KIND=SELECTED_INT_KIND(2))  :: iv2
      INTEGER(KIND=SELECTED_INT_KIND(3))  :: iv3
      INTEGER(KIND=SELECTED_INT_KIND(4))  :: iv4
      INTEGER(KIND=SELECTED_INT_KIND(5))  :: iv5
      INTEGER(KIND=SELECTED_INT_KIND(8))  :: imedium
      INTEGER(KIND=SELECTED_INT_KIND(18))  :: ilarge

c-----------------------------------------------------------------------
c     The Fortran 90 intrinsic function SELECTED_REAL_KIND(P,R) returns
c     the kind-value for the smallest representation that holds real
c     of at least P digits and an exponent range to R.
c
c     What results here is implementation-independent.
c-----------------------------------------------------------------------

      REAL(KIND=SELECTED_REAL_KIND(6,37))  :: rsmall
      REAL(KIND=SELECTED_REAL_KIND(13,307))  :: rmedium

c-----------------------------------------------------------------------
c     In the following, Fortran 90's intrinsic function HUGE is used.
c     It returns the largest number that may be represented by the
c     type and kind of variable that is passed into it.
c-----------------------------------------------------------------------

      i2=HUGE(i2)
      i4=HUGE(i4)
      i8=HUGE(i8)

      WRITE(*,*)
      WRITE(*,*) "largest kind=2 integer ",i2
      WRITE(*,*) "largest kind=4 integer ",i4
      WRITE(*,*) "largest kind=8 integer ",i8
      WRITE(*,*)
      WRITE(*,*) "largest integer covering 10**1 ",HUGE(iv1)
      WRITE(*,*) "largest integer covering 10**2 ",HUGE(iv2)
      WRITE(*,*) "largest integer covering 10**3 ",HUGE(iv3)
      WRITE(*,*) "largest integer covering 10**4 ",HUGE(iv4)
      WRITE(*,*) "largest integer covering 10**5 ",HUGE(iv5)
      WRITE(*,*) "largest integer covering 10**8 ",HUGE(imedium)
      WRITE(*,*) "largest integer covering 10**18 ",HUGE(ilarge)
      WRITE(*,*)

c-----------------------------------------------------------------------
c     In the following, Fortran 90's automatic type conversion is used
c     when assigning these values.
c-----------------------------------------------------------------------

      r4 =1._8/3._8
      r8 =1._8/3._8
      r10 =1._10/3._10
      r16 =1._16/3._16

      WRITE(*,*) "kind=4  version of 1/3 ",r4
      WRITE(*,*) "kind=8  version of 1/3 ",r8
      WRITE(*,*) "kind=10  version of 1/3 ",r10
      WRITE(*,*) "kind=16  version of 1/3 ",r16
      WRITE(*,*)
      WRITE(*,*) "largest real with P=6 ",HUGE(rsmall)
      WRITE(*,*) "largest real with P=13 ",HUGE(rmedium)
      WRITE(*,*)

      END PROGRAM real_int_kind
