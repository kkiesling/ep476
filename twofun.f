c     This F90 program demonstrates the scope of local variables
c     with respect to an external function and a main program that
c     calls the function.

      PROGRAM settwo
      IMPLICIT NONE

      INTEGER, PARAMETER :: rknd=SELECTED_REAL_KIND(12,50)
      REAL(rknd) :: two
      REAL(rknd), EXTERNAL :: twofun

      two=twofun()
      WRITE(*,*) two

      END PROGRAM settwo

c     This function generates a real representation of 2.

      FUNCTION twofun() RESULT(twodum)
      IMPLICIT NONE

      INTEGER, PARAMETER :: rknd=SELECTED_REAL_KIND(12,50)
      REAL(rknd) :: twodum

      REAL(rknd) :: two  !   This local variable is not available
			 !   outside this function.

      two=200._rknd
      twodum=2._rknd

      END FUNCTION twofun

