c     This F90 program demonstrates the scope of local variables
c     with respect to an external subroutine and a main program that
c     calls the subroutine.

      PROGRAM settwo
      IMPLICIT NONE

      INTEGER, PARAMETER :: rknd=SELECTED_REAL_KIND(12,50)
      REAL(rknd) :: two

      CALL subtwo(two)
      WRITE(*,*) two

      END PROGRAM settwo

c     This subroutine returns a real representation of 2 in
c     its only passed parameter.

      SUBROUTINE subtwo(twodum)
      IMPLICIT NONE

      INTEGER, PARAMETER :: rknd=SELECTED_REAL_KIND(12,50)
      REAL(rknd), INTENT(OUT) :: twodum  !  'Dummy' variable allows
				 	 !  passing data.

      REAL(rknd) :: two   !   This local variable is not associated
                          !   with the storage of 'two' in the
                          !   calling program.

      two=2._rknd+2._rknd     !   Does not affect two in main program.
      twodum=two/2._rknd      !   This affects storage for main.

      END SUBROUTINE subtwo

