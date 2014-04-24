!   This program evaluates the quadratic formula given the coefficients
!   of the polynomial: a, b, and c such that
!
!     a*x**2 + b*x + c = 0
!
!   Dialogue allows the user to enter the coefficients interactively,
!   and lower-precision math is used.
!
!   Note that this program is written in the free-format style without
!   the first six columns being reserved for comment symbols, line
!   numbers, and continuation-line markers.  All comments start with
!   exclamation points.

  PROGRAM quadform
  IMPLICIT NONE

  INTEGER, PARAMETER :: r4knd=SELECTED_REAL_KIND(5,10)
  REAL(r4knd) :: acoef,bcoef,ccoef

  WRITE(*,*) 'Enter a b and c to specify the quadratic polynomial.'
  READ(*,*) acoef,bcoef,ccoef
  WRITE(*,*) 'Roots are ',quad_root(1_4),' and ',quad_root(-1_4),'.'

! The "contains" statement follows all executable statements of the
! calling program unit and indicates that what follows are internal
! subprograms, apart from the final "end" statement.

  CONTAINS

! The internal function quad_root finds one of the two solutions of
! the quadratic formula.  The integer input isgn indicates which root
! is desired.

    FUNCTION quad_root(isgn)  RESULT(root)
    IMPLICIT NONE

    COMPLEX(r4knd) :: root
    INTEGER(4), INTENT(IN) :: isgn

    REAL(r4knd) :: disc	

!  Note that acoef, bcoef, and ccoef do not need to be passed,
!  because this subprogram is internal.

    IF (acoef==0._r4knd) THEN
      WRITE(*,*) '   The a-coefficient must be nonzero.'
      STOP
    ENDIF

    disc = bcoef**2 - &           !  The ampersand at the end of a line
           4._r4knd*acoef*ccoef   !  indicates that the next line is
                                  !  a continuation.

    IF (disc>=0._r4knd) THEN
      root=(-bcoef+isgn*SQRT(disc))/(2._r4knd*acoef)
    ELSE
      root=(-bcoef+isgn*(0,1)*SQRT(-disc))/(2._r4knd*acoef)
    ENDIF

    END FUNCTION quad_root

  END PROGRAM quadform

