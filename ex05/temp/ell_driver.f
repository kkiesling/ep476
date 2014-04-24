c-----------------------------------------------------------------------
c     This program finds and writes values of complete elliptic
c     integrals for interactively supplied input.
c-----------------------------------------------------------------------

      PROGRAM ell_driver
      IMPLICIT NONE

      REAL(KIND=SELECTED_REAL_KIND(13,300)) :: kk,ksq,tolerance,ele,elk
      REAL(KIND=SELECTED_REAL_KIND(13,300)), EXTERNAL :: ele_bar

      INTEGER, PARAMETER :: r8=SELECTED_REAL_KIND(13,300)
      INTEGER(KIND=SELECTED_INT_KIND(6)), PARAMETER :: maxits=1000

      CHARACTER(128) ::
     &  statement='The value of the elliptic integral of the '

c-----------------------------------------------------------------------
c     Declarations (as above) must be completed before any executable
c     statements (below).
c-----------------------------------------------------------------------

      WRITE(*,*) ' Enter k (-1<k<+1) and the requested tolerance.'
      READ(*,*) kk,tolerance

c-----------------------------------------------------------------------
c     The elliptic integral of the first kind is coded in a subroutine,
c     which requires a 'call' to the subroutine.
c
c     The elliptic integral of the second kind is coded as a function,
c     which gets used directly, like a variable.
c-----------------------------------------------------------------------

      CALL elk_bar(elk,kk**2,tolerance,maxits)
      ele=ele_bar(kk**2,tolerance,maxits)

      elk=0.5_r8*elk*ACOS(-1._r8)
      ele=0.5_r8*ele*ACOS(-1._r8)

      WRITE(*,'(/2a,es15.7)') TRIM(statement),' first kind is  ',elk
      WRITE(*,'(2a,es15.7/)') TRIM(statement),' second kind is ',ele

      END PROGRAM ell_driver



