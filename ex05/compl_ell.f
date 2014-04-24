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



c-----------------------------------------------------------------------
c     This function computes the complete elliptic integral of the
c     second kind multiplied by 2/pi,
c
c     ele=(2/pi)*int[0,pi/2]{ sqrt( 1 - k2*sin**2(theta) )*dtheta }
c
c     using a power series expansion.  [Adapted from Numerical 
c     Recipies, W. Press, et al.]
c
c     Passed parameters are:
c       k2 [real] {input} = the square of the parameter in the integral
c       tol [real] {input} = tolerance for convergence of the series 
c       maxn [integer] {input} = maximum number of terms in the series
c
c     The function returns a real scalar result, computed here as ele.
c-----------------------------------------------------------------------
      FUNCTION ele_bar(k2,tol,maxn) RESULT(ele)
      IMPLICIT NONE

      REAL(KIND=SELECTED_REAL_KIND(13,300)) :: ele
      REAL(KIND=SELECTED_REAL_KIND(13,300)), INTENT(IN) :: k2,tol
      INTEGER(KIND=SELECTED_INT_KIND(6)), INTENT(IN) :: maxn

      INTEGER :: ii,jj,kk
      INTEGER, PARAMETER :: r8=SELECTED_REAL_KIND(13,300)
      REAL(KIND=SELECTED_REAL_KIND(13,300)) :: new

      ele=1._r8
      new=1._r8
      ii=1
      jj=0

      DO
        jj=jj+2
        new=new*k2*(REAL(jj-1,r8)/jj)**2
        ele=ele-new/(jj-1)
        IF (ABS(new/(jj-1))/ABS(ele)<=tol) EXIT
        ii=ii+1
        IF (ii>maxn) THEN
          WRITE(*,'(a,i4,a)') 'Ele_bar not converging after ',
     &                        maxn,' steps.'
          STOP
        ENDIF
      ENDDO

      END FUNCTION ele_bar

c-----------------------------------------------------------------------
c     This subroutine computes the complete elliptic integral of the
c     first kind multiplied by 2/pi,
c
c     elk=(2/pi)*int[0,pi/2]{ dtheta/sqrt( 1 - k2*sin**2(theta) ) }
c
c     using a power series expansion.  [Adapted from Numerical 
c     Recipies, W. Press, et al.]
c
c     Passed parameters are:
c       elk [real] {output} = the result of the series computation
c       k2 [real] {input} = the square of the parameter in the integral
c       tol [real] {input} = tolerance for convergence of the series 
c       maxn [integer] {input} = maximum number of terms in the series
c-----------------------------------------------------------------------
      SUBROUTINE elk_bar(elk,k2,tol,maxn)
      IMPLICIT NONE

      REAL(KIND=SELECTED_REAL_KIND(13,300)), INTENT(OUT) :: elk
      REAL(KIND=SELECTED_REAL_KIND(13,300)), INTENT(IN) :: k2,tol
      INTEGER(KIND=SELECTED_INT_KIND(6)), INTENT(IN) :: maxn

      INTEGER :: ii,jj,kk
      INTEGER, PARAMETER :: r8=SELECTED_REAL_KIND(13,300)
      REAL(KIND=SELECTED_REAL_KIND(13,300)) :: new

      elk=1._r8
      new=1._r8
      ii=1
      jj=0

      DO
        jj=jj+2
        new=new*k2*(REAL(jj-1,r8)/jj)**2
        elk=elk+new
        IF (ABS(new)/ABS(elk)<=tol) EXIT
        ii=ii+1
        IF (ii>maxn) THEN
          WRITE(*,'(a,i4,a)') 'Elk_bar not converging after ',
     &                        maxn,' steps.'
          STOP
        ENDIF
      ENDDO

      END SUBROUTINE elk_bar

