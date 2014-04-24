c-----------------------------------------------------------------------
c     This function computes the complete elliptic integral of the
c     first kind multiplied by 2/pi,
c
c     elk=(2/pi)*int[0,pi/2]{ dtheta/sqrt( 1 - k2*sin**2(theta) ) }
c
c     using a power series expansion.  [Adapted from Numerical 
c     Recipies, W. Press, et al.]
c
c     Passed parameters are:
c       k2 [real] {input} = the square of the parameter in the integral
c       tol [real] {input} = tolerance for convergence of the series 
c       maxn [integer] {input} = maximum number of terms in the series
c
c     The function returns a real scalar result, computed here as elk.
c-----------------------------------------------------------------------
      FUNCTION elk_bar2(k2,tol,maxn) RESULT(elk)
      USE ell_kind_mod
      IMPLICIT NONE

      REAL(KIND=rknd) :: elk
      REAL(KIND=rknd), INTENT(IN) :: k2,tol
      INTEGER(KIND=iknd), INTENT(IN) :: maxn

      INTEGER :: ii,jj,kk
      REAL(KIND=rknd) :: new

      elk=1._rknd
      new=1._rknd
      ii=1
      jj=0

      DO
        jj=jj+2
        new=new*k2*(REAL(jj-1,rknd)/jj)**2
        elk=elk+new
        IF (ABS(new)/ABS(elk)<=tol) EXIT
        ii=ii+1
        IF (ii>maxn) THEN
          WRITE(*,'(a,i4,a)') 'Elk_bar not converging after ',
     &                        maxn,' steps.'
          STOP
        ENDIF
      ENDDO

      RETURN
      END FUNCTION elk_bar2

