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
      FUNCTION ele_bar2(k2,tol,maxn) RESULT(ele)
      USE ell_kind_mod
      IMPLICIT NONE

      REAL(KIND=rknd) :: ele
      REAL(KIND=rknd), INTENT(IN) :: k2,tol
      INTEGER(KIND=iknd), INTENT(IN) :: maxn

      INTEGER :: ii,jj,kk
      REAL(KIND=rknd) :: new

      ele=1._rknd
      new=1._rknd
      ii=1
      jj=0

      DO
        jj=jj+2
        new=new*k2*(REAL(jj-1,rknd)/jj)**2
        ele=ele-new/(jj-1)
        IF (ABS(new/(jj-1))/ABS(ele)<=tol) EXIT
        ii=ii+1
        IF (ii>maxn) THEN
          WRITE(*,'(a,i4,a)') 'Ele_bar not converging after ',
     &                        maxn,' steps.'
          STOP
        ENDIF
      ENDDO

      RETURN
      END FUNCTION ele_bar2

