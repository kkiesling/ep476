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

