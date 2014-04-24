c-----------------------------------------------------------------------
c     This file contains a subroutine for Lagrange interpolatory
c     polynomials; it is not a module.
c
c     Evaluate the Lagrange interpolatory polynomials and their
c     derivatives at the point specified, provided the data and
c     its positions.  NOTE: there are more efficient ways to perform
c     these operations, but this has been adopted from a code that
c     reuses the coefficients at fixed points.
c
c     The parameters for calling nld_interp are:
c
c       pd	= (int) the polynomial degree of the interpolation
c
c       x_node	= (real array dimension 0:pd) coefficients of the
c		  independent variable 
c		  
c       y_node	= (real array dimension 0:pd) coefficients of the
c		  dependent variable 
c		  
c       neval	= number of interpolations to evaluate.
c
c       x	= (real array dimension 1:neval) independent variables
c		  for the evaluation
c
c       dmode	= (int) set to 0 to just evaluate the function
c		  set to 1 to evaluate the function and its derivative
c
c	y	= OUTPUT (real array dimension 1:neval) value of the
c		  dependent variable at x
c
c	dydx	= OUTPUT (real array dimension 1:neval) derivative of
c		  the dep. variable at x
c
c-----------------------------------------------------------------------
      SUBROUTINE nld_interp(pd,x_node,y_node,neval,x,dmode,y,dydx)
      USE nld_kind_mod
      IMPLICIT NONE

      INTEGER(iknd), INTENT(IN) :: pd,dmode,neval
      REAL(rknd), DIMENSION(0:pd), INTENT(IN) :: x_node,y_node
      REAL(rknd), DIMENSION(neval), INTENT(IN) :: x
      REAL(rknd), DIMENSION(neval), INTENT(OUT) :: y,dydx

      INTEGER(iknd) :: i,j,k
      REAL(rknd), DIMENSION(0:pd) :: c_norm
      REAL(rknd) :: dxtmp(neval)
c-----------------------------------------------------------------------
c     First find the normalization constants.
c-----------------------------------------------------------------------
      c_norm=1._rknd
      DO i=0,pd
        DO j=0,pd
          IF (j==i) CYCLE
          c_norm(i)=c_norm(i)/(x_node(i)-x_node(j))
        ENDDO
      ENDDO
c-----------------------------------------------------------------------
c     Compute the basis values and the value of the function.
c-----------------------------------------------------------------------
      y=0._rknd
      DO i=0,pd
        dxtmp=c_norm(i)
        DO j=0,pd
          IF (j==i) CYCLE
          dxtmp=dxtmp*(x-x_node(j))
        ENDDO
        y=y+dxtmp*y_node(i)
      ENDDO
c-----------------------------------------------------------------------
c     Compute the derivative.
c-----------------------------------------------------------------------
      IF (dmode<1) RETURN
      dydx=0._rknd
      DO i=0,pd
        DO k=0,pd
          IF (k==i) CYCLE
          dxtmp=c_norm(i)
          DO j=0,pd
            IF (j==i) CYCLE
            IF (j==k) CYCLE
            dxtmp=dxtmp*(x-x_node(j))
          ENDDO
          dydx=dydx+dxtmp*y_node(i)
        ENDDO
      ENDDO

      RETURN
      END SUBROUTINE nld_interp
