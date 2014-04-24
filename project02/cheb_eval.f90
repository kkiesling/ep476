      SUBROUTINE cheb_eval(N, x_val, fit_arr)

!======================================================================
!
! This subroutine will calculate the Chebyshev approximation values for
! the given function at values of x between -1 and 1  according to the 
! Clenshaw method.
! 
! f(x) = (c0 - y2)*T0(x) + y1*T1(x)
! where:
!        T0(x) = 1
!        T1(x) = x
!        yn = 2*x*y[n+1] - y[n+2] + cn
!        y[N+1] = y[N+2] = 0
!
! INPUT:       N : integer, maximum index value
!          x_val : real, the value of x between -1 and 1
!        fit_arr : real, coefficient values c0..cN
! OUTPUT:  x_val : real, the calculated approximation value of f(x)
!
!======================================================================
! Declaration of variables

      IMPLICIT NONE

! Parameters for declaring real and integer kinds 
      INTEGER, PARAMETER :: i7=SELECTED_INT_KIND(7)
      INTEGER, PARAMETER :: r143=SELECTED_REAL_KIND(14,300)

! input and output declarations
      INTEGER(i7), INTENT(IN) :: N
      REAL(r143), INTENT(INOUT) :: x_val
      REAL(r143), DIMENSION(0:N), INTENT(IN) :: fit_arr

! variables used in calculations
      REAL(r143), DIMENSION(0:N+2) :: y
      REAL(r143) :: c0

! variables used for indexing
      INTEGER(i7) :: nn

!======================================================================
! Calcuation of approximate f(x) value

! assign coefficient c0 to be used more easily later on
      c0 = fit_arr(0)

! set values of y(N+1) and y(N+2) to be used in iteration
      y(N+1) = 0._r143
      y(N+2) = y(N+1)

! This loop will calculate all values yn starting with y(N) and working
! backwards to y(0)

      DO nn=N,0,-1
         y(nn) = 2._r143*x_val*y(nn+1)-y(nn+2)+fit_arr(nn)
      END DO

! Use y(1) and y(2) to evaluate f(x) according to Clenshaw's method:
! f(x)  = (c0 - y2)*T0(x) + y1*T1(x)
    
      x_val = (c0-y(2))+y(1)*x_val
      
!======================================================================

      END SUBROUTINE
