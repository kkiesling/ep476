      PROGRAM chdriver

!======================================================================
! This program tests two functions against Chebyshev approximations.
! 
!     f1(x) = a*x^2 + b*x + c
!     f2(x) = tanh(x) = (e^x - e^-x)/(e^x + e^-x) 
!
! ACTUAL FUNCTION EVALUATION:
! Each function is first evaluated at a range of x values
! between -1 and 1 for comparison.
!
! CHEBYSHEV APPROXIMATION:
! First, a set of collocation points, x_m are created based off the
! user input N; there are N+1 x_m values (0..N) created by subroutine
! coll_arr. Then, for each function, the subroutine cheb_fit is used 
! to calculate coefficient values c0..cN to be used in the Chebyshev 
! polynomials. Next, cheb_eval will approximate the values of each
! function at the same x values used above using the Clenshaw method.
! 
! ERROR EVALUATION:
! For each function, the difference between the actual
! function value and the Chebyshev approximation value is calculated.
!
!======================================================================
! Declare all variables being used:
      IMPLICIT NONE

!     Parameters used throughout
      INTEGER, PARAMETER :: r143=SELECTED_REAL_KIND(14,300)
      INTEGER, PARAMETER :: i7=SELECTED_REAL_KIND(7)

!     Reoccurring strings of characters
      CHARACTER(LEN=40) :: f1 = 'f(x) = a*x^2 + b*x + c' 
      CHARACTER(LEN=40) :: f2 = 'tanh(x) = (e^x - e^-x)/(e^x + e^-x)'

!     User input: N = max index
!                 a, b, & c = polynomial coefficients 
      INTEGER(i7) :: N
      REAL(r143) :: a, b, c 

!     Variables used in indexing DO loops
      INTEGER(i7) :: index, i, x, steps=20

!     Arrays of values
      REAL(r143), DIMENSION(:), ALLOCATABLE :: coll_arr
      REAL(r143), DIMENSION(:), ALLOCATABLE :: fit_1, fit_2
      REAL(r143), DIMENSION(:), ALLOCATABLE :: error1, error2
      REAL(r143), DIMENSION(:), ALLOCATABLE :: actual1, actual2
      REAL(r143), DIMENSION(:), ALLOCATABLE :: approx1, approx2
      REAL(r143), DIMENSION(:), ALLOCATABLE :: x_reals

!=====================================================================
! Prompt user for maxium value N to be used in Chebyshev polynomials.

      WRITE(*,*) 'Please input desired maximum index N'
      READ(*,*) N

!=====================================================================
! Allocate space for arrays based on input N and number of x_vals

      ALLOCATE(coll_arr(0:N))
      ALLOCATE(fit_1(0:N))
      ALLOCATE(fit_2(0:N))
      ALLOCATE(actual1(0:steps))
      ALLOCATE(approx1(0:steps))
      ALLOCATE(actual2(0:steps))
      ALLOCATE(approx2(0:steps))
      ALLOCATE(error1(0:steps))
      ALLOCATE(error2(0:steps))
      ALLOCATE(x_reals(0:steps))

!=====================================================================
! Run cheb_coll to fill collocation array of x_m values to be used for
! both functions int the Chebyshev approximation.

      CALL cheb_coll(N, coll_arr)

!=====================================================================
!
! FUNCTION 1: f(x) = a*x^2 + b*x + c
!
!-------------------------------------------------------------------
! Generate array of real x values ranging -1 to 1 to be used in 
! evaluation of the actual function and the Chebyshev approximation.

      x_reals(0) = -1._r143
      DO i = 1, steps
          x_reals(i) = x_reals(i-1) + 0.1_r143
      END DO

      WRITE(*,*) 'x values to use in function', f1
      WRITE(*,*)  x_reals

!---------------------------------------------------------------------
! Prompt user for polynomial coefficients (a,b,c):

      WRITE(*,*) 'Fitting to function ', f1
      WRITE(*,*) 'Please input coefficients a, b, & c'
      READ(*,*) a, b, c

!----------------------------------------------------------------------
! Chebyshev approximation:
! Evaluate f(x_m) at collocation points

      DO x = 0,N
          fit_1(x) = a*coll_arr(x)**2 + b*coll_arr(x) + c
      END DO 

! Evaluate coefficients c0..cN based on collocation points and f(x_m)

        CALL cheb_fit(N,fit_1)

!----------------------------------------------------------------------
! Function Evalutation at each value of x: (Actual and Chebyshev)

! The following loop will evaluate the actual and approximate function
! values at each value of x between -1 and 1

      DO i = 0,steps

          ! Evaluate the real function:
          actual1(i) = a*x_reals(i)**2 + b*x_reals(i) + c

          ! Evaluate the Chebyshev approximation and store values:
          CALL cheb_eval(N, x_reals(i),fit_1)
          approx1(i) = x_reals(i)

      END DO
      
      WRITE(*,*) 'Actual values for ', f1
      WRITE(*,*) actual1
      WRITE(*,*) 'Chebyshev approximation values for ', f1
      WRITE(*,*)  approx1

!----------------------------------------------------------------------
! Calculate the difference between the chebyshev fit and the real values.

      error1 = (actual1 - approx1)
      WRITE(*,*) 'Difference between actual and Chebyshev values:'
      WRITE(*,*) error1

!=====================================================================
!
! FUNCTION 2 f(x) = tanh(x) = (e^x - e^-x)/(e^x + e^-x)
!
!-------------------------------------------------------------------
! Generate array of real x values ranging -1 to 1 to be used in 
! evaluation of the actual function and the Chebyshev approximation.

      x_reals(0) = -1._r143
      DO i = 1, steps
          x_reals(i) = x_reals(i-1) + 0.1_r143
      END DO

      WRITE(*,*) 'x values to use in function ', f2
      WRITE(*,*)  x_reals

!---------------------------------------------------------------------

      WRITE(*,*) 'Fitting to function ',f2

!----------------------------------------------------------------------
! Chebyshev approximation:
! Evaluate f(x_m) at collocation points

      DO x = 0,N
          fit_2(x) = tanh(coll_arr(x))
      END DO 

! Evaluate coefficients c0..cN based on collocation points and f(x_m)

        CALL cheb_fit(N,fit_2)

!----------------------------------------------------------------------
! Function Evalutation at each value of x: (Actual and Chebyshev)

! The following loop will evaluate the actual and approximation function
! values at each value of x between -1 and 1

      DO i = 0,steps

          ! Evaluate the real function:
          actual2(i) = tanh(x_reals(i))

          ! Evaluate the Chebyshev approximation and store values:
          CALL cheb_eval(N, x_reals(i),fit_2)
          approx2(i) = x_reals(i)

      END DO
      
      WRITE(*,*) 'Actual values for ', f2
      WRITE(*,*) actual2
      WRITE(*,*) 'Chebyshev approximation values for ', f2
      WRITE(*,*)  approx2

!----------------------------------------------------------------------
! Calculate the difference between the chebyshev fit and the real values.

      error2 = (actual2 - approx2)
      WRITE(*,*) 'Difference between actual and Chebyshev values:'
      WRITE(*,*) error2     

!======================================================================

      END PROGRAM chdriver
