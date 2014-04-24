      SUBROUTINE cheb_fit(N,fit_arr)
!======================================================================
!
! This subroutine will calculate the coefficients, c0..cN, to be used 
! in the Chebyshev polynomials based on the collocation points, function 
! values at those collocation points, and the maximum index N.
! 
! INPUT:        N : integer, maximum index value
!         fit_arr : real array, function values f(x) evaluated at 
!                   collocation points
! OUTPUT: fit_arr : real array, coefficients c0..cN
!
!======================================================================
! Variable Declaration:

      IMPLICIT NONE

! Parameters for defining real and integer values 
      INTEGER, PARAMETER :: i7=SELECTED_INT_KIND(7)
      INTEGER, PARAMETER :: r143=SELECTED_REAL_KIND(14,300)

! Variables used for indexing
      INTEGER(i7), INTENT(IN) :: N
      INTEGER(i7) :: m, i, nn

! Arrays and other variables used in calculations
      REAL(r143), DIMENSION(0:N), INTENT(INOUT) :: fit_arr
      REAL(r143), DIMENSION(0:N) :: coeff
      REAL(r143) :: pi
      REAL(r143) :: temp_val

!======================================================================
! Calculation of coefficients

! set pi as appropriate value
      pi = acos(-1._r143)

!----------------------------------------------------------------------
! Loop for calculation of coefficients:
!
! Different summations are needed to find c0 and c_n for n=1..N 
! For each summation, a temporary value is initialized at 0 and
! every value of summation is added to the constantly changing
! temp_val which ultimately updates temp_val to the total sum.

      DO nn = 0,N

        ! A case statement is used to separate c0 from c1..cN due to
        ! different methods of calulations.

          SELECT CASE (nn)      
!----------------------------------------------------------------------
        ! Calculation of coefficient c0:
          CASE (0)

            ! Initialize temp_val to be used in summation
              temp_val = 0._r143

            ! perform appropriate summation of f(x_m) values
              DO m = 0,N
                  temp_val = temp_val + fit_arr(m)
              END DO

            ! final value for c0 stored into the first position of
            ! the temporary coeff array
              coeff(0) = 1._r143/(N+1._r143)*temp_val

!---------------------------------------------------------------------- 
        ! Calculation of coefficients c1..cN
          CASE (1:)

            ! Initialize temp_val to be used in summation
              temp_val = 0._r143 

            ! perform summation for m values from 0 to N
            ! sum: f(x_m)*cos(pi*n*(m+1/2)/(N+1))
              DO m = 0,N
                  temp_val = temp_val + fit_arr(m)* &
                             cos(pi*nn*(m+0.5_r143)/(N+1._r143))
              END DO

            ! finish calculating coefficients c1..cN and store in array
              coeff(nn) = 2._r143/(N+1._r143)*temp_val
 
          END SELECT
      END DO

!======================================================================
! Overwrite fit_arr with appropriate values of the coefficients then
!  display the coefficients 

      fit_arr = coeff

      WRITE(*,*) 'Coefficients c0..cN:'
      WRITE(*,*) fit_arr

!======================================================================

      END SUBROUTINE cheb_fit
