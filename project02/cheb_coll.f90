      SUBROUTINE cheb_coll(N, coll_arr)

!======================================================================
!
! This subroutine will calculate the collocation points x_m to be used
! when evaluating the given function for the Chebyshev polynomials.  
! It uses the user-defined maximum index, N, to calculate x_m
!
! INPUT:         N : integer, maximum index value
! OUTPUT: coll_arr : real array, collocation points x_m
!
!======================================================================
! Declaration of variables:

      IMPLICIT NONE

! Parameters for defining real and integer values
      INTEGER, PARAMETER :: i7=SELECTED_INT_KIND(7)
      INTEGER, PARAMETER :: r143=SELECTED_REAL_KIND(14,300)

! Indexing variable
      INTEGER(i7) :: m

! Values for calculations
      INTEGER(i7), INTENT(IN) :: N
      REAL(r143), DIMENSION(0:N), INTENT(OUT) :: coll_arr
      REAL(r143) :: pi

!======================================================================
! Calculation of collocation point array

! Set value of pi
      pi = acos(-1._r143)

! Fill coll_array with values
! x_m = cos(pi(m+1/2)/(N+1))
      DO m=0,N
       coll_arr(m) = cos(pi*(m + (1._r143/2._r143))/(N+1._r143))
      END DO

      WRITE(*,*) 'Collocation points x_m for Chebyshev polynomials'
      WRITE(*,*) coll_arr

!======================================================================
 
      END SUBROUTINE cheb_coll
