c     This F90 program demonstrates how arrays are passed into
c     subroutines and functions using the adjustable-size method.

      PROGRAM twoarr
      IMPLICIT NONE

      INTEGER, PARAMETER :: rknd=SELECTED_REAL_KIND(12,50)
      INTEGER, PARAMETER :: nmax=5
      INTEGER :: ii,jj
      REAL(rknd), DIMENSION(nmax) :: twomults
      REAL(rknd) :: tworesult
      REAL(rknd), EXTERNAL :: twosum  !  declare the external function

c     Loop over an index from 1 to nmax.  At each iterate, fill
c     an array segment of the twomults array with multiplies of two,
c     then use the twosum function to add those values.
c
c     Note that the first elements of twomults get reset to the same
c     values multiple times for the demonstration--not efficiency!

      DO ii=1,nmax
        CALL submults(ii,twomults(1:ii))  !  this passes the max index
                                          !  and an array segment into
                                          !  twomults

        tworesult=twosum(ii,twomults(1:ii))  !  also passing array segment
        WRITE(*,*) "Sum of multiplies for index ",ii," is ",tworesult
      ENDDO

c     Use twosum again (with the last set of multiples) directly in
c     another mathematical operation.

      tworesult=twosum(nmax,twomults)/4._rknd
      WRITE(*,*) "One-fourth of the last sum is ",tworesult

      STOP
      END PROGRAM twoarr


c     This subroutine returns an array of multiples of two through arr.
c     The size of the array is set by the integer input imax.

      SUBROUTINE submults(imax,arr)
      IMPLICIT NONE

      INTEGER, PARAMETER :: rknd=SELECTED_REAL_KIND(12,50)
      INTEGER, INTENT(IN) :: imax
      REAL(rknd), DIMENSION(1:imax), INTENT(OUT) :: arr  !  declaring with the passed
                                                         !  integer is an example of
                                                         !  an adjustable array
      INTEGER :: jj

      DO jj=1,imax
        arr(jj)=2._rknd*jj
      ENDDO

      RETURN
      END SUBROUTINE submults

c     This function returns the sum of the input array arr.
c     dummy array arr.  The size of the array is set by the integer
c     input imax.

      FUNCTION twosum(imax,arr) RESULT(sumdum)
      IMPLICIT NONE

      INTEGER, PARAMETER :: rknd=SELECTED_REAL_KIND(12,50)
      INTEGER, INTENT(IN) :: imax
      REAL(rknd), DIMENSION(1:imax), INTENT(IN) :: arr  !  also adjustable-size but
                                                        !  different intent than
                                                        !  the subroutine example
      REAL(rknd) :: sumdum ! automatically output intent only

      sumdum=SUM(arr)  !  using intrinsic F90 function

      RETURN
      END FUNCTION twosum
