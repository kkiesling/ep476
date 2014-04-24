      SUBROUTINE M_gen(rho_0, rho_1, L, N, MM)
! =====================================================================
! DESCRIPTION:
! This subroutine will create the mass matrix MM of the eigenvalue 
! problem below in a format for the DSBGV LAPACK solver.
!      K*u = M*(omega**2)*x
!
! Mass Matrix M is an N by N matrix (below) with a diagonal, one 
! superdiagonal, and one subdiagonal. It is reformatted to be a 2xN 
! array to be used in the DSBGV LAPACK solver (MM array). 
!
!     M = | m m 0 0 0 |
!         | m m m 0 0 |
!         | 0 m m m 0 |
!         | 0 0 m m m |
!         | 0 0 0 m m |
! 
! The ith element | m m | of the M matrix is described below:
!                 | m m |
!
!     rho_i*deltaX*| 1/3  1/6 |
!                  | 1/6  1/3 |
!
! where rho_i=rho_0+rho_1*x_i and deltaX is the step size (step).
! The array MM is the reformatted 2xN array with the top row as the 
! values for the superdiagonal (with 0 as the first entry) and the 
! second row as the diagonal values
!
! INPUT AND OUTPUT:
! This subroutine will calculate the values of MM for the given 
! parameters of the bar.
! 
! input:  rho_0 = density for rho_i(x), real
!         rho_1 = density for rho_i(x), real
!             L = length of bar, real
!             N = number of elements
! output:    MM = mass matrix diagonals, real
!
! ===================================================================== 
! Declaration of variables 
!
! Use a module that specifies the kind of reals and integers 
      use kind_mod
      IMPLICIT NONE

      ! characterstics of the bar defined by user in driver program:
      REAL(rknd), INTENT(IN) :: rho_0, rho_1, L ! densities and length
                                                ! of bar
      INTEGER(iknd), INTENT(IN) :: N            ! number of elements 
                                                ! for FEM 

      ! variables used for creating MM:
      REAL(rknd), DIMENSION(N) :: rho  ! vector of rho values
      REAL(rknd) :: x, step            ! location on bar, x and deltaX
      INTEGER(iknd) :: i,j,ka          ! indices used, ka is number of
                                       ! superdiagonals

      ! output MM used for DSBGV solver:
      REAL(rknd), DIMENSION(2,N), INTENT(OUT) :: MM

! =====================================================================
! CALCULATION OF MM

! define step size based of L and N
      step = L/N
      ka=1 ! number of superdiagonals in mass matrix M

! ---------------------------------------------------------------------
! Create array of rho values:
      DO i=1,N
         x=step*(i-.5_rknd) ! defines x at the midpoint of element i
         ! calculates rho(i) for x
         rho(i)=rho_0 + rho_1*(x/L)
      END DO

! ---------------------------------------------------------------------
! Populate MM:
!
! Create MM array where the top row is the list of superdiagonal values
! and the bottom row is the list of diagonal values starting with the 
! ith element. The first value of the superdiagonal is non existent.
!
! The i and j indices correspond to the ith row and jth column of the 
! mass matrix M (size N by N). The new indices for MM are determined
! based on the specifications given by the LAPACK DSBGV documentation
! where ka is the number of superdiagonals. Elements in that have
! overlapping parts must be taken as a summation of those values for
! each element i that contributes.

      DO i=1,N
         j=i

         ! The first element is a special case because there is no
         ! corresponding superdiagonal value. 
         IF (i==1) THEN
            MM(ka+1+i-j,j)=(rho(i)+rho(i+1))*step*(1._rknd/3._rknd)

         ! The Nth element is also a special case because it does not
         ! have an N+1 element to add.
         ELSE IF (i==N) THEN
            MM(ka+1+i-j,j)=rho(i)*step*(1._rknd/3._rknd)
            MM(ka+1+i-j-1,j)=rho(i)*step*(1._rknd/6._rknd)

         ! The rest of the elements follow the standard method.
         ELSE
            MM(ka+1+i-j,j)=(rho(i)+rho(i+1))*step*(1._rknd/3._rknd)
            MM(ka+1+i-j-1,j)=rho(i)*step*(1._rknd/6._rknd)
         END IF
      END DO

! =====================================================================
      END SUBROUTINE M_gen

