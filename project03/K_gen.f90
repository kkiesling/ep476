      SUBROUTINE K_gen(L,N,E, KK)
! =====================================================================
! DESCRIPTION:
! This subroutine will create the stiffness matrix KK of the eigenvalue 
! problem below in a format for the DSBGV LAPACK solver.
!      K*u = M*(omega**2)*x
!
! Stiffness Matrix K is an N by N matrix (below) with a diagonal, one 
! superdiagonal, and one subdiagonal. It is reformatted to be a 2xN 
! array to be used in the DSBGV LAPACK solver (KK array). 
!
!     K = | k k 0 0 0 |
!         | k k k 0 0 |
!         | 0 k k k 0 |
!         | 0 0 k k k |
!         | 0 0 0 k k |
! 
! The ith element | k k | of the K matrix is described below:
!                 | k k |
!
!     (E/deltaX)*|  1  -1 |
!                | -1   1 |
!
! where E is modulus of elasticity and deltaX is the step size (step).
! The array KK is the reformatted 2xN array with the top row as the 
! values for the superdiagonal (with 0 as the first entry) and the 
! second row as the diagonal values
!
! INPUT AND OUTPUT:
! This subroutine will calculate the values of KK for the given 
! parameters of the bar.
! 
! input:      E = modulus of elasticity, real
!             L = length of bar, real
!             N = number of elements
! output:    KK = stiffness matrix diagonals, real
!
! ===================================================================== 
! Declaration of variables
! 
! Use a module that specifies the kind of reals and integers 
      use kind_mod
      IMPLICIT NONE
 
      ! characterstics of the bar defined by user in driver program:
      REAL(rknd), INTENT(IN) :: E, L ! modulus of elasticity and 
                                     ! length of bar 
      INTEGER(iknd), INTENT(IN) :: N ! number of elements for FEM 

      ! variables used for creating KK:
      REAL(rknd) :: step      ! step size (deltaX)
      INTEGER(iknd) :: i,j,kb ! indices used, kb is number of super-
                              ! diagonals

      ! output KK used for DSBGV solver:
      REAL(rknd), DIMENSION(2,N), INTENT(OUT) ::KK 

! =====================================================================
! CALCULATION OF KK

      step = L/N  ! define step size along bar
      kb=1        ! number of superdiagonals in stiffness matrix K

! Populate KK:
!
! Create KK array where the top row is the list of superdiagonal values
! and the bottom row is the list of diagonal values starting with the 
! ith element. The first value of the superdiagonal is non existent.
!
! The i and j indices correspond to the ith row and jth column of the 
! stiffness matrix K (size NxN). The new indices for KK are determined
! based on the specifications given by the LAPACK DSBGV documentation
! where kb is the number of superdiagonals. Elements in that have
! overlapping parts must be taken as a summation of those values for
! each element i that contributes.

      DO i=1,N
         j=i

         ! The first element is a special case because there is no
         ! corresponding superdiagonal value. 
         IF (i==1) THEN
            KK(kb+1+i-j,j) = 2._rknd*E/step

         ! The Nth element is also a special case because it does not
         ! have an N+1 element to add.
         ELSE IF (i==N) THEN
            KK(kb+1+i-j,j) = E/step
            KK(kb+1+i-j-1,j) = -E/step

         ! The rest of the elements follow the standard method.
         ELSE
            KK(kb+1+i-j,j) = 2._rknd*E/step
            KK(kb+1+i-j-1,j) = -E/step
         END IF
      END DO

! =====================================================================
      END SUBROUTINE K_gen

