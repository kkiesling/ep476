      PROGRAM driver
! =====================================================================
! This is the driver program for solving the ODE below which describes the 
! natural frequencies of oscillation of a solid bar fixed
! at one end. The Finite Element Method and the DSGBV utility 
! of the LAPACK library are used in the solution.
!
! ODE to be solved:
!     (omega**2)*rho(x)*u=-E*d**2(u)/dx**2 
!     where: rho(x)=rho_0+rho_1*x
!            u(x) is the axial displacement along the bar
!            omega**2 are the eigenvalues
!
! USER INPUT: E     = modulus of elatisicty, real
!             L     = length of solid bar, real
!             rho_0 = density constant for rho(x), real
!             rho_1 = density constant for rho(x), real
!             N     = number of elements to be used in FEM, integer
! OUTPUT:     omega**2 = first five eigenvalues for ODE, real
!
! =====================================================================
! 
! Declaration of variables:
! Use a module that specifies the kind of reals and integers 
      use kind_mod
      IMPLICIT NONE

      ! User input variables
      REAL(rknd) :: E, L, rho_0, rho_1 ! user input used to describe ODE
      INTEGER(iknd) :: N ! user input, number of elements for FEM

      ! DSGBV input arrays: (omega**2)*M*u=K*u
          ! M corresponds to Mass Matrix
          ! K corresponds to Stiffness Matrix
      REAL(rknd), DIMENSION(:,:), ALLOCATABLE :: M,K 

      ! Variables needed for DSBGV output
      INTEGER(iknd) :: INFO ! indicates success or failure of subroutine
      ! output arrays:
          ! W = array of eigenvalues
          ! Z = array of eigenvectors
          ! WORK = array used for DSBGV computations
      REAL(rknd), DIMENSION(:), ALLOCATABLE :: W, Z, WORK
!
! =====================================================================
! Main Program
!
! Collect user input:

      write(*,*) "Please input modulus of elasticity E"
      read(*,*) E
      write(*,*) "Please input length of bar L"
      read(*,*) L
      write(*,*) "Please input rho_0 and rho_1 for rho(x)=rho_0+rho_1*x"
      read(*,*) rho_0, rho_1
      write(*,*) "Please input number of elements N to use for solving"
      read(*,*) N

! ---------------------------------------------------------------------
! Allocate necessary arrays based on N
      ALLOCATE(M(2,N))
      ALLOCATE(K(2,N))
      ALLOCATE(W(N))
      ALLOCATE(Z(N))
      ALLOCATE(WORK(3*N))

!----------------------------------------------------------------------
! Generate mass and stiffness matrices using subroutines M_gen and
! K_gen, respectively.

      CALL M_gen(rho_0, rho_1, L, N, M)
      CALL K_gen(L,N,E, K) 

! ---------------------------------------------------------------------
! Use the LAPACK DSBGV subroutine to solve for eigenvalues
! DSBGV solves for eigenvalues/eigenvectors for problems of the form:
!       AB*x = BB*(lambda)*x
!
! Arguments for subroutine:
!
!       dsbgv(JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, 
!       LDBB, W, Z, LDZ, WORK, INFO)
!
!       JOBZ = 'N', no eigenvectors
!       UPLO = 'U', superdiagonals are submitted
!       N = N, number of elements
!       KA = 1, number of superdiagonals in matrix AB (K)
!       KB = 1, number of superdiagonals in matrix BB (M)
!       AB = K, stiffness matrix size (2,N)
!       LDAB = 2, leading dimension of AB
!       BB = M, mass matrix size (2,N)
!       LDBB = 2, leading dimension of BB
!       W (out) = W, allocated array for eigenvalues size (N)
!       Z (out) = Z, allocated array for eigenvectors (not used)
!       LDZ= 1, leading dimension of Z
!       WORK (out) = WORK, allocated array for work size (3*N)
!       INFO (out) = INFO, specifies success or failure

      CALL DSBGV('N','U',N,1,1,K,2,M,2,W,Z,1,WORK,INFO)

! Test for successful completion of DSBGV subroutine
      if (INFO == 0) then
         write(*,*) "Successful completion of DSBGV subroutine"
      else
         write(*,*) "Bad, DSBGV subroutine did not complete properly"
      endif

! ---------------------------------------------------------------------
! Output solutions to ODE
!
! Output first 5 eigenvalues omega**2 for given parameters 
      write(*,*) "The first 5 eigenvalues, omega**2, for N=", N, "are"
      Write(*,*) W(:5)
! =====================================================================
      END PROGRAM driver
