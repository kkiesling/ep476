!=======================================================================
! File chp_inp_mod.f90 contains all variables that are set by the
! user at run-time.  There are scalar variables for physical parameters
! such as particle masses, and the number of particles to be used in
! a computation.  There are array variables that are used to set the
! initial conditions of particle coordinates and velocity vectors
! for each particle.  There are also parameters that control the
! numerical integration and specifications for postprocessing.
!
! Default values are provided for each variable.  The main program
! will read them from a file through namelist read statements.
!=======================================================================

  MODULE chp_inp_mod
  USE chp_kind_mod
  IMPLICIT NONE

! The first set defines physical parameters that may be changed to
! consider a different set of units.  Default values are in MKS units.

  REAL(rknd) :: elem_chrg=1.6022e-19_rknd
  REAL(rknd) :: electron_mass=9.1094e-31_rknd
  REAL(rknd) :: proton_mass=1.6726e-27_rknd
  REAL(rknd) :: neutron_mass=1.6750e-27_rknd

! Set the externally applied uniform fields.

  REAL(rknd) :: ex=0._rknd  !  3 components of electric field
  REAL(rknd) :: ey=0._rknd
  REAL(rknd) :: ez=0._rknd
  REAL(rknd) :: bx=0._rknd  !  3 components of magnetic field
  REAL(rknd) :: by=0._rknd
  REAL(rknd) :: bz=0._rknd

! The next set defines run-time specification of physical parameters
! for a particular computation.

  INTEGER(iknd) :: npart=1_iknd    !   # of particles in a computation
  REAL(rknd) :: t_initial=0._rknd
  REAL(rknd) :: t_final=1._rknd

! The next set defines initial positions and velocity vectors in
! Cartesian components.  The integer parameter npart_max is used to
! declare these initial-value arrays.  If a computation needs more than
! npart_max particles, increase the value here and recompile.  Arrays
! that are used to define the type of each particle (number of sub-
! atomic particles) are also declared.

  INTEGER(iknd), PARAMETER :: npart_max=25
  REAL(rknd), DIMENSION(npart_max) :: x_init=0._rknd
  REAL(rknd), DIMENSION(npart_max) :: y_init=0._rknd
  REAL(rknd), DIMENSION(npart_max) :: z_init=0._rknd
  REAL(rknd), DIMENSION(npart_max) :: vx_init=0._rknd
  REAL(rknd), DIMENSION(npart_max) :: vy_init=0._rknd
  REAL(rknd), DIMENSION(npart_max) :: vz_init=0._rknd
  INTEGER(iknd), DIMENSION(npart_max) :: num_elecs=1_iknd
  INTEGER(iknd), DIMENSION(npart_max) :: num_prots=0_iknd
  INTEGER(iknd), DIMENSION(npart_max) :: num_neuts=0_iknd

! The following set of parameters influence the choice and operation
! of the numerical method used to solve the ODE system.

  CHARACTER(32) :: integrator="RK2"
  INTEGER(iknd) :: nstep=1_iknd

  END MODULE chp_inp_mod
