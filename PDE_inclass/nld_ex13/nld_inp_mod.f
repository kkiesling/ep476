c=======================================================================
c     This file contains the nld_inp module that declares all input
c     parameters and sets default values.  These items are listed in
c     namelists that are in the driver program.
c
c     NOTE: many of the input parameters are not operational in the
c     source code needed for Exercise 13.
c=======================================================================

      MODULE nld_inp_mod
      USE nld_kind_mod
      IMPLICIT NONE

c-----------------------------------------------------------------------
c     Physical parameters for specific heat capacity (J/kg), mass
c     density (kg/m^3), and thermal conductivity (W/m K) are listed
c     first.  The character variable model is used to determine
c     which physical model is solved by the computation.
c-----------------------------------------------------------------------

      CHARACTER(16) :: model="uniform"
      REAL(rknd) :: csp0=1._rknd      !  spec. heat cap - c
      REAL(rknd) :: rho0=1._rknd      !  mass density - rho
      REAL(rknd) :: thc0=1._rknd      !  thermal conductivity - kappa
      REAL(rknd) :: u_src=1._rknd

c-----------------------------------------------------------------------
c     If model is set to "nfuel", the computation has the nonlinear
c     temperature-dependent coefficients of a PuO2 fuel pellet.  The
c     mass density is constant (specified by rho0), and the thermal
c     conductivity is evaluated as:
c
c     kappa = thc0 / (1 + thc1*T)  +  thc3*T**3
c
c     The specific heat capacity is evaluated with a fit.  The maximum
c     number of data points is ncfit_max, which is a parameter.  The
c     user needs to specify ncfit (1 for a linear fit, 2 for quadratic,
c     etc.) and ncfit+1 temperatures and corresponding specific
c     heat capacity values, like a table.  The index for the data starts
c     at 0.
c-----------------------------------------------------------------------

      REAL(rknd) :: thc1=0._rknd
      REAL(rknd) :: thc3=0._rknd

      INTEGER(iknd), PARAMETER :: ncfit_max=20
      INTEGER(iknd) :: ncfit=1
      REAL(rknd), DIMENSION(0:ncfit_max) :: tcfit=0._rknd ! temp data
      REAL(rknd), DIMENSION(0:ncfit_max) :: ccfit=0._rknd ! csp data

c-----------------------------------------------------------------------
c     Specify the length of the non-periodic dimension (in m) and the
c     initial and final times (in s).
c-----------------------------------------------------------------------

      REAL(rknd) :: xlength=1._rknd
      REAL(rknd) :: tinitial=0._rknd
      REAL(rknd) :: tfinal=1._rknd

c-----------------------------------------------------------------------
c     Specify Fourier coefficients of the initial conditions on
c     the dependent variable (in T).  The expansion is
c
c     T(x,t=0)=icc(0)/2 + sum[n=1,n0max]{ icc(n)*COS(n*pi*x/L) +
c                                         ics(n)*SIN(n*pi*x/L) }
c
c     where n0max is an integer parameter that may be changed if
c     the program is recompiled.
c-----------------------------------------------------------------------

      INTEGER(iknd), PARAMETER :: n0max=20_iknd
      REAL(rknd), DIMENSION(0:n0max) :: icc=0._rknd
      REAL(rknd), DIMENSION(1:n0max) :: ics=0._rknd

c-----------------------------------------------------------------------
c     Specify time-independent boundary conditions on the dependent
c     variable.
c-----------------------------------------------------------------------

      REAL(rknd) :: bc_val0=0._rknd
      REAL(rknd) :: bc_vall=0._rknd

c-----------------------------------------------------------------------
c     Numerical resolution parameters.
c-----------------------------------------------------------------------

      INTEGER(iknd) :: nstep=100_iknd  !  number of time-steps
      INTEGER(iknd) :: ncell=10_iknd   !  number of spatial cells

c-----------------------------------------------------------------------
c     Time-step control parameters.  Possible choices for adv_type
c     are
c
c     1) 'explicit'   -  simple explicit advance
c
c     For the first exercise, only adv_type is used.
c-----------------------------------------------------------------------

      CHARACTER(16) :: adv_type='explicit'  
      REAL(rknd) :: estab=1.e10_rknd
      REAL(rknd) :: theta=0.5_rknd
      REAL(rknd) :: nl_tol=1.e-3_rknd
      REAL(rknd) :: nl_maxit=1000_iknd
      REAL(rknd) :: newt_start=0.1_rknd

c-----------------------------------------------------------------------
c     Graphics parameters.
c
c     Data is written every nplt steps.
c-----------------------------------------------------------------------

      INTEGER(iknd) :: nplt=1_iknd

      END MODULE nld_inp_mod
