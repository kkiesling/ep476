c=======================================================================
c     This file contains the nld_data_mod module which holds the
c     dependent variable array and other global information related
c     to the computation itself.
c=======================================================================

      MODULE nld_data_mod
      USE nld_kind_mod
      USE nld_inp_mod
      USE nld_io_mod
      IMPLICIT NONE

c-----------------------------------------------------------------------
c     Generic information.
c-----------------------------------------------------------------------
 
      REAL(rknd) :: pi,twopi

c-----------------------------------------------------------------------
c     Time-step information.
c-----------------------------------------------------------------------
 
      REAL(rknd) :: time,dt,dt0,dx
      INTEGER(iknd) :: istep

c-----------------------------------------------------------------------
c     Independent and dependent variable arrays declared as pointers
c     for flexibility.
c-----------------------------------------------------------------------
 
      REAL(rknd), DIMENSION(:), POINTER :: xvert
      REAL(rknd), DIMENSION(:), POINTER :: dxj
      REAL(rknd), DIMENSION(:), POINTER :: dxh
      REAL(rknd), DIMENSION(:), POINTER :: temp

c-----------------------------------------------------------------------
c     Arrays for matrices and rhs vector.
c-----------------------------------------------------------------------

      REAL(rknd), DIMENSION(:,:), POINTER :: lin_mat
      REAL(rknd), DIMENSION(:), POINTER :: rhs

c-----------------------------------------------------------------------
c     Arrays for spatially varying volumetric heat capacity and 
c     thermal conductivity.  Note that the thermal conductivity is
c     centered between vertices, while the volumetric heat capacity
c     is centered at vertices.
c-----------------------------------------------------------------------

      REAL(rknd), DIMENSION(:), POINTER :: cvol
      REAL(rknd), DIMENSION(:), POINTER :: dcvol
      REAL(rknd), DIMENSION(:), POINTER :: kappa
      REAL(rknd), DIMENSION(:), POINTER :: dkappa

c-----------------------------------------------------------------------
C     Source (spacially varying)
c-----------------------------------------------------------------------

      REAL(rknd), DIMENSION(:), ALLOCATABLE :: src

c-----------------------------------------------------------------------
c     End of module variables.     
c-----------------------------------------------------------------------

      CONTAINS

c-----------------------------------------------------------------------
c     This subroutine is used to initialize data.
c-----------------------------------------------------------------------

      SUBROUTINE nld_data_init
      USE nld_inp_mod
      IMPLICIT NONE

      INTEGER(iknd) :: ix,ifcmp

c-----------------------------------------------------------------------
c     Set the initial value of time and time-step.
c-----------------------------------------------------------------------

      time=tinitial
      dt0=(tfinal-tinitial)/nstep
      dt=dt0

c-----------------------------------------------------------------------
C     Allocate source mesh
c-----------------------------------------------------------------------

      ALLOCATE(src(0:ncell))
      
      src=1._rknd

c-----------------------------------------------------------------------
c     Allocate and fill mesh information.
c-----------------------------------------------------------------------

      ALLOCATE(xvert(0:ncell))
      
      dx=xlength/ncell

      DO ix=0,ncell
        xvert(ix)=ix*dx   !(xlength/ncell)
      ENDDO

c-----------------------------------------------------------------------
c     Half-integer delta-x values:
c-----------------------------------------------------------------------

      ALLOCATE(dxh(1:ncell))

      DO ix=1,ncell
        dxh(ix)=xvert(ix)-xvert(ix-1)
      ENDDO

c-----------------------------------------------------------------------
c     Integer delta-x values that represent the distance from one
c     cell center to the next.  Ghost cells have the same size as
c     the adjacent interior cells.
c-----------------------------------------------------------------------

      ALLOCATE(dxj(0:ncell))

      DO ix=1,ncell-1
        dxj(ix)=0.5_rknd*(xvert(ix+1)-xvert(ix-1))
      ENDDO

      dxj(0)=dxh(1)
      dxj(ncell)=dxh(ncell)
      
c-----------------------------------------------------------------------
c     Set initial conditions by the given Fourier-component information.
c-----------------------------------------------------------------------

      ALLOCATE(temp(0:ncell))
      temp=0.5_rknd*icc(0)

      DO ifcmp=1,n0max
        temp=temp+ics(ifcmp)*SIN(ifcmp*pi*xvert/xlength)+
     $            icc(ifcmp)*COS(ifcmp*pi*xvert/xlength)
      ENDDO

c-----------------------------------------------------------------------
c     Apply boundary-conditions.
c-----------------------------------------------------------------------

      temp(0)=bc_val0
      temp(ncell)=bc_vall

c-----------------------------------------------------------------------
c     Allocate space for matrix elements.
c-----------------------------------------------------------------------

      IF (adv_type/='explicit') THEN
        ALLOCATE(lin_mat(-1:1,0:ncell))
        ALLOCATE(rhs(0:ncell))
      ENDIF

c-----------------------------------------------------------------------
c     Allocate space for spatially varying physical coefficients.
c-----------------------------------------------------------------------

      ALLOCATE(cvol(0:ncell))
      ALLOCATE(dcvol(0:ncell))
      ALLOCATE(kappa(1:ncell))
      ALLOCATE(dkappa(1:ncell))

      RETURN
      END SUBROUTINE nld_data_init

      END MODULE nld_data_mod
