c-----------------------------------------------------------------------
c     This program computes the phi component of vector potential
c     from current in a circular loop to illustrate the use of 
c     allocatable arrays.
c
c     Lengths are normalized to the coil radius, and the magnitude
c     of A_phi is per (mu_0*I/pi).
c-----------------------------------------------------------------------
      PROGRAM aphi_comp2
      USE ell_kind_mod
      IMPLICIT NONE

      INTEGER(KIND=iknd), PARAMETER :: maxits=10000
      INTEGER(KIND=iknd) :: nr,nz,ir,iz

      REAL(KIND=rknd), PARAMETER :: tolerance=1.e-6, offset=3.e-2
      REAL(KIND=rknd) :: kk,rmax,zmax,dr,dz,rc,zc,
     &  rsph,stheta,ele,elk,k2,pi
      REAL(KIND=rknd), DIMENSION(:,:), ALLOCATABLE :: aphi
      REAL(KIND=rknd), DIMENSION(:,:,:), ALLOCATABLE :: rzarr

      REAL(KIND=rknd), EXTERNAL :: ele_bar2,elk_bar2
c-----------------------------------------------------------------------
c     Acquire the problem size and data spacing from the user.
c-----------------------------------------------------------------------
      WRITE(*,*) ' Enter the physical dimensions of the domain'
      WRITE(*,*) ' normalized by the coil radius (making a->1).'
      READ(*,*) rmax,zmax

      WRITE(*,*) ' Enter the number of points in r and z.'
      READ(*,*) nr,nz

      dr=rmax/nr
      dz=zmax/nz
c-----------------------------------------------------------------------
c     Allocate the data arrays.
c-----------------------------------------------------------------------
      ALLOCATE(aphi(0:nr,0:nz))
      ALLOCATE(rzarr(0:nr,0:nz,2))
c-----------------------------------------------------------------------
c     Loop over the positions, determine the spherical coordinate r and
c     the sine of the angle of declination (theta) and fill the array.
c-----------------------------------------------------------------------
      pi=ACOS(-1._rknd)
      DO iz=0,nz
        zc=MAX(offset,iz*dz)          !     offset avoids divergence

c-----------------------------------------------------------------------
c       The geometric axis is a special case since theta, hence k**2
c       goes to zero.
c-----------------------------------------------------------------------

        aphi(0,iz)=0
        rzarr(0,iz,1)=0
        rzarr(0,iz,2)=zc

        DO ir=1,nr

c         coordinates

          rc=ir*dr                          !     cylindrical radius
          rsph=SQRT(rc**2+zc**2)            !     spherical r coordinate
          stheta=rc/MAX(rsph,TINY(rsph))    !     sin(theta-spherical)

c         elliptic integrals

c         the first line determines k**2

          k2=4._rknd*rsph*stheta/(1._rknd+rsph**2+2._rknd*rsph*stheta)
          elk=0.5_rknd*pi*elk_bar2(k2,tolerance,maxits)
          ele=0.5_rknd*pi*ele_bar2(k2,tolerance,maxits)

c         A_phi computation

          aphi(ir,iz)=((2._rknd-k2)*elk-2._rknd*ele)/
     &                (k2*SQRT(1._rknd+rsph**2+2._rknd*rsph*stheta))
          rzarr(ir,iz,:)=(/rc,zc/)
        ENDDO
      ENDDO

c-----------------------------------------------------------------------
c     Write output to a binary file.
c-----------------------------------------------------------------------
      OPEN(UNIT=10,FILE='aphi.bin',STATUS='UNKNOWN',POSITION='REWIND',
     $     FORM='UNFORMATTED')
      WRITE(10) 1_4,0_4,1_4
      WRITE(10) nr,nz
      WRITE(10) REAL(rzarr,4)
      WRITE(10) REAL(aphi,4)
      CLOSE(10)

      END PROGRAM aphi_comp2

