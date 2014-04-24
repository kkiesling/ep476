c-----------------------------------------------------------------------
c     This program computes the phi component of vector potential
c     from current in a circular loop to illustrate the use of 
c     allocatable arrays.
c
c     Lengths are normalized to the coil radius, and the magnitude
c     of A_phi is per (mu_0*I/pi).
c-----------------------------------------------------------------------
      PROGRAM aphi_comp
      IMPLICIT NONE

      INTEGER(KIND=4), PARAMETER :: maxits=10000
      INTEGER(KIND=4) :: nr,nz,ir,iz

      REAL(KIND=SELECTED_REAL_KIND(13,300)), PARAMETER :: 
     &  tolerance=1.e-6, offset=3.e-2
      REAL(KIND=SELECTED_REAL_KIND(13,300)) :: kk,rmax,zmax,dr,dz,rc,zc,
     &  rsph,stheta,ele,elk,k2,pi
      REAL(KIND=SELECTED_REAL_KIND(13,300)), DIMENSION(:,:), 
     &  ALLOCATABLE :: aphi
      REAL(KIND=SELECTED_REAL_KIND(13,300)), DIMENSION(:,:,:), 
     &  ALLOCATABLE :: rzarr

      REAL(KIND=SELECTED_REAL_KIND(13,300)), EXTERNAL :: ele_bar
c-----------------------------------------------------------------------
c     Acquire the problem size and data spacing from the user.
c-----------------------------------------------------------------------
      WRITE(*,*) ' Enter the physical dimensions, Rmax and Zmax, of the'
      WRITE(*,*) ' domain, normalized by the coil radius (making a->1).'
      READ(*,*) rmax,zmax

      WRITE(*,*) ' Enter the number of points in R and the number in Z.'
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
      pi=ACOS(-1._8)
      DO iz=0,nz
        zc=MAX(offset,iz*dz)          !     offset avoids divergence

c-----------------------------------------------------------------------
c       The geometric axis is a special case since theta, hence k**2
c       go to zero.
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

          k2=4._8*rsph*stheta/(1._8+rsph**2+2._8*rsph*stheta)   !   k**2
          CALL elk_bar(elk,k2,tolerance,maxits)
          ele=ele_bar(k2,tolerance,maxits)
          elk=0.5_8*elk*pi
          ele=0.5_8*ele*pi

c         A_phi computation

          aphi(ir,iz)=((2._8-k2)*elk-2._8*ele)/
     &                (k2*SQRT(1._8+rsph**2+2._8*rsph*stheta))
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

      END PROGRAM aphi_comp

