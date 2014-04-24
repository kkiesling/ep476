c======================================================================

      MODULE part_kind
      IMPLICIT NONE

c     This module is used to set an integer parameter to enforce
c     uniform precision for all real variables and constants.

      INTEGER, PARAMETER :: rk=SELECTED_REAL_KIND(12,100)

      END MODULE part_kind

c======================================================================

      MODULE part_mod
      USE part_kind   !  modules may use other modules
      IMPLICIT NONE   !  this applies to all contained subprograms but
                      !  not to other programming units that "use"
                      !  the part_kind module.

c     The following are module variables that are accessible to all 
c     of the contained module subprograms and to any subprogram that 
c     uses this module.

c      REAL(rk), DIMENSION(:), ALLOCATABLE :: marr    ! list of masses
c      REAL(rk), DIMENSION(:,:), ALLOCATABLE :: xyarr ! position list

c     The 'PRIVATE' attribute means that the following variables are 
c     only accessible to the contained module subprograms.

      REAL(rk), PRIVATE :: mtotal=0._rk

c----------------------------------------------------------------------

c     Local variables within any of the module subprograms are not
c     accessible unless information is passed.  The following
c     'CONTAINS' statement starts the module subprograms.

      CONTAINS

c----------------------------------------------------------------------

      SUBROUTINE part_sum(marr,xyarr)

c     Just sum the particle masses and save the result in the private
c     variable mtotal.
      REAL(rk), DIMENSION(:), INTENT(IN) :: marr    ! list of masses
      REAL(rk), DIMENSION(:,:), INTENT(IN) :: xyarr ! position list


      mtotal=SUM(marr)

      END SUBROUTINE part_sum

c----------------------------------------------------------------------
      SUBROUTINE part_com(xcom,ycom,marr,xyarr)

      REAL(rk), DIMENSION(*), INTENT(IN) :: marr    ! list of masses
      REAL(rk), DIMENSION(*), INTENT(IN) :: xyarr ! position list


c     Find the center-of-mass of the collection of particles and
c     return that position in the passed variable.

      REAL(rk), INTENT(OUT) :: xcom,ycom  !  COM coords computed here

      IF (mtotal==0._rk) CALL part_sum(marr,xyarr)

      xcom=SUM(xyarr(0,:)*marr)/mtotal   !  Computations here use array
      ycom=SUM(xyarr(1,:)*marr)/mtotal   !  syntax in the SUM arguments.

      END SUBROUTINE part_com

c----------------------------------------------------------------------

      FUNCTION part_momi(xcom,ycom,marr,xyarr)  RESULT(momi)

c     Find the mass moment of inertia for point masses at fixed
c     separation distances.

      REAL(rk) :: momi  !  scalar moment about COM, computed here

      REAL(rk), INTENT(IN) :: xcom,ycom  !  COM coords are input

      REAL(rk), DIMENSION(*), INTENT(IN) :: marr    ! list of masses
      REAL(rk), DIMENSION(*), INTENT(IN) :: xyarr ! position list


c     Here, we are using array syntax to find the square of the distance
c     between each particle and the center of mass location and then 
c     multiply by the mass of each particle before summing over all
c     particles.

      momi = SUM( ((xyarr(0,:)-xcom)**2+(xyarr(1,:)-ycom)**2)*marr )

      END FUNCTION part_momi

      END MODULE part_mod

c======================================================================

      PROGRAM pt_masses
      USE part_kind         !   <--Not needed since part_mod uses it.
      USE part_mod          !   <--This is the statement that allows
      IMPLICIT NONE         !      access to module variables and
                            !      subprograms in our module part_mod.

      INTEGER :: ii,npts
      REAL(rk) :: xcm,ycm
      REAL(rk), DIMENSION(:), ALLOCATABLE :: marr    ! list of masses
      REAL(rk), DIMENSION(:,:), ALLOCATABLE :: xyarr ! position list


c     This program computes the center of mass and the mass moment
c     of inertia for a test of rigidly connected point masses in
c     two dimensions.

c     User-specified input:

      WRITE(*,*) 'Enter the number of point masses.'
      READ(*,*) npts

c     We can allocate and fill the module variables xarr, yarr, and
c     marr, because this program uses the module and because they
c     do not have the PRIVATE attribute.  mtotal cannot be accessed
c     because it has the PRIVATE attribute.

      ALLOCATE(marr(npts))
      ALLOCATE(xyarr(0:1,npts))

c     Allow the user to enter data:

      WRITE(*,*) 'For each of the point masses enter the x-position'
      WRITE(*,*) 'the y-position and the mass.'

      DO ii=1,npts
        WRITE(*,FMT='(i3,a)',ADVANCE='NO') ii,') '
        READ(*,*) xyarr(:,ii), marr(ii)
      ENDDO

c     Find the center of mass from the module subprogram.

      CALL part_com(xcm,ycm,marr,xyarr)
      WRITE(*,*) 'The center of mass is (',xcm,',',ycm,').'
      WRITE(*,*) 'The moment of interia is ' &
        ,part_momi(xcm,ycm,marr,xyarr),'.'

      END PROGRAM pt_masses

