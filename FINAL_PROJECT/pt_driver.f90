PROGRAM pt_driver
! This is the main program that tracks N particles inside a sphere
! of radius R through Monte Carlo transport.
! 
! USER SPECIFIED INPUTS:
! N : number of particles, integer
! R : radius of sphere, real [cm]
! mu : material linear attenuation coefficient [1/cm]
! POS_0 : initial position of particles, 3x1 array of reals
! POS_s : center of sphere, 3x1 array of reals
!
! OUTPUT:
!

USE knd_mod
USE inp_mod

IMPLICIT NONE
INTEGER(iknd), PARAMETER :: inunit=10,outunit=11
real(rknd) :: rn

! Read in user specified inputs
! required inputs:
NAMELIST /pt_input/ N, R, mu, POS_0, POS_s, s, E

OPEN(UNIT=inunit,FILE='part_trans.in',STATUS='OLD',FORM='FORMATTED')
READ(UNIT=inunit,NML=pt_input)
CLOSE(inunit)


END PROGRAM pt_driver
