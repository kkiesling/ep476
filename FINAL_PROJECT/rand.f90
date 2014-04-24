REAL(rknd) FUNCTION rand()
! Generates next random number using a pseudo-random number
! generator system with seed s specified by user in input.

USE knd_mod
use inp_mod

IMPLICIT NONE
REAL(rknd) :: p, g, c, temp

p=3
g=2
c=0

temp=s*g+c

s=MOD(temp, p)
rand = s/p

END FUNCTION rand
