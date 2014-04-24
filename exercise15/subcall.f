      PROGRAM subcall
      IMPLICIT NONE

      EXTERNAL :: test,test2

      CALL sub1(test)
      CALL sub1(test2)

      END PROGRAM


      SUBROUTINE sub1(tdum)
      IMPLICIT NONE

      EXTERNAL :: tdum
      INTEGER(4) :: itest

      itest=23

      CALL tdum(itest)

      RETURN
      END SUBROUTINE sub1


      SUBROUTINE test(idum)
      IMPLICIT NONE

      INTEGER(4), INTENT(IN) :: idum

      WRITE(*,*)  'This is test ',idum,'.'

      RETURN
      END SUBROUTINE test

      SUBROUTINE test2(idum)
      IMPLICIT NONE

      INTEGER(4), INTENT(IN) :: idum

      WRITE(*,*) 'This second test does an int divide by 2: ',idum/2,'.'

      RETURN
      END SUBROUTINE test2
