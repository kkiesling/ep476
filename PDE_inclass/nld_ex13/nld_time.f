c=======================================================================
c     This subroutine collects operations that are required when using
c     the F90-intrinsic SYSTEM_CLOCK subroutine.  The first call is
c     a special case to get the count rate (clock cycles per second)
c     and the maximum count value after which the counter returns to 0.
c=======================================================================
      SUBROUTINE timer(time)
      USE nld_kind_mod
      IMPLICIT NONE

      REAL(rknd), INTENT(INOUT) :: time

      INTEGER(iknd), SAVE :: count_rate,count_max,count
      INTEGER(ilongknd), SAVE :: last_count,count_tmp
      LOGICAL, SAVE :: first_call=.true.,warned=.false.

c     f90 intrinsic timer:

      IF (first_call) THEN
        CALL SYSTEM_CLOCK(count=count,count_rate=count_rate,
     $                    count_max=count_max)
        count_tmp=count
        first_call=.false.
      ELSE
        CALL SYSTEM_CLOCK(count=count)
        count_tmp=count
        DO WHILE (count_tmp<last_count-count_max/2)
          count_tmp=count_tmp+count_max
        ENDDO
      ENDIF
      time=REAL(count_tmp)/count_rate
      last_count=count_tmp

      RETURN
      END SUBROUTINE timer
