c=======================================================================
c     This file contains the nld_kind_mod module which defines three
c     integer parameters used for setting the F90 'kind' of all
c     variables and constants.
c=======================================================================

      MODULE nld_kind_mod
      IMPLICIT NONE

      INTEGER, PARAMETER :: iknd=SELECTED_INT_KIND(8)
      INTEGER, PARAMETER :: ilongknd=SELECTED_INT_KIND(18)
      INTEGER, PARAMETER :: rknd=SELECTED_REAL_KIND(14,100)
      INTEGER, PARAMETER :: r4knd=SELECTED_REAL_KIND(6,20)

      END MODULE nld_kind_mod
