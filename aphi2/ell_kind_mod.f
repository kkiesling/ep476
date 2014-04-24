c-----------------------------------------------------------------------
c     This module is used to hold common kind definitions for the
c     '2' versions of the elliptic integral functions and driver.
c-----------------------------------------------------------------------
      MODULE ell_kind_mod
      IMPLICIT NONE

      INTEGER, PARAMETER :: rknd=SELECTED_REAL_KIND(13,300)
      INTEGER, PARAMETER :: iknd=SELECTED_INT_KIND(6)

      END MODULE ell_kind_mod
