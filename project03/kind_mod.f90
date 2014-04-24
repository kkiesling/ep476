      MODULE kind_mod
! =====================================================================
! The purpose of this module is to be able to easily specify the kind
! of real variables and interger variables to be used in all programs.
! It ensures consistency and ease of variable declarations.
      
      IMPLICIT NONE
      INTEGER, PARAMETER :: rknd = SELECTED_REAL_KIND(14,300)
      INTEGER, PARAMETER :: iknd = SELECTED_INT_KIND(7)

! =====================================================================
      END MODULE kind_mod
