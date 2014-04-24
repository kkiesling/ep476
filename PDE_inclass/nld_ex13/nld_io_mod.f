c=======================================================================
c     This file contains the nld_io_mod module which defines 
c     input/output unit numbers with mnemonics to avoid confusion.
c=======================================================================

      MODULE nld_io_mod
      USE nld_kind_mod
      IMPLICIT NONE

      INTEGER(iknd), PARAMETER :: nlin_unit=10
      INTEGER(iknd), PARAMETER :: xmsh_unit=11
      INTEGER(iknd), PARAMETER :: time_unit=12
      INTEGER(iknd), PARAMETER :: tout_unit=13
      INTEGER(iknd), PARAMETER :: iter_unit=14

      END MODULE nld_io_mod
