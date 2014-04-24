c-----------------------------------------------------------------------
c     This program illustrates the definition and use of F90
c     user-defined types.  It is convenient to put the definition in
c     a module so that it does not need to be repeated in subprograms.
c-----------------------------------------------------------------------
      MODULE mat_type_mod
      IMPLICIT NONE

      TYPE :: matrix_type
        CHARACTER(32) :: name
        INTEGER :: nrow
        INTEGER :: ncol
c       REAL(8), DIMENSION(:,:), POINTER :: rmat
        REAL(8), DIMENSION(:,:), ALLOCATABLE :: rmat
      END TYPE matrix_type

      END MODULE 

c-----------------------------------------------------------------------
c     The main program uses mat_type_mod to make the TYPE definition
c     available for declaring data structures of type matrix_type.
c-----------------------------------------------------------------------
      PROGRAM usrtype
      USE mat_type_mod
      IMPLICIT NONE

      INTEGER :: ii,jj           ! integer variable declaration
      TYPE(matrix_type) :: amat  ! matrix_type 'variable' declaration
      TYPE(matrix_type) :: bmat  ! another matrix_type declaration

      amat%nrow=5  !  Assignment and use of lowest-level variables
      amat%ncol=4  !  doesn't require anything special.
      amat%name="5x4-sized matrix"

      ALLOCATE(amat%rmat(amat%nrow,amat%ncol))
      
      DO jj=1,amat%ncol
        DO ii=1,amat%nrow
          amat%rmat(ii,jj)=ii+(jj-1)*amat%nrow
        ENDDO
      ENDDO
c
c     Call an external subroutine that copies the amat data structure
c     into a new bmat structure.
c
      CALL mat_copy(amat,bmat)
c
c     Write the copied structure as a check.
c
c     AFTER COMPLETING mat_copy, REPLACE ALL "amat" BELOW WITH "bmat".
c
      WRITE(*,*) bmat%name
      WRITE(*,*) "# of rows=",bmat%nrow," # of cols=",bmat%ncol
      DO jj=1,bmat%ncol
        DO ii=1,bmat%nrow
          WRITE(*,*) ii,jj,bmat%rmat(ii,jj)
        ENDDO
      ENDDO
c
c     TEST WHETHER bmat%rmat HAS ITS OWN MEMORY ALLOCATION.
c
      amat%rmat(1,1)=-1
      WRITE(*,*) 'bmat%rmat(1,1) is now ',bmat%rmat(1,1)

c     Test list-direct write of a user type.

c     WRITE(*,*) bmat


      END PROGRAM usrtype


c-----------------------------------------------------------------------
c     This subprogram also uses the mat_type_mod.  Note that it needs
c     to copy the structure component-by-component.
c-----------------------------------------------------------------------
      SUBROUTINE mat_copy(matin,matcpy)
      USE mat_type_mod
      IMPLICIT NONE

      TYPE(matrix_type), INTENT(IN) :: matin
      TYPE(matrix_type), INTENT(OUT) :: matcpy

      INTEGER :: coption

      WRITE(*,*) 'Select the copy option from the following:'
      WRITE(*,*) ' 1) Use = on entire structure.'
      WRITE(*,*) ' 2) Assign components one at a time.'
      WRITE(*,'(a)',ADVANCE='NO') 'Enter index: '
      READ (*,*) coption
      
      SELECT CASE (coption)
      CASE(1)
        matcpy=matin
      CASE(2)
        matcpy%name=matin%name
        matcpy%nrow=matin%nrow
        matcpy%ncol=matin%ncol
        ALLOCATE(matcpy%rmat(matcpy%nrow,matcpy%ncol))
        matcpy%rmat=matin%rmat
      CASE DEFAULT
        WRITE(*,'(a,i2,a)') 'Copy option ',coption,' not recognized.'
        STOP
      END SELECT

      RETURN
      END SUBROUTINE mat_copy
