      PROGRAM arrsyn
      IMPLICIT NONE

c-----------------------------------------------------------------------
c     This program times different array orders for efficient
c     multiplication of sparse matrices and vectors.  Note that the
c     index ordering for the array mat has its linear algebra
c     row first and column offset storage second, whereas matrev
c     has the order of the indices reversed.
c-----------------------------------------------------------------------

      INTEGER, PARAMETER :: rknd=SELECTED_REAL_KIND(12,100)
      INTEGER, PARAMETER :: nmax=3000000,nr=40
      INTEGER :: ii,jj,count,count_rate,ir

      REAL(rknd), DIMENSION(nmax,-2:2) :: mat
      REAL(rknd), DIMENSION(-2:2,nmax) :: matrev
      REAL(rknd), DIMENSION(-1:nmax+2) :: vec1,vec2
      REAL(rknd) :: diff=0.25
      REAL(rknd) :: t0,t1

c-----------------------------------------------------------------------
c     The rows of the matrix represent a finite-difference approximation
c     for a fourth-order derivative on a uniform mesh.  Array syntax
c     is used for definining the matrices.
c-----------------------------------------------------------------------

      mat(:,-2)= 1._rknd
      mat(:,-1)=-4._rknd
      mat(:, 0)= 6._rknd
      mat(:, 1)=-4._rknd
      mat(:, 2)= 1._rknd
      matrev(-2,:)= 1._rknd
      matrev(-1,:)=-4._rknd
      matrev( 0,:)= 6._rknd
      matrev( 1,:)=-4._rknd
      matrev( 2,:)= 1._rknd

      mat=diff*mat                !    multiply entire operator by diff
      matrev=diff*matrev

c     Boundaries are special:

      mat(1,-2:-1)=0
      mat(2,-2   )=0
      mat(nmax  ,  2)=0
      mat(nmax-1,1:2)=0
      matrev(-2:-1,1)=0
      matrev(-2   ,2)=0
      matrev(  2,nmax  )=0
      matrev(1:2,nmax-1)=0

c     Initialize a square pulse in the vector.

      vec1=0._rknd
      vec1(3*nmax/8:5*nmax/8)=1._rknd
      vec2=vec1

c-----------------------------------------------------------------------
c     First form of matrix-vector multiplication where array syntax
c     is used to multiply the correct vec1 elements with the columns
c     of mat, and the intrinsic SUM adds all terms for each row:
c-----------------------------------------------------------------------

c     Check time (arguments are options and our names are
c     duplicating the intrinsic names):

      CALL system_clock(count=count,count_rate=count_rate)
      t0=REAL(count)/count_rate

      DO ir=1,nr
        DO ii=1,nmax
          vec2(ii) = vec2(ii) + SUM(mat(ii,:)*vec1(ii-2:ii+2))
        ENDDO
        vec2=vec2+vec1
      ENDDO

c     End and report time.

      CALL system_clock(count=count,count_rate=count_rate)
      t1=REAL(count)/count_rate
      WRITE(*,*) "F90 SUM with row index first took ",t1-t0," s."

      vec1=vec2

c-----------------------------------------------------------------------
c     Second form of matrix-vector multiplication where array syntax
c     is used to multiply the correct vec1 elements with the columns
c     of matrev, and the intrinsic SUM adds all terms for each row.
c     The row and column ordering is reversed.
c-----------------------------------------------------------------------

c     Start time.

      CALL system_clock(count=count,count_rate=count_rate)
      t0=REAL(count)/count_rate

      DO ir=1,nr
        DO ii=1,nmax
          vec2(ii) = vec2(ii) + SUM(matrev(:,ii)*vec1(ii-2:ii+2))
        ENDDO
        vec2=vec2+vec1
      ENDDO

c     End and report time.

      CALL system_clock(count=count,count_rate=count_rate)
      t1=REAL(count)/count_rate
      WRITE(*,*) "F90 SUM with row index 2nd took ",t1-t0," s."

      vec1=vec2

c-----------------------------------------------------------------------
c     Third form of matrix-vector multiplication has array syntax
c     for the resultant vector index.  Note how the operand indices
c     are shifted to get the right result.
c-----------------------------------------------------------------------

c     Start time.

      CALL system_clock(count=count,count_rate=count_rate)
      t0=REAL(count)/count_rate

      DO ir=1,nr
        DO ii=-2,2
          vec2(1:nmax) = vec2(1:nmax) + mat(:,ii)*vec1(1+ii:nmax+ii)
        ENDDO
        vec2=vec2+vec1
      ENDDO

c     End and report time.

      CALL system_clock(count=count,count_rate=count_rate)
      t1=REAL(count)/count_rate
      WRITE(*,*) "Vector-array syntax with row index first took ",
     &           t1-t0," s."

      vec1=vec2

c-----------------------------------------------------------------------
c     Fourth form of matrix-vector multiplication has array syntax
c     for the resultant vector index and uses the reversed matrix
c     order.
c-----------------------------------------------------------------------

c     Start time.

      CALL system_clock(count=count,count_rate=count_rate)
      t0=REAL(count)/count_rate

      DO ir=1,nr
        DO ii=-2,2
          vec2(1:nmax) = vec2(1:nmax) + matrev(ii,:)*vec1(1+ii:nmax+ii)
        ENDDO
        vec2=vec2+vec1
      ENDDO

c     End and report time.

      CALL system_clock(count=count,count_rate=count_rate)
      t1=REAL(count)/count_rate
      WRITE(*,*) "Vector-array syntax with row index 2nd took ",
     &           t1-t0," s."

      vec1=vec2

      END PROGRAM arrsyn
