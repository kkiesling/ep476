      SUBROUTINE nld_impladv
      USE nld_kind_mod
      USE nld_inp_mod
      USE nld_data_mod
      USE nld_io_mod
      IMPLICIT NONE
C ---------------------------------------------------------------------
C This subroutine solves for the next time step:
C
C   T(n+1,j)-(theta*kappa*dt)/(cp*dx**2)*(T(n+1,j-1)-2*T(n+1,j)
C             +T(n+1,j+1)) = 
C   T(n,j)+(1-theta)*(kappa*dt)/(cp*dx**@)*(T(n,j-1)-2*T(n,j)+T(n,j+1))
C 
C ---------------------------------------------------------------------

      INTEGER(iknd) :: i, j
      EXTERNAL :: nld_coeff
      REAL(rknd), DIMENSION(0:ncell) :: BB
      REAL(rknd), DIMENSION(0:ncell,0:ncell) :: AA
      REAL(rknd), DIMENSION(1:ncell+1) :: D
      REAL(rknd), DIMENSION(1:ncell) :: E
      REAL(rknd) :: eta
      INTEGER(iknd) :: INFO
      
      src=u_src

      AA=0._rknd

      CALL nld_coeff(.false.) ! false = do not compute derivatives  
     
      temp(0)=bc_val0
      temp(ncell)=bc_vall

      
      DO j=1,ncell-1
            rhs(j)=temp(j)+(1-theta)*(dt*kappa(j))/(cvol(j)*dx**2)*
     $             (temp(j-1)-2._rknd*temp(j)+temp(j+1))
     $             +dt/cvol(j)*src(j)*(time+theta*dt)
      END DO

      DO j=0,ncell
            IF (j==0) THEN
                  BB(j)=temp(0)
                  AA(j,j)=1._rknd
            ELSEIF (j==1) THEN
                  eta=theta*kappa(j)*dt/(cvol(j)*dx**2)
                  BB(j)=rhs(j)+eta*temp(0)
                  AA(j,j)=1._rknd+2._rknd*eta
            ELSEIF (j==ncell-1) THEN
                  eta=theta*kappa(j)*dt/(cvol(j)*dx**2)
                  BB(j)=rhs(j)-eta*temp(ncell)
                  AA(j,j)=1._rknd+2._rknd*eta
                  AA(j,j-1)=-eta
            ELSEIF (j==ncell) THEN
                  eta=theta*kappa(j)*dt/(cvol(j)*dx**2)
                  BB(j)=temp(ncell)
                  AA(j,j)=1._rknd+2._rknd*eta
                  AA(j,j-1)=-eta
            ELSE
                  eta=theta*kappa(j)*dt/(cvol(j)*dx**2)
                  BB(j)=rhs(j)
                  AA(j,j)=1._rknd+2._rknd*eta
                  AA(j,j-1)=-eta
            END IF 
      END DO

C       DPTSV(N,NRHS,D,E,B,LDB,INFO)

      D=0._rknd
      DO i=1,ncell+1
            D(i) = AA(i-1,i-1)
      END DO

      E=0._rknd
      DO i=1,ncell
            E(i)=AA(i-1,i)
      END DO

      CALL DPTSV(ncell+1,1,D,E,BB,ncell+1,INFO)

      temp=BB

      time=time+dt

      END SUBROUTINE nld_impladv

