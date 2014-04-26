      SUBROUTINE nld_expladv
      USE nld_kind_mod
      USE nld_inp_mod
      USE nld_data_mod
      USE nld_io_mod
      IMPLICIT NONE
C ---------------------------------------------------------------------
C This subroutine solves for the next time step:
C
C   T(n+1,j) = T(n,j)+dt/cp*(kappa/dx**2*(T(n,j-1)-2*T(n,j)+T(n,j+1))+S)
C 
C ---------------------------------------------------------------------

      INTEGER(iknd) :: n, j
      EXTERNAL :: nld_coeff
      REAL(rknd), DIMENSION(0:ncell) :: temp_T
      
      src=u_src

      CALL nld_coeff(.false.) ! false = do not compute derivatives  
      
      temp_T(0)=bc_val0
      temp_T(ncell)=bc_vall

      ! Calculate next time step and store to a temporary vector
      DO j=1,ncell-1
C            temp_T(j)=temp(j)+dt*kappa(j)/(cvol(j)*dx**2)*(temp(j-1)
C     $                -2._rknd*temp(j)+temp(j+1))+dt/cvol(j)*src(j)

            temp_T(j)=temp(j)+dt/cvol(j)*(kappa(j)/dx**2*(temp(j-1)-
     $             2._rknd*temp(j)+temp(j+1))+src(j))
      END DO

      temp=temp_T
      time=time+dt

      END SUBROUTINE nld_expladv

