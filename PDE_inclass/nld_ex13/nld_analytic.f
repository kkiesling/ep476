      SUBROUTINE nld_analytic
      USE nld_kind_mod
      USE nld_inp_mod
      USE nld_data_mod
      USE nld_io_mod
      IMPLICIT NONE
C ---------------------------------------------------------------------
C This subroutine solves for the next time step:
C
C   T(n+1,j) = 
C 
C ---------------------------------------------------------------------

      INTEGER(iknd) :: i, j
      EXTERNAL :: nld_coeff
      REAL(rknd), DIMENSION(0:ncell) :: temp_T
      REAL(rknd) :: check_dt
      
      src=u_src

      CALL nld_coeff(.false.) ! false = do not compute derivatives  
      DO i=1,ncell-1
            temp(i)=0.5_rknd*src(i)*cvol(i)/kappa(i)*(xvert(i)**2
     $     -xlength*xvert(i))+xvert(i)/xlength*(bc_vall-bc_val0)+bc_val0
      
      END DO

      time=time+dt

      END SUBROUTINE nld_analytic

