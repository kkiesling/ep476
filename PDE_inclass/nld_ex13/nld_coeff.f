c=======================================================================
c     Subroutine nld_coeff evaluates the volumetric heat capacity
c     (mass density X specific heat capacity) and the thermal
c     conductivity, depending on the model specification.
c=======================================================================
 
      SUBROUTINE nld_coeff(deriv)
      USE nld_kind_mod
      USE nld_inp_mod
      USE nld_data_mod
      IMPLICIT NONE

      LOGICAL, INTENT(IN) :: deriv

      INTEGER(iknd) :: ix,ideriv
      REAL(rknd) :: tcell,den

c-----------------------------------------------------------------------
c     The uniform model just sets the arrays to constant values.
c-----------------------------------------------------------------------

      SELECT CASE (model)
      CASE ("uniform")

        cvol=rho0*csp0
        kappa=thc0

        IF (deriv) THEN
          dcvol=0._rknd
          dkappa=0._rknd
        ENDIF

C      CASE("nfuel")
C        DO ix=1,ncell
C            tcell=.5_rknd*(temp(ix-1)+temp(ix))
c            cvol=rho0*csp0
C            kappa(ix)=thc0/(1+tcell*thc1)+thc3*tcell**3 
C        END DO
C        CALL nld_interp(ncfit,tcfit(0:ncfit),ccfit(0:ncfit),
C     &                  ncell+1_iknd,temp,0_iknd,cvol,dcvol)
      CASE DEFAULT

        WRITE(*,*) "Model ",TRIM(model)," not recognized."

      END SELECT

      RETURN
      END SUBROUTINE nld_coeff
