c=======================================================================
c     This file contains the main program for the nonlinear diffusion
c     program.  It solves initial-value problems for temperature.
c
c	c(T)*rho*dT/dt = d/dx[ kappa(T) * d/dx(T) ] + S
c
c     The computations are performed in rectangular geometry.
c=======================================================================

      PROGRAM nldiff
      USE nld_kind_mod
      USE nld_inp_mod
      USE nld_data_mod
      USE nld_io_mod
      IMPLICIT NONE

      INTEGER(iknd) :: ii
      REAL(rknd) :: time_st,time_en

c-----------------------------------------------------------------------
c     Namelist declarations for input parameters.
c-----------------------------------------------------------------------

      NAMELIST /nld_inp/ model,csp0,rho0,thc0,thc1,thc3,tcfit,ccfit,
     1                   ncfit,xlength,tinitial,tfinal,
     2                   icc,ics,bc_val0,bc_vall,
     3                   nstep,ncell,adv_type,theta,estab,nl_tol,
     4                   nl_maxit,newt_start,nplt,u_src

c-----------------------------------------------------------------------
c     Initialize the timer, and set constants.
c-----------------------------------------------------------------------

      CALL timer(time_st)
      pi=ACOS(-1._rknd)
      twopi=2._rknd*pi

c-----------------------------------------------------------------------
c     Read the namelist data from the text file nld_input.dat.
c-----------------------------------------------------------------------

      OPEN(UNIT=nlin_unit,FILE="nld_input.dat",STATUS="OLD",
     $     FORM="FORMATTED")
      READ(UNIT=nlin_unit,NML=nld_inp)
      CLOSE(nlin_unit)

c-----------------------------------------------------------------------
c     Call the data initialization routine.
c-----------------------------------------------------------------------

      CALL nld_data_init

c-----------------------------------------------------------------------
c     Write the spatial mesh points for plotting information.
c-----------------------------------------------------------------------

      OPEN(UNIT=xmsh_unit,FILE='xmesh.dat',STATUS='UNKNOWN')
      DO ii=0,ncell
        WRITE(xmsh_unit,'(es12.5)') xvert(ii)
      ENDDO
      CLOSE(xmsh_unit)

c-----------------------------------------------------------------------
c     Open data files for the dependent variable and time, and write
c     the first data set.
c-----------------------------------------------------------------------

      OPEN(UNIT=tout_unit,FILE='temperature.dat',STATUS='UNKNOWN')
      OPEN(UNIT=time_unit,FILE='time.dat',STATUS='UNKNOWN')
      CALL nld_write

c-----------------------------------------------------------------------
c     Loop to advance the solution in time.
c-----------------------------------------------------------------------

      istep=0

      tloop: DO

        IF (time_check .eqv. .false.) then
            RETURN
        END IF


c-----------------------------------------------------------------------
c       Call the advance routine.
c-----------------------------------------------------------------------

        SELECT CASE (adv_type)

        CASE( 'explicit' )

          CALL nld_expladv

        CASE( 'implicit' )
            
          CALL nld_impladv

        CASE DEFAULT

          WRITE(*,*) 'Note: adv_type ',adv_type,' not found.'

        END SELECT

        istep=istep+1

c-----------------------------------------------------------------------
c       Write the output for graphics.
c-----------------------------------------------------------------------

        IF (MODULO(istep,nplt)==0.OR.istep==nstep) CALL nld_write

c-----------------------------------------------------------------------
c       Check for the time-loop exit conditions.
c-----------------------------------------------------------------------

        IF (istep>=nstep.OR.time>=tfinal) EXIT
      ENDDO tloop

c-----------------------------------------------------------------------
c     Write the final solution to a text file.
c-----------------------------------------------------------------------

      OPEN(UNIT=tout_unit,FILE='nld_temp.dat',STATUS='UNKNOWN',
     $     POSITION='REWIND',FORM='FORMATTED')

      DO ii=0,ncell
        WRITE(tout_unit,'(i6,2es18.10)') ii,xvert(ii),temp(ii)
      ENDDO 

c-----------------------------------------------------------------------
c     Close the graphics files, complete the timing,
c     and terminate.
c-----------------------------------------------------------------------

      CLOSE(time_unit)
      CLOSE(tout_unit)

      CALL timer(time_en)
      WRITE(*,'(/,a,es11.4,a)') "NL_DIFF ran in ",
     $  time_en-time_st," seconds."
      WRITE(*,'(a,es11.4,a,/)') "Final problem time is ",time,"."

      END PROGRAM nldiff


c=======================================================================
c     Subroutine nld_write writes the dependent variable for Matlab
c     or other graphics.
c=======================================================================

      SUBROUTINE nld_write
      USE nld_kind_mod
      USE nld_inp_mod
      USE nld_data_mod
      USE nld_io_mod
      IMPLICIT NONE

      INTEGER(iknd) :: ix

c-----------------------------------------------------------------------
c     Write the current time to its data file.
c-----------------------------------------------------------------------

      WRITE(time_unit,'(es12.5)') time

c-----------------------------------------------------------------------
c     Loop over position and write the current value of the dependent
c     as a single record in the tout_unit file.
c-----------------------------------------------------------------------

      DO ix=0,ncell-1
        WRITE(tout_unit,'(es14.6)',ADVANCE="NO") temp(ix)
      ENDDO
      WRITE(tout_unit,'(es14.6)') temp(ncell)

      RETURN
      END SUBROUTINE nld_write
