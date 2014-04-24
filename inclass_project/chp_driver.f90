!=======================================================================
! File chp_driver.f90 holds the main program for the charged-particle
! computation.  It solves the ODE system from Newton's equation with
! the Lorentz force for charged particles, plus the kinematic relation:
!
!   dV_i/dt = (q_i/m_i)*[E + V_i X B]
!   dR_i/dt = V_i
!
! where V_i is the velocity vector of the i-th particle, R_i is its
! position vector, and E and B are the electric and magnetic fields,
! respectively.
!
! A variety of numerical integration methods are accessible from
! coded implementations and from a library ODE solver.
!=======================================================================

  PROGRAM charged_part
  USE chp_kind_mod
  USE chp_inp_mod
  USE chp_data_mod
  IMPLICIT NONE

  INTEGER(iknd), PARAMETER :: inunit=10,outunit=11
  INTEGER(iknd) :: nsys,ipart,ioff,itask=1,istate=1,iopt=1, &
                  LRW,LIW, i, itol=2, MF, JAC=1
  REAL(rknd), DIMENSION(:), POINTER :: solnvec,rptr,vptr

  EXTERNAL :: chp_df_eval, chpf
  
  INTEGER, DIMENSION(:), ALLOCATABLE :: IWORK
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: atol, RWORK, v0
  REAL(rknd) :: Bmag, dt, rtol, TOUT, T
! The following namelists statements group input parameters for
! separate read statements.

  NAMELIST /scalar_input/ elem_chrg,electron_mass,proton_mass, &
                          neutron_mass,npart,t_initial,t_final, &
                          integrator,nstep,ex,ey,ez,bx,by,bz

  NAMELIST /vector_input/ x_init,y_init,z_init,vx_init,vy_init,vz_init,&
                          num_elecs,num_prots,num_neuts
!-----------------------------------------------------------------------
! Open the user's text file, chpart.in, and execute namelist reads.
!-----------------------------------------------------------------------

  OPEN(UNIT=inunit,FILE='chpart.in',STATUS='OLD',FORM='FORMATTED')
  READ(UNIT=inunit,NML=scalar_input)
  READ(UNIT=inunit,NML=vector_input)
  CLOSE(inunit)

!-----------------------------------------------------------------------
! Set the system size (nsys) and allocate the solution vector.
!-----------------------------------------------------------------------

  Bmag=sqrt(bx**2+by**2+bz**2)
  nsys=6*npart
  ALLOCATE(solnvec(nsys))  !  position and velocity for each particle

!-----------------------------------------------------------------------
! Initialize the data arrays.
!-----------------------------------------------------------------------

  CALL chp_data_init

!-----------------------------------------------------------------------
! Load the initial conditions. 
!-----------------------------------------------------------------------

  DO ipart=1,npart
    ioff=6*(ipart-1)
    rptr=>solnvec(ioff+1:ioff+3) ! position vector for ipart
    vptr=>solnvec(ioff+4:ioff+6) ! velocity vector for ipart
    rptr(1)=x_init(ipart)
    rptr(2)=y_init(ipart)
    rptr(3)=z_init(ipart)
    vptr(1)=vx_init(ipart)
    vptr(2)=vy_init(ipart)
    vptr(3)=vz_init(ipart)
  ENDDO

!-----------------------------------------------------------------------
! Call the appropriate integrator.
!-----------------------------------------------------------------------

  SELECT CASE(integrator)
  CASE("RK2","RK4","rk2","rk4")

    CALL chp_runkut(chp_df_eval,integrator,nstep,nsys, &
                    t_initial,t_final,solnvec)

  CASE("odepack","ODEPACK")
    ALLOCATE(atol(nsys))
    ALLOCATE(v0(npart))
    dt=(t_final-t_initial)/nstep
    MF=10
    SELECT CASE(MF)
    CASE(10)
      LRW=20+16*nsys
      LIW=20
    CASE(21,22)
      LRW=22+9*nsys+nsys**2
      LIW=20+nsys
    END SELECT
    ALLOCATE(RWORK(LRW))
    ALLOCATE(IWORK(LIW))
    IWORK=0
    IWORk(6)=nstep
    rtol=1.D-15
    DO ipart=1,npart
       v0(ipart)=sqrt(vx_init(ipart)**2+vy_init(ipart)**2 &
                 +vz_init(ipart)**2)
       atol(6*(ipart-1)+1:6*(ipart-1)+3)=rtol*v0(ipart)/ &
                                         abs(qom(ipart))/Bmag
       atol(6*(ipart-1)+4:6*(ipart-1)+6)=rtol*v0(ipart)
    end DO
    
       CALL DLSODE (chp_df_eval,nsys,solnvec,t_initial, t_final, &
                    ITOL, RTOL, ATOL, ITASK, ISTATE, IOPT, RWORK, &
                    LRW, IWORK, LIW, JAC, MF)
    write(*,*) iwork(11)
    write(*,*) iwork(12)
    write(*,*) iwork(13)
  END SELECT

write(*,*) solnvec

!-----------------------------------------------------------------------
! Analytical Solutions
!-----------------------------------------------------------------------
! vx_f=(vx_init-ez/Bmag)*cos(qom*Bmag*t_final)+ &
!     (v_init-ex/Bmag)*sin(qom*Bmag*t_final)
! vz_f=(vy_init-ex/Bmag)*cos(qom*Bmag)+(vz_init-ex/Bmag)*sin(qom*Bmag)



!-----------------------------------------------------------------------
! Unload the final values.
!-----------------------------------------------------------------------

  DO ipart=1,npart
    ioff=6*(ipart-1)
    rptr=>solnvec(ioff+1:ioff+3) ! position vector for ipart
    vptr=>solnvec(ioff+4:ioff+6) ! velocity vector for ipart
    x_init(ipart)=rptr(1)
    y_init(ipart)=rptr(2)
    z_init(ipart)=rptr(3)
    vx_init(ipart)=vptr(1)
    vy_init(ipart)=vptr(2)
    vz_init(ipart)=vptr(3)
  ENDDO

!-----------------------------------------------------------------------
! Open a text output file and echo the parameters.
!-----------------------------------------------------------------------

  OPEN(UNIT=outunit,FILE='chpart.out',STATUS='UNKNOWN',FORM='FORMATTED')
  WRITE(UNIT=outunit,NML=scalar_input)
  WRITE(UNIT=outunit,NML=vector_input)
  CLOSE(outunit)

  END PROGRAM charged_part
