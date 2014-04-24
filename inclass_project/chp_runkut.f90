subroutine chp_runkut(chp_df_eval,rk_type,nint,nsize,t0,t1,soln)
use chp_kind_mod
implicit none

character(*), intent(in) :: rk_type
integer(iknd), intent(in) :: nint, nsize
real(rknd), intent(in) :: t0, t1
real(rknd), dimension(nsize), intent(inout) :: soln

external :: chp_df_eval
integer(iknd) :: iint, ii
real(rknd) :: dt, tt, time
real(rknd), dimension(nsize) :: svec, dfvec
real(rknd), dimension(nsize) :: svec1, svec2, svec3
real(rknd), dimension(nsize) :: dsoln
real(rknd), dimension(nsize) :: pred1, pred2, pred3, pred4

real(rknd), dimension(nsize) :: d1, d2, d3, d4

svec=soln
dfvec=0._rknd
svec1=0._rknd
svec2=0._rknd
svec3=0._rknd
d1=0._rknd
d2=0._rknd
d3=0._rknd
d4=0._rknd

dt = (t1-t0)/real(nint,rknd)
tt=t0

Select case(rk_type)
case("RK2","rk2")
  do iint=1,nint        
     call chp_df_eval(nsize, tt, svec, dfvec)
     do ii=1,nsize
     svec1(ii)=svec(ii)+dt*0.5_rknd*dfvec(ii)
     end do
     time=tt+dt*0.5_rknd
     call chp_df_eval(nsize,time,svec1,dfvec)
     do ii=1,nsize
     svec(ii)=svec(ii)+dt*dfvec(ii)
     end do
     tt=tt+dt
  end do
  soln=svec
case("RK4","rk4")
  do iint=1,nint
!     call chp_df_eval(nsize, tt, svec, dfvec)
!     d1=dt*dfvec

!     svec1=svec+0.5_rknd*d1
!     time=tt+dt/2._rknd
!     call chp_df_eval(nsize, time, svec1, dfvec)
!     d2=dt*dfvec

!     svec2=svec1+0.5_rknd*d2
!     time=tt+dt/2._rknd
!     call chp_df_eval(nsize, time, svec2, dfvec)
!     d3=dt*dfvec

!     svec3=svec2+d3
!     time=tt+dt
!     call chp_df_eval(nsize, time, svec3, dfvec)
!     d4=dt*dfvec

     !svec=svec+1._rknd/6._rknd*(d1+2._rknd*d2+2._rknd*d3+d4)

     !tt=tt+dt
	 
	 CALL chp_df_eval(nsize,tt,soln,dsoln)
          pred1 = dt*dsoln
          CALL chp_df_eval(nsize,tt+dt/2,soln+pred1/2,dsoln)
          pred2 = dt*dsoln
          CALL chp_df_eval(nsize,tt+dt/2,soln+pred2/2,dsoln)
          pred3 = dt*dsoln
          CALL chp_df_eval(nsize,tt+dt,soln+pred3,dsoln)
          pred4 = dt*dsoln
          
          soln = soln+(pred1+2*pred2+2*pred3+pred4)/6
          tt=tt+dt
          
  end do
     !soln=svec
case default
  write(*,*) "unrecognized runge-kutta integration type"
  stop
end select

return
end subroutine chp_runkut
