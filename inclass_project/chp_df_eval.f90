subroutine chp_df_eval(nsize, time, svec, dfvec)
use chp_kind_mod
use chp_inp_mod
use chp_data_mod
implicit none

integer(iknd), intent(in) :: nsize
integer(iknd) :: ipart
real(rknd), intent(in) :: time
real(rknd), dimension(nsize), intent(in) :: svec ! knowns
real(rknd), dimension(nsize), intent(out) :: dfvec
real(rknd) :: vx, vy, vz

do ipart=1,npart
   dfvec(6*(ipart-1)+1)=svec(6*(ipart-1)+4)     ! vx
   dfvec(6*(ipart-1)+2)=svec(6*(ipart-1)+5)     ! vy
   dfvec(6*(ipart-1)+3)=svec(6*(ipart-1)+6)     ! vz

   vx=svec(6*(ipart-1)+4)
   vy=svec(6*(ipart-1)+5)
   vz=svec(6*(ipart-1)+6)
   dfvec(6*(ipart-1)+4)=qom(ipart)*(ex+vy*bz-vz*by)
   dfvec(6*(ipart-1)+5)=qom(ipart)*(ey+vz*bx-vx*bz)
   dfvec(6*(ipart-1)+6)=qom(ipart)*(ez+vx*by-vy*bx)

!   dfvec(6*(ipart-1)+4)=qom(ipart)*(ex+svec(6*(ipart-1)+5)*bz &
!                       -svec(6*(ipart-1)+6)*by)
!   dfvec(6*(ipart-1)+5)=qom(ipart)*(ey+svec(6*(ipart-1)+6)*bx &
!                       -svec(6*(ipart-1)+4)*bz)
!   dfvec(6*(ipart-1)+6)=qom(ipart)*(ez+svec(6*(ipart-1)+4)*by &
!                       -svec(6*(ipart-1)+5)*bx)
end do

end subroutine chp_df_eval
