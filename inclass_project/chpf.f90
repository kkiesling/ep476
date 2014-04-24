Subroutine chpf(NEQ, T, Y, YDOT)

           use chp_kind_mod
           use chp_inp_mod
           use chp_data_mod
           INTEGER NEQ
           DOUBLE PRECISION  T, Y(*), YDOT(*)
           INTEGER(iknd) :: ipart


do ipart=1,npart
   YDOT(6*(ipart-1)+1)=Y(6*(ipart-1)+4)     ! vx
   YDOT(6*(ipart-1)+2)=Y(6*(ipart-1)+5)   ! vy
   YDOT(6*(ipart-1)+3)=Y(6*(ipart-1)+6)   ! vz
   YDOT(6*(ipart-1)+4)=qom(ipart)*(ex+vy_init(ipart)*bz-vz_init(ipart)*by)
   YDOT(6*(ipart-1)+5)=qom(ipart)*(ey+vz_init(ipart)*bx-vx_init(ipart)*bz)
   YDOT(6*(ipart-1)+6)=qom(ipart)*(ez+vx_init(ipart)*by-vy_init(ipart)*bx)
end do

end subroutine chpf
