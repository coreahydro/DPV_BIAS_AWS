subroutine ZH_ZDR_BIAS(n,z_c,zdr_c,N_ZBIAS,dZ_BIAS,z_bias,zdr_bias)
real z_c,zdr_c,z_temp
real z_line(N_ZBIAS),zdr_line(N_ZBIAS)
real z_bias(N_ZBIAS),zdr_bias(N_ZBIAS)

z_temp=z_c
do iz=1,N_ZBIAS
 if(iz.eq.1) then
  z_bias(iz)=0.0
  zdr_bias(iz)=0.0
 else
  z_line(iz)=z_temp
  zdr_line(iz)=0.1015+0.00511*z_temp+0.00049*(z_temp**2)
!  zdr_line(iz)=ZDR_WRC(z_temp)
  z_bias(iz)=z_line(iz)-z_c
  zdr_bias(iz)=zdr_line(iz)-zdr_c
!  write(*,*) z_line(iz),zdr_line(iz),z_bias(iz),zdr_bias(iz)
  z_temp=z_temp+dZ_BIAS
 endif
enddo

return
end

real function ZDR_WRC(Z)
real Z

ZDR_WRC=0.1015+0.00511*Z+0.00049*(Z**2)

return
end

subroutine ZH_KDP_BIAS(n,z_c,kdp_c,N_ZBIAS,dZ_BIAS,z_bias,kdp_bias)
real z_c,kdp_c,z_temp
real z_line(N_ZBIAS),kdp_line(N_ZBIAS)
real z_bias(N_ZBIAS),kdp_bias(N_ZBIAS)

z_temp=z_c
do iz=1,N_ZBIAS
 if(iz.eq.1) then
  z_bias(iz)=0.0
  kdp_bias(iz)=0.0
 else
  z_line(iz)=z_temp
  kdp_line(iz)=2.575*(10.**(-13.))*(z_temp**7.5)
!  kdp_line(iz)=KDP_WRC(z_temp)
  z_bias(iz)=z_line(iz)-z_c
  kdp_bias(iz)=kdp_line(iz)-kdp_c
!  write(*,*) z_line(iz),kdp_line(iz),z_bias(iz),kdp_bias(iz)
  z_temp=z_temp+dZ_BIAS
 endif
enddo

return
end

real function KDP_WRC(Z)
real Z

KDP_WRC=2.575*(10.0**(-13.0))*(Z**7.5)

return
end
