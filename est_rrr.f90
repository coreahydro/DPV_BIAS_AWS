subroutine est_rrr(nrws,nalg,ixy_max,ntime,ntime_dpv_sel,zhh,zdr,kdp,rhv,zhh_bias,zdr_bias,kdp_bias,rrr1)
real*4,dimension(nrws,ixy_max,ntime) :: zhh,zdr,kdp,rhv
real*4,dimension(nrws,ixy_max,ntime) :: rrr1
real*4 :: zhh_bias,zdr_bias,kdp_bias
real*4 :: zhh_temp,zhh_temp_cor,zdr_temp,zdr_temp_cor,kdp_temp,kdp_temp_cor,rhv_temp
real*4 :: zhh_mm,zhh_mm_cor,zdr_mm,zdr_mm_cor
real*4 :: rain_temp1,rain_temp2,rain_temp3,rain_temp4,rain_temp5,rain_temp6

integer, dimension(ntime) :: ntime_dpv_sel

!!write(*,*) ntime,nsel_dpv
!!write(*,*) zhh_bias,zdr_bias,kdp_bias
spval = -99.9;rrr_max=300.0
do irws=1,nrws
do ixy=1,ixy_max
	do it=1,ntime
		ntemp=ntime_dpv_sel(it)

		rrr1(irws,ixy,it)=-88.8
		
		if(ntemp.eq.1) then
			zhh_temp=zhh(irws,ixy,it)
			zdr_temp=zdr(irws,ixy,it)
			kdp_temp=kdp(irws,ixy,it)
			rhv_temp=rhv(irws,ixy,it)
			if(rhv_temp.ge.0.97) then
				if(zhh_temp.ne.spval.and.zdr_temp.ne.spval) then
					zhh_mm=10.**(zhh_temp/10.)
					zhh_temp_cor=zhh_temp+zhh_bias
					zhh_mm_cor=10.**(zhh_temp_cor/10.)
					
					zdr_mm=10.**(zdr_temp/10.)
					zdr_temp_cor=zdr_temp+zdr_bias
					zdr_mm_cor=10.**(zdr_temp_cor/10.)					
					
					kdp_temp_cor=kdp_temp+kdp_bias

!!	1) Marshall and Palmer(1948)
					rain_temp1=0.0365*(zhh_mm_cor**0.625)
					
!!	2) Bringi and Chandraseker(2001)  					
					rain_temp2=0.0067*(zhh_mm_cor**0.93)*(10.**(0.1*(-3.43)*zdr_temp_cor))
					
!!	3) Brandes et al.(2002)					
					rain_temp3=0.00746*(zhh_mm_cor**0.945)*(zdr_mm_cor**(-4.76))
					
!!	4) WRC(2016)					
					rain_temp4=0.0081*(zhh_mm_cor**0.91)*(10.**(0.1*(-4.2467)*zdr_temp_cor))
					
!!	5) CSU					
					if(kdp_temp_cor.ge.0.3.and.zhh_temp_cor.ge.38.and.zdr_temp_cor.ge.0.5) then 
						rain_temp5=90.3*(kdp_temp_cor**0.93)*(10.**(0.1*(-1.69)*zdr_temp_cor))
!!						write(*,*) "CSU(ZDR,KDP) ",zdr_temp_cor,kdp_temp_cor,rain_temp5 
					elseif(kdp_temp_cor.ge.0.3.and.zhh_temp_cor.ge.38.and.zdr_temp_cor.lt.0.5) then
						rain_temp5=40.5*(kdp_temp_cor**0.85)
!!						write(*,*) "CSU(KDP) ",rain_temp5 
					elseif((kdp_temp_cor.lt.0.3.or.zhh_temp_cor.lt.38).and.zdr_temp_cor.ge.0.5) then
						if(zhh_temp_cor.le.spval) then
							rain_temp5=spval
						else
							rain_temp5=0.0067*(zhh_mm_cor**0.93)*(10.**(0.1*(-3.43)*zdr_temp_cor))
						endif 
					elseif((kdp_temp_cor.lt.0.3.or.zhh_temp_cor.lt.38).and.zdr_temp_cor.lt.0.5) then
						if(zhh_temp_cor.le.spval) then
							rain_temp5=spval
						else
							rain_temp5=0.0170*(zhh_mm_cor**0.7143)
						endif
					endif 					
!!	6) NSSL(JPOLE)				
					if(kdp_temp_cor.lt.0) then
						sign=-1
					elseif(kdp_temp_cor.gt.0) then
						sign=1
					else
						sign=0
					endif
					
					temp_Z=0.017*(zhh_mm_cor**0.714)
					temp_Kdp=44.0*(abs(kdp_temp_cor)**0.93)*sign
					f1_Zdr=0.4+5.0*(abs(zdr_mm_cor-1)**1.3)
					f2_Zdr=0.4+3.5*(abs(zdr_mm_cor-1)**1.7)
           
					if(zhh_temp_cor.le.spval) then 
						rain_temp6=spval
					elseif(temp_Z.lt.6) then
						rain_temp6=temp_Z/f1_Zdr
					elseif(temp_Z.ge.6.and.temp_Z.lt.50) then
						if(kdp_temp_cor.gt.0.3) then
							rain_temp6=temp_Kdp/f2_Zdr
						else
							rain_temp6=temp_Z/f1_Zdr
						endif
					elseif(temp_Z.ge.50) then
						if(kdp_temp_cor.gt.0.3) then
							rain_temp6=temp_Kdp
!!						write(*,*) "NSSL(KDP) ",temp_NSSL 
						else
							rain_temp6=temp_Z/f1_Zdr
						endif
					endif				
					
					if(rain_temp1.gt.rrr_max) rain_temp1=spval
					if(rain_temp2.gt.rrr_max) rain_temp2=spval
					if(rain_temp3.gt.rrr_max) rain_temp3=spval
					if(rain_temp4.gt.rrr_max) rain_temp4=spval
					if(rain_temp5.gt.rrr_max) rain_temp5=spval
					if(rain_temp6.gt.rrr_max) rain_temp6=spval					

					if(nalg.eq.1) then
						rrr1(irws,ixy,it)=rain_temp1
					elseif(nalg.eq.2) then
						rrr1(irws,ixy,it)=rain_temp2
					elseif(nalg.eq.3) then						
						rrr1(irws,ixy,it)=rain_temp3
					elseif(nalg.eq.4) then
						rrr1(irws,ixy,it)=rain_temp4
					elseif(nalg.eq.5) then
						rrr1(irws,ixy,it)=rain_temp5
					elseif(nalg.eq.6) then
						rrr1(irws,ixy,it)=rain_temp6									
					endif				

!!		write(*,100) ntemp,zhh_temp_cor,zdr_temp_cor,kdp_temp_cor,rrr1(irws,ixy,it),rrr2(irws,ixy,it) &
!!								,rrr3(irws,ixy,it),rrr4(irws,ixy,it),rrr5(irws,ixy,it),rrr6(irws,ixy,it)					
				endif	
			endif		
!!		write(*,100) ntemp,irws,ixy,it,zhh_temp_cor,zdr_temp_cor,kdp_temp_cor,rhv_temp,rrr1(irws,ixy,it) &
!!			,rrr2(irws,ixy,it),rrr3(irws,ixy,it),rrr4(irws,ixy,it),rrr5(irws,ixy,it),rrr6(irws,ixy,it)					

		endif
	enddo
enddo
enddo

100 format(4i5,100f10.2)
return
end