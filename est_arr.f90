subroutine est_arr(nrws,ixy_max,ntime,nacc_time_arr,ntime_dpv_sel,rrr,ntime_arr_sel,arr)
real*4,dimension(nrws,ixy_max,ntime) :: rrr,arr
integer, dimension(ntime) :: ntime_dpv_sel,ntime_arr_sel

spval = -99.9;rrr_max=300.0
do irws=1,nrws
	do ixy=1,ixy_max
		do it=1,ntime,nacc_time_arr
			nst=it;net=it+nacc_time_arr-1 !!nst(start time), net(end time)
			temp_sum=0;nsum=0
			do iact=nst,net
				ntime_arr_sel(iact)=0
				ntemp=ntime_dpv_sel(iact)
				if(ntemp.eq.1) then
					temp=rrr(irws,ixy,iact)
					if(temp.eq.spval) temp=0
					if(temp.ge.0) then	
						temp_sum=temp_sum+temp				
						nsum=nsum+1
					endif
!!					if(irws.eq.155) write(*,*) iact,temp,temp_sum,nsum
				endif	
			enddo

			ntime_arr_sel(it)=1
			if(nsum.eq.0) then
				arr(irws,ixy,it)=-99.9
			else
				arr(irws,ixy,it)=temp_sum/nsum * (nacc_time_arr/60.0) !! mm/hr convert to mm
			endif
!!			if(irws.eq.11) write(*,*) irws,ixy,it,ntime_arr_sel(it),temp_sum,nsum,arr(irws,ixy,it) !! irws =11 (AWS No. 155)

		enddo
	enddo
enddo

100 format(i3,100f10.2)
return
end