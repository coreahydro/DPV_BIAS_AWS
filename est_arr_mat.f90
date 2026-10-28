subroutine est_arr_mat(NUM_MAX_GAG,nrws,ixy_max,ntime,ntime_arr_sel,gagnum_rad,grf,arr,arr_mat,grf_mat)
real*4,dimension(nrws,ixy_max,ntime) :: arr
real*4,dimension(nrws,ntime) :: arr_mat,grf_mat
real*4,dimension(NUM_MAX_GAG,ntime) :: grf
integer, dimension(NUM_MAX_GAG) :: gagnum_rad
integer, dimension(ntime) :: ntime_arr_sel

do it=1,ntime
	if(ntime_arr_sel(it).eq.1) then 
		do irws=1,nrws
			ntemp=gagnum_rad(irws)
			grf_temp=grf(ntemp,it)
			errmin=10000
			if(grf_temp.gt.0) then
				do ixy=1,ixy_max
					arr_temp=arr(irws,ixy,it)				
					if(arr_temp.ge.0) then
						err_temp=abs(grf_temp-arr_temp)
						if(err_temp.le.errmin) then
							errmin=err_temp
							arr_mat(irws,it)=arr(irws,ixy,it)
							ixy_est=ixy
						endif
					else
						arr_mat(irws,it)=arr(irws,5,it)					
						ixy_est=5							
					endif
!!					if(it.eq.661.and.gagnum_rad(irws).eq.945) write(*,200) outdate(it),it,ixy,gagnum_rad(irws),grf_temp,arr_temp						
!!					if(it.eq.661) write(*,200) outdate(it),it,ixy,gagnum_rad(irws),grf_temp,arr_temp						
!!					if(it.eq.1) write(*,200) outdate(it),it,ixy,gagnum_rad(irws),grf_temp,arr_temp,errmin,err_temp,arr_mat(irws,it)
				enddo
				grf_mat(irws,it)=grf_temp
			else
				arr_mat(irws,it)=arr(irws,5,it)					
				ixy_est=5
				grf_mat(irws,it)=grf_temp
			endif
!			write(*,100) it,gagnum_rad(irws),ixy_est,grf_temp,arr_mat(irws,it),grf_mat(irws,it)
		enddo
	endif
enddo

100 format(3i5,100f10.2)
return
end