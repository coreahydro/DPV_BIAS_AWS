subroutine make_date(NTIME,indate,outdate,lagtime,itv)
integer day(1900:2100,12) 
character*12 indate,datetemp
character*12 outdate(NTIME)

!!write(*,*) indate
read(indate(1:4),'(i4)') nyear_i
read(indate(5:6),'(i2.2)') nmon_i
read(indate(7:8),'(i2.2)') nday_i
read(indate(9:10),'(i2.2)') nhr_i
read(indate(11:12),'(i2.2)') nmin_i
!!read(indate(13:14),'(i2.2)') nsec_i

call cal_day(day,nyear_i) 

if(lagtime.gt.0) then !!forward
	ntemp=1
	outdate(ntemp)=indate
	sum_itv=0
	nmin_o=nmin_i;nhr_o=nhr_i;nday_o=nday_i;nmon_o=nmon_i;nyear_o=nyear_i	
	do while(sum_itv.ne.lagtime)
		ntemp=ntemp+1
		nmin_o=nmin_o+itv
		sum_itv=sum_itv+itv
		if(nmin_o.ge.60) then
			nmin_o=nmin_o-60
			nhr_o=nhr_o+1					
			if(nhr_o.ge.24) then
				nhr_o=nhr_o-24
				nday_o=nday_o+1
				if(nday_o.gt.day(nyear_o,nmon_o)) then
					nday_o=nday_o-day(nyear_o,nmon_o)
					nmon_o=nmon_o+1
					if(nmon_o.gt.12) then
						nmon_o=nmon_o-12
						nyear_o=nyear_o+1
					else
						nyear_o=nyear_o			
					endif
				else
					nmon_o=nmon_o
					nyear_o=nyear_o
				endif
			endif
		endif
	write(datetemp(1:4),'(i4)') nyear_o
	write(datetemp(5:6),'(i2.2)') nmon_o
	write(datetemp(7:8),'(i2.2)') nday_o
	write(datetemp(9:10),'(i2.2)') nhr_o
	write(datetemp(11:12),'(i2.2)') nmin_o 
	!!write(datetemp(13:14),'(i2.2)') nsec_o 
	outdate(ntemp)=datetemp
	!!write(*,*) datetemp
	enddo
 
elseif(lagtime.lt.0) then !!backward
	ntemp=1
	outdate(ntemp)=indate
	sum_itv=0
	nmin_o=nmin_i;nhr_o=nhr_i;nday_o=nday_i;nmon_o=nmon_i;nyear_o=nyear_i	
	do while(sum_itv.ne.lagtime) 
		ntemp=ntemp+1
		nmin_o=nmin_o+itv
		sum_itv=sum_itv+itv
		if(nmin_o.lt.0) then
			nmin_o=nmin_o+60
			nhr_o=nhr_o-1
			if(nhr_o.lt.0) then
				nhr_o=nhr_o+24
				nday_o=nday_o-1
				if(nday_o.lt.1) then
					nmon_o=nmon_o-1
					if(nmon_o.lt.1) then
						nmon_o=nmon_o+12
						nyear_o=nyear_o-1
					else
						nyear_o=nyear_o
					endif
					nday_o=nday_o+day(nyear_o,nmon_o)
				else
					nmon_o=nmon_o
					nyear_o=nyear_o
				endif
			endif
		endif
	write(datetemp(1:4),'(i4)') nyear_o
	write(datetemp(5:6),'(i2.2)') nmon_o
	write(datetemp(7:8),'(i2.2)') nday_o
	write(datetemp(9:10),'(i2.2)') nhr_o
	write(datetemp(11:12),'(i2.2)') nmin_o 
	!!write(datetemp(13:14),'(i2.2)') nsec_o 
	outdate(ntemp)=datetemp
	!!write(*,*) datetemp
	enddo	

elseif(lagtime.eq.0) then
 nyear_o=nyear_i
 nday_o=nday_i
 nmon_o=nmon_i
 nhr_o=nhr_i
 nmin_o=nmin_i
 !!nsec_o=nsec_i	
	write(datetemp(1:4),'(i4)') nyear_o
	write(datetemp(5:6),'(i2.2)') nmon_o
	write(datetemp(7:8),'(i2.2)') nday_o
	write(datetemp(9:10),'(i2.2)') nhr_o
	write(datetemp(11:12),'(i2.2)') nmin_o 
	!!write(datetemp(13:14),'(i2.2)') nsec_o 	
endif

100 format(a12)

return
end

subroutine cal_day(day,ns)                                                        
integer day(1900:2100,12)                                                        
                                                                                 
do i=ns-1,ns+1                                                                       
 do j=1,12                                                                       
  if(j.eq.1) day(i,j)=31                                                         
                                                                                 
  if(j.eq.2) then                                                                
   if(mod(i,4).eq.0) then                                                        
    if(mod(i,100).eq.0) then                                                     
     day(i,j)=28                                                                 
	 if(mod(i,400).eq.0) day(i,j)=29                                               
	else                                                                           
     day(i,j)=29                                                                 
	endif                                                                          
   else                                                                          
    day(i,j)=28                                                                  
   endif                                                                         
  endif                                                                          
                                                                                 
  if((j.eq.3).or.(j.eq.5).or.(j.eq.7).or.(j.eq.8).or.(j.eq.10).or.(j.eq.12)) then
   day(i,j)=31                                                                   
  elseif ((j.eq.4).or.(j.eq.6).or.(j.eq.9).or.(j.eq.11))then                     
   day(i,j)=30                                                                   
  endif                                                                          
 enddo                                                                           
enddo                                                                            
                                                                                 
return                                                                           
end                                                                              
