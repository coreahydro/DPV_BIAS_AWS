subroutine est_err_stat(nr,nt,crt_rf,nt_sel,x,y,INE)
real*4,dimension(nr,nt) :: x,y
real*4,dimension(nr*nt) :: err
integer,dimension(nt) :: nt_sel
real*4 sumx,sumy,sumxy,sumx2,sumy2
real*4 maxx,maxy
real*4 sumerr,sumerr2,sumerrabs
real*4 avex,avey,varx,vary,crossxy,varerr,stdx,stdy,stderr
real*4 ME,BS,MAE,RMSE,INE,CC
real*4 outlier_high,outlier_low

nxy=0;sumx=0;sumy=0;sumxy=0;sumx2=0;sumy2=0
sumerr=0;sumerr2=0;sumerrabs=0
sumratio=0
maxx=0;maxy=0
!!write(*,*) crt_rf
do ir=1,nr
	do it=1,nt
		ntemp=nt_sel(it)
		if(ntemp.eq.1) then
!!			write(*,*) y(ir,it),x(ir,it)
			tempx=x(ir,it) !! Radar rainfall
			tempy=y(ir,it) !! Gauge rainfall
			outlier_high=-99.9;outlier_low=-99.9
			if(tempx.ge.0) outlier_high=6.4*(tempx**0.725)+1
			if(tempx.ge.0) outlier_low=0.045*(tempx**1.45)

			if(tempx.gt.crt_rf.and.tempy.gt.crt_rf) then
!!			if(tempy.ge.outlier_low.and.tempy.le.outlier_high) then
!!				write(*,*) tempx,tempy,outlier_high,outlier_low
				nxy=nxy+1
				sumx=sumx+tempx
				sumy=sumy+tempy
				sumxy=sumxy+tempx*tempy
				sumx2=sumx2+tempx**2
				sumy2=sumy2+tempy**2

				err(nxy)=tempx-tempy
				sumerr=sumerr+err(nxy)
				sumerr2=sumerr2+err(nxy)**2
				sumerrabs=sumerrabs+abs(err(nxy))
				if(tempx.gt.maxx) maxx=tempx
				if(tempy.gt.maxy) maxy=tempy   
!!			endif		
			endif

		endif
	enddo
enddo

if(nxy.gt.0) then
 avex=sumx/nxy
 avey=sumy/nxy
 aveerr=sumerr/nxy
 ME=sumerr/nxy
 MAE=sumerrabs/nxy
else
 avex=-99.9
 avey=-99.9
 ME=-99.9
 MAE=-99.9
endif

if(nxy.gt.1) then
 varx=sumx2/(nxy-1)-nxy/(nxy-1.)*avex**2
 vary=sumy2/(nxy-1)-nxy/(nxy-1.)*avey**2
 crossxy=sumxy/(nxy-1)-nxy/(nxy-1.)*avex*avey
 varerr=sumerr2/(nxy-1)-nxy/(nxy-1.)*aveerr**2
 stdx=sqrt(varx)
 stdy=sqrt(vary)
 stderr=sqrt(varerr)
 RMSE=sqrt(sumerr2/nxy)
 CC=crossxy/(stdx*stdy)
else
 varx=-99.9
 vary=-99.9
 varerr=-99.9
 RMSE=-99.9
 CC=-99.9
endif

!!write(*,*) avex,avey

if(avey.gt.0) then
 BS=avex/avey
 INE=(1-MAE/avey)*100
 if(INE.ge.10000.or.INE.le.-10000) INE=-99.9
else
 BS=-99.9
 INE=-99.9
endif

!!write(*,101) nt
!!write(*,100) nxy,nr,avey,avex,maxy,maxx,INE,ME,BS,MAE,RMSE,CC

100 format(2i8,20f10.3)
101 format(i4,"hr  Num / Total Num / Gauge Ave. / Radar Ave. / Gauge Max / Radar Max /1-NE / ME / BS / MAE / RMSE / CC")

return
end
