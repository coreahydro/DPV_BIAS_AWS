subroutine count1(nrt,nr,x,y,zmax,zdrmax)
!character*150 fileout
real x(nrt),y(nrt)
real z,zdr,dx,dy,zmax,zdrmax

dy=0.1
dx=0.5
z=0
n=0

!open(99,file=trim(fileout))

nmax=0
do ix=1,120
 zdr=-6
 do iy=1,120
  n=n+1
  ntemp=0
  do i=1,nr
   if(x(i).gt.z.and.x(i).le.z+dx) then
    if(y(i).gt.zdr.and.y(i).le.zdr+dy) then
     ntemp=ntemp+1
    endif
   endif   
  enddo
  if(ntemp.gt.nmax) then
   nmax=ntemp
   zmax=z+dx/2
   zdrmax=zdr+dy/2
  endif
!  write(*,100) z+dx,zdr+dy,ntemp
  zdr=zdr+dy  
 enddo
 z=z+dx
enddo
100 format(2f9.3,i6)

return
end 

subroutine count2(nrt,nr,x,y,zmax,kdpmax)
!character*150 fileout
real x(nrt),y(nrt)
real z,kdp,dx,dy,zmax,kdpmax

dy=0.1
dx=0.5
z=0
n=0

!open(99,file=trim(fileout))

nmax=0
do ix=1,120
 kdp=-2
 do iy=1,120
  n=n+1
  ntemp=0
  do i=1,nr
   if(x(i).gt.z.and.x(i).le.z+dx) then
    if(y(i).gt.kdp.and.y(i).le.kdp+dy) then
     ntemp=ntemp+1
    endif
   endif   
  enddo
  if(ntemp.gt.nmax) then
   nmax=ntemp
   zmax=z+dx/2
   kdpmax=kdp+dy/2
  endif
!  write(*,100) z+dx,kdp+dy,ntemp
  kdp=kdp+dy  
 enddo
 z=z+dx
enddo
100 format(2f9.3,i6)

return
end 

subroutine LSM(nrt,nr,x,y)
real x(nrt),y(nrt)
real*8 error,sumerror,rmse
real*8 Z,ZDR

a=0.153
b=0.205
dzdr=-5
rmse_min=10000
do izdr=1,100
 dZ=0
 do iz=1,100
  sumerror=0
  do i=1,nr
   dBZ=x(i)+dZ
   Z=10**(dBZ/10.)
   ZDR_measure=y(i)+dzdr
   ZDR_est=a*(Z**b)
   error=ZDR_measure-ZDR_est
   sumerror=sumerror+error**2   
!   write(*,*) dBZ,Z,ZDR_measure,ZDR_est,sumerror
  enddo
  rmse=sqrt(sumerror/nr)
!  write(*,*) izdr,iz,sumerror,rmse,dZ,dzdr

  if(rmse.lt.rmse_min) then
   rmse_min=rmse
   dZ_min=dZ
   dzdr_min=dzdr
!   write(*,*) izdr,iz,rmse,dZ,dzdr
  endif    
  write(*,100) izdr,iz,dZ,dzdr,rmse,rmse_min,dZ_min,dzdr_min
100 format(2i4,10f8.3)

  dZ=dZ+0.5
 enddo
 dzdr=dzdr+0.1
enddo

write(*,*) rmse_min,dZ_min,dzdr_min
return
end 

subroutine stat1(nrt,nr,x,y,avex,avey,stdx,stdy)
real x(nrt),y(nrt)
real sumx,sumy,sumxy,sumx2,sumy2
real avex,avey,varx,vary,crossxy,stdx,stdy

nxy=0;sumx=0;sumy=0;sumxy=0;sumx2=0;sumy2=0

do i=1,nr
 nxy=nxy+1
 sumx=sumx+x(i)
 sumy=sumy+y(i) 
 sumxy=sumxy+x(i)*y(i)
 sumx2=sumx2+x(i)**2
 sumy2=sumy2+y(i)**2 
enddo

if(nxy.gt.0) then
 avex=sumx/nxy
 avey=sumy/nxy
else
 avex=-99.9
 avey=-99.9
endif

if(nxy.gt.1) then
 varx=sumx2/(nxy-1)-nxy/(nxy-1.)*avex**2
 vary=sumy2/(nxy-1)-nxy/(nxy-1.)*avey**2
 crossxy=sumxy/(nxy-1)-nxy/(nxy-1.)*avex*avey
 stdx=sqrt(varx)
 stdy=sqrt(vary)
 CC=crossxy/(stdx*stdy)
else
 varx=-99.9
 vary=-99.9
endif

return 
end
