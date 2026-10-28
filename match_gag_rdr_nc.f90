subroutine match_gag_rdr_nc(num_gag,num_radar,file_gag,rad_lat,rad_lon,rad_range,rad_grid,nrws,nrx,nry,gagnum_rad)
real*4 :: lat_temp,lon_temp,x_temp,y_temp
real*8 :: rad_x,rad_y
real*8 :: dx,dy,dR
real*8, dimension(num_gag) :: gaglat,gaglon,gag_y,gag_x,dist
integer, dimension(num_radar) :: radnum
integer, dimension(num_gag) :: gagnum,gagnum_rad,nrx,nry
character*150 :: file_gag

COMMON /LAMCON/ R,SLAT1,SLAT2,SN,SF,RO,OLAT,OLON,XO,YO,IFIRST,DD
COMMON /CALCUL/ PI,DEGRAD,RADDEG

!write(*,*) DD,rad_grid
if(DD.ne.rad_grid) then
	write(*,*) "Wrong Input DD(grid size)"
	write(*,*) "Input 0.25km to DD(grid size)"	
	stop
endif
!write(*,*) file_gag
!write(*,*) file_rdr
open(1,file=trim(file_gag))

!! 1. Read Information of Gauges
do ig=1,10000
	read(1,*,end=10) gagnum(ig),gaglat(ig),gaglon(ig) 
	lat_temp=gaglat(ig);lon_temp=gaglon(ig)
	call LAMCPROJ(lat_temp,lon_temp,x_temp,y_temp,0,IERR) 
	gag_y(ig)=y_temp*DD*1000;gag_x(ig)=x_temp*DD*1000
enddo
10 continue
ngag=ig-1

call LAMCPROJ(rad_lat,rad_lon,x_temp,y_temp,0,IERR) 
rad_y=y_temp*DD*1000;rad_x=x_temp*DD*1000     

close(1);

!! 3. Finding the number of cell matching between gauge and radar bin 
!write(*,*) rad_x,rad_y,rad_lat,rad_lon
gridsize=rad_grid*1000 !! meter
nradx=rad_range/rad_grid+1 !! rad_range(km),rad_grid(km)
nrady=nradx
nrws=0
do ia=1,ngag
 dx=gag_x(ia)-rad_x
! dy=-(gag_y(ia)-rad_y) !!좌측상단이 (1,1)일 경우
 dy=(gag_y(ia)-rad_y) !!좌측하단이 (1,1)일 경우
 dR=sqrt(dx**2+dy**2)/1000.
 if(dR.le.rad_range) then
  nrws=nrws+1
  nrx(nrws)=nradx+int(dx/gridsize) 
  nry(nrws)=nrady+int(dy/gridsize) 
  dist(nrws)=dR
  gagnum_rad(nrws)=gagnum(ia)
!  write(*,*) gagnum_rad(nrws),dR,nrx(nrws),nry(nrws)
!  write(*,*) gag_x(ia),gag_y(ia),rad_x,rad_y
 endif
enddo

return
end
