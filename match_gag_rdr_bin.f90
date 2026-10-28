subroutine match_gag_rdr_bin(sel_rad,num_gag,num_radar,file_gag,file_rdr,nrws,nrx,nry,gagnum_rad,dist_rad_gag)
real, dimension(num_radar) :: rad_range,rad_grid 
real*8, dimension(num_radar) :: radlat,radlon,rad_y,rad_x
real*4 :: lat_temp,lon_temp,x_temp,y_temp
real*8 :: dx,dy,dR
real*8, dimension(num_gag) :: gaglat,gaglon,gag_y,gag_x,dist_rad_gag
integer, dimension(num_radar) :: radnum
integer, dimension(num_gag) :: gagnum,gagnum_rad,nrx,nry
character*150 :: file_gag,file_rdr
character*3, dimension(num_radar) :: radnam
character*3 sel_rad

COMMON /LAMCON/ R,SLAT1,SLAT2,SN,SF,RO,OLAT,OLON,XO,YO,IFIRST,DD
COMMON /CALCUL/ PI,DEGRAD,RADDEG

if(DD.ne.1.0) then
	write(*,*) "Wrong Input DD(grid size)"
	write(*,*) "Input 1km to DD(grid size)"	
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

!! 2. Read Information of Radars
open(2,file=trim(file_rdr))
do ir=1,10000
	read(2,*,end=11) radnum(ir),radnam(ir),radlat(ir),radlon(ir),rad_range(ir),rad_grid(ir)
	lat_temp=radlat(ir);lon_temp=radlon(ir)
	call LAMCPROJ(lat_temp,lon_temp,x_temp,y_temp,0,IERR) 
	rad_y(ir)=y_temp*DD*1000;rad_x(ir)=x_temp*DD*1000               
!!	write(*,*) radnum(ir),radnam(ir),radlat(ir),radlon(ir),rad_y(ir),rad_x(ir),rad_range(ir),rad_grid(ir)
	if(radnam(ir).eq.sel_rad) nsel_rad=radnum(ir) 
enddo
11 continue
nrad=ir-1
close(1);close(2)
!!write(*,*) nsel_rad
!! 3. Finding the number of cell matching between gauge and radar bin 
gridsize=DD*1000.0
do ir=nsel_rad,nsel_rad
! write(*,*) rad_x(ir),rad_y(ir),radlat(ir),radlon(ir)
 nradx=rad_range(ir)+1
 nrady=nradx
 nrws=0
 write(*,*) "------------------------------" 
 write(*,*) "Center of Radar grid : NX, NY" 
 write(*,*) nradx,nrady
 write(*,*) "------------------------------" 
 do ia=1,ngag
  dx=gag_x(ia)-rad_x(ir)
!  dy=-(gag_y(ia)-rad_y(ir)) !!좌측상단이 (1,1)일 경우
  dy=(gag_y(ia)-rad_y(ir)) !!좌측하단이 (1,1)일 경우
  dR=sqrt(dx**2+dy**2)/1000.
!!  if(dR.le.rad_range(ir)) then
  if(dR.le.150.0) then
   nrws=nrws+1
   nrx(nrws)=nradx+int(dx/gridsize) 
   nry(nrws)=nrady+int(dy/gridsize) 
   dist_rad_gag(nrws)=dR
   gagnum_rad(nrws)=gagnum(ia)
   write(*,*) gagnum_rad(nrws),dist_rad_gag(nrws),nrx(nrws),nry(nrws)
  endif
 enddo
enddo

return
end
