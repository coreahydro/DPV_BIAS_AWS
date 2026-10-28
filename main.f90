!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! This program finds bias of polarimetric variables for S-band dual-pol. radar using AWS !!
!! Programed by Jungsoo YOON belonging to KICT                                            !!
!! Having any question, send mail to "jungsooyoon@kict.re.kr                              !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE netcdf
logical :: lexist1,lexist2
real*4,dimension(:,:),allocatable :: zhh,zdr,kdp,rhv
real*4,dimension(:),allocatable :: zhh_zdr,zdr_zhh,zhh_kdp,kdp_zhh
real*4,dimension(:,:,:),allocatable :: zhh_sel,zdr_sel,kdp_sel,rhv_sel
real,dimension(:),allocatable :: Z1_BIAS,Z2_BIAS,ZDR_BIAS,KDP_BIAS
real*4,dimension(:,:,:),allocatable :: rrr1
real*4,dimension(:,:,:),allocatable :: arr1
real*4,dimension(:,:),allocatable :: arr1_mat
real*4,dimension(:,:),allocatable :: arr1_raw
real*4,dimension(:,:),allocatable :: arr1_opt
real*4,dimension(:,:),allocatable :: grf
real*4,dimension(:,:),allocatable :: grf1_mat
real*4,dimension(:,:),allocatable :: grf1_opt 
real*8,dimension(:),allocatable :: dist_rad_gag 
real*4 zhh_temp,zdr_temp,kdp_temp,rhv_temp
real*4 zmax1,zmax2,zdrmax,kdpmax
real*4 INE1,INE2,INE3,INE4,INE5,INE6
real*4 INE1_RAW,INE2_RAW,INE3_RAW,INE4_RAW,INE5_RAW,INE6_RAW
real*4 INE1_MAX,INE2_MAX,INE3_MAX,INE4_MAX,INE5_MAX,INE6_MAX
integer, dimension(:),allocatable :: nrx,nry,gagnum_rad
integer, dimension(:),allocatable :: ntime_dpv_sel,ntime_gag_sel,ntime_arr_sel
integer :: lagtime,nalg
character*150 :: file_par,file_gag_coord,file_rdr_coord,file_dpv,file_gag,file_dpv_bias,file_gvsr,makedir
character*150 :: RES_DIR,DPV_DIR,GAG_DIR,OUT_DIR
character*50  :: xname,yname
character :: INFILE1*5,INFORM*2,OUTFORM*2
character :: seltime*12,sel_rdr*3
character :: indate*12,datetemp*12,yyyy*4,mt*2,dy*2,hh*2,mn*2,nalg_char*2
character*12,dimension(:),allocatable :: outdate
COMMON /NPAR/ NACC_TIME,NITV_TIME,NUM_MAX_GAG,NUM_MAX_RDR,NROW_MAX,NCOL_MAX,NMAX,NUM_MAX_DPV
COMMON /LAMCON/ R,SLAT1,SLAT2,SN,SF,RO,OLAT,OLON,XO,YO,IFIRST,DD
COMMON /CALCUL/ PI,DEGRAD,RADDEG
COMMON /CHARDIR/ RES_DIR,DPV_DIR,GAG_DIR,OUT_DIR
COMMON /CHARPAR/ INFORM,OUTFORM
COMMON /ABIAS/ ABIAS_ZHH,ABIAS_ZDR,ABIAS_KDP

call cpu_time(start_time)

nget=1
call getarg(nget,sel_rdr)

!! 1. Read Parameter 
nget=nget+1
call getarg(nget,file_par)
call read_par(file_par)

!! 2. Read File Name of Gauge Information  
nget=nget+1
call getarg(nget,file_gag_coord)

!! 3. Read File Name of Radar Information  
nget=nget+1
call getarg(nget,file_rdr_coord)

!! 4. Input Time and Make Time Data 
nget=nget+1
call getarg(nget,seltime)
indate=seltime
if(NACC_TIME.gt.0) itv_time=1
if(NACC_TIME.lt.0) itv_time=-1
ntime=abs(NACC_TIME)
allocate(outdate(ntime+1))
call make_date(ntime+1,indate,outdate,NACC_TIME,itv_time)

!! 5. Input Algorithm of Radar Rain Rate Estimation  
nget=nget+1
call getarg(nget,nalg_char)
read(nalg_char(1:2), '(i2.2)') nalg

!! 6. Finding the number of cell matching between gauge and radar bin
allocate(nrx(NUM_MAX_GAG),nry(NUM_MAX_GAG),gagnum_rad(NUM_MAX_GAG),dist_rad_gag(NUM_MAX_GAG))
if(INFORM.eq.'BI '.or.INFORM.eq.'UF ') then
	allocate(zhh(NROW_MAX,NCOL_MAX),zdr(NROW_MAX,NCOL_MAX),kdp(NROW_MAX,NCOL_MAX),rhv(NROW_MAX,NCOL_MAX)) 
	call match_gag_rdr_bin(sel_rdr,NUM_MAX_GAG,NUM_MAX_RDR,file_gag_coord,file_rdr_coord,nrws,nrx,nry,gagnum_rad,dist_rad_gag)
elseif(INFORM.eq.'NC') then
	datetemp=outdate(1)
	yyyy=datetemp(1:4);mt=datetemp(5:6);dy=datetemp(7:8);hh=datetemp(9:10);mn=datetemp(11:12)
	file_dpv=trim(DPV_DIR)//'/CPNC/'//yyyy//mt//'/'//dy//'/RDR_BSL_CPI_'//datetemp//'.nc'
	call check(nf90_open(trim(file_dpv),nf90_nowrite,ncid))
	call check(nf90_get_att(ncid,nf90_global,"Latitude",rdr_lat))
	call check(nf90_get_att(ncid,nf90_global,"Longitude",rdr_lon))
	call check(nf90_get_att(ncid,nf90_global,"Max_Range",rdr_range))
	call check(nf90_get_att(ncid,nf90_global,"Grid_Size",rdr_grid))
	call check(nf90_inquire(ncid,ndim,nvar))
	call check(nf90_inquire_dimension(ncid,1,xname,nx_nc))
	call check(nf90_inquire_dimension(ncid,2,yname,ny_nc))
!!	write(*,*) file_dpv
!!  write(*,*) nx,ny,rdr_range,gridsize,rdr_lat,rdr_lon
	allocate(zhh(ny_nc,nx_nc),zdr(ny_nc,nx_nc),kdp(ny_nc,nx_nc),rhv(ny_nc,nx_nc)) 
	call match_gag_rdr_nc(NUM_MAX_GAG,NUM_MAX_RDR,file_gag_coord,rdr_lat,rdr_lon,rdr_range,rdr_grid,nrws,nrx,nry,gagnum_rad)
endif

!! 7. Read Dual-Pol Variables 
crt1_zhh=-90.0;crt1_zdr=-90.0;crt1_rhv=0.97 !!0.97
crt2_zhh=36.0;crt2_kdp=0.0;crt2_rhv=0.97
crt_rf=0.1
ndpv1=0;ndpv2=0
ix_max=3;iy_max=3
ixy_max=ix_max*iy_max
nsel_dpv=nrws*ixy_max
allocate(ntime_dpv_sel(ntime),ntime_gag_sel(ntime),ntime_arr_sel(ntime))
allocate(zhh_zdr(NUM_MAX_DPV),zdr_zhh(NUM_MAX_DPV),zhh_kdp(NUM_MAX_DPV),kdp_zhh(NUM_MAX_DPV))
allocate(zhh_sel(nrws,ixy_max,ntime),zdr_sel(nrws,ixy_max,ntime),kdp_sel(nrws,ixy_max,ntime),rhv_sel(nrws,ixy_max,ntime))
allocate(grf(NUM_MAX_GAG,ntime))

do it=1,ntime,NITV_TIME
	ntime_dpv_sel(it)=0;ntime_gag_sel(it)=0
	datetemp=outdate(it)
	yyyy=datetemp(1:4);mt=datetemp(5:6);dy=datetemp(7:8);hh=datetemp(9:10);mn=datetemp(11:12)
	write(*,*) trim(DPV_DIR)
!! 7.1. In Case of Binary Data 
	if(INFORM.eq.'BI ') then
		nget=nget+1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!		file_dpv=trim(DPV_DIR)//'/CPBI/'//yyyy//mt//'/'//dy//'/RDR_'//sel_rdr//'_SNQDM_CP15_'//datetemp//'.bin.gz'	
!!		file_dpv=trim(DPV_DIR)//'/'//yyyy//mt//'/'//dy//'/RDR_'//sel_rdr//'_SNQDM_CP15_'//datetemp//'.bin.gz'
!!		file_dpv=trim(DPV_DIR)//'/'//yyyy//mt//'/'//dy//'/RDR_'//sel_rdr//'_SFQDM_CP15_'//datetemp//'.bin.gz'
		file_dpv=trim(DPV_DIR)//'/'//yyyy//mt//'/'//dy//'/RDR_'//sel_rdr//'_SNQDM_CP15_R300KM_B1000M_'//datetemp//'.bin.gz'
!!		file_dpv=trim(DPV_DIR)//'/'//yyyy//mt//'/'//dy//'/RDR_'//sel_rdr//'_SFQDM_CP15_R300KM_B1000M_'//datetemp//'.bin.gz'
		write(*,*) trim(file_dpv)
		inquire(file=trim(file_dpv),exist=lexist1)
		if(.not.lexist1) then
			write(*,*) trim(file_dpv), " The DPV BINARY file does not exist!"
!!			write(*,*) " Stop the program"
!!			stop
		else 
			
!! 7.1.1. Read Dual-Pol. Variables in Binary Data 
!!			write(*,*) "INPUT FILE1: ",file_dpv
			ntime_dpv_sel(it)=1
			call read_bin(file_dpv,NROW_MAX,NCOL_MAX,1,zhh,ny,nx)
			call read_bin(file_dpv,NROW_MAX,NCOL_MAX,2,zdr,ny,nx)
			call read_bin(file_dpv,NROW_MAX,NCOL_MAX,3,kdp,ny,nx)
			call read_bin(file_dpv,NROW_MAX,NCOL_MAX,4,rhv,ny,nx)
			write(*,*) "------------------------------" 
			write(*,*) "Number of Radar grid : NX, NY" 
			write(*,*) "------------------------------"  			
			write(*,*) nx,ny
			!!if(it.eq.1) then
			!!write(*,*) outdate(it)
			!!do iy=1,ny
			!!	write(*,*) zhh(iy,1:nx)
			!!enddo
			!!endif
			
!! 7.1.2. Gathering Zhh - Zdr and Zhh - Kdp	
!!!!!!
			ixy=0;ixy_sel=0
			do ix=1,ix_max
				do iy=1,iy_max
					ixy=ixy+1
					do irws=1,nrws
						nytemp=nry(irws)+(-1)+(iy-1)
						nxtemp=nrx(irws)+(-1)+(ix-1)
						if(nytemp.lt.1) then
							nytemp=1
						endif
						if(nytemp.gt.ny) then
							nytemp=ny
						endif
						if(nxtemp.lt.1) then
							nxtemp=1
						endif
						if(nxtemp.gt.nx) then
							nxtemp=nx
						endif									

						zhh_temp=zhh(nytemp,nxtemp)+ABIAS_ZHH
						zdr_temp=zdr(nytemp,nxtemp)+ABIAS_ZDR
						kdp_temp=kdp(nytemp,nxtemp)+ABIAS_KDP
						rhv_temp=rhv(nytemp,nxtemp)
						if(zhh_temp.ge.crt1_zhh.and.zdr_temp.ge.crt1_zdr.and.rhv_temp.ge.crt1_rhv) then
							ndpv1=ndpv1+1
							zhh_zdr(ndpv1)=zhh_temp
							zdr_zhh(ndpv1)=zdr_temp
						endif
						if(zhh_temp.ge.crt2_zhh.and.kdp_temp.ge.crt2_kdp.and.rhv_temp.ge.crt2_rhv) then
							ndpv2=ndpv2+1
							zhh_kdp(ndpv2)=zhh_temp
							kdp_zhh(ndpv2)=kdp_temp
							!write(*,*) zhh_temp,kdp_temp,rhv_temp
						endif

						zhh_sel(irws,ixy,it)=zhh_temp
						zdr_sel(irws,ixy,it)=zdr_temp
						kdp_sel(irws,ixy,it)=kdp_temp
						rhv_sel(irws,ixy,it)=rhv_temp
!!						if(it.ge.661.and.it.le.720.and.gagnum_rad(irws).eq.945) write(*,*) it,ixy,gagnum_rad(irws) &
!!																		,nxtemp,nytemp,zhh_temp,zdr_temp,kdp_temp,rhv_temp
					enddo
				enddo
			enddo
!!!!!
		endif
	elseif(INFORM.eq.'NC ') then
		nget=nget+1
		file_dpv=trim(DPV_DIR)//'/CPNC/'//yyyy//mt//'/'//dy//'/RDR_'//sel_rdr//'_CPI_'//datetemp//'.nc'
		!write(*,*) trim(file_dpv)
		inquire(file=trim(file_dpv),exist=lexist1) 
		if(.not.lexist1) then
!!			write(*,*) " The DPV NETCDF file does not exist!"
!!			write(*,*) " Stop the program"
!!			stop
		else
!!			write(*,*) trim(file_dpv)		
			ntime_dpv_sel(it)=1			
			call read_nc(file_dpv,ny_nc,nx_nc,'CZ',zhh)	
			call read_nc(file_dpv,ny_nc,nx_nc,'DR',zdr)	
			call read_nc(file_dpv,ny_nc,nx_nc,'KD',kdp)	
			call read_nc(file_dpv,ny_nc,nx_nc,'RH',rhv)	
!!			write(*,*) ny_nc,nx_nc
!!			if(it.eq.1) then
!!			do iy=1,ny_nc
!!				write(*,*) zhh(iy,1:nx_nc)
!!			enddo
!!			endif
			
!! 7.2.2. Gathering Zhh - Zdr and Zhh - Kdp	
			ixy=0;ixy_sel=0
			do ix=1,ix_max
				do iy=1,iy_max
					ixy=ixy+1
					do irws=1,nrws
						nytemp=nry(irws)+(-1)+(iy-1)
						nxtemp=nrx(irws)+(-1)+(ix-1)
						if(nytemp.lt.1) then
							nytemp=1
						endif
						if(nytemp.gt.ny_nc) then
							nytemp=ny_nc
						endif
						if(nxtemp.lt.1) then
							nxtemp=1
						endif
						if(nxtemp.gt.nx_nc) then
							nxtemp=nx_nc
						endif									

						zhh_temp=zhh(nytemp,nxtemp)+ABIAS_ZHH
						zdr_temp=zdr(nytemp,nxtemp)+ABIAS_ZDR
						kdp_temp=kdp(nytemp,nxtemp)+ABIAS_KDP
						rhv_temp=rhv(nytemp,nxtemp)
						if(zhh_temp.ge.crt1_zhh.and.zdr_temp.ge.crt1_zdr.and.rhv_temp.ge.crt1_rhv) then
							ndpv1=ndpv1+1
							zhh_zdr(ndpv1)=zhh_temp
							zdr_zhh(ndpv1)=zdr_temp
						endif
						if(zhh_temp.ge.crt2_zhh.and.kdp_temp.ge.crt2_kdp.and.rhv_temp.ge.crt2_rhv) then
							ndpv2=ndpv2+1
							zhh_kdp(ndpv2)=zhh_temp
							kdp_zhh(ndpv2)=kdp_temp
							!write(*,*) zhh_temp,kdp_temp,rhv_temp
						endif
						
						ixy_sel=ixy_sel+1
						zhh_sel(irws,ixy,it)=zhh_temp
						zdr_sel(irws,ixy,it)=zdr_temp
						kdp_sel(irws,ixy,it)=kdp_temp
						rhv_sel(irws,ixy,it)=rhv_temp
					enddo
				enddo
			enddo			
		endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!			
	elseif(INFORM.eq.'UF ') then
		nget=nget+1
		!!file_dpv=trim(DPV_DIR)//'/'//yyyy//mt//'/'//dy//'/RDR_'//sel_rdr//'_RAW_'//datetemp//'.uf.gz'
		file_dpv=trim(DPV_DIR)//'/'//yyyy//mt//'/'//dy//'/RDR_'//sel_rdr//'_RAW_'//datetemp//'.uf'
		write(*,*) trim(file_dpv)
		inquire(file=trim(file_dpv),exist=lexist1) 
		if(.not.lexist1) then
!!			write(*,*) " The DPV BINARY file does not exist!"
!!			write(*,*) " Stop the program"
!!			stop
		else

			grid_resol=1000
			ntime_dpv_sel(it)=1			
			call uf2cp(file_dpv,NROW_MAX,NCOL_MAX,grid_resol,3,zhh,ny,nx)  !!czh
			call uf2cp(file_dpv,NROW_MAX,NCOL_MAX,grid_resol,5,zdr,ny,nx)!!zdr
			call uf2cp(file_dpv,NROW_MAX,NCOL_MAX,grid_resol,17,kdp,ny,nx) !!kdp
			call uf2cp(file_dpv,NROW_MAX,NCOL_MAX,grid_resol,9,rhv,ny,nx) !!rho
!!			write(*,*) ny,nx
!!			if(it.eq.1) then
!!			do iy=1,ny
!!				write(*,*) zhh(iy,1:nx)
!!			enddo
!!			endif
!! 7.1.2. Gathering Zhh - Zdr and Zhh - Kdp	
			ixy=0;ixy_sel=0
			do ix=1,ix_max
				do iy=1,iy_max
					ixy=ixy+1
					do irws=1,nrws
						nytemp=nry(irws)+(-1)+(iy-1)
						nxtemp=nrx(irws)+(-1)+(ix-1)
						if(nytemp.lt.1) then
							nytemp=1
						endif
						if(nytemp.gt.ny) then
							nytemp=ny
						endif
						if(nxtemp.lt.1) then
							nxtemp=1
						endif
						if(nxtemp.gt.nx) then
							nxtemp=nx
						endif									

						zhh_temp=zhh(nytemp,nxtemp)+ABIAS_ZHH
						zdr_temp=zdr(nytemp,nxtemp)+ABIAS_ZDR
						kdp_temp=kdp(nytemp,nxtemp)+ABIAS_KDP
						rhv_temp=rhv(nytemp,nxtemp)
						if(zhh_temp.ge.crt1_zhh.and.zdr_temp.ge.crt1_zdr.and.rhv_temp.ge.crt1_rhv) then
							ndpv1=ndpv1+1
							zhh_zdr(ndpv1)=zhh_temp
							zdr_zhh(ndpv1)=zdr_temp
						endif
						if(zhh_temp.ge.crt2_zhh.and.kdp_temp.ge.crt2_kdp.and.rhv_temp.ge.crt2_rhv) then
							ndpv2=ndpv2+1
							zhh_kdp(ndpv2)=zhh_temp
							kdp_zhh(ndpv2)=kdp_temp
							!write(*,*) zhh_temp,kdp_temp,rhv_temp
						endif

						zhh_sel(irws,ixy,it)=zhh_temp
						zdr_sel(irws,ixy,it)=zdr_temp
						kdp_sel(irws,ixy,it)=kdp_temp
						rhv_sel(irws,ixy,it)=rhv_temp
					enddo
				enddo
			enddo
!!!!!
		endif	

!!!!!!!!!!!!!!!!!!!
	else 
		write(*,*) "INPUT FILE1 is WRONG"
	endif
		
!! 7.2. Read Gauge Data 
	file_gag=trim(GAG_DIR)//'/'//yyyy//mt//'/'//dy//'/AWS_'//datetemp//'.txt'    !!/AWS/RAW 2014yr ~
!!	file_gag=trim(GAG_DIR)//'/'//yyyy//mt//'/'//dy//'/AWS_ACR_'//datetemp//'.txt' !!/AWS/ACR	~2013yr.
	write(*,*) file_gag
	inquire(file=trim(file_gag),exist=lexist2) 
	if(.not.lexist2) then
!!		write(*,*) " Gauge file does not exist!"
!!		write(*,*) " Stop the program"
!!		stop
	else 
		call read_aws(NUM_MAX_GAG,ntime,file_gag,it,grf,num_gag)
		ntime_gag_sel(it)=1
	endif

enddo
!!write(*,*) it,nrws,ixy

avex=0;avey=0;stdx=0;stdy=0
avej=0;avek=0;stdj=0;stdk=0
zmax1=0;zdrmax=0
zmax2=0;kdpmax=0 
!!write(*,*) NUM_MAX_DPV,ndpv1,ndpv2
call stat1(NUM_MAX_DPV,ndpv1,zhh_zdr,zdr_zhh,avex,avey,stdx,stdy)
call stat1(NUM_MAX_DPV,ndpv2,zhh_kdp,kdp_zhh,avej,avek,stdj,stdk)
call count1(NUM_MAX_DPV,ndpv1,zhh_zdr,zdr_zhh,zmax1,zdrmax)
call count2(NUM_MAX_DPV,ndpv2,zhh_kdp,kdp_zhh,zmax2,kdpmax)
write(*,100) ndpv1,zmax1,zdrmax,ndpv2,zmax2,kdpmax
100 format(i10,2f12.5,i10,2f12.5)

N_ZBIAS=12 
dZ_BIAS=1.0 !! BIAS 
allocate(Z1_BIAS(N_ZBIAS));allocate(Z2_BIAS(N_ZBIAS))
allocate(ZDR_BIAS(N_ZBIAS));allocate(KDP_BIAS(N_ZBIAS))
call ZH_ZDR_BIAS(ndpv1,zmax1,zdrmax,N_ZBIAS,dZ_BIAS,Z1_BIAS,ZDR_BIAS)
call ZH_KDP_BIAS(ndpv2,zmax2,kdpmax,N_ZBIAS,dZ_BIAS,Z2_BIAS,KDP_BIAS)

!!write(*,101) Z1_BIAS(1:N_ZBIAS)
!!write(*,101) ZDR_BIAS(1:N_ZBIAS)
!!write(*,101) KDP_BIAS(1:N_ZBIAS)

allocate(rrr1(nrws,ixy_max,ntime))
allocate(arr1(nrws,ixy_max,ntime))
allocate(arr1_mat(nrws,ntime))
allocate(arr1_raw(nrws,ntime))
allocate(arr1_opt(nrws,ntime))
allocate(grf1_mat(nrws,ntime))
allocate(grf1_opt(nrws,ntime))

nacc_time_arr=60
INE1_MAX=0.0
ib1_opt=0
do ib=1,N_ZBIAS !!10 ! N_ZBIAS
	zhh_temp=Z1_BIAS(ib);zdr_temp=ZDR_BIAS(ib);kdp_temp=KDP_BIAS(ib)
	call est_rrr(nrws,nalg,ixy_max,ntime,ntime_dpv_sel,zhh_sel,zdr_sel,kdp_sel,rhv_sel,zhh_temp,zdr_temp,kdp_temp,rrr1) !! estimation of radar rain rate

	call est_arr(nrws,ixy_max,ntime,nacc_time_arr,ntime_dpv_sel,rrr1,ntime_arr_sel,arr1) !!estimation of accumulated radar rainfall
	call est_arr_mat(NUM_MAX_GAG,nrws,ixy_max,ntime,ntime_arr_sel,gagnum_rad,grf,arr1,arr1_mat,grf1_mat) 
	call est_err_stat(nrws,ntime,crt_rf,ntime_arr_sel,arr1_mat,grf1_mat,INE1)
	if(INE1.gt.INE1_MAX) then
		INE1_MAX=INE1
		ib1_opt=ib
		do it=1,ntime
			if(ntime_arr_sel(it).eq.1) then 
				do irws=1,nrws
					arr1_opt(irws,it)=arr1_mat(irws,it)
					grf1_opt(irws,it)=grf1_mat(irws,it)
					if(ib.eq.1) then
						arr1_raw(irws,it)=arr1_mat(irws,it)
						INE1_RAW=INE1
					endif
				enddo
			endif
		enddo	
	endif

enddo

yyyy=indate(1:4);mt=indate(5:6);dy=indate(7:8);hh=indate(9:10);mn=indate(11:12)
makedir="mkdir -p "//trim(OUT_DIR)//'/'//sel_rdr//'/ALG'//nalg_char//'/'//yyyy//mt//'/'//dy
call system(makedir)

file_dpv_bias=trim(OUT_DIR)//'/'//sel_rdr//'/ALG'//nalg_char//'/'//yyyy//mt//'/'//dy// &
							'/RDR_'//sel_rdr//'_DPV_BIAS_'//yyyy//mt//dy//'.txt'

open(1,file=trim(file_dpv_bias),access='append')
!!write(1,101) indate,Z1_BIAS(ib1_opt),ZDR_BIAS(ib1_opt),KDP_BIAS(ib1_opt),INE1_RAW,INE1_MAX

if(ib1_opt.le.0) then
	write(1,101) indate,-99.9,-99.9,-99.9,-99.9,-99.9
else	
	write(1,101) indate,Z1_BIAS(ib1_opt),ZDR_BIAS(ib1_opt),KDP_BIAS(ib1_opt),INE1_RAW,INE1_MAX
endif
101 format(a12,5f10.3)
close(1)

!!file_gvsr=trim(OUT_DIR)//'/'//yyyy//mt//'/'//dy//'/RDR_'//sel_rdr//'_GvsR_'//indate//'.txt'
file_gvsr=trim(OUT_DIR)//'/'//sel_rdr//'/ALG'//nalg_char//'/'//yyyy//mt//'/'//dy// &
					'/RDR_'//sel_rdr//'_GvsR_'//yyyy//mt//dy//'.txt'
open(2,file=trim(file_gvsr),access='append')
do it=1,ntime
	datetemp=outdate(it)
	if(datetemp.eq.indate) then
		if(ntime_arr_sel(it).eq.1) then 
			do irws=1,nrws

				write(2,102) datetemp,gagnum_rad(irws),dist_rad_gag(irws),grf1_opt(irws,it),arr1_raw(irws,it),arr1_opt(irws,it)
				
				!!tempx=arr1_opt(irws,it) !! Radar rainfall
				!!tempy=grf1_opt(irws,it) !! Gauge rainfall
				!!outlier_high=-99.9;outlier_low=-99.9
				!!if(tempx.ge.0) outlier_high=6.4*(tempx**0.725)+1
				!!if(tempx.ge.0) outlier_low=0.045*(tempx**1.45)									
				!!if(tempy.ge.outlier_low.and.tempy.le.outlier_high) then				
				!!	write(2,102) datetemp,gagnum_rad(irws),grf1_opt(irws,it),arr1_raw(irws,it),arr1_opt(irws,it),outlier_low,outlier_high
				!!else
				!!	write(2,102) datetemp,gagnum_rad(irws),-88.8,-88.8,-88.8,outlier_low,outlier_high					
				!!endif

			enddo
		endif
	endif
enddo
102 format(a12,i5,10f10.3)
close(2)

call cpu_time(end_time)
cputime=end_time-start_time
write(*,*) "Time : ",cputime/60," min"

stop
end

