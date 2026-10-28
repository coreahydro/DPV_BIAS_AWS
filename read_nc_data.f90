! This program read netcdf data of which format is netcdf.
! ID and the name of variable in the data are below
!ID     Variable
!1	Height
!2	Field_Name
!3	DZ
!4	VR
!5	SW
!6	CZ
!7	DR
!8	RH
!9	PH
!10	KD
!11	SQ
!12	HC
subroutine read_nc(file_nc,NROW,NCOL,vname_sel,values_nc)
USE netcdf

real*4,dimension(NCOL,NROW) :: values_temp
real*4,dimension(NROW,NCOL) :: values_nc
real*4        :: rdr_lat,rdr_lon
integer       :: xtype,ndim,nDims,nvar,varid
integer       :: dimids(3)
character*50  :: xname,yname,vname,tempname
character*2 vname_sel
character*150 :: file_nc

COMMON /NPAR/ NACC_TIME,NUM_MAX_GAG,NUM_MAX_RDR,NROW_MAX,NCOL_MAX,NMAX,NUM_MAX_DPV
COMMON /LAMCON/ R,SLAT1,SLAT2,SN,SF,RO,OLAT,OLON,XO,YO,IFIRST,DD
COMMON /CALCUL/ PI,DEGRAD,RADDEG
COMMON /CHARDIR/ RES_DIR,DPV_DIR,GAG_DIR,OUT_DIR
COMMON /CHARPAR/ INFORM,OUTFORM
COMMON /ABIAS/ ABIAS_ZHH,ABIAS_ZDR,ABIAS_KDP

!stat=nf90_open(trim(file_nc),nf90_nowrite,ncid)
!write(*,*) stat
call check(nf90_open(trim(file_nc),nf90_nowrite,ncid))

!call check(nf90_inquire(ncid,ndim,nvar))
!call check(nf90_inquire_dimension(ncid,1,xname,nx))
!call check(nf90_inquire_dimension(ncid,2,yname,ny))

call check(nf90_inq_varid(ncid,vname_sel,varid))
call check(nf90_inquire_variable(ncid,varid,vname,xtype,nDims,dimids))

call check(nf90_get_var(ncid,varid,values_temp))

!! convert 
do iy=1,NROW
	do ix=1,NCOL
		temp=values_temp(ix,iy)
		values_nc(NROW-iy+1,ix)=temp
	enddo
enddo
!write(*,*) nx,ny,varid,rdr_range,gridsize,rdr_lat,rdr_lon
 
100 format(1000f10.2)
return
end
