subroutine read_par(file_par)
character*150 :: file_par
character*150 :: RES_DIR,DPV_DIR,GAG_DIR,OUT_DIR
character :: INFORM*2,OUTFORM*2

COMMON /NPAR/ NACC_TIME,NITV_TIME,NUM_MAX_GAG,NUM_MAX_RDR,NROW_MAX,NCOL_MAX,NMAX,NUM_MAX_DPV
COMMON /LAMCON/ R,SLAT1,SLAT2,SN,SF,RO,OLAT,OLON,XO,YO,IFIRST,DD
COMMON /CALCUL/ PI,DEGRAD,RADDEG
COMMON /CHARDIR/ RES_DIR,DPV_DIR,GAG_DIR,OUT_DIR
COMMON /CHARPAR/ INFORM,OUTFORM
COMMON /ABIAS/ ABIAS_ZHH,ABIAS_ZDR,ABIAS_KDP

open(1,file=trim(file_par))
!! Information for input data !!
read(1,*);read(1,*);read(1,*)
read(1,*) NACC_TIME
read(1,*) NITV_TIME
read(1,*) NUM_MAX_GAG
read(1,*) NUM_MAX_RDR
read(1,*) NROW_MAX;read(1,*) NCOL_MAX
read(1,*) NUM_MAX_DPV
read(1,101) INFORM
read(1,100) RES_DIR
read(1,100) DPV_DIR !! Dual Polarimetric Variables data of radar
read(1,100) GAG_DIR !! Gauge data
!!NACC_TIME=NACC_TIME*(-1)
NMAX=NROW*NCOL

!! Information for input data !!
read(1,*);read(1,*);read(1,*)
read(1,101) OUTFORM
read(1,100) OUT_DIR

!! Map Information !!
read(1,*);read(1,*);read(1,*)
read(1,*) R;read(1,*) SLAT1;read(1,*) SLAT2
read(1,*) OLAT;read(1,*) OLON
read(1,*) XO;read(1,*) YO
read(1,*) DD

!! Arbitrary Bias of Polarimetric Variables  !!
read(1,*);read(1,*);read(1,*)
read(1,*) ABIAS_ZHH
read(1,*) ABIAS_ZDR
read(1,*) ABIAS_KDP

write(*,*) "Arbitrary Bias of ZH: ",ABIAS_ZHH
write(*,*) "Arbitrary Bias of ZDR: ",ABIAS_ZDR
write(*,*) "Arbitrary Bias of KDP: ",ABIAS_KDP



PI = ASIN(1.)*2.
DEGRAD = PI/180.
RADDEG = 180./PI
SLAT1 = SLAT1*DEGRAD
SLAT2 = SLAT2*DEGRAD
OLAT = OLAT*DEGRAD
OLON = OLON*DEGRAD
R = R/DD

!!write(*,*) trim(INFORM)
!!write(*,*) trim(RES_DIR)
!!write(*,*) trim(DPV_DIR)
!!write(*,*) trim(GAG_DIR)
!!write(*,*) trim(OUTFORM)
!!write(*,*) trim(OUT_DIR)
!!write(*,*) R
!!write(*,*) DD

100 format(a150)
101 format(a2)

return
end
