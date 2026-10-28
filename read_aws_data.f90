subroutine read_aws(NUM_MAX_GAG,ntime,file_aws,it,grf,num_gag)
real*4 grf(NUM_MAX_GAG,ntime),grf_temp
character*150 :: file_aws
character*12 :: tempdate

open(1,file=trim(file_aws))
read(1,*);read(1,*);read(1,*) !!/AWS/RAW
do i=1,NUM_MAX_GAG
	read(1,100) tempdate,numg,grf_temp !!/AWS/RAW 
	if(tempdate(1:8).eq.'#7777END') goto 10 !!/AWS/RAW 
	grf(numg,it)=grf_temp !!/AWS/RAW 2014yr. ~
!!!	read(1,100,end=10) tempdate,numg,grf(numg,it) !!/AWS/ACR ~ 2013yr.
enddo
10 continue
100 format(a12,i6,63x,f7.1) !!/AWS/RAW
!!!100 format(a12,i6,14x,f7.1) !!/AWS/ACR
num_gag=i-1
close(1)

return
end