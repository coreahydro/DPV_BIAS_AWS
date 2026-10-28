FF=gfortran
FC=gcc

NETCDFF_ROOT = /usr/local/netcdf-fortran
NETCDFC_ROOT = /usr/local/netcdf
RSL_ROOT    = /usr/local/trmm/GVBOX
#NETCDFF_ROOT = /usr/local/netcdf-fortran/4.4.3
#NETCDFC_ROOT = /usr/local/netcdf-c/4.4.0
#RSL_ROOT    = /usr/local/RSL/1.49

INCLUDES    = -I${NETCDFF_ROOT}/include \
	            -I${NETCDFC_ROOT}/include \
							-I${RSL_ROOT}/include \
				
LIBRARIES  = -L${NETCDFF_ROOT}/lib -lnetcdff -lz -lm \
	      		-L${NETCDFC_ROOT}/lib -lnetcdf \
						-L${RSL_ROOT}/lib -lrsl \
#						-L${RSL_ROOT}/lib -lrsl -ltirpc \
				
SRCS_C = read_bin_data.c\
         uf2cp.c\

SRCS_F = main.f90\
     read_par.f90\
     lamcproj.f\
     match_gag_rdr_bin.f90\
     match_gag_rdr_nc.f90\
     make_date.f90\
     read_nc_data.f90\
     read_aws_data.f90\
     check.f90\
     stat.f90\
     dpv_relation.f90\
     est_rrr.f90\
     est_arr.f90\
     est_arr_mat.f90\
     est_err_stat.f90\

DEPLOY_DIR      = "../BIN"

OBJS_C = ${SRCS_C:.c=.o}
OBJS_F = ${SRCS_F:.f=.o}

all : dpv_bias_aws

.SUFFIXES : .o .c .f

dpv_bias_aws : ${OBJS_C} ${OBJS_F}
	${FF} -fbounds-check -o $@ $^  $(INCLUDES) ${LIBRARIES}
	@cp -a $@ ${DEPLOY_DIR}

${OBJS_C}: ${SRCS_C} 
	${FC} -c  ${SRCS_C} $(INCLUDES) ${LIBRARIES}	
	
${OBJS_F}: ${SRCS_F}
	${FF} -c ${SRCS_F} $(INCLUDES) ${LIBRARIES}

clean :
	\rm -f *.c~ *.h~ *.o *.f~ Makefile~ dpv_bias_aws
