SUBROUTINE check(istatus)
USE netcdf
IMPLICIT NONE
INTEGER,INTENT(IN) :: istatus
IF (istatus /= nf90_noerr) THEN
 write(*,*) TRIM(ADJUSTL(nf90_strerror(istatus)))
END IF
END SUBROUTINE check 
