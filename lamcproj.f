C***********************************************************************
      SUBROUTINE LAMCPROJ(ALAT,ALON,X,Y,N,IERR)
C***********************************************************************
C   *** Lambert Conformal Conic Projection ***
C
C         ALAT, ALON : (latitude,longitude) at earth  [degree]
C         X, Y       : (x,y) coordinate in map  [grid]
C          * N = 0   : (lat,lon) --> (x,y)
C          * N = 1   : (x,y) --> (lat,lon)
C-----------------------------------------------------------------------
      COMMON /LAMCON/ R,SLAT1,SLAT2,SN,SF,RO,OLAT,OLON,XO,YO,IFIRST,DD
      COMMON /CALCUL/ PI,DEGRAD,RADDEG
C
C... INITIALIZE.
C
      IF (IFIRST.EQ.0) THEN
!         CALL LAMCINIT
         IF (SLAT1.EQ.SLAT2.OR.
     1      ABS(SLAT1).GE.PI*0.5.OR.ABS(SLAT2).GE.PI*0.5) THEN
            PRINT *,'ERROR  [ LAMCPROJ ]'
            PRINT *,'(SLAT1,SLAT2) :',SLAT1*RADDEG,SLAT2*RADDEG
            STOP
         ENDIF
         SN = TAN(PI*0.25+SLAT2*0.5)/TAN(PI*0.25+SLAT1*0.5)
         SN = ALOG(COS(SLAT1)/COS(SLAT2))/ALOG(SN)
         SF = (TAN(PI*0.25+SLAT1*0.5))**SN*COS(SLAT1)/SN
         IF (ABS(OLAT).GT.89.9*DEGRAD) THEN
            IF (SN*OLAT.LT.0.0) THEN
               PRINT *,'ERROR  [ LAMCPROJ ]'
               PRINT *,'(SLAT1,SLAT2) :',SLAT1*RADDEG,SLAT2*RADDEG
               PRINT *,'(OLAT ,OLON ) :',OLAT*RADDEG,OLON*RADDEG
               STOP
            ENDIF
            RO = 0.
         ELSE
            RO = R*SF/(TAN(PI*0.25+OLAT*0.5))**SN
         ENDIF
         IFIRST = 1
      ENDIF
C
C... CONVERT.
C
      IF (N.EQ.0) THEN
         IF (ABS(ALAT).GT.89.9) THEN
            IF (SN*ALAT.LT.0.0) GOTO 999
            RA = 0.
         ELSE
            RA = R*SF/(TAN(PI*0.25+ALAT*DEGRAD*0.5))**SN
         ENDIF
         THETA = ALON*DEGRAD - OLON
         IF (THETA.GT.PI)  THETA = THETA - 2.*PI
         IF (THETA.LT.-PI) THETA = 2.*PI + THETA
         THETA = SN*THETA
         X = RA*SIN(THETA) + XO
         Y = RO - RA*COS(THETA) + YO
      ELSEIF (N.EQ.1) THEN
         XN = X - XO
         YN = RO - Y + YO
         RA = SQRT(XN*XN+YN*YN)
         IF (SN.LT.0) RA = -RA
         ALAT = (R*SF/RA)**(1./SN)
         ALAT = 2.*ATAN(ALAT)-PI*0.5
         IF (ABS(XN).LE.0.0) THEN
            THETA = 0.
         ELSE
            IF (ABS(YN).LE.0.0) THEN
               THETA = PI*0.5
               IF (XN.LT.0.0) THETA = -THETA
            ELSE
               THETA = ATAN2(XN,YN)
            ENDIF
         ENDIF
         ALON = THETA/SN + OLON
         ALAT = ALAT*RADDEG
         ALON = ALON*RADDEG
      ENDIF
      RETURN
C
C... ERROR.
C
  999 CONTINUE
      PRINT *,'OVER   [ LAMCPROJ ]'
      PRINT *,'(SLAT1,SLAT2) :',SLAT1*RADDEG,SLAT2*RADDEG
      PRINT *,'( OLAT, OLON) :',OLAT*RADDEG,OLON*RADDEG
      PRINT *,'( ALAT, ALON) :',ALAT,ALON
      IERR = 1

      RETURN
      END
C***********************************************************************
      SUBROUTINE LAMCINIT
C***********************************************************************
C   Lambert Conformal Conic Projection Initialize
C
C-----------------------------------------------------------------------
      COMMON /LAMCON/ R,SLAT1,SLAT2,SN,SF,RO,OLAT,OLON,XO,YO,IFIRST,DD
      COMMON /CALCUL/ PI,DEGRAD,RADDEG

      PI = ASIN(1.)*2.
      DEGRAD = PI/180.
      RADDEG = 180./PI

CCC FCST_QPF 
C      R = 6370.19584           ! [km]     : Earth Radius
      R = 6371.00877           ! [km]     : Earth Radius
      SLAT1 = 35.0             ! [degree] : Standard Latitude 1
      SLAT2 = 37.0             ! [degree] : Standard Latitude 2
      OLAT = 33.7              ! [degree] : Latitude of known point in map
      OLON = 124.45             ! [degree] : Longitude of known point in map
      XO = 0.0               ! [grid]   : X-coordinate of known point
      YO = -200.0               ! [grid]   : Y-coordinate of known point
      DD = 1.0                 ! [km]     : Grid distance in map

      SLAT1 = SLAT1*DEGRAD
      SLAT2 = SLAT2*DEGRAD
      OLAT = OLAT*DEGRAD
      OLON = OLON*DEGRAD
      R = R/DD

      RETURN
      END
