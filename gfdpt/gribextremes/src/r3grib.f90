!
! This file reads GRIB1 files for the NCEP GES, ANL and ECMWF ANL
! and stores matching mandatory heights, wds and tmps
! into respective three dimensional arrays for further use 
!
! Author: V. Krishna Kumar NCEP/Central Operations/SIB September 2010 
! Mod   : Bradley Ballish  NCEP/Central Operations/PMB November 2010 
!
!.... use fcmpsp to compile....
!     KPDS         INTEGER (200) UNPACKED PDS PARAMETERS
!     KGDS         INTEGER (200) UNPACKED GDS PARAMETERS
!     JPDS         INTEGER (200) PDS PARAMETERS FOR WHICH TO SEARCH
!                  (=-1 FOR WILDCARD)
!          (1)   - ID OF CENTER
!          (2)   - GENERATING PROCESS ID NUMBER
!          (3)   - GRID DEFINITION
!          (4)   - GDS/BMS FLAG (RIGHT ADJ COPY OF OCTET 8)
!          (5)   - INDICATOR OF PARAMETER
!          (6)   - TYPE OF LEVEL
!          (7)   - HEIGHT/PRESSURE , ETC OF LEVEL
!          (8)   - YEAR INCLUDING (CENTURY-1)
!          (9)   - MONTH OF YEAR
!          (10)  - DAY OF MONTH
!          (11)  - HOUR OF DAY
!          (12)  - MINUTE OF HOUR
!          (13)  - INDICATOR OF FORECAST TIME UNIT
!          (14)  - TIME RANGE 1
!          (15)  - TIME RANGE 2
!          (16)  - TIME RANGE FLAG
!          (17)  - NUMBER INCLUDED IN AVERAGE
!          (18)  - VERSION NR OF GRIB SPECIFICATION
!          (19)  - VERSION NR OF PARAMETER TABLE
!          (20)  - NR MISSING FROM AVERAGE/ACCUMULATION
!          (21)  - CENTURY OF REFERENCE TIME OF DATA
!          (22)  - UNITS DECIMAL SCALE FACTOR
!          (23)  - SUBCENTER NUMBER
!          (24)  - PDS BYTE 29, FOR NMC ENSEMBLE PRODUCTS
!                  128 IF FORECAST FIELD ERROR
!                   64 IF BIAS CORRECTED FCST FIELD
!                   32 IF SMOOTHED FIELD
!                  WARNING: CAN BE COMBINATION OF MORE THAN 1
!          (25)  - PDS BYTE 30, NOT USED
!     JGDS         INTEGER (200) GDS PARAMETERS FOR WHICH TO SEARCH
!                  (ONLY SEARCHED IF JPDS(3)=255)
!                  (=-1 FOR WILDCARD)
!          (1)   - DATA REPRESENTATION TYPE
!          (19)  - NUMBER OF VERTICAL COORDINATE PARAMETERS
!          (20)  - OCTET NUMBER OF THE LIST OF VERTICAL COORDINATE
!                  PARAMETERS
!                  OR
!                  OCTET NUMBER OF THE LIST OF NUMBERS OF POINTS
!                  IN EACH ROW
!                  OR
!                  255 IF NEITHER ARE PRESENT
!          (21)  - FOR GRIDS WITH PL, NUMBER OF POINTS IN GRID
!          (22)  - NUMBER OF WORDS IN EACH ROW
!       LATITUDE/LONGITUDE GRIDS
!          (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
!          (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
!          (4)   - LA(1) LATITUDE OF ORIGIN
!          (5)   - LO(1) LONGITUDE OF ORIGIN
!          (6)   - RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
!          (7)   - LA(2) LATITUDE OF EXTREME POINT
!          (8)   - LO(2) LONGITUDE OF EXTREME POINT
!          (9)   - DI LONGITUDINAL DIRECTION OF INCREMENT
!          (10)  - DJ LATITUDINAL DIRECTION INCREMENT
!          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
!
!   OUTPUT ARGUMENTS OF GETGB:
!     KF           INTEGER NUMBER OF DATA POINTS UNPACKED
!     K            INTEGER MESSAGE NUMBER UNPACKED
!                  (CAN BE SAME AS J IN CALLING PROGRAM
!                  IN ORDER TO FACILITATE MULTIPLE SEARCHES)
!     KPDS         INTEGER (200) UNPACKED PDS PARAMETERS
!     KGDS         INTEGER (200) UNPACKED GDS PARAMETERS
!     LB           LOGICAL*1 (KF) UNPACKED BITMAP IF PRESENT
!     F            REAL (KF) UNPACKED DATA
!     IRET         INTEGER RETURN CODE
!                    0      ALL OK
!                    96     ERROR READING INDEX FILE
!                    97     ERROR READING GRIB FILE
!                    98     NUMBER OF DATA POINTS GREATER THAN JF
!                    99     REQUEST NOT FOUND
!                    OTHER  W3FI63 GRIB UNPACKER RETURN CODE
!******************************************************
       CHARACTER (len=80) :: input_gfs, input_ecmwf, input_ges
!
       REAL, ALLOCATABLE, DIMENSION(:,:,:) :: hgt, tmp, uwd, vwd, rh
       REAL, ALLOCATABLE, DIMENSION(:,:,:) :: hgte, tmpe, uwde, vwde, rhe
       REAL, ALLOCATABLE, DIMENSION(:,:,:) :: hgtg, tmpg, uwdg, vwdg, rhg
       REAL, ALLOCATABLE, DIMENSION(:) :: tbias,zbias,wbias,trms,zrms,wrms
       DIMENSION nlevs(14)
       DIMENSION wlat(181)
       DIMENSION pwgtz(14),pwgtt(14)
!
       INTEGER :: ilev,i,j,k,n,nr,nlev, ii, ll, status, ierr, leng, lu
       DATA nlevs/10,20,50,100,150,200,250,300,400,500,700,850,925,1000/
       DATA pwgtz/0.5,0.7,0.8,0.9,1.0,1.0,1.0,1.0,1.0,1.0,0.7,0.6,0.5,0.3/
       DATA pwgtt/0.5,0.7,0.8,0.9,1.0,1.0,1.0,1.0,1.0,0.8,0.5,0.5,0.4,0.3/
!
       idim = 360
       jdim = 181
       nlev = 14
       PI=4.0*ATAN(1.0)
!
       call GETENV("input_gfs",input_gfs)
       write(*,*) "input_gfs= ", input_gfs
!
       call GETENV("input_ecmwf",input_ecmwf)
       write(*,*) "input_ecmwf= ", input_ecmwf
!
       call GETENV("input_ges",input_ges)
       write(*,*) "input_ges= ", input_ges  

       print*,'idim,jdim  ',idim,jdim

       ALLOCATE (tbias(nlev),zbias(nlev),wbias(nlev), STAT=istatusb)
       ALLOCATE (trms(nlev),zrms(nlev),wrms(nlev), STAT=istatusr)
       ALLOCATE (hgt(idim,jdim,nlev), tmp(idim,jdim,nlev), uwd(idim,jdim,nlev), vwd(idim,jdim,nlev), rh(idim,jdim,nlev), STAT=status)
       If (status .ne. 0 ) THEN
           write(*,*) "Allocation is not successful! Stop here."
           stop
       END IF 

       ALLOCATE (hgte(idim,jdim,nlev), tmpe(idim,jdim,nlev), uwde(idim,jdim,nlev), vwde(idim,jdim,nlev), rhe(idim,jdim,nlev), STAT=status)
       If (status .ne. 0 ) THEN
           write(*,*) "Allocation is not successful! Stop here."
           stop
       END IF

       ALLOCATE (hgtg(idim,jdim,nlev), tmpg(idim,jdim,nlev), uwdg(idim,jdim,nlev), vwdg(idim,jdim,nlev), rhg(idim,jdim,nlev), STAT=status)
       If (status .ne. 0 ) THEN
           write(*,*) "Allocation is not successful! Stop here."
           stop
       END IF

! calculate average cosine weights, skip poles in this loop
       DO j = 2, jdim -1
       ang1=pi*float(91-(j+1))/180.0
       ang2=pi*float(91-(j-1))/180.0
       wlat(j)=abs(sin(ang1)-sin(ang2))
       ENDDO

! calculate average cosine weights, at poles, j=1 and 181
       ang1=pi*float(91-1)/180.0   ! is equivalent to 90 degrees
       ang1=pi*float(91-2)/180.0   
       wlat(1)=abs(sin(ang1)-sin(ang2))
       wlat(181)=wlat(1)

       DO i = 1, idim
       DO j = 1, jdim
       DO k = 1, nlev
            hgt(i,j,k) = -999
            tmp(i,j,k) = -999
            uwd(i,j,k) = -999
            vwd(i,j,k) = -999
            rh(i,j,k) = -999
            hgte(i,j,k) = -999
            tmpe(i,j,k) = -999
            uwde(i,j,k) = -999
            vwde(i,j,k) = -999
            rhg(i,j,k) = -999
            hgtg(i,j,k) = -999
            tmpg(i,j,k) = -999
            uwdg(i,j,k) = -999
            vwdg(i,j,k) = -999
            rhg(i,j,k) = -999
       END DO
       END DO
       END DO 

!  Read GFS grib data and store them in hgt, tmp, uwd, vwd, rh
       lu = 11
       leng = LEN_TRIM(input_gfs)
       write(*,*)  leng, input_gfs
       CALL read_grib(lu, input_gfs, leng, idim, jdim, nlev, hgt, tmp, uwd, vwd, rh, ierr) 
       Print*,'ierr=  ', ierr

!  Read ECMWF grib data and store them in hgte, tmpe, uwde, vwde, rhe
       lu = 12
       leng = LEN_TRIM(input_ecmwf)
       write(*,*)  leng, input_ecmwf
       CALL read_grib(lu, input_ecmwf, leng, idim, jdim, nlev, hgte, tmpe, uwde, vwde, rhe, ierr)
       Print*,'ierr=  ', ierr


!  Read GES grib data and store them in hgtg, tmpg, uwdg, vwdg, rhg
       lu = 13
       leng = LEN_TRIM(input_ges)
       write(*,*)  leng, input_ges
       CALL read_grib(lu, input_ges, leng, idim, jdim, nlev, hgtg, tmpg, uwdg, vwdg, rhg, ierr)
       Print*,'ierr=  ', ierr
       101 FORMAT(3I5, 5F10.2)
       301 FORMAT('ZEUG',' ',3I5, 6F10.1)
       302 FORMAT('TEUG',' ',3I5, 6F10.1)
       303 FORMAT('WEUG',' ',3I5, 8F8.1)

       DO k = 1, nlev
       iplev=nlevs(k)
       tbias(k)=0.0 
       zbias(k)=0.0 
       wbias(k)=0.0 
       trms(k)=0.0 
       zrms(k)=0.0 
       wrms(k)=0.0
       tmax=0.0 
       wmax=0.0 
       zmax=0.0 
       DO i = 1, idim
       DO j = 1, jdim
       lat=91-j
         dzg=hgt(i,j,k)-hgtg(i,j,k)
         dza=hgte(i,j,k)-hgt(i,j,k)
         dze=hgte(i,j,k)-hgtg(i,j,k)
         if(abs(dza) .gt. zmax) zmax=abs(dza) 
         dtg=tmp(i,j,k)-tmpg(i,j,k)
         dt=tmp(i,j,k)-tmpe(i,j,k)
         dte=tmpe(i,j,k)-tmpg(i,j,k)
         if(abs(dt) .gt. tmax) tmax=abs(dt) 
         dwg=(uwd(i,j,k)-uwdg(i,j,k))*(uwd(i,j,k)-uwdg(i,j,k))
         dwg=dwg+(vwd(i,j,k)-vwde(i,j,k))*(vwd(i,j,k)-vwdg(i,j,k))
         dwg=sqrt(dwg)
         dw=(uwd(i,j,k)-uwde(i,j,k))*(uwd(i,j,k)-uwde(i,j,k))
         dw=dw+(vwd(i,j,k)-vwde(i,j,k))*(vwd(i,j,k)-vwde(i,j,k))
         dw=sqrt(dw)
         if(dw .gt. wmax) wmax=dw 
         tbias(k)=tbias(k)+dt
         zbias(k)=zbias(k)+dza
         wbias(k)=wbias(k)+dw
         trms(k)=trms(k)+dt*dt
         zrms(k)=zrms(k)+dza*dza
         wrms(k)=wrms(k)+dw*dw

         if(abs(pwgtz(k)*dzg) .gt. 30.0 ) then 

         if(dzg .lt. 0.0 .and. dze .gt. -10.0) then 
         write(*,301) iplev,i,lat,hgt(i,j,k),hgtg(i,j,k),hgte(i,j,k),dzg,dze,dza
         endif 

         if(dzg .gt. 0.0 .and. dze .lt. 10.0) then 
         write(*,301) iplev,i,lat,hgt(i,j,k),hgtg(i,j,k),hgte(i,j,k),dzg,dze,dza
         endif 

         endif 

         if(abs(pwgtt(k)*dtg) .gt. 2.0 ) then 

         if(dtg .lt. 0.0 .and. dte .gt. -0.5) then 
         write(*,302) iplev,i,lat,tmp(i,j,k),tmpg(i,j,k),tmpe(i,j,k),dtg,dte,dt
         endif 

         if(dtg .gt. 0.0 .and. dte .lt. 0.5) then 
         write(*,302) iplev,i,lat,tmp(i,j,k),tmpg(i,j,k),tmpe(i,j,k),dtg,dte,dt
         endif 

         endif 

         if(dwg .gt. 12.0 .and. dw .gt. 8.0) then 
         write(*,303) iplev,i,lat,uwd(i,j,k),uwdg(i,j,k),uwde(i,j,k),vwd(i,j,k),vwdg(i,j,k),vwde(i,j,k),dwg,dw
         endif 

       END DO
       END DO
       tbias(k)=tbias(k)/65160.0
       zbias(k)=zbias(k)/65160.0
       wbias(k)=wbias(k)/65160.0
       vart=trms(k)/65160.0 - tbias(k)*tbias(k)
       tstd=sqrt(vart)
       trms(k)=sqrt(trms(k)/65160.0)
       varz=zrms(k)/65160.0 - zbias(k)*zbias(k)
       zstd=sqrt(varz)
       zrms(k)=sqrt(zrms(k)/65160.0)
       varw=wrms(k)/65160.0 - wbias(k)*wbias(k)
       wstd=sqrt(varw)
       wrms(k)=sqrt(wrms(k)/65160.0)
       write(*,501) k,zbias(k),zstd,zrms(k),zmax
       write(*,502) k,tbias(k),tstd,trms(k),tmax
       write(*,503) k,wbias(k),wstd,wrms(k),wmax
       501 FORMAT('STAT Z',' ',I3, 4F10.2)
       502 FORMAT('STAT T',' ',I3, 4F10.2)
       503 FORMAT('STAT W',' ',I3, 4F10.2)

       END DO

       stop
       end
