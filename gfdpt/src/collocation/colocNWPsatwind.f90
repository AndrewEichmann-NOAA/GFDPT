!$Id: colocNWPsatwind.f90 3331 2013-08-26 13:25:25Z chrisg $
!===============================================================
! Name:    colocNWPsatwind.f90
!
!
! Type:    Main Program
!
!
! Description:
!       Program that collocates the NWP gridded data into an
!       orbit-based space using AMV satellite wind measurements.
!       The program outputs two files. 
!         1.) Scene dependent atmospheric state and surface properties
!            interpolated onto the time/location of the wind measurement
!         2.) Wind profile interpolated onto the time/location and pressure
!            of the satellite wind product.
!
! Modules needed:
!       - misc
!       - Consts
!       - utils
!       - IO_Scene
!       - IO_SceneAMV
!       - IO_Misc
!       - GeophCovBkg
!       - ErrorHandling
!       - Preclassif
!
!
! History:
!       04-02-2014      Eric S. Maddy          Created.  Based on colocNWPwRad.f90
!       04-15-2014      Eric S. Maddy          Fixed bug in time interpolation of NWP
!       02-08-2015      Eric S. Maddy          Added surface_wind flag for scatterometers
!
!===============================================================

program colocNWPsatwind

  USE Consts
  USE misc
  USE utils
  USE IO_MeasurData
  USE IO_Scene
  USE IO_SceneAMV
  USE IO_Misc
  USE IO_Misc_LE
  USE GeophCovBkg
  USE ErrorHandling
  USE Preclassif

  implicit none

  !---INTRINSIC functions used in this module
  INTRINSIC :: ADJUSTL,COUNT,INDEX,INT,MAXVAL,MINVAL,PACK,REAL,SIZE,SQRT,TRIM,ALL,MOD,LEN_TRIM
  !---Different parameters
  INTEGER            :: iu_listwind=20,iu_listnwpsfc=30,iu_listnwpatm=40, iu_listnwpwind=21
  INTEGER            :: nvarSfc,nvarAtm,nAnalys2use,nLay,nLev,sfcTypeIdx, nVarWind
  INTEGER, PARAMETER :: len=256
  INTEGER, PARAMETER :: nlat=181,nlon=360
  REAL,    PARAMETER :: minLatNWP=-90,maxLatNWP=90,minLonNWP=0,maxLonNWP=359
  INTEGER, PARAMETER :: nAbsorb=2
  INTEGER            :: nqc=7
  INTEGER            :: ntime=2
  INTEGER            :: len_file=0
  LOGICAL            :: use138

  !---Pointers and other arrays
  CHARACTER(LEN=len), DIMENSION(:),       POINTER     :: WindFiles,nwpFiles,nwpWNDFiles
  CHARACTER(LEN=len), DIMENSION(:),       POINTER     :: sfcNWPfiles,atmNWPfiles,windNWPfiles
  CHARACTER(LEN=len), DIMENSION(:),       POINTER     :: dumfiles
  REAL,               DIMENSION(:,:,:,:), ALLOCATABLE :: sfcArr,atmArr,windArr
  REAL,               DIMENSION(:,:,:),   ALLOCATABLE :: sfcArr0,atmArr0
  REAL,               DIMENSION(:,:,:),   ALLOCATABLE :: windArr0
  REAL,               DIMENSION(:),       ALLOCATABLE :: latNWP,lonNWP
  REAL                   :: julHour_Ym
  !---Wind
  TYPE(SceneAMV_type)     :: AMVm
  
  !---Single variables
  CHARACTER(LEN=10)  :: OutFilePrefix(2)
  CHARACTER(LEN=10)  :: OutFileSuffix
  INTEGER            :: iuMeasur,iuOut,indx,i,ierr,Error_status,allocate_status
  INTEGER            :: iuOutW
  INTEGER            :: nfile,ifile,nprofiles,nProfsYm,iprof,ilat,ilon,itime  
  INTEGER            :: nProfsProcessedOKqc,nProfsProcessedbadqc,nProfsProcessed
  REAL               :: PresBotT,PresTopT,PresTopQ,PresBotQ,PresTopW,PresBotW
  REAL               :: PresBotclwc,PresTopclwc,PresBotciwc,PresTopciwc
  REAL               :: secs
  !---NWP-related scene data
  TYPE(Scene_type)                      :: Scene    
  TYPE(SceneAMV_type)                   :: SceneAMV    
  INTEGER,    DIMENSION(:), ALLOCATABLE :: AbsorbID
  INTEGER(2), DIMENSION(:), ALLOCATABLE :: qc
  INTEGER                               :: nFilesNWPsfc,nFilesNWPatm,nFilesWindNWPatm,iSfcTyp,iSfcTypOut
  INTEGER                               :: nLayOut,nLevOut,iSfcTypCRTM,ilev,nlevWOut, nlevW
  INTEGER,  DIMENSION(:), ALLOCATABLE   :: polarity
  INTEGER                               :: nLevsAboveSfc, nchan, polar
  REAL,     DIMENSION(:),   ALLOCATABLE :: CentrFreq
  INTEGER                               :: SfcClass,month,day
  INTEGER                               :: julDay_NWP, julday
  INTEGER,  DIMENSION(:),   ALLOCATABLE :: idxPLevs2Use
  REAL,     DIMENSION(:),   POINTER     :: pressLayOut,presslevOut
  REAL,     DIMENSION(:,:), ALLOCATABLE :: timeNWP
  REAL,     DIMENSION(:),   ALLOCATABLE :: height,temper,relHum,specHum
  REAL,     DIMENSION(:),   ALLOCATABLE :: clwc,ciwc
  REAL,     DIMENSION(:),   ALLOCATABLE :: temper2Use,relHum2Use !,clwc2Use
  REAL,     DIMENSION(:),   ALLOCATABLE :: level_p,level_p2Use
  REAL,     DIMENSION(:),   ALLOCATABLE :: layer_t,layer_q,layer_rh,layer_sh
  REAL,     DIMENSION(:),   ALLOCATABLE :: layer_clwc,level_clwc
  REAL,     DIMENSION(:),   ALLOCATABLE :: layer_ciwc,level_ciwc
  REAL,     DIMENSION(:),   ALLOCATABLE :: layer_rain,level_rain
  REAL                                  :: julHour_NWP,Min_julHour_NWP,Max_julHour_NWP
  REAL                                  :: dLatNWP,dLonNWP,TskPreclass
  REAL                                  :: SkinTemp=DEFAULT_VALUE_REAL
  REAL                                  :: SnwDepth=DEFAULT_VALUE_REAL
  REAL                                  :: WindSp=DEFAULT_VALUE_REAL
  REAL                                  :: SfcPress=DEFAULT_VALUE_REAL
  REAL                                  :: windU=DEFAULT_VALUE_REAL
  REAL                                  :: windV=DEFAULT_VALUE_REAL
  REAL                                  :: SurfTemp=DEFAULT_VALUE_REAL
  REAL                                  :: SurfRH=DEFAULT_VALUE_REAL
  REAL                                  :: SurfMixRatio=DEFAULT_VALUE_REAL
  REAL                                  :: tpw=DEFAULT_VALUE_REAL
  REAL                                  :: clw=DEFAULT_VALUE_REAL
  REAL                                  :: ice=DEFAULT_VALUE_REAL
  REAL                                  :: prate=DEFAULT_VALUE_REAL
  REAL                                  :: ptotal=DEFAULT_VALUE_REAL

  REAL,     DIMENSION(:),   ALLOCATABLE :: AMVwindU
  REAL,     DIMENSION(:),   ALLOCATABLE :: AMVwindV
  REAL,     DIMENSION(:),   ALLOCATABLE :: AMVwindP

  REAL,     DIMENSION(:),   ALLOCATABLE :: AMVwindU_p
  REAL,     DIMENSION(:),   ALLOCATABLE :: AMVwindV_p
  
  INTEGER :: year0,jday0,sec0, month0,day0,hour0  ! starting time 
  INTEGER :: year1,day1,month1, jday1
  INTEGER :: dHourNWP=6  ! NWP file hour step or hour interval
  
  !---Namelist data 
  CHARACTER(LEN=len) :: WindFileList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: atmNWPFilesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: sfcNWPFilesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: WindNWPFilesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=200) :: Coeff_Path
  CHARACTER(LEN=len) :: pathNWPout=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: CovBkgFileAtm=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: LogFile=DEFAULT_VALUE_STR4
  INTEGER            :: norbits2process=DEFAULT_VALUE_INT
  INTEGER            :: nprofs2process=DEFAULT_VALUE_INT
  INTEGER            :: sensor_id=DEFAULT_VALUE_INT
  INTEGER            :: nwp_source=DEFAULT_VALUE_INT
  INTEGER            :: surface_wind=DEFAULT_VALUE_INT 

  NAMELIST /ControlNWP/WindFileList,atmNWPFilesList,sfcNWPFilesList,WindNWPFilesList,pathNWPout,&
            CovBkgFileAtm,norbits2process,LogFile,nprofs2process,nwp_source,&
            sensor_id, surface_wind

  !---Initialize variables
  julHour_NWP=-999.
  julDay_NWP=-999
  Min_julHour_NWP = -999.
  Max_julHour_NWP = -999.
  !---number of output wind levels
  nLevWOut = 1

  !---surface wind flag for scatterometer (ISS RapidScat, OSCAT, ASCAT) 
  !   default = not surface (10m) wind
  surface_wind = 0

  !-----------------------------------------------------
  !     Read control-data from namelist
  !-----------------------------------------------------
  READ(*,NML=ControlNWP)
  !---Prepare Log file
  CALL OpenLogFile(Logfile)
  if( sensor_id .eq. SENSOR_ID_TRMM ) nqc = 14

  !---Set conditional variables
  IF (nwp_source .eq. 1) THEN
      nvarSfc     = 14
      nvarAtm     = 73
      nvarWind    = 73
      sfcTypeIdx  = 8
      OutFilePrefix(1)  = 'NWP_GDAS'
      OutFilePrefix(2)  = 'NWP_GDASWN'
      OutFileSuffix  = ''
      nLay = 26
      nLev = nLay+1
      dHourNWP = 6
  ELSE IF (nwp_source .eq. 2) THEN
      use138 = .false.
      nvarSfc     = 6
      nvarAtm     = 91*6+1
      nvarWind    = 91*6+1
      sfcTypeIdx  = 1
      OutFilePrefix(1)  = 'NWP_ECM'
      OutFilePrefix(2)  = 'NWP_ECMWN'
      OutFileSuffix  = ''
      nLay = 90
      nLev = nLay+1
      nLevW = nLev
      dHourNWP = 6
  ELSE IF (nwp_source .eq. 3) THEN
      nvarSfc     = 13
      nvarAtm     = 52
      nvarWind    = 52
      sfcTypeIdx  = 1
      OutFilePrefix(1)  = 'NWP_GFS'
      OutFilePrefix(2)  = 'NWP_GFSWN'
      OutFileSuffix  = ''
      nLay = 26
      nLev = nLay+1
      nLevW = nlev
      dHourNWP = 6
  ENDIF
  
  !---Read the file names of wind data and build output NWP files names
  call ReadList4(iu_listwind,trim(WindFileList),WindFiles,nfile,nwpFiles,pathNWPout,trim(OutFilePrefix(1)),&
       trim(OutFileSuffix))
  IF (nfile .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(wind files in coloc)')

  call ReadList4(iu_listnwpwind,trim(WindFileList),WindFiles,nfile,nwpWNDFiles,pathNWPout,trim(OutFilePrefix(2)),&
       trim(OutFileSuffix)) 
  IF (nfile .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(wind files in coloc)')

  nfile=minval((/norbits2process,nfile/))
  !---Read the file names of sfc/atm analyses data 
  call ReadList(iu_listnwpsfc,trim(sfcNWPFilesList),sfcNWPFiles,nFilesNWPsfc,dumFiles,'.','')
  DEALLOCATE(dumFiles)
  call ReadList(iu_listnwpatm,trim(atmNWPFilesList),atmNWPFiles,nFilesNWPatm,dumFiles,'.','')
  DEALLOCATE(dumFiles)
  call ReadList(iu_listnwpwind,trim(WindNWPFilesList),WindNWPFiles,nFilesWindNWPatm,dumFiles,'.','')
  DEALLOCATE(dumFiles)

  IF (nfilesNWPatm .ne. nfilesNWPsfc .or. nfilesNWPatm .ne. nfilesWindNWPatm) &
       CALL ErrHandl(ErrorType,Err_InconsNumber,'of # of analyses found atm/sfc') 
  nAnalys2Use=nfilesNWPatm
  ntime = nfilesNWPatm
  
  !-----------------------------------------------------
  !     Read  pressure-grid from covariance matrix file
  !-----------------------------------------------------
  CALL GetPressFromCovFile(CovBkgFileAtm,nLayOut,pressLayOut,nLevOut,pressLevOut)
 
  !-----------------------------------------------------
  !     Loop-Reading over the gridded analyses files
  !-----------------------------------------------------
  ALLOCATE(level_p(nLev),temper(nLev),relHum(nLev),height(nLev),specHum(nLev), &
       clwc(nLev),ciwc(nLev),AMVWindU_p(nLevW), AMVWindV_p(nLevW),AMVWindU(nLevWOut),&
       AMVWindV(nLevWOut), AMVWindP(nLevWOut))
  ALLOCATE(sfcArr(nlat,nlon,nvarSfc,nAnalys2use),atmArr(nlat,nlon,nvarAtm,nAnalys2use),&
       windArr(nlat,nlon,nvarWind,nAnalys2use),&
       latNWP(nlat),lonNWP(nlon),sfcArr0(nlat,nlon,nvarSfc),atmArr0(nlat,nlon,nvarAtm),&
       windArr0(nlat,nlon,nvarWind),&
       layer_t(nLayOut),layer_q(nLayOut),layer_rh(nLayOut),layer_sh(nLayOut),&
       layer_clwc(nLayOut),layer_ciwc(nLayOut),layer_rain(nLayOut),&
       level_clwc(nLevOut),level_ciwc(nLevOut),level_rain(nLevOut),&
       timeNWP(nAnalys2Use,4))
  
  do i=1,ntime

      !---- nwp file name: gfs_sfc2011-04-09.t00,gdas_sfc2011-04-09.t00,ecmwf_sfc2011-04-09.t00
      len_file = LEN_TRIM(sfcNWPFiles(i))
      indx = len_file - 13
      read(sfcNWPFiles(i)(indx:indx+3),    *) timeNWP(i,1) ! yyyy
      read(sfcNWPFiles(i)(indx+5:indx+6),  *) timeNWP(i,2) ! mm
      read(sfcNWPFiles(i)(indx+8:indx+9),  *) timeNWP(i,3) ! dd
      read(sfcNWPFiles(i)(indx+12:indx+13),*) timeNWP(i,4) ! hh
      
      IF( i .eq. 1 ) THEN
         year0  = INT(timeNWP(i,1))
         month0 = INT(timeNWP(i,2))
         day0   = INT(timeNWP(i,3))
         hour0  = INT(timeNWP(i,4))
         call compJulDay(year0,month0,day0,jday0)
      ENDIF

      IF (nwp_source .eq. 1) THEN
        level_p=(/10.,20.,30.,50.,70.,100.,150.,200.,250.,300., &
             350.,400.,450.,500.,550.,600.,650.,700.,750.,800., &
             850.,900.,925.,950.,975.,1000.,1100./)
        call readGDASanalys(sfcNWPFiles(i),sfcArr0(:,:,:),nlat,nlon,nvarSfc)
        call readGDASanalys(atmNWPFiles(i),atmArr0(:,:,:),nlat,nlon,nvarAtm)
        call readGDASanalys(WindNWPFiles(i),windArr0(:,:,:),nlat,nlon,nvarWind)
        sfcArr(:,:,:,i)=sfcArr0(:,:,:)
        atmArr(:,:,:,i)=atmArr0(:,:,:)
        windArr(:,:,:,i)=windArr0(:,:,:)
      ELSE IF (nwp_source .eq. 2) THEN
        !---- check if the date is after ECMWF updated model levels
        call compJulDay(2013,6,24,julday)
        year1  = INT(timeNWP(i,1))
        month1 = INT(timeNWP(i,2))
        day1   = INT(timeNWP(i,3))
        call compJulDay(year1,month1,day1,jday1)
        IF (jday1 .gt. julday) use138 = .TRUE.
        CALL readECMWFanalys(sfcNWPFiles(i),atmNWPFiles(i),sfcArr0(:,:,:),nvarSfc,1,use138)
        CALL readECMWFanalys(sfcNWPFiles(i),atmNWPFiles(i),atmArr0(:,:,:),nvarAtm,2,use138)
        CALL readECMWFanalys(sfcNWPFiles(i),WindNWPFiles(i),WindArr0(:,:,:),nvarWind,3,use138)
        sfcArr(:,:,:,i)=sfcArr0(nlat:1:-1,:,:)
        atmArr(:,:,:,i)=atmArr0(nlat:1:-1,:,:)
        windArr(:,:,:,i)=windArr0(nlat:1:-1,:,:)
      ELSE IF (nwp_source .eq. 3) THEN
        level_p=(/10., 20., 30., 50., 70., 100., 150., 200., 250.,&
                  300., 350., 400., 450., 500., 550., 600., 650.,&
                  700., 750., 800., 850., 900., 925., 950., 975., 1000./)
!        level_p=(/100.,150.,200.,250.,300.,350.,400.,450.,500.,550.,600.,&
!                  650.,700.,750.,800.,850.,900.,925.,950.,975.,1000./)
        call readGFSforcst(sfcNWPFiles(i),sfcArr0(:,:,:),nlat,nlon,nvarSfc)
        call readGFSforcst(atmNWPFiles(i),atmArr0(:,:,:),nlat,nlon,nvarAtm)
        call readGFSforcst(windNWPFiles(i),windArr0(:,:,:),nlat,nlon,nvarWind)
        sfcArr(:,:,:,i)=sfcArr0(:,:,:)
        atmArr(:,:,:,i)=atmArr0(:,:,:)
        windArr(:,:,:,i)=windArr0(:,:,:)
        
      ENDIF

     
      dLatNWP=(maxLatNWP-minLatNWP)/(nLat-1)
      dLonNWP=(maxLonNWP-minLonNWP)/(nLon-1)
      latNWP(1:nLat:1) = minLatNWP +(/(i-1,i=1,nLat)/)*dLatNWP !+ dLatNWP/2.
      lonNWP(1:nLon:1) = minLonNWP +(/(i-1,i=1,nLon)/)*dLonNWP !+ dLonNWP/2.
      
  enddo

  !-----------------------------------------------------
  !    Initialize some items to go in the scene files
  !-----------------------------------------------------
  ALLOCATE(AbsorbID(nAbsorb),qc(nqc))
  AbsorbID = (/1,3/)
  nchan = 1
  ALLOCATE(polarity(nchan), CentrFreq(nchan))
  !-----------------------------------------------------
  !     Loop over the Wind files
  !-----------------------------------------------------
  FilesLoop: DO ifile=1,nfile

      !---Open and Read the header of the wind measurement(s) file 
      Call ReadHdrSceneAMV(iuMeasur,WindFiles(ifile),AMVm,nProfsYm)
      !---fix range of longitude if 0,360 instead of -180,180.
      nProfiles=minval((/nprofs2process,nProfsYm/))
      print *, 'nProfiles = ', nProfiles
      !---Initialize the output (NWP) scene file and write header
      CentrFreq = -999.9
      polarity     = -999
      CALL InitHdrScene(nLevOut,nLayOut,nChan,CentrFreq,                 &
          polarity,pressLevOut,pressLayOut,Scene,nLayOut,nLayOut,nLayOut,nLayOut,nLayOut,&
          nAbsorb,AbsorbID,nqc,0,Scene%nPosScan,Scene%nScanLines,0)
      CALL WriteHdrScene(iuOut,nwpFiles(ifile),Scene,nProfiles)     

      !---Initialize output SceneAMV structure and write header
      nqc = AMVm%nqc
      CALL InitHdrSceneAMV(nqc,0,0,SceneAMV)
      CALL WriteHdrSceneAMV(iuOutW,nwpWNDFiles(ifile),AMVm,nProfiles)     

      !----Loop over the profiles
      nProfsProcessed      = 0
      nProfsProcessedOKqc  = 0
      nProfsProcessedbadqc = 0
      ProfLoop: DO iprof=1,nprofiles
        qc(:) = 0
        layer_t(1:nLayOut) = -999.
        layer_q(1:nLayOut) = -999.
        layer_clwc(1:nLayOut) = -999.
        layer_ciwc(1:nLayOut) = -999.
        layer_rain(1:nLayOut) = -999.
        !---Read the AMV measurement(s)
        call ReadSceneAMV(iuMeasur,AMVm,ierr)
        IF (ierr.ne.0) THEN
           CALL ErrHandl(WarningType,Warn_readInvalid,'')
           EXIT ProfLoop
        ENDIF
        IF (AMVm%lon .gt. 180) AMVm%lon = AMVm%lon-360.

        call compJulDay(AMVm%scanyear,AMVm%scanmonth,AMVm%scanday,julDay)

        nProfsProcessed = nProfsProcessed+1
        !---Determine the surface type  (from topography)
        !   Note: This type could be (and is) different from GDAS definition of soil type
!!$        call Read_topography(Topogr,AMVm%lat,AMVm%lon,xalt,iSfcTypOut,xcover,iSfcTypCRTM)

        !---FIND CORRESPONDING NWP SCENES AND FILL SCENE STRUCTURE
        IF ( AMVm%qc(1) .eq. 0 .and. AMVm%lat .ge. -90. ) THEN
!           secs = ???
           secs = AMVm%scanUTC
           !---Determine the corresponding Analysis file(s) -Sfc and Atm-
           call  determineNWPindexAMV(AMVm%lat,AMVm%lon,minLatNWP,dLatNWP,minLonNWP,dLonNWP,&
                 AMVm%scanYear,julDay,secs,dHourNWP,year0,jday0,hour0,&
                 ilat,ilon,itime,nlat,nlon,ntime)

           !---GDAS Case
           IF (nwp_source .eq. 1) THEN
              !---Initialize some arrays
              relHum(:) = -999
              temper(:) = -999
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,1, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SkinTemp,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,3, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SnwDepth,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,11,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SurfTemp,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,13,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SurfRH,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,14,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SfcPress,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,9, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,WindU,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,10,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,WindV,qc,dHourNWP)
              DO ilev=1,nlev-1
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,atmArr,&
                      iLev,sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,height(iLev),qc,dHourNWP)
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,atmArr,&
                      nLev-1+iLev,sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,temper(iLev+1),qc,dHourNWP)
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,windArr,ilev, &
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,AMVWindU_p(iLev+1),qc,dHourNWP)
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,windArr,ilev+nlev-1,&
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,AMVWindV_p(iLev+1),qc,dHourNWP)
              ENDDO

              DO ilev=1,nlev-6
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,atmArr,&
                      2*(nLev-1)+iLev,sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,relHum(iLev+1),qc,dHourNWP)
                 relHum(iLev+1)=maxval((/relHum(iLev+1),0./)) 
              ENDDO
             
              relHum(nLev-4:nLev) = 0. 
              surfMixRatio        = RelHum_to_mixingratio(SurfRH/100.,SurfTemp,SfcPress/100.)
              SfcPress            = SfcPress/100.
              !---Reverse array order to match level_p
              temper  = temper(nLev:1:-1)
              relHum  = relHum(nLev:1:-1)
              !---Find Sfc Press and index only layers above surface
              nLevsAboveSfc = COUNT(level_p(:) .lt. sfcPress)
              ALLOCATE(idxPLevs2Use(nlevsAboveSfc),level_p2Use(nLevsAboveSfc+1),&
                temper2Use(nLevsAboveSfc+1),relHum2Use(nLevsAboveSfc+1))
              temper2Use(:) = -999
              relHum2Use(:) = -999   
              idxPLevs2Use = PACK( (/(i,i=1,SIZE(level_p(:)))/), (level_p(:) .lt. sfcPress))
              level_p2Use(1:nLevsAboveSfc) = level_p(idxPLevs2Use)

              temper2Use(1:nLevsAboveSfc)  = temper(idxPLevs2Use)
              relHum2Use(1:nLevsAboveSfc)  = relHum(idxPLevs2Use)
              level_p2Use(nLevsAboveSfc+1) = sfcPress
              temper2Use(nLevsAboveSfc+1)  = SurfTemp
              relHum2Use(nLevsAboveSfc+1)  = SurfRH
              !---Converting the relative humidity into mixing ratio (not currently being used for interpolation)
              !do i=1,nLev-4
              !   WVmixratio(i)    = RelHum_to_mixingratio(RelHum(i)/100.,Temper(i),level_p(i))
              !enddo
              IF (relHum(5) .lt. 1) relHum(5) = 0.
              IF (relHum(4) .lt. 1) relHum(4) = 0.
              !---Compute layer values
              CALL intrpLevs2Lays(temper2Use(:),level_p2Use(:),nLevOut,pressLevOut,PresBotT,PresTopT,pressLayOut,layer_t,qc)
              CALL intrpLevs2Lays(relHum2Use(4:nLevsAboveSfc+1),level_p2Use(4:nLevsAboveSfc+1),nLevOut,pressLevOut,PresBotQ,&
                   PresTopQ,pressLayOut,layer_rh,qc)
              !---Convert interpolated RH to Mixing Ratio
              DO i=1,nLayOut
                 layer_q(i) = -999
                 IF (layer_rh(i) .gt. 0 .and. layer_t(i) .gt. 0) THEN
                    layer_q(i) = RelHum_to_mixingratio(layer_rh(i)/100.,layer_t(i),PressLayOut(i))
                 ENDIF
              ENDDO
              AMVWindP(1) = AMVm%WindPress
              IF (surface_wind == 1) THEN
                 AMVWindP(1) = MAXVAL(level_p)-1.
                 AMVm%WindPress = AMVWindP(1)
              ENDIF
              !---Interpolate the Wind Profiles to AMV wind pressures
              CALL interprofilenval(AMVWindU_p,level_p,nLevWOut,AMVWindP,PresBotW,PresTopW,AMVWindU,qc)
              CALL interprofilenval(AMVWindV_p,level_p,nLevWOut,AMVWindP,PresBotW,PresTopW,AMVWindV,qc)
              IF (surface_wind .eq. 1) THEN
                 AMVWindU(1) = WindU
                 AMVWindV(1) = WindV
              ENDIF

              DEALLOCATE(idxPLevs2Use,level_p2Use,temper2Use,relHum2Use)

           !---ECMWF CASE
           ELSE IF (nwp_source .eq. 2) THEN
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,2,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SfcPress,qc,dHourNWP)  
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,3,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SkinTemp,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,4,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SurfTemp,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,5,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,WindU,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,6,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,WindV,qc,dHourNWP)
              DO ilev=1,nLev
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,atmArr,ilev+1,&
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,temper(iLev),qc,dHourNWP)
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,atmArr,ilev+nLev+1,&
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,specHum(iLev),qc,dHourNWP)
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,atmArr,ilev+2*nLev+1,&
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,clwc(iLev),qc,dHourNWP)
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,atmArr,ilev+3*nLev+1,&
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,ciwc(iLev),qc,dHourNWP)
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,atmArr,ilev+5*nLev+1,&
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,level_p(iLev),qc,dHourNWP)
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,windArr,ilev+1, &
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,AMVWindU_p(iLev),qc,dHourNWP)
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,windArr,ilev+nLev+1,&
                      sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,AMVWindV_p(iLev),qc,dHourNWP)
              ENDDO
              CALL intrpLevs2Lays(temper,level_p,nLevOut,pressLevOut,PresBotT,PresTopT,pressLayOut,layer_t,qc)
              CALL intrpLevs2Lays(specHum,level_p,nLevOut,pressLevOut,PresBotQ,PresTopQ,pressLayOut,layer_sh,qc)

              AMVWindP(1) = AMVm%WindPress
              AMVWindP(1) = AMVm%WindPress
              IF (surface_wind == 1) THEN
                 AMVWindP(1) = SfcPress - 10. *(100./1000.) 
                 AMVWindP(1) = MAXVAL(level_p)-1.
                 AMVm%WindPress = AMVWindP(1)
              ENDIF
!              print *, 'surface_wind=',surface_wind
              !---Interpolate the Wind Profiles to AMV wind pressures
              CALL interprofilenval(AMVWindU_p,level_p,nLevWOut,AMVWindP,PresBotW,PresTopW,AMVWindU,qc)
              CALL interprofilenval(AMVWindV_p,level_p,nLevWOut,AMVWindP,PresBotW,PresTopW,AMVWindV,qc)

!!$              print *, 'iprofile  =', iprof, ', qc =', qc
!!$              print *, AMVm%lat, AMVm%lon
!!$              print *, level_p
!!$              print *, 'AMVWindU_p=', AMVWindU_p
!!$              print *, 'AMVWindV_p=', AMVWindV_p 
!!$              print *, 'temper    =', temper
!!$              print *, 'relhum    =', relhum
!!$              print *, AMV
!!$              print *, 'AMVWindP  =', AMVWindP 
!!$              print *, 'AMVWindU  =', AMVWindU
!!$              print *, 'AMVWindV  =', AMVWindV
!!$              print *, 'WindU     =', WindU
!!$              print *, 'WindV     =', WindV
              IF (surface_wind .eq. 1) THEN
                 AMVWindU(1) = WindU
                 AMVWindV(1) = WindV
              ENDIF
!!$              print *, PresBotW, PresTopW, qc
!              stop
              CALL interprofile(clwc,level_p,nLevOut,pressLevOut,PresBotclwc,PresTopclwc,level_clwc,qc)
              CALL interprofile(ciwc,level_p,nLevOut,pressLevOut,PresBotciwc,PresTopciwc,level_ciwc,qc)
              CALL LayersICWC(level_clwc,pressLevOut,layer_clwc) 
              CALL LayersICWC(level_ciwc,pressLevOut,layer_ciwc) 
              !---Convert Specific Humidity to Mixing Ratio
              DO i=1,nLayOut
                 layer_q(i) = -999
                 IF (layer_sh(i) .gt. 0) THEN
                    layer_q(i) = layer_sh(i)/(1-(0.001*layer_sh(i)))
                 ENDIF
              ENDDO
           
           !--- GFS CASE
           ELSE IF (nwp_source .eq. 3) THEN
              !---Initialize some arrays
              temper(:) = -999.
              relHum(:) = -999.
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,2,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SfcPress,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,3,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SurfTemp,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,3,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SkinTemp,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,5, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,tpw,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,6, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,clw,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,7, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SnwDepth,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,8, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,ice,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,9, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,WindU,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,10, &
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,WindV,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,11,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,SurfRH,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,12,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,prate,qc,dHourNWP)
              CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,sfcArr,13,&
                   sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,ptotal,qc,dHourNWP)
              DO ilev=1,nlev-1
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,atmArr,&
                      ilev,sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,temper(iLev),qc,dHourNWP)
              ENDDO
              DO ilev=1,nlev-1
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,atmArr,&
                      26+ilev,sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,relHum(ilev),qc,dHourNWP)
              ENDDO
              !---esm 02/10
              DO ilev=1,nlevW-1
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,windArr,&
                      ilev,sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,AMVWindU_p(iLev),qc,dHourNWP)
                 CALL interpolBetw4points(AMVm%lat,AMVm%lon,AMVm%scanYear,AMVm%scanmonth,AMVm%scanday,secs,windArr,&
                      26+ilev,sfcTypeIdx,ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,AMVWindV_p(iLev),qc,dHourNWP)
              ENDDO
              
              AMVWindP(1) = AMVm%WindPress
              IF (surface_wind == 1) THEN
                 AMVWindP(1) = SfcPress - 10. *(100./1000.) 
                 AMVm%WindPress = AMVWindP(1)
              ENDIF
              !---Interpolate the Wind Profiles to AMV wind pressures
              CALL interprofilenval(AMVWindU_p,level_p,nLevWOut,AMVWindP,PresBotW,PresTopW,AMVWindU,qc)
              CALL interprofilenval(AMVWindV_p,level_p,nLevWOut,AMVWindP,PresBotW,PresTopW,AMVWindV,qc)
!              print *, 'iprofile  =', iprof, ', qc =', qc
!              print *, AMVm%lat, AMVm%lon
!              print *, 'AMVWindU_p=', AMVWindU_p
!              print *, 'AMVWindV_p=', AMVWindV_p 
!              print *, 'temper    =', temper
!              print *, 'relhum    =', relhum
!              print *, 'AMVWindP  =', AMVWindP 
!              print *, 'AMVWindU  =', AMVWindU
!              print *, 'AMVWindV  =', AMVWindV
!              print *, PresBotW, PresTopW, qc
              IF (surface_wind .eq. 1) THEN
                 AMVWindU(1) = WindU
                 AMVWindV(1) = WindV
              ENDIF

              !---Compute layer values
              CALL intrpLevs2Lays(temper,level_p,nLevOut,pressLevOut,PresBotT,PresTopT,pressLayOut,layer_t,qc)
              CALL intrpLevs2Lays(relHum,level_p,nLevOut,pressLevOut,PresBotQ,PresTopQ,pressLayOut,layer_rh,qc)
              DO i=1,nLayOut
                 layer_q(i) = -999
                 IF (layer_rh(i) .gt. 0 .and. layer_t(i) .gt. 0) THEN
                    layer_q(i) = RelHum_to_mixingratio(layer_rh(i)/100.,layer_t(i),PressLayOut(i))
                 ENDIF
              ENDDO
              
              layer_clwc(1:nLayOut) = clw / nLayOut
              layer_rain(1:nLayOut) = ptotal / nLayOut / 6.0

              SfcPress = SfcPress * 0.01
           ENDIF
  
           windSp = sqrt(WindU**2+WindV**2)
   
           iSfcTyp = INT(sfcArr(ilat,ilon,sfcTypeIdx,itime)) 

           !---Fill-up the scene content before output
           call SetUpScene(pressLevOut,pressLayOut,AMVm%angle,Scene,       &
                SfcClass,iProf,AMVm%lat,AMVm%lon,AMVm%node,julDay,AMVm%scanYear,secs,AMVm%iscanPos,&
                AMVm%iscanLine,Scene%RelAziAngle,Scene%SolZenAngle)
           Scene%SfcPress                         = SfcPress
           Scene%Temp_lay(1:nLayOut)              = layer_t(1:nLayOut)
           Scene%Absorb_lay(1:nLayOut,Scene%iH2o) = layer_q(1:nLayOut)
           Scene%Absorb_lay(1:nLayOut,Scene%iO3)  = 0.                !O3
           Scene%CLW(1:nLayOut)          = layer_clwc(1:nLayOut)
           Scene%Rain(1:nLayOut)         = layer_rain(1:nLayOut)
           Scene%Snow(1:nLayOut)         = 0.
           Scene%Ice(1:nLayOut)          = 0.
           Scene%Graupel(1:nLayOut)      = layer_ciwc(1:nLayOut)
           Scene%Emiss(1:nChan)          = 0.5
           Scene%Windsp                  = windSp
           Scene%Tskin                   = SkinTemp
           Scene%SnowDepth               = SnwDepth/10.
           Scene%DeltaT                  = SurfTemp-SkinTemp
           Scene%WindU                   = WindU
           Scene%WindV                   = WindV
           Scene%Refl(1:nChan)           = 0.5
           Scene%qc(1:nqc)               = qc(1:nqc)

           !---Write Emissivity and Reflectivity values to scene
           Scene%Emiss(1:nChan) = -999.0
           Scene%Refl(1:nChan)  = -999.0
           IF (ALL(Scene%Temp_Lay .lt. 0)) THEN
              call setSceneEDRs2default(Scene)
              Scene%qc=1
              nProfsProcessedbadqc=nProfsProcessedbadqc+1
           ELSE
              nProfsProcessedOKqc=nProfsProcessedOKqc+1
           ENDIF

           !---Fill-up the sceneAMV content before output 
           SceneAMV%WindU       = AMVWindU(1)
           SceneAMV%WindV       = AMVWindV(1)
           SceneAMV%WindPress   = AMVm%WindPress
           !---calculate the wind vector magnitude and direction 
           SceneAMV%WindSp      = SQRT(AMVWindU(1)*AMVWindU(1)+AMVWindV(1)*AMVWindV(1))
           SceneAMV%WindDir     = 180./PI * ATAN2(-1.*SceneAMV%WindU,-1.*SceneAMV%WindV)
           !---angle between 0-360 deg. from which wind is blowing
           IF (SceneAMV%WindDir.lt.0) SceneAMV%WindDir = SceneAMV%WindDir + 360.0

!           Print*, AMVm%WindU, AMVm%WindV, AMVm%WindPress
!           Print*, AMVWindU(1), AMVWindV(1), AMVm%WindPress

        ELSE
           !---FILL SCENE STRUCTURE
           qc(1:nqc) = 1
           !---Determine now the classifier-driven surface type (used in 1DVAR pre-classification)
           TskPreclass = -999.
!           call PreClassSfc(AMVm%scanYear,julDay,nchan,Scene%CentrFreq,AMVm%lat,AMVm%lon,&
!                iSfcTypOut,sensor_id,CentrFreq,SfcClass,TskPreclass)
           call SetUpScene(pressLevOut,pressLayOut,AMVm%angle,Scene,       &
                SfcClass,iProf,AMVm%lat,AMVm%lon,AMVm%node,julDay,AMVm%ScanYear,secs,AMVm%iscanPos,&
                AMVm%iscanLine,Scene%RelAziAngle,Scene%SolZenAngle)
           Scene%qc  = qc
           Scene%lat = AMVm%lat
           Scene%lon = AMVm%lon
           CALL setSceneEDRs2default(Scene)
           nProfsProcessedbadqc = nProfsProcessedbadqc+1
           CALL set2defaultAMV(SceneAMV)
        ENDIF
        SceneAMV%lat                  = AMVm%lat
        SceneAMV%lon                  = AMVm%lon
        !-------------------------------------------
        !---Output results
        !-------------------------------------------
        CALL WriteScene(iuOut,Scene)
        CALL WriteSceneAMV(iuOutW,SceneAMV)
      ENDDO ProfLoop
      write(*,'(A)')'Input  Wind File='//TRIM( WindFiles(ifile) )
      write(*,'(A)')'Output NWP File='//TRIM( nwpFiles(ifile) )
      write(*,'(A)')'Output NWP Wind File='//TRIM( nwpWNDFiles(ifile) )
      print *, 'Number of profiles processed (total):',nProfsProcessed
      print *, 'Number of profiles processed (OK qc):',nProfsProcessedOKqc
      print *, 'Number of profiles processed (failed qc):',nProfsProcessedbadqc
      write(*,*)
      !--- Release memory allocated in InitHdrScene of src/lib/io/IO_Scene.f90
      !--- Release memory allocated in InitHdrSceneAMV of src/lib/io/IO_SceneAMV.f90
      CALL DestroyScene(Scene)
      CALL DestroySceneAMV(AMVm)
      CALL DestroySceneAMV(SceneAMV)
      CLOSE(iuOut)
      CLOSE(iuMeasur)
      CLOSE(iuOutW)
  ENDDO FilesLoop
  
  DEALLOCATE(WindFiles)
  DEALLOCATE(nwpFiles)
  DEALLOCATE(nwpWNDFiles)
  DEALLOCATE(sfcNWPFiles)
  DEALLOCATE(atmNWPFiles)
  !--- Release memory allocated in GetPressFromCovFile of src/lib/InversProcess/GeophCovBkg.f90
  DEALLOCATE(pressLayOut,pressLevOut)
  DEALLOCATE(polarity, CentrFreq)
  DEALLOCATE(level_p,temper,relHum,height,specHum,clwc,ciwc)
  DEALLOCATE(WindArr, WindArr0, AMVWindU, AMVWindV, AMVWindP,&
       AMVWindU_p, AMVWindV_p)

  DEALLOCATE(sfcArr,atmArr,timeNWP,latNWP,lonNWP,sfcArr0,atmArr0,&
       layer_t,layer_q,layer_rh,layer_sh,layer_clwc,layer_ciwc,layer_rain,&
       level_clwc,level_ciwc,level_rain)
  
  CALL CloseLogFile()

CONTAINS

!===============================================================
! Name:         interpolBetw4points
!
!
! Type:         Subroutine
!
!
! Description:  Performs interpolation between four NWP points
!               to the location of the measurement.
!
!
! Arguments:
!
!      Name                 Type            Description
!      ---------------------------------------------------
!       - lat                I              Latitude of measur
!       - lon                I              Longitude
!       - Year               I              Year
!       - month              I              Month
!       - day                I              Day
!       - secs               I              Seconds
!       - Arr                I              Array of NWP data
!       - iparam             I              Index (within Arr) of param
!       - ilat               I              Index of latitude (within Arr)
!       - ilon               I              Index of longitude
!       - itime              I              Index of time
!       - nlat               I              Number of latitude grids
!       - nlon               I              Number of longitude grids
!       - latNWP             I              vector of latitudes of NWP grid 
!       - lonNWP             I              Vector of longitudes of NWP grid
!       - timeNWP            I              Vector of times of NWP fields
!       - outVar             O              Interpolated variable
!       - qc                 I/O            QC associated with variable
!       - qc                 I              hour interval
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!       04-28-2011      Wanchun Chen, Added dhour to be flexible
!
!===============================================================

  SUBROUTINE interpolBetw4points(lat,lon,year,month,day,secs,Arr,iparam,idxSfcTyp,&
             ilat,ilon,itime,nlat,nlon,latNWP,lonNWP,timeNWP,outVar,qc,dhour)
    REAl,    DIMENSION(:,:,:,:) :: Arr
    INTEGER                     :: iparam,ilat,ilon,itime,nlat,nlon,iLat0,iLat1,iLon0,iLon1
    INTEGER                     :: year,month,day,idxSfcTyp
    REAL                        :: OutVar,OutVar1,OutVar2,outVarInterInlat(2),lat,lon,secs
    REAL                        :: fracLat,fracLon,latLoc,lonLoc,hourLoc,fracHour
    REAL,    DIMENSION(:)       :: latNWP,lonNWP 
    REAL,    DIMENSION(:,:)     :: timeNWP    
    INTEGER(2), DIMENSION(:)    :: qc
    INTEGER                     :: dhour  ! hour interval or hour step
    
    lonLoc              = lon
    latLoc              = lat
    hourLoc             = secs/3600.
    IF (lonLoc .lt. 0.) LonLoc=lonLoc+360.
    iLat0  = iLat
    iLat1  = iLat+1
    iLon0  = iLon
    iLon1  = iLon+1
    
    if(iLat1 .gt. nlat) iLat1=nlat
    if(iLat1 .lt. 1   ) iLat1=1
    
    if(iLon1 .gt. nlon) iLon1=1
    if(iLon1 .lt. 1   ) iLon1=1
    
    OutVar = DEFAULT_VALUE_REAL
    !---Fractions
    fracLat             = (latLoc-latNWP(iLat0))/(latNWP(iLat1)-latNWP(iLat0))
    fracLon             = (lonLoc-lonNWP(ilon0))/(lonNWP(ilon1)-lonNWP(ilon0))
    fracHour            = (hourLoc-timeNWP(itime,4))/(dhour*1.0)
    !----4-points space-interpolation at time index itime
    IF (idxSfcTyp .gt. 0 .and. Arr(ilat0,ilon0,idxSfcTyp,itime)   .lt. 0) qc(1) = 1
    IF (idxSfcTyp .gt. 0 .and. Arr(ilat1,ilon0,idxSfcTyp,itime)   .lt. 0) qc(1) = 1
    IF (idxSfcTyp .gt. 0 .and. Arr(ilat0,ilon1,idxSfcTyp,itime)   .lt. 0) qc(1) = 1
    IF (idxSfcTyp .gt. 0 .and. Arr(ilat1,ilon1,idxSfcTyp,itime)   .lt. 0) qc(1) = 1

    IF (qc(1) .eq. 0) THEN 
       outVarInterInlat(1) = Arr(iLat0,iLon0,iparam,itime)+fracLat*               &
            (Arr(iLat1,iLon0,iparam,itime)-Arr(iLat0,iLon0,iparam,itime))
       outVarInterInlat(2) = Arr(ilat0,ilon1,iparam,itime)+fracLat*               &
            (Arr(iLat1,ilon1,iparam,itime)-Arr(ilat0,ilon1,iparam,itime))
       OutVar1             = outVarInterInlat(1)+fracLon*(outVarInterInlat(2)-outVarInterInlat(1))
    ENDIF

    !---Time-interpolation 
    IF (idxSfcTyp .gt. 0 .and. Arr(ilat0,ilon0,idxSfcTyp,itime+1) .lt. 0) qc(1) = 1
    IF (idxSfcTyp .gt. 0 .and. Arr(ilat1,ilon0,idxSfcTyp,itime+1) .lt. 0) qc(1) = 1
    IF (idxSfcTyp .gt. 0 .and. Arr(ilat0,ilon1,idxSfcTyp,itime+1) .lt. 0) qc(1) = 1
    IF (idxSfcTyp .gt. 0 .and. Arr(ilat1,ilon1,idxSfcTyp,itime+1) .lt. 0) qc(1) = 1

    IF (qc(1) .eq. 0) THEN 
       outVarInterInlat(1) = Arr(iLat0,iLon0,iparam,itime+1)+fracLat*&
            (Arr(iLat1,iLon0,iparam,itime+1)-Arr(iLat0,iLon0,iparam,itime+1))
       outVarInterInlat(2) = Arr(ilat0,ilon1,iparam,itime+1)+fracLat*&                      (Arr(iLat1,ilon1,iparam,itime+1)-Arr(ilat0,ilon1,iparam,itime+1))
       OutVar2  = outVarInterInlat(1)+fracLon*(outVarInterInlat(2)-outVarInterInlat(1))
       OutVar   = OutVar1+fracHour*(OutVar2-OutVar1)
    ENDIF

    RETURN
  END SUBROUTINE interpolBetw4points


!===============================================================
!  to compute hour difference of two times
!   
!  04-28-2011    Wanchun Chen       original coder
!===============================================================

 subroutine hour_diff( year1,jday1,hour1, year2,jday2,hour2, nhour )
   
   !---- input variables
   integer :: year1,jday1,hour1,year2,jday2,hour2
   
   !---- output variables
   integer :: nhour
   
   !---- local variables
   integer :: nday, nleap, year
   
   nday = 0
   nleap = 0
    
   if( year1 .eq. year2 ) then
      nday = jday2 - jday1
   else
      ! how many leap years
      do year = year1, year2-1
       if( ( MOD(year,4) .eq. 0 .and. MOD(year,100) .ne. 0 ) .or. MOD(year,400) .eq. 0 ) then
        nleap = nleap+1
      endif 
      enddo
      nday = (year2-year1) * 365 + nleap + jday2 - jday1
   endif
   
   nhour = nday * 24 + ( hour2 - hour1 )
   
   return
   
 end subroutine hour_diff


!===============================================================
! Name:         determineNWPindexAMV
!
!
! Type:        Subroutine
!
!
! Description:  Determines the indexes within the NWP field for
!               latitude, longitude, time, etc
!
!
! Arguments:
!
!         Name              Type            Description
!      ---------------------------------------------------
!       - lat                I              Latitude of measur        
!       - lon                I              Longitude of measur
!       - julDay             I              Julian day
!       - Year               I              Year
!       - month              O              Month
!       - day                O              Day
!       - secs               I              seconds
!       - ilat               O              Latitude index
!       - ilon               O              Longitude index
!       - itime              O              Time index
!       - minLatNWP          I              Minimum latitude in NWP grid
!       - dLatNWP            I              Latitude increment in NWP grid
!       - minLonNWP          I              Minimum longitude in NWP grid
!       - dLonNWP            I              Latitude increment in NWP grid
!       - latNWP             I              Latitudes vector in NWP grid
!       - lonNWP             I              Longitudes vector in NWP grid
!       - timeNWP            I              Times vector in NWP grid
!       - nAnalys2use        I              Number of analyses to use
!       - nlat               I              Number of latitudes (not used)
!       - nlon               I              Number of longitudes (not used)
!       - ntime              I              Number of times
!
!
! Modules needed:
!       - None
!
!
! History:
!       03-22-2007      Sid Ahmed Boukabara, IMSG Inc @ NOAA/NESDIS/ORA
!       04-28-2011      Wanchun Chen, Modified to handle any numer case
!
!===============================================================

  SUBROUTINE determineNWPindexAMV(lat,lon,minLatNWP,dLatNWP,minLonNWP,dLonNWP,&
             year,jday,sec,dhour,year0,jday0,hour0,&
             ilat,ilon,itime, nlat,nlon,ntime)
    
    !---- input variables
    REAL     :: lat,lon,minLatNWP,dLatNWP,minLonNWP,dLonNWP ! space info
    INTEGER  :: year,jday           ! time info of year and jday
    REAL     :: sec                 ! time info of seconds
    INTEGER  :: year0,jday0,hour0   ! starting time info
    INTEGER  :: dhour               ! hour interval or hour step
    
    INTEGER  :: nlat,nlon,ntime     ! total number of lat/lon/time
    
    !---- output variables
    INTEGER  :: ilat,ilon,itime
    
    !---- local variable
    INTEGER  :: hour, nhour
    REAL     :: latLoc,lonLoc
    
    !---- Determine the lat/lon indexes (to point to the right grid-point)
    lonLoc = lon
    latLoc = lat
    IF( lonLoc .lt. 0. ) LonLoc = lonLoc+360.
    ilat=int((latLoc-minLatNWP)/dLatNWP)+1
    ilon=int((lonLoc-minLonNWP)/dLonNWP)+1
    
    if(ilat .gt. nlat) ilat=nlat
    if(ilat .lt. 1   ) ilat=1
    
    if(ilon .gt. nlon) ilon=1
    if(ilon .lt. 1   ) ilon=1
    
    !---- Determine the time index (to point to the right time, pick the index of low boundary)
    hour = INT(sec/3600.)
!!$    print *, 'year0 = ', year0 
!!$    print *, 'jday0 = ', jday0 
!!$    print *, 'hour0 = ', hour0
!!$    
!!$    print *, 'year = ', year 
!!$    print *, 'jday = ', jday 
!!$    print *, 'hour = ', hour 
!!$    print *, 'sec = ', sec 
    !---- compute hour difference
    call hour_diff(year0,jday0,hour0,year,jday,hour, nhour)
    itime = INT( (nhour*1.0) / (dhour*1.0) ) + 1
   
    if( itime .lt. 1     ) itime = 1
    if( itime .ge. ntime ) itime = ntime-1
    
    RETURN

  END SUBROUTINE determineNWPindexAMV

end program colocNWPsatwind
