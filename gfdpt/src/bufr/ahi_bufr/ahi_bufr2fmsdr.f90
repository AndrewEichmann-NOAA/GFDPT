!------------------------------------------------------------------------------
!
! NAME:
!       AHI_bufr2fmsdr
!
! PURPOSE:
!       This is a subroutine for reading the AHI BUFR file.
!
!       Written by: Yi Song (10/21/2014)
!                   IMSG 
!                   Yi.Song@noaa.gov
!
!------------------------------------------------------------------------------
!

   PROGRAM Ahi_bufr2fmsdr

!
! Declare modules used
!
!

  !---CRTM modules for conversion of radiance to BT   
  use crtm_module, only: crtm_destroy,crtm_init,crtm_channelinfo_type, success, &
      crtm_kind => fp, strlen
!  use crtm_module, only: crtm_channelinfo, crtm_kind => fp
  use crtm_planck_functions, only: crtm_planck_temperature,crtm_planck_radiance

  USE Consts
  USE misc
  USE utils
  USE IO_MeasurData
  USE IO_Scene
  USE IO_Misc
  USE IO_InstrConfig
  USE ErrorHandling
  USE Preclassif
  USE MathFcts
  USE FwdOperator


  IMPLICIT NONE

  !---INTRINSIC functions used in this module
  INTRINSIC :: ADJUSTL,COUNT,INDEX,INT,MAXVAL,MINVAL,PACK,REAL,SIZE,SQRT,TRIM,ALL,MOD,LEN_TRIM

!
!  Variable declarations
!

   CHARACTER(250) :: BUFR_FileName
   CHARACTER(250) :: BUFR_Table_FileName
   INTEGER, PARAMETER :: Long   = selected_int_kind(8)
   INTEGER, PARAMETER :: Double = selected_real_kind(12)
   INTEGER(LONG)  :: Error_Status
   INTEGER(LONG) :: LUNDX      ! BUFR table unit number
   INTEGER(LONG) :: LUBFR      ! BUFR file unit number
   INTEGER(LONG), PARAMETER :: ARRDIM = 79 
   REAL(DOUBLE) :: ARR(ARRDIM)
   INTEGER(LONG) :: IDATE
   INTEGER(LONG) :: IRET
   CHARACTER(8) :: BUFR_Subset
   INTEGER(LONG), PARAMETER :: MXLV = 1
   INTEGER(LONG) :: BUFR_Status
   CHARACTER(8) :: SUBSET

   INTEGER(LONG) :: Open_Status

   REAL(DOUBLE) ::  Latitude, Longitude, CHNM, CHWL, TMBR, ALBD, RAD1
   INTEGER(LONG) :: Year
   INTEGER(LONG) :: Month
   INTEGER(LONG) :: Day
   INTEGER(LONG) :: Hour
   INTEGER(LONG) :: Minute
   REAL          :: Second
   INTEGER(LONG), PARAMETER :: nChan=10 
   REAL          :: angle, rlat, rlon, anglea(nChan), BT(nChan), RAD(nChan), QCc(nChan)
   REAL          :: r1, v1, v2, dlat, dlon

   INTEGER(LONG), PARAMETER :: SATID = 173
   INTEGER(LONG), PARAMETER :: CENTID = 160     ! 007=ncep and 160=nesdis
   INTEGER(LONG), PARAMETER :: SubCENTID = 0   ! 0=nesdis/ora
   INTEGER(LONG), PARAMETER :: SIID = 297

   INTEGER(LONG) :: Satellite_ID
   INTEGER(LONG) :: Center_ID
   INTEGER(LONG) :: SubCenter_ID
   INTEGER(LONG) :: Instrument_ID
   INTEGER(LONG) :: Satellite_Classification

   INTEGER(LONG) :: READMG_IRET
   INTEGER(LONG) :: READSB_IRET,NX, K
   INTEGER(LONG) :: readmg_counter, obs_counter

   integer(LONG) :: julday, ifov, iscn, AD, qcflg(2), lunOut, lunIn
   integer(LONG)  :: sensorindex
   
   integer,parameter:: nimghdr=13
   integer,parameter:: maxinfo=37
   character(80),parameter:: hdrh8  = &            ! goes imager header
!ZZ        'SAID YEAR MNTH DAYS HOUR MINU SECW CLATH CLONH'
!ZZ4proxy!!   'SAID YEAR MNTH DAYS HOUR MINU SECW CLATH CLONH SIID'
              'SAID YEAR MNTH DAYS HOUR MINU SECW CLATH CLONH SAZA SOZA BEARAZ SOLAZI'
   real(8),dimension(nimghdr) :: hdrh8arr       !   Himawari8 AHI data
   real(8),dimension(3,12) :: dataimg             !   Himawari8 AHI data
   REAL          :: UTC, solarZen, RelAz
   REAL, PARAMETER :: Altitude=35786.0
   INTEGER(LONG) :: readsb_counter, BufferSize, nfile, ifile
   !---Namelist data 
   INTEGER, PARAMETER :: len=256
   INTEGER(LONG)      :: nscanl, nqc, nmeasure, sensor_id 
   CHARACTER(LEN=len) :: BUFRFilesList=DEFAULT_VALUE_STR4
   CHARACTER(LEN=len) :: pathBinOut=DEFAULT_VALUE_STR4
   character(LEN=len) :: InstrConfigFile
   CHARACTER(LEN=len) :: Coeff_Path
   CHARACTER(LEN=10)  :: OutFilePrefix
   CHARACTER(LEN=10)  :: OutFileSuffix
   TYPE(CRTM_ChannelInfo_type), DIMENSION(:),   POINTER     :: ChannelInfo
   INTEGER(LONG)      :: nskip
   !---Pointers and other arrays
   CHARACTER(LEN=len), DIMENSION(:),       POINTER     :: BUFRFiles,BinOutFiles
   real(crtm_kind) :: temperature
   real(crtm_kind) :: radiance
   INTEGER         :: nSensors,ichan
   CHARACTER(STRLEN),           DIMENSION(:),   ALLOCATABLE :: CHAR_SensorID
         
   type(InstrConfig_type)        :: InstrConfig
   
   NAMELIST /ControlBUFRdump/BUFRFilesList, PathBinOut, Coeff_Path, InstrConfigFile, nskip !, use_edges
   sensorindex = 1


   !-----------------------------------------------------
   !     Read control-data from namelist
   !-----------------------------------------------------
   nskip = 0
   READ(*,NML=ControlBUFRdump)

   lunIn = 10
   lunOut = 11
   
   sensor_id = 74
   CALL GetSensorInfo(sensor_id,CHAR_SensorID,nSensors)
   
   ALLOCATE(ChannelInfo(nSensors))
   Error_Status = CRTM_Init(CHAR_SensorID,ChannelInfo,File_Path=Coeff_Path)
   IF (Error_Status .ne. 0) CALL ErrHandl(ErrorType,Err_CRTMneCNTRL,'CRTM Init Failed')
   print *, ChannelInfo(1)%Sensor_Channel(1:10)
   print *, ChannelInfo(1)%Channel_Index(1:10)
   CALL ReadInstrConfig(InstrConfigFile,InstrConfig)

   OutFilePrefix  = 'FMSDR_'
   OutFileSuffix  = '.bin'
   PRINT*, ' '//OutFilePrefix
   !---Read the file names of BUFR data and build output Binary files names
   CALL ReadList4(LunIn,trim(BUFRFilesList),BUFRFiles,nfile,BinOutFiles,pathBinout,&
        trim(OutFilePrefix),trim(OutFileSuffix))
   
   IF (nfile .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(BUFR files in convertBUFR)')
   CLOSE(lunIn)

   PRINT*,'Starting Ahi_bufr2fmsdr'
   !---Initialize some variables.
   SUBSET = 'NC021205'

   FilesLoop: DO ifile = 1, nfile
     !---Open the BUFR file.
     LUBFR = 21;
     OPEN (UNIT=LUBFR, File=trim(BUFRFiles(ifile)), &
          FORM="UNFORMATTED", Status="OLD", &
          IOSTAT=Open_Status)
     
     IF(Open_Status /= 0)THEN
        print *, 'Failed to open input BUFR file'
     ENDIF
     
     !---Open the BUFR table.
     LUNDX = 31
     OPEN (UNIT=LUNDX, File = "AHI_bufr_table_new.txt", Status="OLD", &
          IOSTAT=Open_Status)
     
     IF(Open_Status /= 0)THEN
        print *, 'Failed to open BUFR table file'
     ENDIF
     
     !
     !  Associated the BUFR file and table and identify them to the 
     !  BUFRLIB software.
     !
     
     CALL OPENBF(LUBFR,'IN',LUNDX)
     !
     !  Read a BUFR message (1 record at a time) into an internal array.
     !
     
     CALL READMG(LUBFR,BUFR_Subset,IDate,READMG_IRET)
     
     print *, "IDate0=", IDate
     readmg_counter = 1
     BufferSize = 1
     obs_counter = 0
     DO WHILE(READMG_IRET == 0)
        
        !      PRINT*,'readmg_counter = ',readmg_counter
        
        !
        !  Read a BUFR message subset into an internal array.
        !
        
        CALL READSB(LUBFR,READSB_IRET)
        
        readsb_counter = 1
        
        SBLoop: DO WHILE(READSB_IRET == 0)
           
           !         PRINT*,'readsb_counter = ',readsb_counter
           
           !
           !  Extract that internal array so we can use it.
           !

           
           CALL UFBSEQ(LUBFR,ARR,ARRDIM,MXLV,BUFR_Status,SUBSET)
           
           !
           !  Reassign the missing values in the BUFR (10e10) to those we use (-9999).
           !
           
           WHERE(INT(ARR) >= 10e12 )ARR = -9999.0
           
           !
           !  "BUFR_Status" is equal to the number of levels actually read within the subset.
           !  It should always be equal to the MXLV (the maximum number of data levels
           !  requested to be read).
           !
           
           IF(BUFR_Status /= MXLV)THEN
              print *, 'Failed to read MXLV'
           ENDIF
           
!
           !  Begin the copying of data from the BUFR array into the data structure.
           !
           
           Satellite_ID = INT(ARR(1))
           IF( Satellite_ID /= SATID )THEN
              print *, 'Satellite ID does not match a known type', Satellite_ID, SATID
           ENDIF
           
           Center_ID = INT(ARR(2))
           IF( Center_ID /= CENTID )THEN
              print *, 'Center ID does not match a known type', Center_ID, CENTID
           ENDIF
           
           Instrument_ID = INT(ARR(4))
           IF( Instrument_ID /= SIID )THEN
              print *, 'Instrument_ID does not match a known type', Instrument_ID, SIID
           ENDIF
           
           NX = 4
           NX = NX + 1
           
!           Year = INT(ARR(5))
!           Month = INT(ARR(6))
!           Day = INT(ARR(7))
!           Hour = INT(ARR(8))
!           Minute = INT(ARR(9))
!           Second = INT(ARR(10))
!           Latitude=ARR(13)
!           Longitude=ARR(14)
           
!           WRITE(50,*) ARR(1:ARRDIM)
           CALL READSB(LUBFR,READSB_IRET)
           readsb_counter = readsb_counter + 1 
           BufferSize=BufferSize+1
           IF (MOD(BufferSize,nskip) .ne. 0) cycle SBLoop
           obs_counter = obs_counter + 1
!           if (obs_counter .eq. 4) stop
        ENDDO SBLoop
        CALL READMG(LUBFR,BUFR_Subset,IDate,READMG_IRET)
        !print *, "IDate=", IDate
        readmg_counter = readmg_counter + 1 
        
     ENDDO
     
     print *, "BufferSize =", BufferSize
     print *, "Number of Obs =", obs_counter
     nqc = 2
     nscanl = 1
     nmeasure = BufferSize-1
     nmeasure = obs_counter
     obs_counter = obs_counter + 1
     print *, 'Writing File: ', trim(BinOutFiles(iFile))
!     print *, nqc, nmeasure, nscanl, nchan
!     print *, InstrConfig%CentrFreq
     CALL WriteHdrMeasurmts(BinOutFiles(iFile),lunOut,nmeasure,nqc,nchan,nscanl,&
          InstrConfig%CentrFreq,InstrConfig%polarity,nscanl)  
     
     !
     !  Associated the BUFR file and table and identify them to the 
     !  BUFRLIB software.
     !
     
     CALL CLOSBF(LUBFR)
     BUFR_FileName = BUFRFiles(ifile)
     OPEN (UNIT=LUBFR, File=trim(BUFR_FileName), &
          FORM="UNFORMATTED", Status="OLD", &
          IOSTAT=Open_Status)
     
     IF(Open_Status /= 0)THEN
        print *, 'Failed to open input BUFR file'
     ENDIF
     CALL OPENBF(LUBFR,'IN',LUNDX)
     !
     !  Read a BUFR message (1 record at a time) into an internal array.
     !
     
     CALL READMG(LUBFR,BUFR_Subset,IDate,READMG_IRET)
     
!     print *, "IDate0=", IDate
     readmg_counter = 1
     BufferSize = 1
     
     DO WHILE(READMG_IRET == 0)
        
        !      PRINT*,'readmg_counter = ',readmg_counter
        
        !
        !  Read a BUFR message subset into an internal array.
        !
        
        CALL READSB(LUBFR,READSB_IRET)
        
        readsb_counter = 1
        
        SBLoop2:  DO WHILE(READSB_IRET == 0)
           
           !         PRINT*,'readsb_counter = ',readsb_counter
           
           !
           !  Extract that internal array so we can use it.
           !
           
           !  Read through each reacord
           call ufbint(lubfr,hdrh8arr,nimghdr,1,iret,hdrh8)
!           if(hdrh8arr(1) /= kidsat) cycle read_loop
           !ZZ notes: Brightness Temperature for test now !!!
           call ufbint(lubfr,dataimg,3,12,iret,'TMBR CHNM CHWL')
           
           CALL UFBSEQ(LUBFR,ARR,ARRDIM,MXLV,BUFR_Status,SUBSET)

           
           !
           !  Reassign the missing values in the BUFR (10e10) to those we use (-9999).
           !
           
           WHERE(INT(ARR) >= 10e12 )ARR = -9999.0
           
           !
           !  "BUFR_Status" is equal to the number of levels actually read within the subset.
           !  It should always be equal to the MXLV (the maximum number of data levels
           !  requested to be read).
           !
           
           IF(BUFR_Status /= MXLV)THEN
              print *, 'Failed to read MXLV'
           ENDIF
           
           !
           !  Begin the copying of data from the BUFR array into the data structure.
           !
           
           Satellite_ID = INT(ARR(7))
!           IF( Satellite_ID /= SATID )THEN
!              print *, 'Satellite ID does not match a known type'
!           ENDIF
           
!           Center_ID = INT(ARR(5))
!           IF( Center_ID /= CENTID )THEN
!              print *, 'Center ID does not match a known type'
!           ENDIF
           
!           Instrument_ID = INT(ARR(8))
!           IF( Instrument_ID /= SIID )THEN
!              print *, 'Instrument_ID does not match a known type', Instrument_ID
!           ENDIF
           
           
           NX = 4
           NX = NX + 1
           
           Year  = INT(ARR(5))
           Month = INT(ARR(6))
           Day   = INT(ARR(7))
!           Year  = 2014
!           Month = 8
!           Day   = 10

           Hour  = INT(ARR(8))
           Minute = INT(ARR(9))
           Second = ARR(10)
           Latitude=ARR(13)
           Longitude=ARR(14)

           DO K= 3, 12
             !---only one QC parameter --- repeat for all channels for testing 
             NX = 19
             QCc(K-2)= ARR(NX)
             NX   =NX+1+(K-1)*5
             CHNM =ARR(NX)
             NX   =NX+1
             CHWL =ARR(NX)
             print *, instrConfig%CentrFreq(K-2), CHWL
             NX   =NX+1
             TMBR =ARR(NX)
             BT(K-2)=TMBR
             NX   =NX+1
             ALBD =ARR(NX)
             NX   =NX+1
             RAD1  =ARR(NX)
             RAD(K-2)=RAD1
             
             print *, K, INT(CHNM), SNGL(CHWL)
             print *, SNGL(dataimg(1,k)), SNGL(TMBR)
!             print *, K, CHNM, CHWL
!             write(50) Latitude, Longitude, CHNM, CHWL, TMBR, ALBD, RAD
           ENDDO
!           PRINT *, QCc(1)
!           IF (BufferSize > 3) stop
           CALL READSB(LUBFR,READSB_IRET)
           BufferSize=BufferSize+1
           IF (MOD(BufferSize,nskip) .ne. 0) cycle SBLoop2
           obs_counter = obs_counter + 1
!           IF (MOD(BufferSize,nskip) .eq. 0) cycle SBLoop2
!           readsb_counter = readsb_counter + 1 
           qcflg(1:nqc) = SUM(QCc(1:nChan))
           IF (QCc(1) .ne. 16) print *, 'buff=', buffersize, 'qc=',QCc(1)
           qcflg(1:nqc) = 0
           rlat     = Latitude
           rlon     = Longitude
!           dist = 100.*SQRT( (rlat*rlat) + (rlon - 140.)*(rlon - 140.) )
           dlon = 3.14159265*(rlon - 140.)/180.
           dlat = 3.14159265*rlat/180.
           
           r1   = 1. + Altitude / 6378.16
           v1   = r1*COS(dlat)*COS(dlon)-1.
           v2   = r1*SQRT( 1. - COS(dlat)*COS(dlat)*COS(dlon)*COS(dlon) )
           
           angle = ABS(90. - (180./3.14159265)*ATAN2(v1,v2))
!           print *, angle, ARR(15)
           angle = ARR(15)
!           IF (angle .ge. 75) print *, angle
!           print *, rlat, rlon, angle
           IF (ABS(angle) .ge. 75) angle = 75.0
           anglea(1:nchan) = angle
           iscn     = INT(ARR(11))
           ifov     = INT(ARR(12))
           relAz    = 0.
           solarZen = ARR(17)
           AD       = 0
           UTC      = (Minute*60.0) + (Hour*3600.0) + Second
           DO k = 1, 10
            !---BUFR table says it is W / (m**2 sr um) 
            ! - conversion would be lambda^2 to mW / (m**2 sr cm-1).
            ! - I think actual unit is mW / ...
            !   divide conversion by 1000.
            radiance = instrconfig%CentrFreq(k)*instrconfig%CentrFreq(k)*RAD(k)/10.000  !.e3
            IF (radiance .gt. 0.) THEN 
              call crtm_planck_temperature(sensorindex,k,radiance,temperature)
            ELSE
              temperature = -9999.
              BT(K) = -9999.
            ENDIF
!            IF (MOD(BufferSize,100) .eq. 0) THEN 
!               print *, rlat, rlon 
!               print *, k, instrconfig%CentrFreq(k), radiance, temperature, BT(k)
!               temperature = BT(k)
!               call crtm_planck_radiance(sensorindex,k,temperature,radiance)
!               print *, radiance
!            ENDIF
            IF (temperature .gt. 350. .or. temperature .lt. 150.) THEN 
               IF (BT(K) .gt. 350. .or. BT(K) .lt. 150.) BT(1:nchan) = -9999.
            ELSE
               BT(K) = temperature
            ENDIF
            
           ENDDO
!           stop
!           UTC      = Second
           call compJulDay(Year,Month,Day,julday)
           IF (MOD(BufferSize,1000) .eq. 0) THEN 
             print *, BufferSize
             print *, rlat, rlon, iscn, ifov, angle
             print *, 'UTC     =', UTC
             print *, 'Hour    =', Hour 
             print *, 'Minute  =', Minute
             print *, 'Second  =', Second
             print *, 'DDDDDD  =', Minute*60. + Hour*3600.
             print *, 'Day     =', Day, ' ', julday 
             print *, 'CentrFrq=', instrConfig%CentrFreq(10)
             print *, 'radiance=', radiance
             print *, 'RAD(k)  =', RAD(10), radiance
             print *, 'temp    =', BT(10)
             print *, 'temp    =', temperature
!             print *, BT(1:nchan)
           ENDIF

           CALL WRITEMEASURMTS(lunOut,nqc,qcflg,nchan,anglea,BT,rlat,rlon,&
                AD,UTC,julday,Year,&
                ifov,iscn,relAz,solarZen)

        ENDDO SBLoop2

        CALL READMG(LUBFR,BUFR_Subset,IDate,READMG_IRET)
        !print *, "IDate=", IDate
        readmg_counter = readmg_counter + 1 
     ENDDO
     
     CALL CLOSBF(LUBFR)
     CLOSE(LunOut)

         ! twelve channels :: (5, 6, 7, 8, 9,10, 
         !                    11,12,13,14,15,16) 
!!$         
!!$         DO K=1, 12
!!$            NX = 15
!!$            NX=NX+1+(K-1)*5
!!$            !         IF (INT(ARR(NX)) == 10) THEN  !  channel number 10
!!$            CHNM=ARR(NX)
!!$            NX=NX+1
!!$            CHWL=ARR(NX)
!!$            NX=NX+1
!!$            TMBR=ARR(NX)
!!$            NX=NX+1
!!$            ALBD=ARR(NX)
!!$            NX=NX+1
!!$            RAD=ARR(NX)
!!$            !          print *, INT(ARR(NX))
!!$            write(50) Latitude, Longitude, CHNM, CHWL, TMBR, ALBD, RAD
!!$            !         ENDIF
!!$         ENDDO

!     stop
  ENDDO FilesLoop
  PRINT*,'Finishing Ahi_bufr2fmsdr'

 END PROGRAM Ahi_bufr2fmsdr

