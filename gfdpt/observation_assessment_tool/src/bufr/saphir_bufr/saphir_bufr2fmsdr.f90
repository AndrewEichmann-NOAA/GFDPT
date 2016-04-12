!------------------------------------------------------------------------------

PROGRAM saphir_bufr2fmsdr
!-----------------------------------------------------------------------------------------------
! Name:         saphir_bufr2fmsdr
! 
! Type:         F90 main program :
!
! Description:  convert METEO FRANCE SAPHIR BUFR data to 
!               MIRS FMSDR format
!
! Modules needed:
!       - Consts
!       - ErrorHandling
!       - BUFR
!       - misc
!       - IO_Misc
!       - IO_MeasurData
!       - IO_InstrConfig
!
! Subroutines contained:
!       None
!
! Data type included:  IO_MeasurData FMSDR type
! Limitations : 
!
! History: 2014/11/21 Eric S. Maddy - Created
!-----------------------------------------------------------------------------------------------

!
! Declare modules used
!
  USE Consts
  USE IO_MeasurData
  USE IO_InstrConfig
  USE ErrorHandling
  USE misc
  USE IO_Misc

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
   INTEGER(LONG) :: LUNDX      ! BUFR table unit number
   INTEGER(LONG) :: LUBFR      ! BUFR file unit number
   INTEGER(LONG), PARAMETER :: ARRDIM = 92 
   REAL(DOUBLE) :: ARR(ARRDIM)
   REAL(DOUBLE), PARAMETER :: BMISS = 10e10
   INTEGER(LONG) :: IDATE
   INTEGER(LONG) :: IRET
   CHARACTER(8) :: BUFR_Subset
   INTEGER(LONG), PARAMETER :: MXLV = 1
   INTEGER(LONG) :: BUFR_Status
   CHARACTER(8) :: SUBSET

   INTEGER(LONG) :: Open_Status

   INTEGER(LONG), PARAMETER :: nChan=6 
   REAL(DOUBLE)  :: Latitude, Longitude, angle
   REAL          :: rlat, rlon, anglea(nChan), BT(nChan), QCc(nChan)
   INTEGER(LONG) :: nchanl, nscanl, nmeasure, nqc
   INTEGER(LONG) :: Year
   INTEGER(LONG) :: Month
   INTEGER(LONG) :: Day
   INTEGER(LONG) :: Hour
   INTEGER(LONG) :: Minute
   REAL(DOUBLE)  :: Second

   INTEGER(LONG), PARAMETER :: SATID = 440
   INTEGER(LONG), PARAMETER :: CENTID = 254     ! 254 -- eumetsat 
   INTEGER(LONG), PARAMETER :: SubCENTID = 0    ! 0=nesdis/ora
   INTEGER(LONG), PARAMETER :: SIID = 941

   INTEGER(LONG) :: Satellite_ID
   INTEGER(LONG) :: Center_ID
   INTEGER(LONG) :: SubCenter_ID
   INTEGER(LONG) :: Instrument_ID
   INTEGER(LONG) :: Satellite_Classification

   INTEGER(LONG) :: READMG_IRET
   INTEGER(LONG) :: READSB_IRET,NX, K
   INTEGER(LONG) :: readmg_counter
   integer(LONG) :: julday, ifov, iscn, AD, qcflg(2), lunOut, lunIn
   
   REAL          :: UTC, solarZen, RelAz
   INTEGER(LONG) :: readsb_counter, BufferSize, nfile, ifile
   !---Namelist data 
   INTEGER, PARAMETER                    :: len=256
   CHARACTER(LEN=len) :: BUFRFilesList=DEFAULT_VALUE_STR4
   CHARACTER(LEN=len) :: pathBinOut=DEFAULT_VALUE_STR4
   character(LEN=len) :: InstrConfigFile
   character(len=10)  :: jsatid
   CHARACTER(LEN=10)                     :: OutFilePrefix
   CHARACTER(LEN=10)                     :: OutFileSuffix
   !---Pointers and other arrays
   CHARACTER(LEN=len), DIMENSION(:),       POINTER     :: BUFRFiles,BinOutFiles

   type(InstrConfig_type)        :: InstrConfig
   
   NAMELIST /ControlBUFRdump/BUFRFilesList, PathBinOut, InstrConfigFile !, use_edges
   
   !-----------------------------------------------------
   !     Read control-data from namelist
   !-----------------------------------------------------
   READ(*,NML=ControlBUFRdump)
   
   CALL ReadInstrConfig(InstrConfigFile,InstrConfig)

   OutFilePrefix  = 'FMSDR_'
   OutFileSuffix  = '.bin'
   PRINT*, ' '//OutFilePrefix
   !---Read the file names of BUFR data and build output Binary files names
   CALL ReadList4(LunIn,trim(BUFRFilesList),BUFRFiles,nfile,BinOutFiles,pathBinout,&
        trim(OutFilePrefix),trim(OutFileSuffix))
   
   IF (nfile .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(BUFR files in convertBUFR)')
   CLOSE(lunIn)
!
!  Begin the program.
!

   PRINT*,'Starting AHI_read_bufr'

!
!  Initialize some variables.
!

   SUBSET = 'NC003010'
!
!  Open the BUFR file.
!
   FilesLoop: DO ifile = 1, nfile 
     LUBFR = 21
     BUFR_FileName = BUFRFiles(ifile)
     OPEN (UNIT=LUBFR, File=trim(BUFR_FileName), &
          FORM="UNFORMATTED", Status="OLD", &
          IOSTAT=Open_Status)
     
     IF(Open_Status /= 0)THEN
        print *, 'Failed to open input BUFR file'
     ENDIF

     !
     !  Open the BUFR table.
     !
     
     LUNDX = 31
     OPEN (UNIT=LUNDX, File = "table.info", Status="OLD", &
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
     
     DO WHILE(READMG_IRET == 0)
        
        !      PRINT*,'readmg_counter = ',readmg_counter
        
        !
        !  Read a BUFR message subset into an internal array.
        !
        
        CALL READSB(LUBFR,READSB_IRET)
        
        readsb_counter = 1
        
        DO WHILE(READSB_IRET == 0)
           
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
           
           Satellite_ID = INT(ARR(7))
           IF( Satellite_ID /= SATID )THEN
              print *, 'Satellite ID does not match a known type'
           ENDIF
           
           Center_ID = INT(ARR(5))
           IF( Center_ID /= CENTID )THEN
              print *, 'Center ID does not match a known type'
           ENDIF
           
           Instrument_ID = INT(ARR(8))
           IF( Instrument_ID /= SIID )THEN
              print *, 'Instrument_ID does not match a known type', Instrument_ID
           ENDIF
           
           CALL READSB(LUBFR,READSB_IRET)
           readsb_counter = readsb_counter + 1 
           BufferSize=BufferSize+1
           
        ENDDO
        CALL READMG(LUBFR,BUFR_Subset,IDate,READMG_IRET)
        !print *, "IDate=", IDate
        readmg_counter = readmg_counter + 1 
        
     ENDDO

     CALL CLOSBF(LUBFR)
     nqc = 2
     nchanl = nChan
     nscanl = 1
     nscanl = 1
     nmeasure = BufferSize-1
     CALL WriteHdrMeasurmts(BinOutFiles(iFile),lunOut,nmeasure,nqc,nchanl,nscanl,&
          InstrConfig%CentrFreq,InstrConfig%polarity,nscanl)  

     !
     !  Associated the BUFR file and table and identify them to the 
     !  BUFRLIB software.
     !
     
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
     
     print *, "IDate0=", IDate
     readmg_counter = 1
     BufferSize = 1
     
     DO WHILE(READMG_IRET == 0)
        
        !      PRINT*,'readmg_counter = ',readmg_counter
        
        !
        !  Read a BUFR message subset into an internal array.
        !
        
        CALL READSB(LUBFR,READSB_IRET)
        
        readsb_counter = 1
        
        DO WHILE(READSB_IRET == 0)
           
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
           
           Satellite_ID = INT(ARR(7))
           IF( Satellite_ID /= SATID )THEN
              print *, 'Satellite ID does not match a known type'
           ENDIF
           
           Center_ID = INT(ARR(5))
           IF( Center_ID /= CENTID )THEN
              print *, 'Center ID does not match a known type'
           ENDIF
           
           Instrument_ID = INT(ARR(8))
           IF( Instrument_ID /= SIID )THEN
              print *, 'Instrument_ID does not match a known type', Instrument_ID
           ENDIF
           
           
           NX = 6
           NX = NX + 1
           
           Year = INT(ARR(16))
           Month = INT(ARR(17))
           Day = INT(ARR(18))
           Hour = INT(ARR(19))
           Minute = INT(ARR(20))
           Second = ARR(21)
           Latitude=ARR(22)
           Longitude=ARR(23)
           Angle=ARR(29)
           NX = 47
           DO K = 1, 6
              BT(K) = ARR(NX)
              QCc(K) = ARR(NX-3)
              NX = NX + 9
           ENDDO
           
           CALL READSB(LUBFR,READSB_IRET)
           readsb_counter = readsb_counter + 1 
           BufferSize=BufferSize+1
           qcflg(1:nqc) = SUM(QCc(1:nChan))
           anglea(1:nchan) = angle
           ifov     = INT(ARR(12))
           iscn     = INT(ARR(11))
           relAz    = 0.
           solarZen = 0.
           AD       = 0.
           UTC      = (Minute*60.0) + (Hour*3600.0) + Second
!           UTC      = Second
           rlat     = Latitude
           rlon     = Longitude
           
           call compJulDay(Year,Month,Day,julday)
           IF (MOD(BufferSize,100000) .eq. 0) THEN 
             print *, BufferSize
!             print *, rlat, rlon, iscn, ifov, angle
             print *, 'UTC   =', UTC
             print *, 'Hour  =', Hour 
             print *, 'Minute=', Minute
             print *, 'Second=', Second
             print *, 'DDDDDD=', Minute*60. + Hour*3600.
             print *, 'Day   =', Day, ' ', julday 
             print *, BT(1:6)
           ENDIF

           CALL WRITEMEASURMTS(lunOut,nqc,qcflg,nchanl,anglea,BT,rlat,rlon,&
                AD,UTC,julday,Year,&
                ifov,iscn,relAz,solarZen)
        ENDDO
        CALL READMG(LUBFR,BUFR_Subset,IDate,READMG_IRET)
        !print *, "IDate=", IDate
        readmg_counter = readmg_counter + 1 
        
     ENDDO
     
     CALL CLOSBF(LUBFR)
   ENDDO FilesLoop

   print *, "BufferSize =", BufferSize
   PRINT*,'Finishing AHI_read_bufr'

   END PROGRAM

