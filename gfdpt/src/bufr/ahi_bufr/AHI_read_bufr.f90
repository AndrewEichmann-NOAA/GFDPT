!------------------------------------------------------------------------------
!
! NAME:
!       AHI_read_bufr
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

   PROGRAM AHI_read_bufr

!
! Declare modules used
!

   IMPLICIT NONE

!
!  Variable declarations
!

   CHARACTER(250) :: BUFR_FileName
   CHARACTER(250) :: BUFR_Table_FileName

   INTEGER, PARAMETER :: Long   = selected_int_kind(8)
   INTEGER, PARAMETER :: Double = selected_real_kind(12)
   INTEGER(LONG) :: LUNDX      ! BUFR table unit number
   INTEGER(LONG) :: LUBFR      ! BUFR file unit number
   INTEGER(LONG), PARAMETER :: ARRDIM = 75 
   REAL(DOUBLE) :: ARR(ARRDIM)
   REAL(DOUBLE), PARAMETER :: BMISS = 10e10
   INTEGER(LONG) :: IDATE
   INTEGER(LONG) :: IRET
   CHARACTER(8) :: BUFR_Subset
   INTEGER(LONG), PARAMETER :: MXLV = 1
   INTEGER(LONG) :: BUFR_Status
   CHARACTER(8) :: SUBSET

   INTEGER(LONG) :: Open_Status

   REAL(DOUBLE) ::  Latitude, Longitude, CHNM, CHWL, TMBR, ALBD, RAD
   INTEGER(LONG) :: Year
   INTEGER(LONG) :: Month
   INTEGER(LONG) :: Day
   INTEGER(LONG) :: Hour
   INTEGER(LONG) :: Minute
   INTEGER(LONG) :: Second

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
   INTEGER(LONG) :: readmg_counter
   INTEGER(LONG) :: readsb_counter, BufferSize

!
!  Begin the program.
!

   PRINT*,'Starting AHI_read_bufr'

!
!  Initialize some variables.
!

   SUBSET = 'NC021205'
!
!  Open the BUFR file.
!
   READ(*,*) BUFR_FileName
   LUBFR = 21;
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
   OPEN (UNIT=LUNDX, File = "AHI_bufr_table.txt.3", Status="OLD", &
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

         Satellite_ID = INT(ARR(1))
         IF( Satellite_ID /= SATID )THEN
            print *, 'Satellite ID does not match a known type'
         ENDIF

         Center_ID = INT(ARR(2))
         IF( Center_ID /= CENTID )THEN
            print *, 'Center ID does not match a known type'
         ENDIF

         Instrument_ID = INT(ARR(4))
         IF( Instrument_ID /= SIID )THEN
            print *, 'Instrument_ID does not match a known type', Instrument_ID
         ENDIF

         NX = 4
         NX = NX + 1

         Year = INT(ARR(5))
         Month = INT(ARR(6))
         Day = INT(ARR(7))
         Hour = INT(ARR(8))
         Minute = INT(ARR(9))
         Second = INT(ARR(10))
         Latitude=ARR(13)
         Longitude=ARR(14)
         print *, Latitude, Longitude
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

         WRITE(50) ARR(1:ARRDIM)
         CALL READSB(LUBFR,READSB_IRET)
         readsb_counter = readsb_counter + 1 
         BufferSize=BufferSize+1

      ENDDO
      CALL READMG(LUBFR,BUFR_Subset,IDate,READMG_IRET)
      !print *, "IDate=", IDate
      readmg_counter = readmg_counter + 1 

   ENDDO

   CALL CLOSBF(LUBFR)

   print *, "BufferSize =", BufferSize
   PRINT*,'Finishing AHI_read_bufr'

   END PROGRAM

