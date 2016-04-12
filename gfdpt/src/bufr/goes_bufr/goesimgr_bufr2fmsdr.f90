PROGRAM goesimgr_bufr2fmsdr
!-----------------------------------------------------------------------------------------------
! Name:         goesimgr_bufr2fmsdr
! 
! Type:         F90 main program
!
! Description:  convert NCEP GOES imager BUFR data to 
!               MIRS FMSDR format
!
! Modules needed:
!       - Consts
!       - ErrorHandling
!       - IO_MeasurData
!       - BUFR
!       - IO_Misc
!
! Subroutines contained:
!       None
!
! Data type included:
! 
! History: 2014/10/08 Eric S. Maddy - Created
!-----------------------------------------------------------------------------------------------

  USE Consts
  USE IO_MeasurData
  USE ErrorHandling
  USE misc
  USE IO_Misc

  IMPLICIT NONE

  !---INTRINSIC functions used in this module
  INTRINSIC :: ADJUSTL,COUNT,INDEX,INT,MAXVAL,MINVAL,PACK,REAL,SIZE,SQRT,TRIM,ALL,MOD,LEN_TRIM

  !---local variables
  INTEGER                               :: nqc 
  integer, parameter                    :: maxchanl=4
  real , DIMENSION(maxchanl)            :: cfreq 
  INTEGER, DIMENSION(maxchanl)          :: pol
  real, DIMENSION(maxchanl)             :: angle
  real                                  :: angle1, rlat, rlon 
  CHARACTER(256)                        :: infile, outfile
  INTEGER                               :: LunIn
  INTEGER                               :: LunOut
  INTEGER, DIMENSION(2)                 :: qcflg
  INTEGER                               :: i1, i2   ! --- string concatenation
  INTEGER                               :: nfile, ifile, AD, ifov
  INTEGER, PARAMETER                    :: len=256
  CHARACTER(LEN=10)                     :: OutFilePrefix
  CHARACTER(LEN=10)                     :: OutFileSuffix
  !---Pointers and other arrays
  CHARACTER(LEN=len), DIMENSION(:),       POINTER     :: BUFRFiles,BinOutFiles
  !---Namelist data 
  CHARACTER(LEN=len) :: BUFRFilesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: pathBinOut=DEFAULT_VALUE_STR4
  character(len=10)  :: jsatid, obstype

! Declare local parameters
  integer, parameter :: nimghdr=13
  integer, parameter :: maxinfo=37
  real,    parameter :: tbmin=50.0
  real,    parameter :: tbmax=550.0
  character(80),parameter:: hdrgoes  = &            ! goes imager header
        'SAID YEAR MNTH DAYS HOUR MINU SECO CLAT CLON SAZA SOZA BEARAZ SOLAZI'

  character(8)  subset

  integer nchanl,ilzah,iszah,irec,next
  integer idate
  integer ireadmg,ireadsb,iret
  integer k,kidsat,iscan
  integer idate5(5)
  integer nrec, nmeasure, nscanl

  real    tdiff, UTC, solarZen, relAz
  integer julday

  real(8),dimension(nimghdr) :: hdrgoesarr       !  goes imager header
  real(8),dimension(3,6)     :: dataimg          !  goes imager data
  real   ,dimension(maxchanl) :: imgrbt
  
  NAMELIST /ControlBUFRdump/BUFRFilesList, PathBinOut, jsatid, obstype

  !-----------------------------------------------------
  !     Read control-data from namelist
  !-----------------------------------------------------
  READ(*,NML=ControlBUFRdump)

  !--- which goes instrument are we parsing? which detector to dump?
  if(jsatid == 'g08') kidsat = 252
  if(jsatid == 'g09') kidsat = 253
  if(jsatid == 'g10') kidsat = 254
  if(jsatid == 'g11') kidsat = 255
  if(jsatid == 'g12') kidsat = 256
  if(jsatid == 'g13') kidsat = 257
  if(jsatid == 'g14') kidsat = 258
  if(jsatid == 'g15') kidsat = 259

  i1 = INDEX(jsatid,' ')-1
  i2 = INDEX(obstype,' ')-1
  OutFilePrefix  = jsatid(1:i1) // obstype(1:i2)
  PRINT*, OutFilePrefix
!110  FORMAT(2x,'Found ',i8, ' records out of ',i8, ' records.  Writing to file ...')
  OutFileSuffix  = '.bin'
  !---Read the file names of BUFR data and build output Binary files names
  CALL ReadList4(LunIn,trim(BUFRFilesList),BUFRFiles,nfile,BinOutFiles,pathBinout,&
       trim(OutFilePrefix),trim(OutFileSuffix))
  
  IF (nfile .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(BUFR files in convertBUFR)')

  !---initialize some variables and defaults
  LunIn     = 10  ! unit for BUFR file 
  LunOut    = 11  ! unit for output SceneAMV_type file
  nqc       = 2   ! default number of QC parameters
  nchanl    = 4       
  ifov      = -999
  tdiff     = 0.
  ilzah=10       ! satellite zenith angle
  iszah=11       ! solar zenith angle

  cfreq  = (/3.9e-6, 6.75e-6, 10.7e-6, 12.0e-6/)
  !---convert wavelength into frequency in wavenumber
  cfreq  = 10000./(1.e6*cfreq)
  !-------------------------------------------------
  !     Loop over the GOES files (instruments, etc.)
  !-------------------------------------------------
  FilesLoop1: DO ifile=1,nfile
     nrec = 0
     nmeasure = 0
     infile = BUFRFiles(ifile)
     outfile = BinOutFiles(ifile)
     !---open the BUFR file and count the number of records
     CALL CLOSBF(lunin)
     OPEN(lunin,file=infile,form='unformatted')
     CALL OPENBF(lunin,'IN',lunin)
     CALL DATELEN(10)
     read_subset1: do while(ireadmg(LunIn,subset,idate)>=0)
        irec=irec+1
        read_loop1: do while (ireadsb(LunIn)==0)
           nrec = nrec + 1
           !---Read through each reacord
           call ufbint(lunin,hdrgoesarr,nimghdr,1,iret,hdrgoes)
           if(hdrgoesarr(1) /= kidsat) cycle read_loop1
           call ufbrep(lunin,dataimg,3,6,iret,'TMBRST NCLDMNT SDTB')
           !---first step QC filter out data with less clear sky fraction
           if (hdrgoesarr(1) == 256.0d0 .and. dataimg(2,3) < 70.0) cycle read_loop1
           if (hdrgoesarr(1) == 254.0d0 .and. dataimg(2,3) < 40.0) cycle read_loop1
           if (hdrgoesarr(ilzah) > 60.0) cycle read_loop1
           
           !---test for bad lat/lon data
           if(abs(hdrgoesarr(8))>90.0 .or. abs(hdrgoesarr(9))>360.0) cycle read_loop1
           !---Convert obs location to radians
           if (hdrgoesarr(9)==360.0) hdrgoesarr(9)=0.0
           if (hdrgoesarr(9) > 180.0) hdrgoesarr(9) = hdrgoesarr(9)-360.0

           nmeasure=nmeasure+1
        END do read_loop1
     END do read_subset1
     !---Open then read the bufr data
     !open(LunIn,file=infile,form='unformatted')
     !call openbf(LunIn,'IN',LunIn)
     !call datelen(10)
     
     !---Allocate arrays to hold data
     ! nstinfo : number of noaa surface sst analysis points ? 
     !           something with ocean sst information??
     ! nreal  = maxinfo + nstinfo
     ! nele   = nreal   + nchanl
     ! allocate(data_all(nele,itxmax),nrec(itxmax))
     
  !---Big loop to read data file
     next=0
     irec=0
     nscanl = 0
     nscanl = nmeasure
     print 110, nmeasure, nrec
110  FORMAT(2x,'Found ',i8, ' records out of ',i8, ' records.  Writing to file ...')
     print *, nscanl
     CALL WriteHdrMeasurmts(BinOutFiles(iFile),lunOut,nmeasure,nqc,nchanl,nscanl,cfreq,pol,nscanl)  
     !GMISQ1(1)=int(GMISQ)    ! --- QC 1 for GMI 
     infile = BUFRFiles(ifile)
     outfile = BinOutFiles(ifile)
     
     !---open the BUFR file and write to IO_MeasureData format
     CALL CLOSBF(lunin)
     OPEN(lunin,file=infile,form='unformatted')
     CALL OPENBF(lunin,'IN',lunin)
     CALL DATELEN(10)
  
     read_subset: do while(ireadmg(LunIn,subset,idate)>=0)
        irec=irec+1
        read_loop: do while (ireadsb(LunIn)==0)
           
           !---Read through each reacord
           call ufbint(lunin,hdrgoesarr,nimghdr,1,iret,hdrgoes)
           if(hdrgoesarr(1) /= kidsat) cycle read_loop
           call ufbrep(lunin,dataimg,3,6,iret,'TMBRST NCLDMNT SDTB')
           !---first step QC filter out data with less clear sky fraction
           if (hdrgoesarr(1) == 256.0d0 .and. dataimg(2,3) < 70.0) cycle read_loop
           if (hdrgoesarr(1) == 254.0d0 .and. dataimg(2,3) < 40.0) cycle read_loop
           if (hdrgoesarr(ilzah) > 60.0) cycle read_loop
           
           !---test for bad lat/lon data
           if(abs(hdrgoesarr(8))>90.0 .or. abs(hdrgoesarr(9))>360.0) cycle read_loop
           if (hdrgoesarr(9)==360.0) hdrgoesarr(9)=0.0
           if (hdrgoesarr(9) > 180.0) hdrgoesarr(9) = hdrgoesarr(9)-360.0

           nmeasure=nmeasure+1
           ! Extract obs time.  If not within analysis window, skip obs
           ! Extract date information.  If time outside window, skip this obs
           idate5(1) = nint(hdrgoesarr(2))  !year
           idate5(2) = nint(hdrgoesarr(3)) !month
           idate5(3) = nint(hdrgoesarr(4)) !day
           idate5(4) = nint(hdrgoesarr(5)) !hour
           idate5(5) = nint(hdrgoesarr(6)) !minute
           
           UTC= hdrgoesarr(7) + (idate5(5)*60.0) + (idate5(4)*3600.0)
           call compJulDay(idate5(1),idate5(2),idate5(3),julday)
              
           !---test for bad lat/lon data
           if(abs(hdrgoesarr(8))>90.0 .or. abs(hdrgoesarr(9))>360.0) cycle read_loop
           if (hdrgoesarr(9)==360.0) hdrgoesarr(9)=0.0

           !---Set common predictor parameters
           
          
           !---Increment goes sounder data counter
           !---Extract brightness temperatures
           !       Map obs to grids
           if(hdrgoesarr(1) == 256.0d0) then
              dataimg(1,5)=dataimg(1,6)              ! use  brightness tem. 6 not 5
              dataimg(3,5)=dataimg(3,6)              ! use BT tem. var. 6 not 5 
           endif
           iscan = nint(hdrgoesarr(ilzah))+1.001000 ! integer scan position
        
           !---Locate the observation on the analysis grid.  Get sst and land/sea/ice
           !    mask.  
           !     isflg    - surface flag
           !                0 sea
           !                1 land
           !                2 sea ice
           !                3 snow
           !                4 mixed 
           !--- ESM can we call this routine ? 
           !call deter_sfc(dlat,dlon,dlat_earth,dlon_earth,t4dv,isflg,idomsfc,sfcpct, &
           !   ts,tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10,sfcr)
          
           !---If not goes data over ocean , read next bufr record
           !if(isflg /= 0) cycle read_loop
           
           !   Following lines comment out because only using data over ocean
           !       crit1 = crit1 + rlndsea(isflg)  
           !       call checkob(dist1,crit1,itx,iuse)
           !       if(.not. iuse)cycle read_loop
           
           solarZen = sngl(hdrgoesarr(iszah))        ! solar zenith angle
           relAz    = sngl(hdrgoesarr(13))           ! solar azimut angle
           rlat     = sngl(hdrgoesarr(8))
           rlon     = sngl(hdrgoesarr(9))
           angle1   = sngl(hdrgoesarr(ilzah))        ! satellite zenith angle
           angle    = angle1
           print *, angle1, solarZen, relAz
           relAz    = 0.0
           IF (angle1 > 60.) stop
           qcflg(1:2) = 0
           AD         = 0
           do k = 1, nchanl
              imgrbt(k) = sngl(dataimg(3,k+1))
           enddo
           ifov = iscan
           CALL WRITEMEASURMTS(lunOut,nqc,qcflg,nchanl,angle,imgrbt,rlat,rlon,AD,UTC,julday,idate5(1),&
                ifov,iscan,relAz,solarZen)
        end do read_loop
     END do read_subset
     CALL CLOSBF(LunIn)
     !---close MeasurData record 
     CLOSE(LunOUT)
  END DO FilesLoop1
     
END PROGRAM goesimgr_bufr2fmsdr
