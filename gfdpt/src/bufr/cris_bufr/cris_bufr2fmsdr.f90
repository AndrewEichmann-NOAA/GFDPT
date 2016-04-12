
PROGRAM cris_bufr2fmsdr
!-----------------------------------------------------------------------------------------------
! Name:         cris_bufr2fmsdr
! 
! Type:         F90 main program :
!
! Description:  convert NCEP CRIS BUFR data to 
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
! History: 2014/10/12 Eric S. Maddy - Created
!-----------------------------------------------------------------------------------------------

  USE Consts
  USE IO_MeasurData
  USE IO_InstrConfig
  USE ErrorHandling
  USE misc
  USE IO_Misc

  IMPLICIT NONE

  !---INTRINSIC functions used in this module
  INTRINSIC :: ADJUSTL,COUNT,INDEX,INT,MAXVAL,MINVAL,PACK,REAL,SIZE,SQRT,TRIM,ALL,MOD,LEN_TRIM

  ! Declare local parameters
  ! Number of channels for sensors in BUFR
  integer,parameter :: n_crischan = 399  !--- 399 subset ch out of 1305 ch for CRIS
  integer,parameter :: maxinfo    =  33
  integer,parameter :: maxfor     =  81000
  integer,parameter :: maxfovperfor   =  9
  integer,parameter :: maxtest    = 5
  real, DIMENSION(2,maxfor,maxfovperfor) :: radcoher
  real, DIMENSION(2,maxfor,maxtest)      :: coherence
  integer, DIMENSION(2) :: ichcoher=(/186,188/)   ! index into 399
  INTEGER                               :: nqc 
  real, DIMENSION(n_crischan)           :: angle
  real                                  :: angle1, rlat, rlon 
  CHARACTER(256)                        :: infile, outfile
  INTEGER                               :: LunIn
  INTEGER                               :: LunOut
  !---variables for BUFR library
  INTEGER, DIMENSION(maxtest*2+1)       :: qcflg
  INTEGER                               :: ix
  INTEGER                               :: nfile, ifile, AD
  INTEGER, PARAMETER                    :: len=256
  CHARACTER(LEN=10)                     :: OutFilePrefix
  CHARACTER(LEN=10)                     :: OutFileSuffix
  !---Pointers and other arrays
  CHARACTER(LEN=len), DIMENSION(:),       POINTER     :: BUFRFiles,BinOutFiles
  !---Namelist data 
  CHARACTER(LEN=len) :: BUFRFilesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: pathBinOut=DEFAULT_VALUE_STR4
  character(LEN=len) :: InstrConfigFile
  character(len=10)  :: jsatid

  real  ,parameter:: rad2deg1=PI/180.0
  real  ,parameter:: deg2rad1=180.0/PI

  !---variables for BUFR read
  real(8),dimension(7)           :: linele
  real(8),dimension(13)        :: allspot
  real(8),dimension(2,n_crischan)   :: allchan
  real(8),dimension(n_crischan)     :: temperature
  real(8)   :: rsat, radiance
  real      ,parameter:: R90    =  90.00
  real      ,parameter:: R360   = 360.00
  real      ,parameter:: tbmin  = 50.00
  real      ,parameter:: tbmax  = 550.00
  real      ,parameter:: ZERO   = 0.0
  
  real              :: dlat_earth, dlon_earth
  real, parameter   :: coh_min = 1.0
  real, parameter   :: coh_mid = 3.0
  real, parameter   :: coh_max = 7.0
  character(len=4)  :: senname
  character(len=80) :: allspotlist
  integer           :: nchanl, ll, i, j
  integer           :: iret, ireadmg,ireadsb, jstart
  real, dimension(n_crischan)  :: sndrbt
  character(8)  subset

  integer         ioffset,ioff
  integer         ksatid, kidsat 
  integer         idate
  integer         iskip,l,ifov
  integer         ndata 
  integer         iscan, nscanl
  integer         ntest
  integer,dimension(5) :: idate5
  integer         irec, nrec, nmeasure

  real   tdiff
  real   disterrmax,r01

  real   :: sat_zenang, sol_zenang, sat_aziang, sol_aziang
  real   :: UTC, solarZen, relAz
  integer julday, bad_line, ifor, iscn

  type(InstrConfig_type)        :: InstrConfig

  NAMELIST /ControlBUFRdump/BUFRFilesList, PathBinOut, jsatid, InstrConfigFile !, use_edges

  !-----------------------------------------------------
  !     Read control-data from namelist
  !-----------------------------------------------------
  READ(*,NML=ControlBUFRdump)

  CALL ReadInstrConfig(InstrConfigFile,InstrConfig)

  senname = 'CRIS'
  if(jsatid == 'npp') then  ! Suomi-NPP
     kidsat=224
  else 
     write(*,*) 'READ_CrIS: Unrecognized value for jsatid '//jsatid//': RETURNING'
     stop
  end if
  
  OutFilePrefix  = jsatid // senname
  OutFileSuffix  = '.bin'
  PRINT*, ' '//OutFilePrefix
  !---Read the file names of BUFR data and build output Binary files names
  CALL ReadList4(LunIn,trim(BUFRFilesList),BUFRFiles,nfile,BinOutFiles,pathBinout,&
       trim(OutFilePrefix),trim(OutFileSuffix))
  
  IF (nfile .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(BUFR files in convertBUFR)')

  !---initialize some variables and defaults
  LunIn      = 10  ! unit for BUFR file 
  LunOut     = 11  ! unit for output SceneAMV_type file
  nqc        = 2*maxtest+1   ! default number of QC parameters
  r01        = 0.01
  bad_line   = -1
  disterrmax = 0.0
  ntest      = 0
  ndata      = 0
  ifov       = -999
  tdiff      = 0.
  ix         = 1
  nchanl     = n_crischan
  allspotlist= &
     'SAID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA BEARAZ SOZA SOLAZI'
  ! cfreq from -chnm BUFR variable and NUCAPS file 1305 channels total
  ! /tmp052012/out/CrIS_d20120515_t2141229_e2141527.asc 
  ! https://www.wmo.int/pages/prog/sat/meetings/documents/RARS-ITSC18_Inf_02_NOAA-TR-133.pdf
  !------------------------------------------------------
  !     Loop over the CrIS BUFR files (instruments, etc.)
  !------------------------------------------------------

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
           !    Read CRIS FOV information
           call ufbint(lunin,linele,7,1,iret,'FOVN SLNM QMRKH MJFC HMSL FORN  (CRCHN)')

           !    Check that the number of channels in BUFR is what we are expecting
           if (nint(linele(7)) /= n_crischan) then 
              print *, linele(7)
             exit read_subset1
           endif
           if(linele(3) /= 0) cycle read_loop1
                                                 ! May want to eventually set to 
                                                 ! QMRHK <= 1, as data is, and I
                                                 ! quote, 'slightly suspect'

           if ( bad_line == nint(linele(2))) then
              !        zenith angle/scan spot mismatch, reject entire line
              cycle read_loop1
           else
              bad_line = -1
           endif

           ifov = nint(linele(1))               ! field of view
           ifor = nint(linele(6))               ! field of regard

           !  CRIS field-of-view ranges from 1 to 9, corresponding to the 9 sensors measured
           !  per field-of-regard.  The field-of-regard ranges from 1 to 30.  For reference, FOV 
           !  pattern within the FOR is :
           !                FOV#      7 8 9|7 8 9
           !                FOV#      4 5 6|4 5 6
           !                FOV#      1 2 3|1 2 3 (spacecraft velocity up the screen)
           !                ----------------------
           !                FOR#        x    x+1
           !  FORs are scanned from left limb (FOR=1) to right limb (FOR=30)
           !
           !  For now, we will simply choose IFOV=5.  See Fig. 58 of CrIS SDR ATBD (Rev. D) for a picture.
           
           !---Read CRIS channel number(CHNM) and radiance (SRAD)
           call ufbint(lunin,allchan,2,nchanl,iret,'SRAD CHNM')
           radcoher(1:2,nmeasure+1,ifov) = 1000.*allchan(1,ichcoher)
           !    Only use central IFOV
           if (ifov /= 5) cycle read_loop1
           
           iscn = nint(linele(2))               ! scan line
           
           !    Check field of view (FOVN), field-of-regard (FORN), and satellite zenith angle (SAZA)
           if( ifov < 1 .or. ifov > 9  .or. & ! FOVN not betw. 1 & 9
                ifor < 1 .or. ifor > 30 )then  ! FORN not betw. 1 & 30
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS INFO(FOVN,FORN,SLNM):', ifov, ifor, iscn
              cycle read_loop1
           endif
           call ufbint(lunin,allspot,13,1,iret,allspotlist)
           if(iret /= 1) cycle read_loop1

           !       Extract satellite id.  If not the one we want, read next record
           rsat=allspot(1) 
           ksatid=nint(allspot(1))
           if(ksatid /= kidsat) cycle read_loop1
           
           !    Check observing position
           dlat_earth = allspot(8)   ! latitude
           dlon_earth = allspot(9)   ! longitude
           if( abs(dlat_earth) > R90  .or. abs(dlon_earth) > R360 .or. &
                (abs(dlat_earth) == R90 .and. dlon_earth /= ZERO) )then
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS POINT (LAT,LON):', dlat_earth, dlon_earth
              cycle read_loop1
           endif
           !---Check obs time
           idate5(1) = nint(allspot(2)) ! year
           idate5(2) = nint(allspot(3)) ! month
           idate5(3) = nint(allspot(4)) ! day
           idate5(4) = nint(allspot(5)) ! hour
           idate5(5) = nint(allspot(6)) ! minute

           if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
                idate5(2) < 1    .or. idate5(2) >   12 .or. &
                idate5(3) < 1    .or. idate5(3) >   31 .or. &
                idate5(4) <0     .or. idate5(4) >   24 .or. &
                idate5(5) <0     .or. idate5(5) >   60 )then
              
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
              cycle read_loop1
              
           endif
           !    Observational info
           sat_zenang  = allspot(10)            ! satellite zenith angle
           !    Check satellite zenith angle (SAZA)
           if(sat_zenang > 90. ) then
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS INFO(SAZA):', allspot(10)
              cycle read_loop1
           endif
           
           !    Compare CRIS satellite scan angle and zenith angle
! 
!           look_angle_est = (start + float(ifor)*step)*deg2rad
!           sat_look_angle=asin(rato*sin(sat_zenang*deg2rad))
!           
!           if(abs(sat_look_angle)*rad2deg > MAX_SENSOR_ZENITH_ANGLE) then
!              write(6,*)'READ_CRIS WARNING lza error ',sat_look_angle,look_angle_est
!              cycle read_loop1
!           end if
!           
!           if (abs(sat_look_angle - look_angle_est)*rad2deg > one) then
!              bad_line = iscn
!              cycle read_loop1
!           endif

           !---Read CRIS channel number(CHNM) and radiance (SRAD)
           call ufbint(lunin,allchan,2,nchanl,iret,'SRAD CHNM')
           if( iret /= nchanl)then
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   iret, ' CH DATA IS READ INSTEAD OF ',nchanl
              cycle read_loop1
           endif
!           print *, allchan(2,1:nchanl)

           nmeasure=nmeasure+1
        END do read_loop1
     END do read_subset1
     coherence = 0.  ! zero out array
     do irec = 1, nmeasure
      do i = 1, 2
       !---max minus min contrast over FOR  
       coherence(i,irec,1) = & 
            maxval(radcoher(i,irec,1:9))- minval(radcoher(i,irec,1:9))
       !---max minus FOV 5 contrast over FOR  
       coherence(i,irec,2) = & 
            maxval(radcoher(i,irec,1:9))- radcoher(i,irec,5)
         
!      print *, i, irec
!      print *, radcoher(i,irec,1:9) 
!      print *, 'max(1) = ', maxval(radcoher(i,irec,1:9))-radcoher(i,irec,5)
!      print *, 'min(1) = ', minval(radcoher(i,irec,1:9))-radcoher(i,irec,5)
      enddo
!      print *, irec, coherence(1:2,irec)
     enddo
     nrec = 0
     !---Big loop to read data file
     irec = 0
     nscanl = 0
     nscanl = nmeasure
     print 110, nmeasure, nrec
110  FORMAT(2x,'Found ',i8, ' records out of ',i8, ' records.  Writing to file ...')
     CALL WriteHdrMeasurmts(BinOutFiles(iFile),lunOut,nmeasure,nqc,nchanl,nscanl,&
          InstrConfig%CentrFreq,InstrConfig%polarity,nscanl)  
     print *, InstrConfig%CentrFreq
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
           !    Read CRIS FOV information
           call ufbint(lunin,linele,7,1,iret,'FOVN SLNM QMRKH MJFC HMSL FORN  (CRCHN)')

           !    Check that the number of channels in BUFR is what we are expecting
           if (nint(linele(7)) /= n_crischan) then 
            exit read_subset
           endif
           if(linele(3) /= 0) cycle read_loop
                                                 ! May want to eventually set to 
                                                 ! QMRHK <= 1, as data is, and I
                                                 ! quote, 'slightly suspect'

           if ( bad_line == nint(linele(2))) then
              !        zenith angle/scan spot mismatch, reject entire line
              cycle read_loop
           else
              bad_line = -1
           endif

           ifov = nint(linele(1))               ! field of view
           ifor = nint(linele(6))               ! field of regard

           !  CRIS field-of-view ranges from 1 to 9, corresponding to the 9 sensors measured
           !  per field-of-regard.  The field-of-regard ranges from 1 to 30.  For reference, FOV 
           !  pattern within the FOR is :
           !                FOV#      7 8 9|7 8 9
           !                FOV#      4 5 6|4 5 6
           !                FOV#      1 2 3|1 2 3 (spacecraft velocity up the screen)
           !                ----------------------
           !                FOR#        x    x+1
           !  FORs are scanned from left limb (FOR=1) to right limb (FOR=30)
           !
           !  For now, we will simply choose IFOV=5.  See Fig. 58 of CrIS SDR ATBD (Rev. D) for a picture.
           

           !    Only use central IFOV
           if (ifov /= 5) cycle read_loop           
           iscn = nint(linele(2))               ! scan line
           
           !    Check field of view (FOVN), field-of-regard (FORN), and satellite zenith angle (SAZA)
           if( ifov < 1 .or. ifov > 9  .or. & ! FOVN not betw. 1 & 9
                ifor < 1 .or. ifor > 30 )then  ! FORN not betw. 1 & 30
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS INFO(FOVN,FORN,SLNM):', ifov, ifor, iscn
              cycle read_loop
           endif
           call ufbint(lunin,allspot,13,1,iret,allspotlist)
           if(iret /= 1) cycle read_loop

           !       Extract satellite id.  If not the one we want, read next record
           rsat=allspot(1) 
           ksatid=nint(allspot(1))
           if(ksatid /= kidsat) cycle read_loop
           
           !    Check observing position
           dlat_earth = allspot(8)   ! latitude
           dlon_earth = allspot(9)   ! longitude
           if( abs(dlat_earth) > R90  .or. abs(dlon_earth) > R360 .or. &
                (abs(dlat_earth) == R90 .and. dlon_earth /= ZERO) )then
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS POINT (LAT,LON):', dlat_earth, dlon_earth
              cycle read_loop
           endif
           !---Check obs time
           idate5(1) = nint(allspot(2)) ! year
           idate5(2) = nint(allspot(3)) ! month
           idate5(3) = nint(allspot(4)) ! day
           idate5(4) = nint(allspot(5)) ! hour
           idate5(5) = nint(allspot(6)) ! minute

           if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
                idate5(2) < 1    .or. idate5(2) >   12 .or. &
                idate5(3) < 1    .or. idate5(3) >   31 .or. &
                idate5(4) <0     .or. idate5(4) >   24 .or. &
                idate5(5) <0     .or. idate5(5) >   60 )then
              
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
              cycle read_loop
              
           endif
           UTC= allspot(7) + (idate5(5)*60.0) + (idate5(4)*3600.0)
           call compJulDay(idate5(1),idate5(2),idate5(3),julday)
           !    Observational info
           sat_zenang  = allspot(10)            ! satellite zenith angle
           !    Check satellite zenith angle (SAZA)
           if(sat_zenang > 90. ) then
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS INFO(SAZA):', allspot(10)
              cycle read_loop
           endif
           if ( ifor <= 15 ) sat_zenang = -sat_zenang
           
           !    Compare CRIS satellite scan angle and zenith angle
 
!           look_angle_est = (start + float(ifor)*step)*deg2rad
!           sat_look_angle=asin(rato*sin(sat_zenang*deg2rad))
!           
!           if(abs(sat_look_angle)*rad2deg > MAX_SENSOR_ZENITH_ANGLE) then
!              write(6,*)'READ_CRIS WARNING lza error ',sat_look_angle,look_angle_est
!              cycle read_loop
!           end if
!           
!           if (abs(sat_look_angle - look_angle_est)*rad2deg > one) then
!              write(6,*)' READ_CRIS WARNING uncertainty in look angle ', &
!                   look_angle_est*rad2deg,sat_look_angle*rad2deg,sat_zenang,sis,ifor,start,step,allspot(11),allspot(12),allspot(13)
!              bad_line = iscn
!              cycle read_loop
!           endif
           
           !---Read CRIS channel number(CHNM) and radiance (SRAD)
           call ufbint(lunin,allchan,2,nchanl,iret,'SRAD CHNM')
           if( iret /= nchanl)then
              write(6,*)'READ_CRIS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   iret, ' CH DATA IS READ INSTEAD OF ',nchanl
              cycle read_loop
           endif  
           iskip = 0
           jstart=1
           do i=1,nchanl
              !  Check that channel radiance is within reason and channel number is consistent with CRTM initialisation
              !  Negative radiance values are entirely possible for shortwave channels due to the high noise, but for
              !  now such spectra are rejected.  
              temperature(i)=zero
              if (( allchan(1,i) > zero .and. allchan(1,i) < 99999.) ) then 
!                 .and. &  ! radiance bounds
!                   (allchan(2,i) == ChannelInfo(1) % Sensor_Channel(i) )) then                  !---radiance to BT calculation
                 ! check unit --- ESM
                 radiance = allchan(1,i) * 1000.0    ! Conversion from W to mW
                 temperature(i) = PLANCK_TEMPERATURE(radiance,InstrConfig%CentrFreq(i))
                 if(temperature(i) < tbmin .or. temperature(i) > tbmax ) then
                    temperature(i) = min(tbmax,max(zero,temperature(i)))
                    iskip = iskip + 1
                 endif
              else           ! error with channel number or radiance
                 temperature(i) = min(tbmax,max(zero,temperature(i)))
                 iskip = iskip + 1
              endif
           end do
! sun glint angle < 25.0 ?? 
!  constants -- outside of loop
!  r90        = 90._r_kind
!  coscon     = cos( (r90-55.0_r_kind)*deg2rad )
!  sincon     = sin( (r90-55.0_r_kind)*deg2rad )
!
!              bearaz= (270._r_kind-data_s(ilazi_ang,n))*deg2rad
!              sun_zenith=data_s(iszen_ang,n)*deg2rad
!              sun_azimuth=(r90-data_s(isazi_ang,n))*deg2rad
!              sgagl =  acos(coscon * cos( bearaz ) * cos( sun_zenith ) * cos( sun_azimuth ) + &
!                       coscon * sin( bearaz ) * cos( sun_zenith ) * sin( sun_azimuth ) +  &
!                       sincon *  sin( sun_zenith )) * rad2deg

           solarZen = SNGL(allspot(12))                       ! solar zenith angle
           relAz    = SNGL(allspot(13))                       ! solar azimut angle
           rlat     = dlat_earth
           rlon     = dlon_earth
           angle1   = sat_zenang                      ! satellite zenith angle
           angle    = angle1
           qcflg(1) = 0
           nrec = nrec + 1
           j = 2
           do i = 1, maxtest
             !  if (coherence < coh_min) qc = 0  -- default
             qcflg(j) = 0
             if (coherence(1,nrec,i) >= coh_max) qcflg(j) = 4
             if (coherence(1,nrec,i) > coh_mid .and. &
               coherence(1,nrec,i) < coh_max) qcflg(j) = 2
             if (coherence(1,nrec,i) >= coh_min .and. &
               coherence(1,nrec,i) <= coh_mid) qcflg(j) = 1
             j = j+1
             !  if (coherence < coh_min) qc = 0  -- default
             qcflg(j) = 0
             if (coherence(2,nrec,i) >= coh_max) qcflg(j) = 4
             if (coherence(2,nrec,i) > coh_mid .and. &
               coherence(2,nrec,i) < coh_max) qcflg(j) = 2
             if (coherence(2,nrec,i) >= coh_min .and. &
               coherence(2,nrec,i) <= coh_mid) qcflg(j) = 1
             j = j+1
           enddo
           != allchan(l+ioffset)   ! brightness temerature           
           sndrbt(1:n_crischan) = SNGL(temperature(1:n_crischan))
           print *, sndrbt(1:10)
           AD = 1  
!           AD         = 0  --- test?
           CALL WRITEMEASURMTS(lunOut,nqc,qcflg,nchanl,angle,sndrbt,rlat,rlon,AD,UTC,julday,idate5(1),&
                ifov,iscn,relAz,solarZen)
        end do read_loop
     END do read_subset
     CALL CLOSBF(LunIn)
     !---close MeasurData record 
     CLOSE(LunOUT)
  END DO FilesLoop1

CONTAINS 
  FUNCTION Planck_Temperature(Radiance, Frequency) RESULT (Temperature)
    IMPLICIT NONE
    REAL(8), INTENT(in) :: Radiance
    REAL   , INTENT(in) :: Frequency
    REAL(8)  :: DFrequency
    REAL(8)  :: Temperature
    REAL(8)  :: x_c_1, x_c_2, Logarithm
    REAL(8), parameter :: ONE = 1.0d0
    REAL(8), parameter :: ZERO = 0.0d0
    REAL(8), parameter :: SCALE_FACTOR=1.0d0
    real(8), parameter :: BOLTZMNS = 1.3806503D-16 
    real(8), parameter :: PLANCKS  = 6.62606876D-27 
    real(8), parameter :: CLIGHT   = 2.99792458D+10     
    real(8), parameter :: C_1   = 2.0*PLANCKS*CLIGHT*CLIGHT
    real(8), parameter :: C_2   = PLANCKS*CLIGHT/BOLTZMNS 

    Temperature = -9999.9d0
    !---test for bad input
    DFrequency = DBLE(Frequency)
    IF (Radiance <= ZERO .or. DFrequency <= ZERO) return 

    x_c_1        = C_1 * (DFrequency*DFrequency*DFrequency)
    x_c_2        = C_2 * DFrequency
    Logarithm    = LOG( ( SCALE_FACTOR * x_c_1 / Radiance ) + ONE )
    Temperature  = x_c_2 / Logarithm

  END FUNCTION Planck_Temperature
     
END PROGRAM cris_bufr2fmsdr
