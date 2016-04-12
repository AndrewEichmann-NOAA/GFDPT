PROGRAM goessndr_bufr2fmsdr
!-----------------------------------------------------------------------------------------------
! Name:         goessndr_bufr2fmsdr
! 
! Type:         F90 main program
!
! Description:
!
! Modules needed:
!       - Consts
!       - ErrorHandling
!       - SceneAMV_type
!       - BUFR
!
! Subroutines contained:
!       None
!
! Data type included:
!
! 
! History:
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
  real , DIMENSION(18)                  :: cfreq 
  INTEGER, DIMENSION(18)                :: pol
  real, DIMENSION(18)                   :: angle
  real                                  :: angle1, rlat, rlon 
  CHARACTER(256)                        :: infile, outfile
  INTEGER                               :: LunIn
  INTEGER                               :: LunOut
  INTEGER                               :: j, k1, k2, kl, m
  !---variables for BUFR library
  
  INTEGER, DIMENSION(2)                 :: qcflg
  INTEGER                               :: i1, i2   ! --- string concatenation
  INTEGER                               :: nfile, ifile, AD
  INTEGER, PARAMETER                    :: len=256
  CHARACTER(LEN=10)                     :: OutFilePrefix
  CHARACTER(LEN=10)                     :: OutFileSuffix
  !---Pointers and other arrays
  CHARACTER(LEN=len), DIMENSION(:),       POINTER     :: BUFRFiles,BinOutFiles
  CHARACTER(LEN=1)   :: node 
  !---Namelist data 
  CHARACTER(LEN=len) :: BUFRFilesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: pathBinOut=DEFAULT_VALUE_STR4
  character(len=10)  :: jsatid, obstype

! Declare local parameters
  integer ,parameter:: maxinfo=33
  integer ,parameter:: mfov=25   ! maximum number of fovs (currently 5x5)

  real  ,parameter:: r360=360.0
  real  ,parameter:: tbmin=50.0
  real  ,parameter:: tbmax=550.0
  real  ,parameter:: rad2deg1=PI/180.0
  real  ,parameter:: deg2rad1=180.0/PI

  character(80),parameter:: hdstr = &
     'CLON CLAT ELEV SOEL BEARAZ SOLAZI SAID DINU YEAR MNTH DAYS HOUR MINU SECO ACAV' 
  character(80),parameter:: hdstr5 = &
     'XOB YOB ELEV SOEL BEARAZ SOLAZI SAID TYP ACAV DHR SID '
  character(80),parameter:: rbstr = 'TMBR'

! Declare local variables
  logical outside,iuse,g5x5,assim

  character(8)  subset

  integer         kx,levs,ldetect
  integer         lnbufr,nchanl,nreal,iret,ksatid,lsatid
  integer         idate
  integer         ilat,ilon,isflg,idomsfc
  integer         itx,k,i,itt,iskip,l,ifov,n
  integer         ichan8,ich8, ndata 
  integer         nele,iscan,nmind, nscanl, nscan
  integer         ntest,ireadsb,ireadmg,irec,isub,next
  integer        ,dimension(5):: idate5
  integer         ibfms         ! BUFR missing value function
  integer         nrec, nmeasure

  real   dlon,dlat,timedif,emiss,sfcr
  real   dlon_earth,dlat_earth, two
  real   ch8,sstime
  real   pred,crit1,tdiff,dist1,toff,t4dv
  real   disterr,disterrmax,dlon00,dlat00,r01

  real  ,dimension(0:4):: rlndsea
  real  ,dimension(0:3):: sfcpct
  real  ,dimension(0:3):: ts
  real   :: tsavg,vty,vfr,sty,stp,sm,sn,zz,ff10
  real   :: zob,tref,dtw,dtc,tz_tr, UTC, solarZen, relAz
  integer julday

  real  ,allocatable,dimension(:,:):: data_all
  real  bmiss 
  ! tests for bmiss==1e9, bu
  real(8),dimension(15):: hdr
  real(8),dimension(18):: grad
  real   ,dimension(18):: sndrbt
  
  NAMELIST /ControlBUFRdump/BUFRFilesList, PathBinOut, jsatid, obstype

  !-----------------------------------------------------
  !     Read control-data from namelist
  !-----------------------------------------------------
  READ(*,NML=ControlBUFRdump)

  !--- which goes instrument are we parsing? which detector to dump?
  g5x5 = jsatid == 'g08_prep' .or. jsatid == 'g09_prep' .or.     &
         jsatid == 'g10_prep' .or. jsatid == 'g11_prep' .or.     &
         jsatid == 'g12_prep' .or. jsatid == 'g13_prep' .or.     &
         jsatid == 'g14_prep' .or. jsatid == 'g15_prep'

  !---do we want to read the prepbufr data? 
  if(g5x5)then
       if(jsatid=='g08_prep')lsatid=252
       if(jsatid=='g09_prep')lsatid=253
       if(jsatid=='g10_prep')lsatid=254
       if(jsatid=='g11_prep')lsatid=255
       if(jsatid=='g12_prep')lsatid=256
       if(jsatid=='g13_prep')lsatid=257
       if(jsatid=='g14_prep')lsatid=258
       if(jsatid=='g15_prep')lsatid=259
   else
       if(jsatid=='g08')lsatid=252
       if(jsatid=='g09')lsatid=253
       if(jsatid=='g10')lsatid=254
       if(jsatid=='g11')lsatid=255
       if(jsatid=='g12')lsatid=256
       if(jsatid=='g13')lsatid=257
       if(jsatid=='g14')lsatid=258
       if(jsatid=='g15')lsatid=259
       if(obstype == 'sndrd1')ldetect = 1
       if(obstype == 'sndrd2')ldetect = 2
       if(obstype == 'sndrd3')ldetect = 3
       if(obstype == 'sndrd4')ldetect = 4
  end if
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
  r01       = 0.01
  bmiss     = 1.0e9
  disterrmax=0.0
  ntest     = 0
  ich8      = 8        !channel 8
  ndata     = 0
  nchanl    = 18       
  ifov      = -999
  tdiff     = 0.
  !  wave = (/14.7100,     14.3700,     14.0600,     13.6400,     13.3700,     12.6600,&
  !       12.0200,     11.0300,      9.7100,      7.4300,      7.0200,      6.5100,&
  !       4.5700,      4.5200,      4.4500,      4.1300,      3.9800,      3.7400/)
  cfreq  = (/20380.1798,  20862.3842,  21322.3630,  21978.9184,  22422.7723,  23680.2875,&
       24941.1351,  27179.7337,  30874.6073,  40348.9193,  42705.4764,  46051.0672,&
       65600.0932,  66325.7623,  67369.0912,  72588.9738,  75324.7338,  80158.4091/)

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
           !---Extract type, date, and location information
!           print *, 'g5x5=',g5x5
           if(g5x5)then
              !---Prepbufr file
              call ufbint(LunIn,hdr,11,1,iret,hdstr5)
              kx = hdr(8)
              if(kx /= 164 .and. kx /= 165 .and. kx /= 174 .and. kx /= 175)cycle read_loop1
              !          If not goes data over ocean , read next bufr record
              !          if(kx /= 174 .and. kx /= 175)cycle read_loop
              
              ksatid=nint(hdr(7))
              !---if not proper satellite read next bufr record
              if (ksatid /= lsatid) cycle read_loop1
              
              !---Extract number of averaged FOVS
              ifov = hdr(9) ! number of averaged FOVS 
              if(ifov <= 3) cycle read_loop1
              !---Extract obs time difference. 
              tdiff=hdr(10)  ! relative obs time in hours
              t4dv=toff+tdiff
           else
              !---GOES 1x1 or 5x5 file
              call ufbint(LunIn,hdr,15,1,iret,hdstr)
              
              ksatid=hdr(7)   !bufr satellite id
              !---if not proper satellite/detector read next bufr record
!              print *, 'ksatid=',ksatid, lsatid
              if (ksatid /=lsatid) cycle read_loop1

              if(obstype /= 'sndr')then
!                 print *, 'ldetect=',ldetect, nint(hdr(8))
                 if(ldetect /= nint(hdr(8)))cycle read_loop1
              end if
              
              !---test for case when hdr(15) comes back with bmiss signifying 1x1 data
              !      write(6,'(" in read_goesndr, bmiss,hdr(15)=",2ES25.18)')bmiss,hdr(15) !???????for debug only
!              print *,abs(dble(hdr(15))-bmiss), epsilon, hdr(15)
              !---test for case when hdr(15) comes back with bmiss signifying 1x1 data
              if (ibfms(hdr(15)) .eq. 1) then
!              if (abs(dble(hdr(15))-bmiss)<epsilon.or.hdr(15)>bmiss) then !???bad way to test???
                 ifov = 0
              else ! 5x5 data
                 ifov = nint(hdr(15)) ! number of averaged FOVS
                 if(ifov < mfov .and. ifov > 0)then
                    if(ifov <= 3) cycle read_loop1  !---skip < 3 fovs in average
                 end if
              endif

              !          Extract obs time.  If not within analysis window, skip obs
              !          Extract date information.  If time outside window, skip this obs
              !              idate5(1) = nint(hdr(9))  !year
              !              idate5(2) = nint(hdr(10)) !month
              !              idate5(3) = nint(hdr(11)) !day
              !              idate5(4) = nint(hdr(12)) !hour
              !              idate5(5) = nint(hdr(13)) !minute
              !              
              !              UTC= hdr(14) + (idate5(5)*60.0) + (idate5(4)*3600.0)
              !              call compJulDay(idate5(1),idate5(2),idate5(3),julday)
              
           end if

           !       If not within analysis window, skip obs
           !       ESM commented out 
           !       twind    - input group time window(hours)
           !        if (l4dvar) then
           !           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
           !        else
           !        if (abs(tdiff)>twind) cycle read_loop
           !        endif
           
           !---test for bad lat/lon data
           if(abs(hdr(2))>90.0 .or. abs(hdr(1))>360.0) cycle read_loop1
           !---Convert obs location to radians
           if (hdr(1)==360.0) hdr(1)=0.0
           if (hdr(1)< 0.0) hdr(1)=hdr(1)+360.0
           
           
           !---Set common predictor parameters
           
           timedif = 6.0*abs(tdiff)        ! range:  0 to 18
           
           crit1=0.01+timedif
           if(ifov < mfov .and. ifov > 0)then
              crit1=crit1+2.0*float(mfov-ifov)
           end if
           
           ! thinning of GOES data 
           !        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
           !        if(.not. iuse)cycle read_loop
           
           !---Increment goes sounder data counter
           !---Extract brightness temperatures
           call ufbint(LunIn,grad,1,18,levs,rbstr)
           
           iskip = 0
           do l=1,nchanl
              
              if( grad(l) < tbmin .or. grad(l) > tbmax )then
                 iskip = iskip + 1
                 if(l == ich8)iskip = nchanl
              endif
           end do
           
           if( iskip >= nchanl )cycle read_loop1
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
           
           !---Extract type, date, and location information
           if(g5x5)then
              !---Prepbufr file
              call ufbint(LunIn,hdr,11,1,iret,hdstr5)
              kx = hdr(8)
              if(kx /= 164 .and. kx /= 165 .and. kx /= 174 .and. kx /= 175)cycle read_loop
              !          If not goes data over ocean , read next bufr record
              !          if(kx /= 174 .and. kx /= 175)cycle read_loop
              
              ksatid=nint(hdr(7))
              !---if not proper satellite read next bufr record
              if (ksatid /= lsatid) cycle read_loop
              
              !---Extract number of averaged FOVS
              ifov = hdr(9) ! number of averaged FOVS 
              if(ifov <= 3) cycle read_loop
              !---Extract obs time difference. 
              tdiff=hdr(10)  ! relative obs time in hours
              t4dv=toff+tdiff
           else
              !---GOES 1x1 or 5x5 file
              call ufbint(LunIn,hdr,15,1,iret,hdstr)
              
              ksatid=hdr(7)   !bufr satellite id
              !---if not proper satellite/detector read next bufr record
              if (ksatid /=lsatid) cycle read_loop
              if(obstype /= 'sndr')then
                 if(ldetect /= nint(hdr(8)))cycle read_loop
              end if
              
              !---test for case when hdr(15) comes back with bmiss signifying 1x1 data
              !      write(6,'(" in read_goesndr, bmiss,hdr(15)=",2ES25.18)')bmiss,hdr(15) !???????for debug only
              !              if (abs(dble(hdr(15))-bmiss)<epsilon.or.hdr(15)>bmiss) then !???bad way to test???
              if (ibfms(hdr(15)) .eq. 1) then
                 ifov = 0
              else ! 5x5 data
                 ifov = nint(hdr(15)) ! number of averaged FOVS
                 if(ifov < mfov .and. ifov > 0)then
                    if(ifov <= 3) cycle read_loop  !---skip < 3 fovs in average
                 end if
              end if
              ! Extract obs time.  If not within analysis window, skip obs
              ! Extract date information.  If time outside window, skip this obs
              idate5(1) = nint(hdr(9))  !year
              idate5(2) = nint(hdr(10)) !month
              idate5(3) = nint(hdr(11)) !day
              idate5(4) = nint(hdr(12)) !hour
              idate5(5) = nint(hdr(13)) !minute
      
              UTC= hdr(14) + (idate5(5)*60.0) + (idate5(4)*3600.0)
              call compJulDay(idate5(1),idate5(2),idate5(3),julday)
              
           end if

           !       If not within analysis window, skip obs
           !       ESM commented out 
           !       twind    - input group time window(hours)
           !        if (l4dvar) then
           !           if (t4dv<zero .OR. t4dv>winlen) cycle read_loop
           !        else
           !        if (abs(tdiff)>twind) cycle read_loop
           !        endif

           !---test for bad lat/lon data
           if(abs(hdr(2))>90.0 .or. abs(hdr(1))>360.0) cycle read_loop
           !---Convert obs location to radians
           if (hdr(1)==360.0) hdr(1)=0.0
!           if (hdr(1)< 0.0) hdr(1)=hdr(1)+360.0
           
           !           dlon_earth = hdr(1)*deg2rad1   !convert degrees to radians
           !           dlat_earth = hdr(2)*deg2rad1
           !           
           !           dlon = dlon_earth 
           !           dlat = dlat_earth 

           !---Set common predictor parameters
           
           timedif = 6.0*abs(tdiff)        ! range:  0 to 18
           
           crit1=0.01+timedif
           if(ifov < mfov .and. ifov > 0)then
              crit1=crit1+2.0*float(mfov-ifov)
           end if

           ! thinning of GOES data 
           !        call map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis)
           !        if(.not. iuse)cycle read_loop
           
           !---Increment goes sounder data counter
           !---Extract brightness temperatures
           call ufbint(LunIn,grad,1,18,levs,rbstr)
           
           iskip = 0
           do l=1,nchanl
              
              if( grad(l) < tbmin .or. grad(l) > tbmax )then
                 iskip = iskip + 1
                 if(l == ich8)iskip = nchanl
              endif
              sndrbt(l) = sngl(grad(l))
           end do

           if( iskip >= nchanl )cycle read_loop
        
           !---"Score" observation.   We use this information to id "best" obs.

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
           
           !---Set data quality predictor
           iscan   = nint(hdr(3))+0.001   ! "scan" position
           !           if (newpc4pred) then
           !              ch8     = grad(ich8) -ang_rad(ichan8)*cbias(iscan,ichan8) -  &
           !                   predx(1,ichan8)*air_rad(ichan8)
           !           else
           !              ch8     = grad(ich8) -ang_rad(ichan8)*cbias(iscan,ichan8) -  &
           !                   r01*predx(1,ichan8)*air_rad(ichan8)
           !           end if
           !           emiss=0.992-0.013*(hdr(3)/65.0)**3.5-0.026*(hdr(3)/65.0)**7.0
           !           pred = abs(ch8-tsavg*emiss)
           !           
           !           !---Compute "score" for observation.  All scores>=0.0.  Lowest score is "best"
           !           crit1 = crit1+10.0*pred  
           
           !---what does this do?
           !   -- satthin::finalcheck returns logical based on various stuff
           ! call finalcheck(dist1,crit1,itx,iuse)
           !if(.not. iuse)cycle read_loop
           
           !---interpolate NSST variables to Obs. location and get dtw, dtc, tz_tr
           !
           !if ( nst_gsi > 0 ) then
           !  tref  = ts(0)
           !  dtw   = zero
           !  dtc   = zero
           !  tz_tr = one
           !  if ( sfcpct(0) > zero ) then
           !     call deter_nst(dlat_earth,dlon_earth,t4dv,zob,tref,dtw,dtc,tz_tr)
           !  endif
           !endif
           
           !---Transfer observation location and other data to local arrays
           
           !        data_all(1,itx) = ksatid                       ! satellite id
           !        data_all(3,itx) = dlon                         ! grid relative longitude
           !        data_all(4,itx) = dlat                         ! grid relative latitude
           !        data_all(5,itx) = hdr(3)*deg2rad1               ! satellite zenith angle
           !        data_all(6,itx) = hdr(5)                       ! satellite azimut angle
           !        data_all(7,itx) = zero                         ! local zenith angle
           !        data_all(8,itx) = iscan                        ! "scan" position
           !        data_all(9,itx) = hdr(4)                       ! solar zenith angle
           !        data_all(10,itx)= hdr(6)                       ! solar azimut angle
           !        data_all(11,itx) = sfcpct(0)                   ! sea percentage of
           !        data_all(12,itx) = sfcpct(1)                   ! land percentage
           !        data_all(13,itx) = sfcpct(2)                   ! sea ice percentage
           !        data_all(14,itx) = sfcpct(3)                   ! snow percentage
           !        data_all(15,itx)= ts(0)                        ! ocean skin temperature
           !        data_all(16,itx)= ts(1)                        ! land skin temperature
           !        data_all(17,itx)= ts(2)                        ! ice skin temperature
           !        data_all(18,itx)= ts(3)                        ! snow skin temperature
           !        data_all(19,itx)= tsavg                        ! average skin temperature
           !        data_all(20,itx)= vty                          ! vegetation type
           !        data_all(21,itx)= vfr                          ! vegetation fraction
           !        data_all(22,itx)= sty                          ! soil type
           !        data_all(23,itx)= stp                          ! soil temperature
           !        data_all(24,itx)= sm                           ! soil moisture
           !        data_all(25,itx)= sn                           ! snow depth
           !        data_all(26,itx)= zz                           ! surface height
           !        data_all(27,itx)= idomsfc + 0.001              ! dominate surface type
           !        data_all(28,itx)= sfcr                         ! surface roughness
           !        data_all(29,itx)= ff10                         ! ten meter wind factor
           !        data_all(30,itx)= dlon_earth*rad2deg1           ! earth relative longitude (degrees)
           !        data_all(31,itx)= dlat_earth*rad2deg1           ! earth relative latitude (degrees)
           !        ! what is this ? 
           !        data_all(32,itx)= val_goes
           !        data_all(33,itx)= itt
           
           !if ( nst_gsi > 0 ) then
           !  data_all(maxinfo+1,itx) = tref         ! foundation temperature
           !  data_all(maxinfo+2,itx) = dtw          ! dt_warm at zob
           !  data_all(maxinfo+3,itx) = dtc          ! dt_cool at zob
           !  data_all(maxinfo+4,itx) = tz_tr        ! d(Tz)/d(Tr)
           !endif
           
           !do k=1,nchanl
           !   data_all(k+nreal,itx)=grad(k)
           !end do
           solarZen = sngl(hdr(4))                       ! solar zenith angle
           relAz    = sngl(hdr(6))                       ! solar azimut angle
           rlat     = sngl(hdr(2))
           rlon     = sngl(hdr(1))
!           IF (rlon.
!           SUBROUTINE WriteMeasurmts(iu,nqc,qc,nchan,angle,tb,lat,lon,node,&
!                scanUTC,scanDAY,scanYear,iscanPos,iscanLine,RelAziAngle,SolZenAngle)
           angle1   = sngl(hdr(3))                     ! satellite zenith angle
           angle    = angle1
           qcflg(1:2) = 0
           AD         = 0
           CALL WRITEMEASURMTS(lunOut,nqc,qcflg,nchanl,angle,sndrbt,rlat,rlon,AD,UTC,julday,idate5(1),&
                ifov,iscan,relAz,solarZen)
        end do read_loop
     END do read_subset
     CALL CLOSBF(LunIn)
     !---close MeasurData record 
     CLOSE(LunOUT)
  END DO FilesLoop1
     
END PROGRAM goessndr_bufr2fmsdr
