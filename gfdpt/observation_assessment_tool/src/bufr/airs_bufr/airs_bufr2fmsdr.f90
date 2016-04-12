PROGRAM airs_bufr2fmsdr
!-----------------------------------------------------------------------------------------------
! Name:         airs_bufr2fmsdr
! 
! Type:         F90 main program :
!
! Description:  convert NCEP AIRS BUFR data to 
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
! History: 2014/10/08 Eric S. Maddy - Created
!-----------------------------------------------------------------------------------------------

  USE Consts
  USE IO_MeasurData
  USE ErrorHandling
  USE IO_InstrConfig
  USE misc
  USE IO_Misc

  IMPLICIT NONE

  !---INTRINSIC functions used in this module
  INTRINSIC :: ADJUSTL,COUNT,INDEX,INT,MAXVAL,MINVAL,PACK,REAL,SIZE,SQRT,TRIM,ALL,MOD,LEN_TRIM

  ! Declare local parameters
  ! Number of channels for sensors in BUFR
  integer,parameter :: n_airschan = 281  !--- 281 subset ch out of 2378 ch for AIRS
  integer,parameter :: n_amsuchan =  15
  integer,parameter :: n_hsbchan  =   4
  integer,parameter :: n_totchan  = n_amsuchan+n_airschan+n_hsbchan+1
  integer,parameter :: n_totchanp4 = n_totchan + 4
  integer,parameter :: maxinfo    =  33
  !---variables for coherence tests
  integer,parameter :: maxfor       =  81000  ! max number of FORs per cycle
  integer,parameter :: maxfovperfor =  9
  integer,parameter :: maxtest      = 5
  real, DIMENSION(2,maxfor,maxfovperfor) :: radcoher
  real, DIMENSION(2,maxfor,maxtest)      :: coherence
  integer, DIMENSION(2) :: ichcoher=(/165,166/)   ! index into 281
  real, parameter   :: coh_min = 1.0
  real, parameter   :: coh_mid = 3.0
  real, parameter   :: coh_max = 7.0
  INTEGER                               :: nqc 
!  real , DIMENSION(n_airschan)          :: cfreq 
!  INTEGER, DIMENSION(n_airschan)        :: pol=0
  real, DIMENSION(n_airschan)           :: angle
  real                                  :: angle1, rlat, rlon 
  CHARACTER(256)                        :: infile, outfile
  INTEGER                               :: LunIn, lnbufrtab
  INTEGER                               :: LunOut
  !---variables for BUFR library
  INTEGER, DIMENSION(2)                 :: qcflg
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

  real  ,parameter:: rad2deg1=PI/180.0
  real  ,parameter:: deg2rad1=180.0/PI

  !---variables for BUFR read
  real(8),dimension(2)           :: aquaspot
  real(8),dimension(12,3)        :: allspot
  real(8),dimension(n_totchan)   :: allchan
  real(8),dimension(n_totchanp4) :: airflag

  real      ,parameter:: R90    =  90.00
  real      ,parameter:: R360   = 360.00
  real      ,parameter:: d1     = 0.75400
  real      ,parameter:: d2     = -2.26500
  real      ,parameter:: tbmin  = 50.00
  real      ,parameter:: tbmax  = 550.00
  real      ,parameter:: ZERO   = 0.0
  real              :: dlat_earth, dlon_earth
  integer   ,parameter:: MAXBUFR_OBS = 729000
  real              :: latitude(MAXBUFR_OBS)
  integer           :: scanline(MAXBUFR_OBS)
!  real              :: step, start,
  character(len=4)  :: senname
  character(len=80) :: allspotlist, table_file
  integer           :: nchanl,nchanlr, ll
  integer           :: iret, ireadmg,ireadsb
  real, dimension(n_airschan)  :: sndrbt
  character(8)  subset

  integer         ioffset,ioffset_acqf,ioff
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
  integer  :: julday, i, j
  logical airstab
  type(InstrConfig_type)        :: InstrConfig
  !---flag --- don't use FOVs at edges 
  ! logical use_edges

  NAMELIST /ControlBUFRdump/BUFRFilesList, PathBinOut, InstrConfigFile !, use_edges

  !-----------------------------------------------------
  !     Read control-data from namelist
  !-----------------------------------------------------
  READ(*,NML=ControlBUFRdump)

  CALL ReadInstrConfig(InstrConfigFile,InstrConfig)

  senname = 'AIRS'
  OutFilePrefix  = senname
  OutFileSuffix  = '.bin'
  PRINT*, ' '//OutFilePrefix
  !---Read the file names of BUFR data and build output Binary files names
  CALL ReadList4(LunIn,trim(BUFRFilesList),BUFRFiles,nfile,BinOutFiles,pathBinout,&
       trim(OutFilePrefix),trim(OutFileSuffix))
  
  IF (nfile .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(BUFR files in convertBUFR)')

  !---initialize some variables and defaults
  LunIn      = 10  ! unit for BUFR file 
  Lnbufrtab  = 12  ! unit for BUFR file 
  LunOut     = 11  ! unit for output SceneAMV_type file
  nqc        = 2*maxtest+1   ! default number of QC parameters
  r01        = 0.01
  disterrmax = 0.0
  ntest      = 0
  ndata      = 0
  ifov       = -999
  tdiff      = 0.
  ix         = 1
  nchanl  = n_airschan
  nchanlr = n_airschan
  !---what does this do? ESM
  !ioff=newchn(sis,1)-1
  ioffset=0
  ioffset_acqf=0
  !---what does this do ? ESM 
  !ichansst   = newchn(sis,914)
  !ichsst     = ichansst-ioff+ioffset
  allspotlist='SIID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA BEARAZ FOVN'
  !cfreq(1:n_airschan)  = 1.0  ! need to set this here
  ! from /airsb/regress/airs_281.lst and prog/airs/doc_281.pro
!  cfreq(1:n_airschan) = &
!       (/649.548, 650.742, 650.981, 651.700, 651.940, 652.903, 653.144, 653.385, 654.109, 654.352,&
!       654.594, 655.079, 655.807, 656.051, 656.538, 658.004, 658.740, 658.985, 659.477, 661.700,&
!       661.948, 662.445, 662.693, 662.942, 663.690, 664.439, 664.689, 665.943, 666.194, 666.698,&
!       666.950, 667.202, 667.454, 667.707, 667.959, 668.212, 668.466, 668.719, 668.972, 669.480,&
!       669.734, 669.989, 670.498, 672.031, 672.287, 673.572, 673.829, 674.345, 675.120, 675.378,&
!       676.156, 676.675, 676.935, 677.455, 678.238, 678.499, 680.071, 680.333, 681.386, 681.650,&
!       689.418, 689.689, 691.046, 691.318, 692.681, 692.955, 694.325, 694.600, 695.150, 695.977,&
!       696.806, 697.637, 697.915, 698.192, 698.748, 699.027, 699.305, 699.584, 700.142, 700.702,&
!       700.982, 701.542, 702.385, 702.666, 703.794, 704.359, 706.060, 706.914, 707.770, 708.628,&
!       709.488, 710.927, 711.215, 712.661, 714.112, 714.403, 715.862, 721.758, 722.055, 722.949,&
!       723.247, 724.443, 724.742, 726.244, 727.752, 734.067, 735.298, 735.607, 737.152, 738.704,&
!       742.142, 743.400, 745.928, 747.517, 752.970, 755.237, 759.485, 793.074, 801.001, 804.287,&
!       809.080, 820.731, 843.805, 871.201, 917.209, 918.649, 937.807, 948.080, 965.323, 979.017,&
!       1001.268,1005.146,1008.182,1010.362,1012.991,1016.516,1020.956,1030.405,1034.965,1036.340,&
!       1036.800,1039.102,1040.026,1040.952,1042.343,1055.975,1059.314,1061.231,1061.712,1063.155,&
!       1063.637,1065.085,1068.479,1072.383,1074.345,1092.314,1103.060,1106.686,1114.532,1123.017,&
!       1131.082,1135.428,1216.837,1218.359,1228.086,1236.397,1237.968,1251.213,1285.323,1291.555,&
!       1310.608,1315.898,1330.813,1334.442,1339.549,1345.174,1357.094,1367.110,1377.280,1381.066,&
!       1392.004,1396.985,1402.002,1407.620,1419.570,1427.072,1432.313,1437.005,1441.728,1468.661,&
!       1471.743,1476.079,1484.199,1493.043,1498.784,1502.636,1513.656,1518.896,1520.871,1524.173,&
!       1542.266,1544.298,1547.698,1551.796,1553.852,1555.915,1563.521,1567.701,1569.799,1571.903,&
!       1576.126,1586.065,1598.298,1604.849,2181.250,2182.155,2183.969,2184.876,2187.604,2188.514,&
!       2191.251,2195.827,2196.744,2197.663,2223.682,2229.336,2230.281,2235.968,2239.775,2248.388,&
!       2252.236,2378.164,2379.133,2380.103,2382.045,2383.017,2384.964,2385.938,2386.914,2387.890,&
!       2388.867,2389.845,2390.824,2391.803,2392.784,2393.765,2394.747,2395.729,2396.713,2397.698,&
!       2398.683,2399.669,2400.656,2401.643,2406.594,2412.562,2419.564,2445.918,2450.020,2454.135,&
!       2465.523,2491.792,2500.313,2513.201,2531.682,2540.470,2560.853,2600.214,2603.375,2607.601,&
!       2610.779,2616.095,2622.503,2632.175,2637.580,2639.748,2641.920,2648.457,2656.125,2657.225,&
!       2663.839/)
  !-------------------------------------------------
  !     Loop over the GOES files (instruments, etc.)
  !-------------------------------------------------

  !---Open BUFR table
  table_file = 'airs_bufr.table'      ! make table file name
  inquire(file=table_file,exist=airstab)
  write(6,*)'READ_AIRS:  Reading BUFR Table A file: ',trim(table_file)
  if (airstab) then
    open(lnbufrtab,file=trim(table_file))
    call openbf(lunin,'IN',lnbufrtab)
  else 
    call openbf(lunin,'IN',lunin)
  endif
   
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
           call ufbrep(lunin,allspot,12,3,iret,allspotlist)
           if(iret /= 3) cycle read_loop1

           sat_aziang=allspot(11,ix)
           if (abs(sat_aziang) > r360) then
              cycle read_loop1
           endif

           !---Remove data on edges
           ifov = nint( allspot(12,ix) )  !---fov number
!           if (.not. use_edges .and. &
!                (ifov < radedge_min .OR. ifov > radedge_max )) cycle read_loop
           
           !---Check observational info
           sat_zenang  = allspot(10,ix) 
           if( ifov < 0 .or. ifov > 100 .or. abs(sat_zenang) > 360.) then
              
              write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS INFO(FOV,SAZA):', allspot(12,ix), allspot(10,ix)
              cycle read_loop1
              
           endif
           
           dlat_earth = allspot(8,ix)
           dlon_earth = allspot(9,ix)
           !---Check observing position
           if( abs(dlat_earth) > R90  .or. abs(dlon_earth) > R360 .or. &
                (abs(dlat_earth) == R90 .and. dlon_earth /= ZERO) )then
              cycle read_loop1
           endif
           
           !---Retrieve observing position
           if(dlon_earth >= R360)then
              dlon_earth = dlon_earth - R360
           endif
           !---Check obs time
           idate5(1) = nint(allspot(2,ix)) ! year
           idate5(2) = nint(allspot(3,ix)) ! month
           idate5(3) = nint(allspot(4,ix)) ! day
           idate5(4) = nint(allspot(5,ix)) ! hour
           idate5(5) = nint(allspot(6,ix)) ! minute

           if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
                idate5(2) < 1    .or. idate5(2) >   12 .or. &
                idate5(3) < 1    .or. idate5(3) >   31 .or. &
                idate5(4) <0     .or. idate5(4) >   24 .or. &
                idate5(5) <0     .or. idate5(5) >   60 )then
              
              write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
              cycle read_loop1
              
           endif
           UTC= allspot(7,ix) + (idate5(5)*60.0) + (idate5(4)*3600.0)
           call compJulDay(idate5(1),idate5(2),idate5(3),julday)
           !---Read the brightness temperatures
           call ufbrep(lunin,allchan,1,n_totchan,iret,'TMBR')
           if( iret /= n_totchan)then
              write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   iret, ' TEMPERATURE CH DATA IS READ INSTEAD OF ',n_totchan
              cycle read_loop1
           endif
           
           radcoher(1:2,nmeasure+1,ifov) = allchan(ichcoher)
           !---Set common predictor parameters
           
           sat_aziang  = allspot(11,ix)  
           
           !---Read AQUASPOT
           call ufbint(lunin,aquaspot,2,1,iret,'SOZA SOLAZI')
           sol_zenang = aquaspot(1)

           ! qc based on tests from Wisc.  
           ! compare SST to calculated SST from AIRS
           ! based on surface type.
           ! tests require deter_sfc and other info
           ! skipped for now (see read_airs.f90)
           
           !---check for missing channels (if key channel reject)
           iskip = 0
           do l=1+ioffset,nchanl+ioffset
              ll=(l-ioffset)+ioff
              ! lluse = iuse_rad(ll) >= 0
              ! if( lluse .and. (allchan(l)<tbmin .or. allchan(l)>tbmax) ) then
              if( (allchan(l)<tbmin .or. allchan(l)>tbmax) ) then
                 iskip = iskip + 1
              endif
           end do

           if( iskip >= nchanl )cycle read_loop1
           !---Replace popped AIRS channel Tb with zero
           !---(should be moved before iskip)
           !---Read the quality flag. Will catch "popped" channels
           call ufbrep(lunin,airflag,1,n_totchanp4,iret,'ACQF')
           if( iret /= n_totchanp4)then
              write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   iret, ' QUALITY FLAG DATA IS READ INSTEAD OF ',n_totchanp4
              cycle read_loop1
           endif
           do l=1,nchanl
              if (airflag(l+ioffset_acqf) /= zero) allchan(l+ioffset) = zero
           end do

           sol_aziang = aquaspot(2)
           nmeasure=nmeasure+1
        END do read_loop1
     END do read_subset1

     !---Big loop to read data file
     irec=0
     nscanl = 0
     nscanl = nmeasure
     print 110, nmeasure, nrec
110  FORMAT(2x,'Found ',i8, ' records out of ',i8, ' records.  Writing to file ...')
     CALL WriteHdrMeasurmts(BinOutFiles(iFile),lunOut,nmeasure,nqc,nchanl,nscanl,&
          InstrConfig%CentrFreq,InstrConfig%polarity,nscanl)  
     infile = BUFRFiles(ifile)
     outfile = BinOutFiles(ifile)
     
     !---open the BUFR file and write to IO_MeasureData format
     CALL CLOSBF(lunin)
     OPEN(lunin,file=infile,form='unformatted')
     CALL OPENBF(lunin,'IN',lunin)
     CALL DATELEN(10)
  
     coherence = 0.  ! zero out array
     do irec = 1, nmeasure
      do i = 1, 2
       !---max minus min contrast over FOR  
       coherence(i,irec,1) = & 
            maxval(radcoher(i,irec,1:maxfovperfor))- minval(radcoher(i,irec,1:maxfovperfor))
       !---max minus FOV 5 contrast over FOR  
       coherence(i,irec,2) = & 
            maxval(radcoher(i,irec,1:maxfovperfor))- radcoher(i,irec,5)
!      print *, i, irec
!      print *, radcoher(i,irec,1:maxfovperfor) 
!      print *, 'max(1) = ', maxval(radcoher(i,irec,1:maxfovperfor))-radcoher(i,irec,maxfovperfor)
!      print *, 'min(1) = ', minval(radcoher(i,irec,1:maxfovperfor))-radcoher(i,irec,5)
      enddo
!      print *, irec, coherence(1:2,irec)
     enddo
     read_subset: do while(ireadmg(LunIn,subset,idate)>=0)
        irec=irec+1
        read_loop: do while (ireadsb(LunIn)==0)
           nrec = nrec + 1
           call ufbrep(lunin,allspot,12,3,iret,allspotlist)
           if(iret /= 3) cycle read_loop

           sat_aziang=allspot(11,ix)
           if (abs(sat_aziang) > r360) then
              cycle read_loop
           endif

           !---Remove data on edges
           ifov = nint( allspot(12,ix) ) !---fov number
!           if (.not. use_edges .and. &
!                (ifov < radedge_min .OR. ifov > radedge_max )) cycle read_loop
           
           !---Check observational info
           sat_zenang  = allspot(10,ix) 
           if( ifov < 0 .or. ifov > 100 .or. abs(sat_zenang) > 360.) then
              
              write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS INFO(FOV,SAZA):', allspot(12,ix), allspot(10,ix)
              cycle read_loop
              
           endif
           
           dlat_earth = allspot(8,ix)
           dlon_earth = allspot(9,ix)
           !---Check observing position
           if( abs(dlat_earth) > R90  .or. abs(dlon_earth) > R360 .or. &
                (abs(dlat_earth) == R90 .and. dlon_earth /= ZERO) )then
              cycle read_loop
           endif
           
           !---Retrieve observing position
           if(dlon_earth >= R360)then
              dlon_earth = dlon_earth - R360
           endif
           !---Check obs time
           idate5(1) = nint(allspot(2,ix)) ! year
           idate5(2) = nint(allspot(3,ix)) ! month
           idate5(3) = nint(allspot(4,ix)) ! day
           idate5(4) = nint(allspot(5,ix)) ! hour
           idate5(5) = nint(allspot(6,ix)) ! minute

           if( idate5(1) < 1900 .or. idate5(1) > 3000 .or. &
                idate5(2) < 1    .or. idate5(2) >   12 .or. &
                idate5(3) < 1    .or. idate5(3) >   31 .or. &
                idate5(4) <0     .or. idate5(4) >   24 .or. &
                idate5(5) <0     .or. idate5(5) >   60 )then
              
              write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
              cycle read_loop
              
           endif
           UTC= allspot(7,ix) + (idate5(5)*60.0) + (idate5(4)*3600.0)
           call compJulDay(idate5(1),idate5(2),idate5(3),julday)

           !---Read the brightness temperatures
           call ufbrep(lunin,allchan,1,n_totchan,iret,'TMBR')
           if( iret /= n_totchan)then
              write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   iret, ' TEMPERATURE CH DATA IS READ INSTEAD OF ',n_totchan
              cycle read_loop
           endif
           
           !---Set common predictor parameters
           
           sat_aziang  = allspot(11,ix)  
           
           !---Read AQUASPOT
           call ufbint(lunin,aquaspot,2,1,iret,'SOZA SOLAZI')
           sol_zenang = aquaspot(1)

           ! qc based on tests from Wisc.  
           ! compare SST to calculated SST from AIRS
           ! based on surface type.
           ! tests require deter_sfc and other info
           ! skipped for now (see read_airs.f90)
           
           !---check for missing channels (if key channel reject)
           iskip = 0
           do l=1+ioffset,nchanl+ioffset
              ll=(l-ioffset)+ioff
              ! lluse = iuse_rad(ll) >= 0
              ! if( lluse .and. (allchan(l)<tbmin .or. allchan(l)>tbmax) ) then
              if( (allchan(l)<tbmin .or. allchan(l)>tbmax) ) then
                 iskip = iskip + 1
              endif
           end do

           if( iskip >= nchanl )cycle read_loop
           !---Replace popped AIRS channel Tb with zero
           !---(should be moved before iskip)
           !---Read the quality flag. Will catch "popped" channels
           call ufbrep(lunin,airflag,1,n_totchanp4,iret,'ACQF')
           if( iret /= n_totchanp4)then
              write(6,*)'READ_AIRS:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   iret, ' QUALITY FLAG DATA IS READ INSTEAD OF ',n_totchanp4
              cycle read_loop
           endif
           do l=1,nchanl
              if (airflag(l+ioffset_acqf) /= zero) allchan(l+ioffset) = zero
           end do

           sol_aziang = aquaspot(2)
           solarZen = sol_zenang                       ! solar zenith angle
           relAz    = sol_aziang                       ! solar azimut angle
           rlat     = dlat_earth
           rlon     = dlon_earth
           angle1   = sat_zenang                       ! satellite zenith angle
           angle    = angle1
           qcflg(1) = 0
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
           sndrbt(1:n_airschan) = SNGL(allchan(1:n_airschan))
           AD = 1  
!           AD         = 0  --- test?
           CALL WRITEMEASURMTS(lunOut,nqc,qcflg,nchanl,angle,sndrbt,rlat,rlon,AD,UTC,julday,idate5(1),&
                ifov,iscan,relAz,solarZen)
        end do read_loop
     END do read_subset
     CALL CLOSBF(LunIn)
     !---close MeasurData record 
     CLOSE(LunOUT)
     CLOSE(lnbufrtab)
  END DO FilesLoop1
     
END PROGRAM airs_bufr2fmsdr
