PROGRAM iasi_bufr2fmsdr
!-----------------------------------------------------------------------------------------------
! Name:         iasi_bufr2fmsdr
! 
! Type:         F90 main program :
!
! Description:  convert NCEP IASI BUFR data to 
!               MIRS FMSDR format
!
! Modules needed:
!       - Consts
!       - ErrorHandling
!       - BUFR
!       - misc
!       - IO_Misc
!       - IO_MeasurData
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
  USE misc
  USE IO_Misc

  IMPLICIT NONE

  !---INTRINSIC functions used in this module
  INTRINSIC :: ADJUSTL,COUNT,INDEX,INT,MAXVAL,MINVAL,PACK,REAL,SIZE,SQRT,TRIM,ALL,MOD,LEN_TRIM

  ! Declare local parameters
  ! Number of channels for sensors in BUFR
  integer,parameter :: nchanl     = 616  !--- 616 subset out of 8461
  integer,parameter :: n_totchan  = 616  !--- 616 subset out of 8461
  integer,parameter :: maxinfo    =  33
  INTEGER                               :: nqc 
  real , DIMENSION(nchanl)          :: cfreq 
  INTEGER, DIMENSION(nchanl)        :: pol=0
  real, DIMENSION(nchanl)           :: angle
  real                                  :: angle1, rlat, rlon 
  CHARACTER(256)                        :: infile, outfile
  INTEGER                               :: LunIn
  INTEGER                               :: LunOut
  !---variables for BUFR library
  INTEGER, DIMENSION(2)                 :: qcflg
  INTEGER                               :: i, j, k
  INTEGER                               :: nfile, ifile, AD
  INTEGER, PARAMETER                    :: len=256
  CHARACTER(LEN=10)                     :: OutFilePrefix
  CHARACTER(LEN=10)                     :: OutFileSuffix
  !---Pointers and other arrays
  CHARACTER(LEN=len), DIMENSION(:),       POINTER     :: BUFRFiles,BinOutFiles
  !---Namelist data 
  CHARACTER(LEN=len) :: BUFRFilesList=DEFAULT_VALUE_STR4
  CHARACTER(LEN=len) :: pathBinOut=DEFAULT_VALUE_STR4
  character(len=7)  :: jsatid=DEFAULT_VALUE_STR4

  real  ,parameter:: rad2deg1=PI/180.0
  real  ,parameter:: deg2rad1=180.0/PI

  !---variables for BUFR read
  real(8),dimension(5)  :: linele
  real(8),dimension(13) :: allspot
  real(8),dimension(2,n_totchan) :: allchan
  real(8)               :: radiance
  real(8), dimension(n_totchan) :: temperature
  real(8),dimension(3,10):: cscale
  real(8),dimension(6):: cloud_frac

  real      ,parameter:: R90    =  90.00
  real      ,parameter:: R360   = 360.00
  real      ,parameter:: d1     = 0.75400
  real      ,parameter:: d2     = -2.26500
  real      ,parameter:: tbmin  = 50.00
  real      ,parameter:: tbmax  = 550.00
  real      ,parameter:: earth_radius = 6371000.0

  real      ,parameter:: ZERO   = 0.0
  real      ,parameter:: ten    = 10.0
  real              :: clr_amt,piece
  real              :: rsat, dlon, dlat
  real              :: sat_height_ratio
  real              :: dlat_earth, dlon_earth
  real              :: step, start,step_adjust
  character(len=4)  :: senname
  character(len=80) :: allspotlist
  integer           :: nchanlr, ll
  integer           :: iret, ireadmg,ireadsb
  real, dimension(nchanl)  :: sndrbt
  character(8)  subset

  integer         ioffset,ioff
  integer         kidsat, ksatid
  integer         idate
  integer         iskip,l,ifov, jstart, iscn, ifovn
  integer         ndata 
  integer         iscan, nscanl
  integer         ntest
  integer,dimension(5) :: idate5
  integer         irec, nrec, nmeasure

  real   tdiff
  real   disterrmax,r01

  real   :: sat_zenang, sol_zenang, sat_aziang, sol_aziang
  real   :: UTC, solarZen, relAz
  real, dimension(10) :: sscale
  integer julday, iexponent, bad_line, i1, i2
  !---flag --- don't use FOVs at edges
 
  ! logical use_edges

  NAMELIST /ControlBUFRdump/BUFRFilesList, PathBinOut, jsatid !, use_edges

  !-----------------------------------------------------
  !     Read control-data from namelist
  !-----------------------------------------------------
  READ(*,NML=ControlBUFRdump)

  if(jsatid == 'metop-a')kidsat=4
  if(jsatid == 'metop-b')kidsat=3
  if(jsatid == 'metop-c')kidsat=5

  senname = 'IASI'
  i1 = INDEX(jsatid,' ')-1
  i2 = INDEX(senname,' ')-1
!  OutFilePrefix  = jsatid(1:i1) // obstype(1:i2)
  OutFilePrefix  = jsatid//senname
  OutFileSuffix  = '.bin'
  PRINT*, ' '//OutFilePrefix
  !---Read the file names of BUFR data and build output Binary files names
  CALL ReadList4(LunIn,trim(BUFRFilesList),BUFRFiles,nfile,BinOutFiles,pathBinout,&
       trim(OutFilePrefix),trim(OutFileSuffix))
  
  IF (nfile .lt. 1) CALL ErrHandl(ErrorType,Err_NoFilesFound,'(BUFR files in convertBUFR)')

  !---initialize some variables and defaults
  LunIn      = 10  ! unit for BUFR file 
  LunOut     = 11  ! unit for output SceneAMV_type file
  nqc        = 2   ! default number of QC parameters
  r01        = 0.01
  step_adjust= 0.625 ! frequency space
  disterrmax = 0.0
  ntest      = 0
  ndata      = 0
  ifov       = -999
  tdiff      = 0.
  nchanlr = nchanl
  !---what does this do? ESM
  !ioff=newchn(sis,1)-1
  ioffset=0
  bad_line = -1
  !---what does this do ? ESM 
  !ichansst   = newchn(sis,914)
  !ichsst     = ichansst-ioff+ioffset
  allspotlist= &
   'SAID YEAR MNTH DAYS HOUR MINU SECO CLATH CLONH SAZA BEARAZ SOZA SOLAZI'
  cfreq(1:nchanl) =(/ &
        648.75,  652.00,  652.75,  653.50,  654.25,  655.00,  655.75,  656.50,  657.00,  657.25, &
        657.50,  658.00,  658.50,  658.75,  659.00,  659.50,  660.00,  660.25,  660.50,  661.25, &
        661.75,  662.25,  662.75,  663.25,  663.75,  664.25,  664.50,  665.00,  665.25,  665.50, &
        665.75,  666.00,  666.25,  666.50,  667.00,  667.75,  668.00,  668.50,  669.00,  669.50, &
        670.00,  670.50,  670.75,  671.25,  672.00,  672.25,  672.50,  673.00,  673.75,  674.50, &
        675.25,  676.00,  676.75,  677.50,  678.00,  678.50,  679.25,  680.00,  680.75,  681.25, &
        681.75,  682.25,  682.50,  683.25,  684.00,  684.50,  684.75,  685.00,  685.50,  686.50, &
        687.25,  688.00,  688.75,  689.50,  689.75,  691.00,  691.50,  692.50,  693.00,  694.00, &
        694.50,  694.75,  695.25,  695.50,  696.00,  696.50,  697.25,  697.75,  698.00,  698.25, &
        699.00,  699.25,  699.50,  700.25,  700.75,  701.00,  701.25,  701.75,  702.25,  702.50, &
        702.75,  703.75,  704.00,  704.50,  705.50,  706.25,  707.00,  707.75,  708.25,  709.50, &
        709.75,  710.25,  711.00,  711.50,  712.00,  713.50,  714.50,  715.25,  716.00,  718.25, &
        718.75,  719.50,  719.75,  720.50,  721.25,  722.00,  723.00,  724.75,  725.50,  726.25, &
        726.50,  727.00,  727.75,  728.50,  731.00,  731.50,  732.25,  733.25,  733.75,  734.75, &
        735.50,  736.25,  737.50,  737.75,  738.00,  738.50,  739.00,  739.50,  740.00,  740.50, &
        741.25,  742.00,  744.25,  745.00,  745.75,  746.00,  746.50,  746.75,  747.25,  747.50, &
        748.25,  748.75,  749.25,  750.50,  751.25,  751.75,  752.75,  753.00,  753.25,  754.50, &
        755.25,  756.00,  757.25,  759.00,  759.50,  762.75,  764.00,  765.50,  772.00,  773.50, &
        781.25,  782.75,  784.50,  786.25,  787.50,  788.00,  789.25,  790.75,  793.25,  801.00, &
        806.25,  810.25,  811.75,  821.00,  829.50,  833.75,  844.00,  861.50,  871.25,  875.00, &
        901.50,  906.25,  917.25,  919.25,  925.00,  928.00,  938.00,  942.50,  943.25,  950.25, &
        962.50,  965.50,  979.25,  997.00,  998.25,  999.75, 1000.75, 1001.50, 1002.25, 1003.25, &
       1004.75, 1005.25, 1006.00, 1007.25, 1008.25, 1009.75, 1010.50, 1012.00, 1013.25, 1014.50, &
       1015.50, 1016.50, 1018.25, 1018.75, 1020.25, 1021.00, 1022.00, 1022.25, 1023.00, 1024.25, &
       1025.00, 1026.25, 1027.00, 1027.75, 1028.75, 1029.00, 1030.00, 1031.00, 1031.75, 1033.00, &
       1034.75, 1036.75, 1038.25, 1039.50, 1040.50, 1041.00, 1041.50, 1046.25, 1051.25, 1054.50, &
       1055.50, 1057.75, 1059.25, 1059.50, 1061.25, 1062.50, 1063.50, 1065.00, 1068.25, 1069.00, &
       1072.25, 1091.25, 1092.50, 1096.00, 1104.50, 1115.75, 1123.00, 1131.25, 1131.50, 1142.50, &
       1149.50, 1168.25, 1174.50, 1198.00, 1204.50, 1212.50, 1217.00, 1225.00, 1228.00, 1231.25, &
       1232.00, 1232.75, 1234.50, 1236.50, 1238.25, 1244.25, 1251.25, 1285.25, 1320.00, 1330.00, &
       1331.00, 1334.75, 1349.50, 1367.00, 1371.50, 1372.25, 1374.50, 1375.00, 1379.50, 1380.75, &
       1381.00, 1381.75, 1382.50, 1384.25, 1387.50, 1389.00, 1391.00, 1391.75, 1392.25, 1392.50, &
       1393.00, 1395.25, 1396.75, 1398.25, 1401.50, 1402.00, 1402.25, 1403.75, 1406.50, 1407.00, &
       1407.75, 1408.00, 1408.50, 1409.25, 1410.75, 1412.00, 1416.50, 1418.00, 1419.25, 1421.00, &
       1421.50, 1422.25, 1423.75, 1426.50, 1427.00, 1428.75, 1431.25, 1432.50, 1434.75, 1436.00, &
       1436.75, 1438.50, 1439.25, 1442.00, 1446.50, 1451.75, 1455.75, 1456.75, 1457.75, 1458.75, &
       1460.50, 1465.00, 1468.50, 1470.50, 1472.00, 1472.75, 1475.25, 1476.25, 1483.25, 1486.25, &
       1488.50, 1489.25, 1497.50, 1498.75, 1502.75, 1504.25, 1504.75, 1505.25, 1505.75, 1506.25, &
       1506.75, 1507.25, 1507.75, 1508.25, 1509.25, 1511.50, 1513.75, 1515.75, 1517.50, 1519.00, &
       1519.50, 1520.75, 1521.25, 1522.00, 1524.25, 1526.50, 1533.50, 1538.50, 1539.00, 1539.75, &
       1540.25, 1541.25, 1542.00, 1544.50, 1547.25, 1551.25, 1554.25, 1556.25, 1558.00, 1559.25, &
       1560.00, 1563.00, 1567.00, 1569.75, 1572.25, 1576.25, 1585.50, 1598.25, 1605.00, 1616.75, &
       1652.75, 1659.50, 1661.75, 1665.25, 1668.50, 1684.75, 1703.25, 1709.00, 1747.50, 1769.25, &
       1774.75, 1782.75, 1786.50, 1796.75, 1806.25, 1819.25, 1846.75, 1857.00, 1874.75, 1879.50, &
       1881.50, 1886.50, 1892.50, 1893.75, 1898.50, 1901.75, 1908.75, 1926.75, 1927.25, 1930.75, &
       1937.25, 1939.25, 1940.50, 1941.75, 1942.50, 1986.75, 1987.50, 1989.50, 1990.00, 1990.50, &
       1994.00, 1994.50, 1995.00, 1995.50, 1996.00, 2006.25, 2008.50, 2012.75, 2014.75, 2015.50, &
       2016.00, 2017.75, 2019.00, 2020.25, 2021.50, 2022.00, 2024.00, 2026.75, 2034.25, 2069.00, &
       2073.25, 2082.00, 2086.25, 2091.00, 2094.25, 2094.50, 2095.00, 2099.00, 2103.00, 2103.25, &
       2103.75, 2107.00, 2107.50, 2107.75, 2111.00, 2112.00, 2115.00, 2115.75, 2119.00, 2119.75, &
       2123.75, 2127.75, 2131.75, 2135.50, 2136.75, 2139.25, 2141.75, 2142.75, 2143.25, 2144.00, &
       2145.50, 2146.75, 2150.50, 2151.25, 2154.50, 2158.00, 2158.75, 2161.50, 2162.50, 2165.25, &
       2166.00, 2169.25, 2172.75, 2176.25, 2178.50, 2179.75, 2182.00, 2183.25, 2184.25, 2185.00, &
       2186.75, 2188.25, 2190.25, 2191.50, 2196.00, 2197.00, 2198.00, 2224.00, 2229.50, 2230.25, &
       2236.25, 2240.00, 2242.50, 2267.00, 2385.25, 2386.25, 2387.25, 2388.50, 2389.00, 2390.25, &
       2391.00, 2391.50, 2392.00, 2392.50, 2393.00, 2393.50, 2394.00, 2394.50, 2394.75, 2395.75, &
       2396.75, 2398.00, 2398.75, 2400.00, 2400.75, 2401.50, 2402.00, 2402.75, 2404.25, 2405.50, &
       2406.25, 2407.00, 2412.00, 2412.75, 2413.75, 2415.00, 2415.75, 2417.00, 2419.50, 2447.00, &
       2450.25, 2452.50, 2453.50, 2456.50, 2461.50, 2462.00, 2465.75, 2492.00, 2499.50, 2500.50, &
       2500.75, 2501.25, 2501.75, 2502.50, 2503.75, 2505.75, 2513.50, 2532.00, 2540.75, 2561.00, &
       2561.25, 2602.50, 2603.75, 2608.00, 2611.00, 2616.00, 2616.75, 2622.75, 2632.25, 2637.75, &
       2639.75, 2643.50, 2646.50, 2648.50, 2658.50, 2664.25/)
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
           !    Read IASI FOV information
           call ufbint(lunin,linele,5,1,iret,'FOVN SLNM QGFQ MJFC SELV')
           if ( linele(3) /= zero) cycle read_loop1 ! problem with profile (QGFQ)
           
           if ( bad_line == nint(linele(2))) then
              !        zenith angle/scan spot mismatch, reject entire line
              cycle read_loop1
           else
              bad_line = -1
           endif
           ifov = nint(linele(1))               ! field of view
           
           !    IASI fov ranges from 1 to 120.   Current angle dependent bias
           !    correction has a maximum of 90 scan positions.   Geometry
           !    of IASI scan allows us to remap 1-120 to 1-60.   Variable
           !    ifovn below contains the remapped IASI fov.  This value is
           !    passed on to and used in setuprad
           ifovn = (ifov-1)/2 + 1
           iscn = nint(linele(2))               ! scan line

           !    Check field of view (FOVN) and satellite zenith angle (SAZA)
           if( ifov <= 0 .or. ifov > 120) then
              write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS INFO(FOVN,SLNM):', ifov, iscn
              cycle read_loop1
           endif
           
           call ufbint(lunin,allspot,13,1,iret,allspotlist)
!           print *, iret
           if(iret /= 1) cycle read_loop1
           
           !  Extract satellite id.  If not the one we want, read next record
           ksatid=nint(allspot(1))
!           print *, ksatid, kidsat
           if(ksatid /= kidsat) cycle read_loop1
           rsat=allspot(1) 
           
           !---Check observational info
!           sat_zenang  = allspot(10) 
!           if( ifov < 0 .or. ifov > 100 .or. abs(sat_zenang) > 360.) then
 !             write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
  !                 ' STRANGE OBS INFO(FOV,SAZA):', allspot(12), allspot(10)
   !           cycle read_loop1
    !          
     !      endif
           !    Observational info
           sat_zenang  = allspot(10)            ! satellite zenith angle
           
           !---Check  satellite zenith angle (SAZA)
           if(sat_zenang > 90.0 ) then
              write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS INFO(FOVN,SLNM,SAZA):', ifov, iscn, allspot(10)
              cycle read_loop1
           endif
           if ( ifov <= 60 ) sat_zenang = -sat_zenang

!    Compare IASI satellite scan angle and zenith angle
!!$        piece = -step_adjust
!!$        if ( mod(ifovn,2) == 1) piece = step_adjust
!!$        lza = ((start + float((ifov-1)/4)*step) + piece)*deg2rad
!!$        sat_height_ratio = (earth_radius + linele(5))/earth_radius
!!$        lzaest = asin(sat_height_ratio*sin(lza))*rad2deg
!!$        if (abs(sat_zenang - lzaest) > one) then
!!$           write(6,*)' READ_IASI WARNING uncertainty in lza ', &
!!$              lza*rad2deg,sat_zenang,sis,ifov,start,step,allspot(11),allspot(12),allspot(13)
!!$           bad_line = iscn
!!$           cycle read_loop1
!!$        endif

           !   Clear Amount  (percent clear)
           call ufbrep(lunin,cloud_frac,1,6,iret,'FCPH')
           clr_amt = cloud_frac(1)
           clr_amt=max(clr_amt,zero)
           clr_amt=min(clr_amt,100.0)

           dlat_earth = allspot(8)
           dlon_earth = allspot(9)
           !---Check observing position
           if( abs(dlat_earth) > R90  .or. abs(dlon_earth) > R360 .or. &
                (abs(dlat_earth) == R90 .and. dlon_earth /= ZERO) )then
              print *, 'error in lat/lon'
              cycle read_loop1
           endif
           
           !---Retrieve observing position
           if(dlon_earth >= R360)then
              dlon_earth = dlon_earth - R360
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
              
              write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
              cycle read_loop1
              
           endif
          
           ! qc based on tests from Wisc.  
           ! compare SST to calculated SST from IASI
           ! based on surface type.
           ! tests require deter_sfc and other info
           ! skipped for now (see read_iasi.f90)
           
           call ufbint(lunin,allchan,2,n_totchan,iret,'SCRA CHNM')
           if( iret /= n_totchan)then
              write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   iret, ' CH DATA IS READ INSTEAD OF ',n_totchan
              cycle read_loop1
           endif

           nmeasure=nmeasure+1
        END do read_loop1
     END do read_subset1

     !---Big loop to read data file
     irec=0
     nscanl = 0
     nscanl = nmeasure
     print 110, nmeasure, nrec
110  FORMAT(2x,'Found ',i8, ' records out of ',i8, ' records.  Writing to file ...')
     CALL WriteHdrMeasurmts(BinOutFiles(iFile),lunOut,nmeasure,nqc,nchanl,nscanl,cfreq,pol,nscanl)  
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
           nrec = nrec + 1
           !    Read IASI FOV information
           call ufbint(lunin,linele,5,1,iret,'FOVN SLNM QGFQ MJFC SELV')
           if ( linele(3) /= zero) cycle read_loop ! problem with profile (QGFQ)
           
           if ( bad_line == nint(linele(2))) then
              !        zenith angle/scan spot mismatch, reject entire line
              cycle read_loop
           else
              bad_line = -1
           endif
           ifov = nint(linele(1))               ! field of view
           
           !    IASI fov ranges from 1 to 120.   Current angle dependent bias
           !    correction has a maximum of 90 scan positions.   Geometry
           !    of IASI scan allows us to remap 1-120 to 1-60.   Variable
           !    ifovn below contains the remapped IASI fov.  This value is
           !    passed on to and used in setuprad
           ifovn = (ifov-1)/2 + 1
           iscn = nint(linele(2))               ! scan line

           !    Check field of view (FOVN) and satellite zenith angle (SAZA)
           if( ifov <= 0 .or. ifov > 120) then
              write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS INFO(FOVN,SLNM):', ifov, iscn
              cycle read_loop
           endif
           
           call ufbint(lunin,allspot,13,1,iret,allspotlist)
           if(iret /= 1) cycle read_loop
           
           !  Extract satellite id.  If not the one we want, read next record
           ksatid=nint(allspot(1))
           if(ksatid /= kidsat) cycle read_loop
           rsat=allspot(1) 

           !---Check observational info
           sat_zenang  = allspot(10) 
           !---Check  satellite zenith angle (SAZA)
           if(sat_zenang > 90.0 ) then
              write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS INFO(FOVN,SLNM,SAZA):', ifov, iscn, allspot(10)
              cycle read_loop
           endif
           if ( ifov <= 60 ) sat_zenang = -sat_zenang
           
           dlat_earth = allspot(8)
           dlon_earth = allspot(9)
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
              
              write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   ' STRANGE OBS TIME (YMDHM):', idate5(1:5)
              cycle read_loop
           endif

           UTC= allspot(7) + (idate5(5)*60.0) + (idate5(4)*3600.0)
           call compJulDay(idate5(1),idate5(2),idate5(3),julday)

           !---Set common predictor parameters
           
           sat_aziang  = allspot(11)  

           ! qc based on tests from Wisc.  
           ! compare SST to calculated SST from IASI
           ! based on surface type.
           ! tests require deter_sfc and other info
           ! skipped for now (see read_iasi.f90)

           call ufbrep(lunin,cscale,3,10,iret,'STCH ENCH CHSF')
           if(iret /= 10) then
              write(6,*) 'READ_IASI  read scale error ',iret
              cycle read_loop
           end if

           ! The scaling factors are as follows, cscale(1) is the start channel number,
           !                                     cscale(2) is the end channel number,
           !                                     cscale(3) is the exponent scaling factor
           ! In our case (616 channels) there are 10 groups of cscale (dimension :: cscale(3,10))
           !  The units are W/m2..... you need to convert to mW/m2.... (subtract 5 from cscale(3)
           do i=1,10  ! convert exponent scale factor to int and change units
              iexponent = -(nint(cscale(3,i)) - 5)
              sscale(i)=ten**iexponent
           end do
           
           !---Read IASI channel number(CHNM) and radiance (SCRA)
           
           call ufbint(lunin,allchan,2,n_totchan,iret,'SCRA CHNM')
           if( iret /= n_totchan)then
              write(6,*)'READ_IASI:  ### ERROR IN READING ', senname, ' BUFR DATA:', &
                   iret, ' CH DATA IS READ INSTEAD OF ',n_totchan
              cycle read_loop
           endif

           iskip = 0
           jstart=1
           do i=1,n_totchan
              !---check that channel number is within reason
              if (( allchan(1,i) > zero .and. allchan(1,i) < 99999.) .and. &  ! radiance bounds
                   (allchan(2,i) < 8462. .and. allchan(2,i) > zero )) then     ! chan # bounds
                 !---radiance to BT calculation
                 radiance = allchan(1,i)
                 scaleloop2: do j=jstart,10
                    if(allchan(2,i) >= cscale(1,j) .and. allchan(2,i) <= cscale(2,j))then
                       radiance = allchan(1,i)*sscale(j)
                       jstart=j
                       exit scaleloop2
                    end if
                 end do scaleloop2
                 temperature(i) = Planck_Temperature(radiance,cfreq(i))
!                 call crtm_planck_temperature(sensorindex,i,radiance,temperature(i))
                 if(temperature(i) < tbmin .or. temperature(i) > tbmax ) then
                    temperature(i) = tbmin
                    iskip = iskip + 1
                 endif
              else           ! error with channel number or radiance
                 temperature(i) = tbmin
!                 if(iuse_rad(ioff+i) >= 0)iskip = iskip + 1
                 iskip = iskip + 1
              endif
           end do

           solarZen = sol_zenang                       ! solar zenith angle
           relAz    = sol_aziang                       ! solar azimut angle
           rlat     = dlat_earth
           rlon     = dlon_earth
           angle1   = sat_zenang                      ! satellite zenith angle
           angle    = angle1
           qcflg(1:2) = 0
           != allchan(l+ioffset)   ! brightness temerature           
           sndrbt(1:nchanl) = SNGL(temperature(1:nchanl))
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
  FUNCTION Planck_Temperature(Radiance, Frequency)
    IMPLICIT NONE
    REAL(8) :: Planck_Temperature
    REAL(8) :: Radiance, Temperature
    REAL    :: Frequency
    REAL(8) :: x_c_1, x_c_2, Logarithm
    REAL(8), parameter :: ONE = 1.0d0
    REAL(8), parameter :: SCALE_FACTOR=1.0d0
    real(8), parameter :: BOLTZMNS = 1.3806503D-16 
    real(8), parameter :: PLANCKS  = 6.62606876D-27 
    real(8), parameter :: CLIGHT   = 2.99792458D+10     
    real(8), parameter :: C_1   = 2.0*PLANCKS*CLIGHT*CLIGHT
    real(8), parameter :: C_2   = PLANCKS*CLIGHT/BOLTZMNS 

    x_c_1        = C_1 * DBLE((Frequency*Frequency*Frequency))
    x_c_2        = C_2 * DBLE(Frequency)
    Logarithm    = LOG( ( SCALE_FACTOR * x_c_1 / Radiance ) + ONE )
    Planck_Temperature  = x_c_2 / Logarithm

  END FUNCTION Planck_Temperature
END PROGRAM iasi_bufr2fmsdr
