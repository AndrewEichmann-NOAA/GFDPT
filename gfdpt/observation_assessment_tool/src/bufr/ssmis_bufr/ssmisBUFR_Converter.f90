program read_ssmis
!---------------------------------------------------------------------------------
! Name: ssmisBUFR_Converter.f90
!
! Description:
!    Decode SSMIS UPP BUFR file and convert it into SDR format
!
! Author: Deyong Xu (RTI) @ JCSDA,
!         Deyong.Xu@noaa.gov
! Version: Apr 22, 2014, DXu, Initial coding
!
!---------------------------------------------------------------------------------
use kinds, only: r_kind,r_double,i_kind
use fmsdr_struct
use constants, only: deg2rad,rad2deg,zero,half,one,two,four,r60inv
use IO_MeasurData

implicit none

!--------------------------------------
! Enfore function signature checking
!--------------------------------------
interface 
   function ireadmg(lnBUFR, csubset, idate)
     integer :: ireadmg   ! return type
     integer, intent(in) :: lnBUFR
     character(len=8), intent(inout) :: csubset
     integer, intent(in) :: idate
   end function ireadmg

   function ireadsb(lnBUFR)
     integer :: ireadsb   ! return type
     integer, intent(in) :: lnBUFR
   end function ireadsb

   function jday(yyyy, mm, dd) result(ival)
      integer             :: ival
      integer, intent(in) :: yyyy, mm, dd
   end function jday
end interface


! Define some constants
integer(i_kind),parameter :: maxProf = 2000000   ! 2,000,000
integer(i_kind),parameter :: maxLine = 50000     ! 50,000 lines /orbit
real(r_kind),parameter    :: r360=360.0_r_kind
real(r_kind),parameter    :: tbMIN=70.0_r_kind
real(r_kind),parameter    :: tbMAX=320.0_r_kind
real(r_kind),parameter    :: one_minute=0.01666667_r_kind
real(r_kind),parameter    :: minus_one_minute=-0.01666667_r_kind
integer, parameter :: INT_FILL_VALUE = -999
real, parameter :: REAL_FILL_VALUE = -999.99

! Define variables used for decoding BUFR file
character(len=8):: csubset 
integer :: lnBUFR = 10   ! input bufr logical number
integer :: lnSDR= 11     ! output sdr logical number
character(len=80):: infile
character(len=80):: outfile
real :: satid
integer :: idate
integer :: nlv
integer :: lineCounter
integer :: prevLine
integer :: currLine

namelist /nl_main_read_ssmis/satid, infile, outfile

! Array needed in GSI version
!----------------------------------------------------
real(r_double),dimension(7)         :: bufrInit
real(r_double),dimension(3,5)       :: bufrYMD
real(r_double),dimension(2,2)       :: bufrHM
real(r_double),dimension(2,29)      :: bufrLatLon
real(r_double),dimension(2,maxChan) :: bufrTb
real(r_double),dimension(2,28)      :: bufrRadarRaa
!----------------------------------------------------

! Define variables to hold data decoded from BUFR file
type(sdrHeader) dataHeader
type(sdrBody), allocatable, dimension(:) :: dataBody

integer(i_kind) :: iProf
integer(i_kind) :: iMsg
integer(i_kind) :: lineNumber
integer(i_kind) :: posNumber
logical :: containSubsetFlag 
logical :: firstScanLineFlag
integer(i_kind),dimension(5) :: iobsdate

real(r_double) :: prevScanLine
real(r_double) :: currScanLine
real(r_double) :: prevOrbitNo
real(r_double) :: currOrbitNo
real(r_double) :: prevLat
real(r_double) :: currLat
integer :: controller
integer :: indexLine
integer :: indexOrbit
integer orbitNo
integer lineNo
integer nodeFlag
integer curr_orbitNo
integer curr_lineNo
integer curr_nodeFlag
! orbit number, scan line number and nodeFlag
integer(i_kind),dimension(maxLine,3) :: nodeFlagArr
! index of first scan line of each orbit in nodeFlagArr
integer(i_kind),dimension(5) ::  firstLineArr

! Only for tmp indexing need, not for meaningful things
integer :: i, j, k

! Fill nodeFlagArr with Fill values
nodeFlagArr = INT_FILL_VALUE
firstLineArr = INT_FILL_VALUE

! Read in input/output filenames
read(*, NML=nl_main_read_ssmis)
print *, 'satid ', satid

!-------------------------------------------------------
! Step 1: 
! Now start read BUFR file 
!-------------------------------------------------------
! Allocate momery
allocate(dataBody(maxProf))

! Need to reopen the BUFR file since closbf closes it. 
open(lnBUFR, file=infile, form='unformatted', status='old', err=500)  
call openbf(lnBUFR, 'IN', lnBUFR)
call datelen(10)

! BUFR msg counter
iMsg = 0
! Profile counter
iProf = 0
firstScanLineFlag = .true.
controller = 0

! store orbit mode for each center FOV on each scan line
indexLine=0
indexOrbit=0

! Initialize dataBody%nodeFlag
dataBody%nodeFlag = INT_FILL_VALUE

read_msg: do while( ireadmg(lnBUFR,csubset,idate)>=0 )
   containSubsetFlag = .false.
   read_subset: do while( ireadsb(lnBUFR)==0 .and. iProf < maxProf )

      call ufbint(lnBUFR,bufrInit, 7, 1, nlv, &
		 "SAID SECO SLNM FOVN SFLG RFLAG ORBN" )

      ! Only need F18 data
      if (bufrInit(1) /= satid) exit read_subset

      controller = controller + 1
      !if (controller > 811) exit read_msg

      call ufbrep(lnBUFR,bufrYMD, 3, 5, nlv, "YEAR MNTH DAYS" )
      call ufbrep(lnBUFR,bufrHM, 2, 2, nlv, "HOUR MINU" )
      call ufbrep(lnBUFR,bufrLatLon, 2, 29, nlv, "CLAT CLON" )
      call ufbrep(lnBUFR,bufrTb, 2, maxChan, nlv, "CHNM TMBR" )
      call ufbrep(lnBUFR,bufrRadarRaa, 2, 28, nlv, "RAIA BEARZA")
      
      containSubsetFlag = .true.
      iProf = iProf + 1                ! Count profs for F18
    
      ! Save data for each profile
      dataBody(iProf)%lat = bufrLatLon(1, 1)
      dataBody(iProf)%lon = bufrLatLon(2, 1)
      dataBody(iProf)%raa = REAL_FILL_VALUE   ! raa is not right in bufr
      dataBody(iProf)%sza = REAL_FILL_VALUE   ! not sza in bufr
      ! dataBody(iProf)%nodeFlag = ' to be set later'
      dataBody(iProf)%scanPos = bufrInit(4)
      dataBody(iProf)%scanLine = bufrInit(3)
      dataBody(iProf)%orbitNumber= bufrInit(7) ! new
      dataBody(iProf)%scanYear = bufrYMD(1,1)
      dataBody(iProf)%scanDay = jday(int(bufrYMD(1,1)), &
                                     int(bufrYMD(2,1)), & 
                                     int(bufrYMD(3,1)))
      dataBody(iProf)%scanUTC = bufrHM(1,1) * 3600 +  bufrHM(2,1) * 60 +  bufrInit(2)
      !print *, 'hour min info ', bufrHM(1,1) , bufrHM(2,1)
      !print *, '   end        ', bufrHM(1,2) , bufrHM(2,2)
      !dataBody(iProf)%angleArr  = angleValArr
      dataBody(iProf)%angleArr  = bufrRadarRaa(1,1)
      dataBody(iProf)%tbArr = bufrTb(2,1:maxChan)
      dataBody(iProf)%QC_Arr = QC_ValArr

      ! Only check the middle FOV of each scan line
      ! to decidd it's in ascending/descending mode.
      if (bufrInit(4) == 30) then
         if ( firstScanLineFlag ) then
            prevLat = bufrLatLon(1, 1)
            prevScanLine = bufrInit(3)
            prevOrbitNo = bufrInit(7)

            firstScanLineFlag = .false.

            ! Leave space for the first scan line in nodeFlagArr
            indexLine = indexLine + 1
            ! Save the index of first scan line of
            ! each orbit in nodeFlagArr
            indexOrbit = indexOrbit + 1
            firstLineArr(indexOrbit) = indexLine
         else
            ! Get lat, scan line and orbit info
            currLat = bufrLatLon(1, 1)
            currScanLine = bufrInit(3)
            currOrbitNo = bufrInit(7)
            ! Decide orbit mode: same orbit && different scan line
            ! Check to see if it's the same oribit
            if ( ( currOrbitNo == prevOrbitNo )  &
                  .and. ( currScanLine /= prevScanLine ) ) then
               ! Compare lat to decide orbit mode
               if ( currLat > prevLat ) then
                  dataBody(iProf)%nodeFlag = 0   ! 0 is asc
                  indexLine = indexLine + 1
                  nodeFlagArr(indexLine, 1) = currOrbitNo
                  nodeFlagArr(indexLine, 2) = currScanLine
                  nodeFlagArr(indexLine, 3) = 0
               endif

               if ( currLat < prevLat ) then
                  dataBody(iProf)%nodeFlag = 1   ! 1 is desc
                  indexLine = indexLine + 1
                  nodeFlagArr(indexLine, 1) = currOrbitNo
                  nodeFlagArr(indexLine, 2) = currScanLine
                  nodeFlagArr(indexLine, 3) = 1
               endif

               ! For cases where lats are equal.
               ! We will copy node flag from neighbor
               ! line (above/below)
               ! Treat them same as the lines that don't have 30th FOV.
               if ( currLat == prevLat ) then
                  ! Use the same node flas as prevous scan line
                  dataBody(iProf)%nodeFlag = nodeFlagArr(indexLine, 3)

                  indexLine = indexLine + 1
                  nodeFlagArr(indexLine, 1) = currOrbitNo
                  nodeFlagArr(indexLine, 2) = currScanLine
                  nodeFlagArr(indexLine, 3) = dataBody(iProf)%nodeFlag
               endif

               ! Save current info: lat, line and orbit
               prevLat = currLat
               prevScanLine = currScanLine
               prevOrbitNo = currOrbitNo
            ! New orbit
            else
               !firstScanLineFlag = .true.
               prevLat = bufrLatLon(1, 1)
               prevScanLine = bufrInit(3)
               prevOrbitNo = bufrInit(7)
               ! Leave space for the first scan line in nodeFlagArr
               indexLine = indexLine + 1
               ! Save the index of first scan line of
               ! each orbit in nodeFlagArr
               indexOrbit = indexOrbit + 1
               firstLineArr(indexOrbit) = indexLine
            endif   ! done checking orbit number
         endif  ! done checking if 1st scan line
      endif  ! done checking middle FOV

   end do read_subset

   if (containSubsetFlag) iMsg = iMsg + 1 
   ! if (containSubsetFlag) print *, 'bufr msg', iMsg , 'is processed...'

end do read_msg

print *, 'done reading  BUFR !!!'

! Close BUFR file by executing FORTRAN "CLOSE" 
call closbf(lnBUFR)

! Find out number of lines in all all orbits
! deyong : find number of scan lines
lineCounter = 0
do i = 1, iProf
   if (i == 1) then 
      prevLine = dataBody(i)%scanLine
   endif

   currLine = dataBody(i)%scanLine

   if ( currLine /= prevLine ) then 
      lineCounter = lineCounter + 1 
      ! print *, 'line is ', prevLine
      prevLine = dataBody(i)%scanLine
   endif
enddo

!print *, 'last line is ', prevLine

! Add the last line in
lineCounter = lineCounter + 1

print *, ' lineCounter ', lineCounter
print *,' firstLineArr  ',  firstLineArr

print *, 'Number of BUFR msgs = ', iMsg 
print *, 'Number of profs = ',  lineCounter * 60
print *, 'Number of lines = ', lineCounter
print *, 'Number of orbit = ', indexOrbit
print *, ' firstLineArr is ', firstLineArr

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Update orbit node info. 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copy orbit node from 2nd line to the 1st line for each orbit.
! Assuming there are five orbits in each bufr
do i = 1, 5
   if (firstLineArr(i) /= INT_FILL_VALUE) then
      j = firstLineArr(i)
      ! Copy orbit node.
      nodeFlagArr(j, 1) = nodeFlagArr(j+1, 1)      ! same orbit
      nodeFlagArr(j, 2) = nodeFlagArr(j+1, 2) - 1  ! line minus 1
      nodeFlagArr(j, 3) = nodeFlagArr(j+1, 3)      ! same node flag
   endif
enddo

! For scan lines that have 30th scan position,
! Copy the node flag of 30th FOV to rest FOVs on the same line
do i = 1, iProf
do j = 1, indexLine
   ! Get saved orbit, line and nodeFlag info
   orbitNo = nodeFlagArr(j, 1)
   lineNo = nodeFlagArr(j, 2)
   nodeFlag = nodeFlagArr(j, 3)

   ! Get current FOV's orbit, line and nodeFlag info
   curr_orbitNo = dataBody(i)%orbitNumber
   curr_lineNo = dataBody(i)%scanLine
   curr_nodeFlag = dataBody(i)%nodeFlag

   if ( curr_orbitNo == orbitNo &              ! same orbit
       .and. orbitNo /= INT_FILL_VALUE   &     ! saved orbit is not fill value
       .and. curr_lineNo == lineNo ) then      ! same line
      dataBody(i)%nodeFlag = nodeFlag
   endif
enddo
enddo

! For scan lines that doesn't have 30th scan position,
! use orbit node as above / down line.
do i = 1, iProf
do j = 1, indexLine
   ! Get saved orbit, line and nodeFlag info
   orbitNo = nodeFlagArr(j, 1)
   lineNo = nodeFlagArr(j, 2)
   nodeFlag = nodeFlagArr(j, 3)

   ! Get current FOV's orbit, line and nodeFlag info
   curr_orbitNo = dataBody(i)%orbitNumber
   curr_lineNo = dataBody(i)%scanLine
   curr_nodeFlag = dataBody(i)%nodeFlag

   if ( curr_orbitNo == orbitNo &                  ! same orbit
       .and. orbitNo /= INT_FILL_VALUE   &         ! save orbit not fill
       .and. curr_nodeFlag == INT_FILL_VALUE  &    ! node flag still not set
       .and. ( (curr_lineNo == lineNo + 1)   &
       .or. (curr_lineNo == lineNo + 2)    &
       .or. (curr_lineNo == lineNo + 3)    &
       .or. (curr_lineNo == lineNo + 4)    &
       .or. (curr_lineNo == lineNo + 5) ) ) then   ! copy node flag of one/more lines above
      dataBody(i)%nodeFlag = nodeFlag
   endif
enddo
enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Done with updating node information.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!do i = 1, iProf
!   print *, dataBody(i)%scanLine, dataBody(i)%scanPos
!   print *, dataBody(i)%nodeFlag, dataBody(i)%orbitNumber
!enddo

!-------------------------------------------------------
! Step 2
! Now output data into fmsdr binary file 
!-------------------------------------------------------
! Create SDR header
dataHeader%nProf = lineCounter * 60
dataHeader%nChan = maxChan
dataHeader%nPosScan = 60
dataHeader%nScanLine = lineCounter
dataHeader%nQC= NUM_QC
dataHeader%freqArr = freqValArr
dataHeader%polarArr = polarValArr
!print *, 'coded nQC is ',  dataHeader%nQC 

! Write data to SDR binary file
call writeFMSDR_Output(lnSDR, outfile, maxProf, dataHeader, dataBody)

! Deallocate spaces allocated in alloc_Array call.
deallocate(dataBody)

500 continue
print *, 'End of program ' 

end program

!-------------------------------------------------
! Locally defined help functions (External function)
!-------------------------------------------------
subroutine writeFMSDR_Output(lnSDR, outfile, maxProf, dataHeader, dataBody)
   use fmsdr_struct
   use IO_MeasurData, only : WriteHdrMeasurmts, WriteMeasurmts

   implicit none

   integer, intent(in) :: lnSDR
   character(len=*), intent(in):: outfile
   integer, intent(in):: maxProf
   type(sdrHeader), intent(in):: dataHeader
   type(sdrBody), dimension(maxProf), intent(in) :: dataBody
   type(sdrBody), dimension(60) :: dataPerLine
   logical, dimension(60) :: absentFlag 
   !----------------------------------------
   ! declaration of external function
   !----------------------------------------
   type(sdrBody) createDefaultProf   
 
   integer, parameter :: INT_FILL_VALUE = -999
   real, parameter :: REAL_FILL_VALUE = -999.99

   ! Local variables for header
   integer :: nProf
   integer :: nChan
   integer :: nPosScan
   integer :: nScanLine
   integer :: nQC
   real, dimension(maxChan) :: freqArr
   integer, dimension(maxChan) :: polarArr

   ! Local variables for body
   real :: lat
   real :: lon
   real :: raa
   real :: sza
   integer :: nodeFlag
   integer :: scanPos 
   integer :: scanLine
   integer :: scanYear
   integer :: scanDay 
   real :: scanUTC 
   real, allocatable, dimension(:) :: angleArr
   real, allocatable, dimension(:):: tbArr
   integer, allocatable, dimension(:) :: QC_Arr 
  
   ! For local tmp use
   integer i, j 
   integer :: prevScanLine
   integer :: prevScanPos
   integer :: currScanLine
   integer :: currScanPos
   integer :: posCounter

   ! Initialize data and flag to fill value
   ! 60-element array
   absentFlag = .true.
   dataPerLine = createDefaultProf()

   nProf = dataHeader%nProf
   nChan = dataHeader%nChan
   nPosScan = dataHeader%nPosScan
   nScanLine = dataHeader%nScanLine
   nQC = dataHeader%nQC
   print *, 'nQC from header ', nQC
   freqArr = dataHeader%freqArr
   polarArr = dataHeader%polarArr

   print *, ' ----------------------'
   print *, ' header info ... '
   print *, 'nProf, nChan, nPosScan, nScanLine, nQC, freqArr, polarArr '
   print *, nProf, nChan, nPosScan, nScanLine, nQC, freqArr, polarArr
   print *, ' ----------------------'

   !---------------------
   ! Write SDR header 
   !---------------------
   call WriteHdrMeasurmts(outfile,lnSDR,nProf, nQC, maxChan, nPosScan, &
      freqArr, polarArr, nScanLine)

   print *, ' DONE with writing header !!!!!  '

   allocate(angleArr(nChan))
   allocate(tbArr(nChan))
   allocate(QC_Arr(nQC))

   posCounter = 0
   ! Loop through all the profiles from BUFR
   do i = 1, nProf
      ! Copy data 
      lat = dataBody(i)%lat
      lon = dataBody(i)%lon
      raa = dataBody(i)%raa
      sza = dataBody(i)%sza
      nodeFlag = dataBody(i)%nodeFlag
      scanPos = dataBody(i)%scanPos
      scanLine = dataBody(i)%scanLine
      scanYear = dataBody(i)%scanYear
      scanDay = dataBody(i)%scanDay   ! jday
      scanUTC = dataBody(i)%scanUTC
      angleArr = dataBody(i)%angleArr
      tbArr = dataBody(i)%tbArr
      QC_Arr = dataBody(i)%QC_Arr

      ! Save line and pos info for the first profile of each scan line.
      if ( i == 1 ) then 
         prevScanLine = dataBody(i)%scanLine
         prevScanPos = dataBody(i)%scanPos
      endif

      currScanLine = dataBody(i)%scanLine
      currScanPos = dataBody(i)%scanPos

      ! Same line
      if ( currScanLine == prevScanLine ) then 
         ! Still the same line, then save data into dataPerLine
         do j = 1, 60 
            if ( j == currScanPos ) then 
               dataPerLine(j) = dataBody(i)
               !print *, 'dataBody(i)  lat ',  dataBody(i)%lat
               !print *, 'dataBody(i)  node ',  dataBody(i)%nodeFlag
               !print *, 'dataPerLine(j) lat  ',  dataPerLine(j)%lat
               !print *, 'dataPerLine(j) node  ',  dataPerLine(j)%nodeFlag
               absentFlag(j) = .false.
            endif
         enddo

         posCounter = posCounter + 1
      else
         ! If new line starts, check posCounter
         !print *, 'prev Line is ', prevScanLine
         !print *, 'counter is ', posCounter
         if ( posCounter /= 60 ) then 
            print *,   'line having less 60 is ', prevScanLine, posCounter
            ! No good, less than 60 positions for previous line. 
            ! Check absent flag array to set default profile value for missing pos.
            do j = 1, 60 
               if ( absentFlag(j) == .true. ) then 
                  ! Overwrite line, pos and QC_Arr info
                  dataPerLine(j)%scanLine = prevScanLine
                  dataPerLine(j)%scanPos = j
                  dataPerLine(j)%QC_Arr = 1   ! set bad QC
                  print *, 'missing : line, pos ', prevScanLine, j
               endif
            enddo
         endif 

         ! Now write previous line data out: always 60 positios per line
         do j = 1, 60 
	    lat = dataPerLine(j)%lat
	    lon = dataPerLine(j)%lon
	    raa = dataPerLine(j)%raa
	    sza = dataPerLine(j)%sza
	    nodeFlag = dataPerLine(j)%nodeFlag
	    scanPos = dataPerLine(j)%scanPos
	    scanLine = dataPerLine(j)%scanLine
	    scanYear = dataPerLine(j)%scanYear
	    scanDay = dataPerLine(j)%scanDay   ! jday
	    scanUTC = dataPerLine(j)%scanUTC
	    angleArr = dataPerLine(j)%angleArr
	    tbArr = dataPerLine(j)%tbArr
	    QC_Arr = dataPerLine(j)%QC_Arr

            !-----------------------------
            ! Write SDR body prof-by-prof
            !-----------------------------
            call WriteMeasurmts(lnSDR, nQC, QC_Arr, nChan, angleArr, tbArr, &
               lat, lon, nodeFlag, scanUTC, scanDAY, scanYear, scanPos, &
               scanLine, raa, sza)

	    !print *, 'lat', lat
	    !print *, 'lon', lon
	    !print *, 'raa', raa
	    !print *, 'sza', sza
	    !print *, 'nodeFlag', nodeFlag
	    !print *, 'pos', scanPos 
	    !print *, 'line', scanLine
	    !print *, 'year',  scanYear
	    !print *, 'day', scanDay 
	    !print *, 'secs utc', scanUTC 
	    !print *, 'angleArr', angleArr
	    !print *, 'tbArr', tbArr 
	    !print *, 'QC_Arr', QC_Arr
	    !print *, '----------'

         enddo

         !print *, ' absentflag ', absentFlag
         !print *, ' done writing data for line ', prevScanLine
         !print *, ''

         ! Reset posCounter, absentFlag
         absentFlag = .true.
         dataPerLine = createDefaultProf()

         ! Save line and pos infor as the prev line for the 1st profile of new scan  line. 
         prevScanLine = dataBody(i)%scanLine
         prevScanPos = dataBody(i)%scanPos
         posCounter = 1
         ! Now save the 1st profile for current line
         do j = 1, 60
            if ( j == currScanPos ) then
               dataPerLine(j) = dataBody(i)
               absentFlag(j) = .false.
            endif
         enddo

      endif 
      

   enddo

   ! Release memory 
   deallocate(angleArr, stat=i)
   deallocate(tbArr,stat=i)
   deallocate(QC_Arr,stat=i)

   ! Close output SDR binary file 
   close(lnSDR)  
end subroutine

function jday(yyyy, mm, dd) result(ival)
! Return jday given date: yyyy, mm and dd
! eg: jday(1984, 4, 22) = 113

   integer, intent(in) :: yyyy, mm, dd
   integer             :: ival

   ival = 3055*(mm+2)/100 - (mm+10)/13*2 -91 +  &
         (1-(modulo(yyyy, 4)+3)/4              &
          + (Modulo(yyyy, 100) + 99)/100 -        &
         (modulo(yyyy, 400)+399)/400)*(mm+10)/13 + dd

   return
end function jday

function createDefaultProf() result(result)
   use fmsdr_struct
   integer, parameter :: INT_FILL_VALUE = -999
   real, parameter :: REAL_FILL_VALUE = -999.99
   type(sdrBody) :: result

   result%lat = REAL_FILL_VALUE
   result%lon = REAL_FILL_VALUE
   result%raa = REAL_FILL_VALUE
   result%sza = REAL_FILL_VALUE
   result%nodeFlag = INT_FILL_VALUE
   result%scanPos = INT_FILL_VALUE
   result%scanLine = INT_FILL_VALUE
   result%orbitNumber = INT_FILL_VALUE
   result%scanYear = INT_FILL_VALUE
   result%scanDay = INT_FILL_VALUE
   result%scanUTC = REAL_FILL_VALUE
   result%angleArr = REAL_FILL_VALUE
   result%tbArr = REAL_FILL_VALUE
   result%QC_Arr = INT_FILL_VALUE
   return 
end function createDefaultProf

