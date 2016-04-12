module fmsdr_struct
!---------------------------------------------------------------------------------
! Name: sdr_struct.f90
!
! Description:
!    Define data structure for SDR(sensor data record) format
!
! Author: Deyong Xu (RTI) @ JCSDA,
!         Deyong.Xu@noaa.gov
! Version: May 2, 2014, DXu, Initial coding
!
!---------------------------------------------------------------------------------
use kinds, only: r_kind,r_double,i_kind

! Hard-coded there is only one QC flag
integer(i_kind),parameter :: NUM_QC = 8
integer(i_kind),parameter :: maxChan = 24
real(r_double), parameter, dimension(maxChan) :: freqValArr= &
   (/50.30000, 52.80000, 53.59600, 54.40000, 55.50000, &
     57.29000, 59.40000, 150.0000, 183.3100, 183.3100, &
     183.3100, 19.35000, 19.35000, 22.23500, 37.00000, &
     37.00000, 91.65500, 91.65500, 63.28325, 60.79267, &
     60.79267, 60.79267, 60.79267, 60.79267/)
integer, parameter, dimension(maxChan) :: polarValArr =  &
   (/5, 5, 5, 5, 5, 2, 2, 5, 5, 5, 5, 5, &
     4, 4, 5, 4, 4, 5, 2, 2, 2, 2, 2, 2/)
real, parameter, dimension(maxChan):: angleValArr = & 
   (/53,53,53,53,53,53,53,53,53,53,53,53, &
     53,53,53,53,53,53,53,53,53,53,53,53/)
integer, parameter, dimension(NUM_QC):: QC_ValArr = &
   (/0,0,0,0,0,0,0,0/)

! Define data structure 
! This is the exact data structure as one defined in MIRS
! SDR Header
type sdrHeader
   integer nProf
   integer nChan
   integer nPosScan
   integer nScanLine
   integer :: nQC
   real  freqArr(maxChan)
   integer polarArr(maxChan)
end type sdrHeader

! This is the exact data structure as one defined in MIRS
! SDR Body
type sdrBody
   real lat 
   real lon
   real raa
   real sza
   integer :: nodeFlag
   integer scanPos
   integer scanLine
   integer orbitNumber
   integer scanYear
   integer scanDay
   real(r_double) scanUTC
   real,dimension(maxChan):: angleArr 
   real(r_double),dimension(maxChan):: tbArr
   integer, dimension(NUM_QC):: QC_Arr
end type sdrBody

end module
