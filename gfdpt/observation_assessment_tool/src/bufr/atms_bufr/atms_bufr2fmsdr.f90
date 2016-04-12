


program atms_bufr2fmsdr

  use atms_spatial_average_mod
  use IO_MeasurData
  use IO_InstrConfig
  use misc
  use kinds
!  use quick_sort
  use crtm_module, only : max_sensor_zenith_angle


  implicit none

  integer(i_kind),parameter :: n1bhdr=12
  integer(i_kind),parameter :: n2bhdr=4
  integer(i_kind),parameter :: maxinfo=33
  integer(i_kind),parameter :: maxobs = 800000
  integer(i_kind),parameter :: nchanl = 22
  real(r_kind),parameter :: r360=360.0_r_kind
  real(r_kind),parameter :: tbmin=50.0_r_kind
  real(r_kind),parameter :: tbmax=550.0_r_kind
  ! The next two are one minute in hours
  real(r_kind),parameter :: one_minute=0.01666667_r_kind
  real(r_kind),parameter :: minus_one_minute=-0.01666667_r_kind
  real(r_kind),parameter :: rato=1.1363987_r_kind ! ratio of satellite height to 
                                                 ! distance from Earth's centre

  character(8)    :: subset
  character(80)   :: hdr1b,hdr2b

  integer,parameter :: itxmax=800000
  integer(i_kind)   :: ireadsb,ireadmg,irec,isub
  integer(i_kind)   :: i,j,k,ntest,llll,iob
  integer(i_kind)   :: iret,idate,n,idomsfc(1)
  integer(i_kind)   :: kidsat,instrument
  integer(i_kind)   :: nmind,itx,nreal,nele,itt,ninstruments, num_obs
  integer(i_kind)   :: lnbufr,ksatid
  integer(i_kind),dimension(6) :: idate5
  integer(i_kind)              :: instr,ichan,icw4crtm
  integer(i_kind)              :: error_status,ier
  integer(i_kind)              :: radedge_min, radedge_max
  integer(i_kind), POINTER     :: ifov, ifov1
  integer(i_kind), TARGET      :: ifov_save(maxobs)
  integer(i_kind), ALLOCATABLE :: IScan(:)


!  real(r_kind) pred
  real(r_kind)   :: dlat,panglr,dlon,tdiff
  real(r_kind)   :: dlon_earth_deg,dlat_earth_deg,r01
  real(r_kind)   :: step,start,dist1
  real(r_kind)   :: tt,lzaest
  real(r_kind), allocatable :: Relative_Time_In_Seconds(:)
  real(r_kind), POINTER     :: rsat, t4dv, solzen, solazi
  real(r_kind), POINTER     :: dlon_earth,dlat_earth,satazi, lza
  real(r_kind), DIMENSION(:), POINTER :: bt_in(:)


  real :: pi
  real :: cdeg2rad
  real :: crad2deg

  real(r_kind), DIMENSION(maxobs), TARGET :: rsat_save
  real(r_kind), DIMENSION(maxobs), TARGET :: t4dv_save
  real(r_kind), DIMENSION(maxobs), TARGET :: dlon_earth_save
  real(r_kind), DIMENSION(maxobs), TARGET :: dlat_earth_save
  real(r_kind), DIMENSION(maxobs), TARGET :: lza_save
  real(r_kind), DIMENSION(maxobs), TARGET :: satazi_save
  real(r_kind), DIMENSION(maxobs), TARGET :: solzen_save
  real(r_kind), DIMENSION(maxobs), TARGET :: solazi_save
  real(r_kind), DIMENSION(nchanl,maxobs), TARGET :: bt_save
  real(r_kind), DIMENSION(nchanl,maxobs) :: bt_savesort
  real(r_kind), DIMENSION(maxobs) :: sec_save
  real(8)     , DIMENSION(maxobs) :: time_atms
  real, DIMENSION(:), ALLOCATABLE :: list
  integer, DIMENSION(:), ALLOCATABLE :: order

  INTEGER, DIMENSION(6,maxobs) :: idate5_save
  INTEGER, DIMENSION(maxobs)   :: node_save

  real(r_double),  dimension(nchanl)    :: data1b8
  real(r_double),  dimension(n1bhdr)    :: bfr1bhdr
  real(r_double),  dimension(n2bhdr)    :: bfr2bhdr

  real,    dimension(nchanl)    :: angle,btwrite
  real                          :: secs_UTC,solzenith,lat,lon,lat4node
  integer                       :: julday,node
  integer                       :: nobs,count
  integer                       :: nScanLines,nScanPos
  integer                       :: iuFMSDR
  integer, parameter            :: nqc=11
  integer, dimension(nqc)       :: qc
  type(InstrConfig_type)        :: InstrConfig

  !---Namelist vars
  character(LEN=300) :: FMSDRfile
  character(LEN=300) :: fileBUFR
  character(LEN=300) :: pathFMSDR
  character(LEN=300) :: InstrConfigFile

  
  NAMELIST /ReadBufrATMS/FMSDRfile,fileBUFR,pathFMSDR,InstrConfigFile

  !---Read control-data from namelist
  READ(*,NML=ReadBufrATMS)

  CALL ReadInstrConfig(InstrConfigFile,InstrConfig)

  pi = acos(-1.)
  cdeg2rad = pi/180.0
  crad2deg = 1/cdeg2rad
  num_obs = 0
! Reopen unit to satellite bufr file
  iob=1
  lnbufr=get_lun()
  call closbf(lnbufr)
  open(lnbufr,file=fileBUFR,form='unformatted',status = 'old',err = 500)

  call openbf(lnbufr,'IN',lnbufr)
  hdr1b ='SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON CLATH CLONH'
  hdr2b ='SAZA SOZA BEARAZ SOLAZI'

  print *,'READING BUFR FILE: ',fileBUFR,lnbufr   
  ! Loop to read bufr file
  irec=0
  read_subset: do while(ireadmg(lnbufr,subset,idate)>=0 .AND. iob < maxobs)

  !   print *,ireadmg(lnbufr,subset,idate),lnbufr,subset,idate

     read_loop: do while (ireadsb(lnbufr)==0 .and. iob < maxobs)

        rsat       => rsat_save(iob)
        t4dv       => t4dv_save(iob)
        dlon_earth => dlon_earth_save(iob)
        dlat_earth => dlat_earth_save(iob)
    !   crit1      = crit1_save(iob)
        ifov       => ifov_save(iob)
        lza        => lza_save(iob)
        satazi     => satazi_save(iob)
        solzen     => solzen_save(iob)
        solazi     => solazi_save(iob)



!       Read header record.  (llll=1 is normal feed, 2=EARS data)
        call ufbint(lnbufr,bfr1bhdr,n1bhdr,1,iret,hdr1b)

!       Extract satellite id.  If not the one we want, read next record
        kidsat=224
        rsat=bfr1bhdr(1) 
        ksatid=nint(bfr1bhdr(1))
        if(ksatid /= kidsat) cycle read_subset

!       Extract observation location and other required information
        if(abs(bfr1bhdr(11)) <= 90. .and. abs(bfr1bhdr(12)) <= r360)then
           dlat_earth = bfr1bhdr(11)
           dlon_earth = bfr1bhdr(12)
        elseif(abs(bfr1bhdr(9)) <= 90. .and. abs(bfr1bhdr(10)) <= r360)then
           dlat_earth = bfr1bhdr(9)
           dlon_earth = bfr1bhdr(10)
        else
           cycle read_loop
        end if
        if(dlon_earth<0)  dlon_earth = dlon_earth+r360
        if(dlon_earth>=r360) dlon_earth = dlon_earth-r360

!       Extract date information.  If time outside window, skip this obs
        idate5(1) = bfr1bhdr(3) !year
        idate5(2) = bfr1bhdr(4) !month
        idate5(3) = bfr1bhdr(5) !day
        idate5(4) = bfr1bhdr(6) !hour
        idate5(5) = bfr1bhdr(7) !minute
        idate5(6) = bfr1bhdr(8) !sec?
        idate5_save(1:6,iob) =  idate5(1:6)
        sec_save(iob) = bfr1bhdr(8)
        CALL compjulday(idate5(1),idate5(2),idate5(3),julday)  
        time_atms(iob) = julday*3600.0d0*24.0d0 + idate5(4)*3600.0d0 + &
             idate5(5)*60.0 + sec_save(iob)
        
        call ufbint(lnbufr,bfr2bhdr,n2bhdr,1,iret,hdr2b)

        satazi=bfr2bhdr(3)
        if (abs(satazi) > r360) then
           satazi=0
        endif

        ifov = nint(bfr1bhdr(2))
        lza = cdeg2rad*bfr2bhdr(1)      ! local zenith angle
        if(ifov <= 48)    lza=-lza

!        panglr=cdeg2rad*(start+float(ifov-1)*step)
        !---ESM :: satellite height is not constant with orbital position
!        lzaest = asin(rato*sin(panglr))

!        if(abs(crad2deg*lza) > max_sensor_zenith_angle) then
!          write(6,*)'READ_ATMS WARNING lza error ',lza,panglr
!         cycle read_loop
!        end if

!       Check for errors in satellite zenith angles 
 !       if(abs(crad2deg*(lzaest-lza)) > 1) then
 !         write(6,*)' READ_ATMS WARNING uncertainty in lza ', &
 !            crad2deg*lza,crad2deg*lzaest,ifov,start,step
 !         cycle read_loop
 !       end if
        solzen_save(iob)=bfr2bhdr(2) 
        solazi_save(iob)=bfr2bhdr(4) 
!       Read data record.  Increment data counter
!       TMBR is actually the antenna temperature for most microwave sounders but for
!       ATMS it is stored in TMANT.
!       ATMS is assumed not to come via EARS
        call ufbrep(lnbufr,data1b8,1,nchanl,iret,'TMANT')
     !   print *,data1b8(1:nchanl)
        bt_save(1:nchanl,iob) = data1b8(1:nchanl)
        !print *,bt_save(1:nchanl,iob)
        iob=iob+1
        
     end do read_loop
  end do read_subset
  call closbf(lnbufr)
   

  num_obs = iob-1

500 continue
  if (num_obs <= 0) then
     write(*,*) 'READ_ATMS: No ATMS Data were read in...EXITING!'
     STOP
  end if

  print *,'NUMBER OF OrBSERVATIONS READ: ',num_obs
  allocate(list(Num_obs),order(Num_Obs))
  list(1:Num_Obs) = time_atms(1:Num_Obs)
  order = (/ (i,i=1,Num_Obs) /)
!  Num_Obs = 96*280
  DO i = 1, Num_Obs, 30000
    print *, i, i + 30000, MINVAL( (/i+30000,Num_Obs/))
    !call quicksort(list, i, MINVAL( (/i+30000,Num_Obs/)), order)
  ENDDO
!  print *, order(1:Num_Obs)/96
!  stop
  print *,'Finished Sorting data'
  DO i = 1, Num_obs
     BT_savesort(1:nchanl,i) = Bt_save(1:nchanl,order(i))
     ifov1 => ifov_save(order(i))
     ifov => ifov_save(i)
     ifov = ifov1
  ENDDO
  ALLOCATE(Relative_Time_In_Seconds(Num_Obs))
  ALLOCATE(IScan(Num_Obs))
 ! Relative_Time_In_Seconds = 3600.0*T4DV_Save(1:Num_Obs)
  write(*,*) 'Calling ATMS_Spatial_Average'
  CALL ATMS_Spatial_Average(Num_Obs, NChanl, IFOV_Save(1:Num_Obs), &
       Relative_Time_In_Seconds, BT_Savesort(1:nchanl,1:Num_Obs), IScan, IRet)
  write(*,*) 'ATMS_Spatial_Average Called with IRet=',IRet
  DEALLOCATE(Relative_Time_In_Seconds)
  
!  IF (IRet /= 0) THEN
!     write(*,*) 'Error Calling ATMS_Spatial_Average from READ_ATMS'
!     STOP
!  ENDIF

! Allocate arrays to hold all data for given satellite
  nreal = maxinfo + 0
  nele  = nreal   + nchanl
!  allocate(data_all(nele,itxmax),nrec(itxmax))

  call WriteHdrMeasurmts(FMSDRfile,iuFMSDR,num_obs,nqc,nchanl,96, &
       InstrConfig%CentrFreq,InstrConfig%polarity,nScanlines)
  count=0
!  nrec=999999
  !---Loop to get node direction
  !---ESM :: assumes full scans --- not necessarily true
  node_save(:)=0
  NodeLoop: do iob = 1, num_obs
     count=count+iob
     
     dlat_earth => dlat_earth_save(order(iob))
     ifov       => ifov_save(iob)
 
     if (ifov .eq. 48 .and. iob .le. 96) then        
        lat4node = dlat_earth
     endif
     if (ifov .eq. 48 .and. iob .gt. 96 .and. iob .le. 192) then
        if (dlat_earth .lt. lat4node) then 
           node_save(1:96)          = 1
           node_save(iob-47:iob+48) = 1
           lat4node = dlat_earth
        endif
     endif
     if (ifov .eq. 48 .and. iob .gt. 192) then
        if (dlat_earth .lt. lat4node) node_save(iob-47:iob+48)=1
        lat4node=dlat_earth
     endif

  enddo NodeLoop


  print *,'Writing the FMSDR file: ',FMSDRfile
  ObsLoop: do iob = 1, num_obs  
     rsat       => rsat_save(order(iob))
     t4dv       => t4dv_save(order(iob))
     dlon_earth => dlon_earth_save(order(iob))
     dlat_earth => dlat_earth_save(order(iob))
     !crit1      = crit1_save(iob)
     ifov       => ifov_save(iob)
     lza        => lza_save(order(iob))
     satazi     => satazi_save(order(iob))
     solzen     => solzen_save(order(iob))
     solazi     => solazi_save(order(iob))
     bt_in      => bt_save(1:nchanl,order(iob))
     btwrite    = bt_in
     dlat_earth_deg = dlat_earth
     dlon_earth_deg = dlon_earth
     dlat_earth = cdeg2rad*dlat_earth
     dlon_earth = cdeg2rad*dlon_earth   



     solzenith=solzen
     btwrite=bt_in
     lat=dlat_earth_deg
     lon=dlon_earth_deg

     ! Re-calculate look angle
     !panglr=(start+float(ifov-1)*step)*cdeg2rad
     CALL compjulday(idate5_save(1,order(iob)),idate5_save(2,order(iob)),idate5_save(3,order(iob)),julday)  
     !---ESM :: seconds = idate5_save(6,iob) should be real 11/03/2014
     secs_UTC=(idate5_save(4,order(iob))*3600.)+(idate5_save(5,order(iob))*60.)+sec_save(order(iob))

     angle(:)=crad2deg*lza
!     print *,'====================='
!     print *,'WRITING TBs ',btwrite
!     print *,'lat/lon/node',lat,lon,node_save(iob),angle(1)
!     print *,'sec,julday,year',secs_UTC,julday,idate5_save(1,iob)
!     print *,'====================='

     CALL WriteMeasurmts(iuFMSDR,nqc,qc,nchanl,angle,btwrite,lat,lon,node_save(iob), &
          secs_UTC,julday,idate5_save(1,order(iob)),ifov,1,0.,solzenith)


  ENDDO ObsLoop
  Print *,'Done writing FMSDR file. EXITING'

end program atms_bufr2fmsdr
