PROGRAM IO_bufr2binary

  USE IO_MeasurData
  USE misc
  IMPLICIT NONE
 
!!!! for AMSR2 only
    CHARACTER(LEN=80)        :: MeasurFile,MFroot,MFroot_sub,bufrfile,bufrtab
    INTEGER            :: lnbufr,unit_table,lnMF
    INTEGER            :: nchan             ! test for LR only   
    INTEGER            :: nrec
    INTEGER            :: nread
    INTEGER            :: nscan     ! 486 for high res chan
    INTEGER            :: nscanl 
    INTEGER            :: i,l,n
    CHARACTER(LEN=8)   :: subset        !- BUFR function output char
    INTEGER            :: ireadmg        !- BUFR function
    INTEGER            :: idate          !- BUFR function
    INTEGER            :: ireadsb        !- BUFR function
    INTEGER            :: iret           !- BUFR function
    INTEGER            :: iline,irec           !- record number
    INTEGER            :: n_arguments,nmeasure


    INTEGER            :: AD             ! 1 for ascending, 2 for descending
    REAL(8)            :: SAID           !- SATELLITE ID
    REAL               :: CLATH            !- LAT,
    REAL               :: CLONH            !- LON
    REAL               :: YEAR,MNTH,DAYS,HOUR,MINU,SECO
    REAL               :: UTC
    REAL(8)            :: OGCE,GSES,SIID,SCLF,ORBN,SLNM,FOVN
!   MAKE THESE ARRAYS?
    REAL(8)                 :: SOLAZI,SOEL,IANG,AANG,ACQF,MTYP
    REAL                 :: relAz,solarZen
    REAL, ALLOCATABLE    :: CHNM1(:,:,:),TMBR1(:,:,:),SCCF1(:,:,:),ANPO1(:,:,:),VIIRSQ1(:,:,:),ALFR1(:,:,:)
    REAL, DIMENSION(14)  ::CHNM,TMBR,SCCF,ANPO,cfreq                             ! nchan dimension
    REAL(8), DIMENSION(6)::timedate
    REAL(8), DIMENSION(2)::latlon    
    REAL(8), DIMENSION(6,14):: obsR, obsC                              ! nchan dimension
    integer              :: str_id0,str_id1,str_id2,str_ida
    REAL, DIMENSION(14)  :: angle    ! correct?                       ! nchan dimension
    INTEGER              :: nqc        ! VIIRSQ and ACQF 
    INTEGER              :: chanm
    INTEGER              :: yr,mon,day,julday
    INTEGER              :: fov, sln
    INTEGER              :: wr_header
    INTEGER, DIMENSION(14) :: pol                                     ! nchan dimension
!   INTEGER, DIMENSION(12) :: ANPO
    INTEGER, DIMENSION(14) :: VIIRSQ
    LOGICAL                :: iopened
    CHARACTER(LEN=200)      :: namecheck

    n_arguments = iargc()
    if (n_arguments .eq. 1) then
      call getarg(1, bufrfile)
    endif

    str_ida = len(bufrfile)
    str_id0 = index(bufrfile(0:str_ida),'D_L1SGBTBR')
    if(str_id0 > 0)then
      AD=1                       ! descending
    else if(str_id0 <= 0)then
      AD=0                       ! ascending
    endif

    str_id1 = index(bufrfile(0:str_ida),'GW1_')
    str_id2 = index(bufrfile(0:str_ida),'.bufr')
    MFroot_sub=bufrfile((str_id1+3):(str_id2-2))
    MeasurFile= 'FMSDR_GW1AM2_' // trim(MFroot_sub) // '.CR'

print*,'bufrfile=',bufrfile
!print*,'str_id1=',str_id1
!print*,'str_id2=',str_id2    
print*,'MeasurFile=',MeasurFile
!print*,'AD=',AD

    bufrtab='bufrtab.021'
    nread=0
    nchan = 14
    lnbufr=10
    unit_table=12
!    lnbufr=get_lun()
    nscan=243
    angle=(/55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0,55.0/)
    nqc=1                         ! 1 (VIIRSQ) or 2 (VIIRSQ and ACQF)?

    cfreq=(/6.925,6.925,7.3,7.3,10.65,10.65,18.7,18.7,23.8,23.8,36.5,36.5,89.0,89.0/)    ! add 89s
!    pol=(/0,1,0,1,0,1,0,1,0,1,0,1,0,1/)
!    pol=(/5,4,5,4,5,4,5,4,5,4,5,4,5,4/)   ! H,V,H,V,H,V.... 2,1,4,3,6,5...
    pol=(/4,5,4,5,4,5,4,5,4,5,4,5,4,5/)   !V,H,V,H.... 1,2,3,4,5...

    open(lnbufr,file=trim(bufrfile),form='unformatted',action='read')

inquire(lnbufr, opened=iopened, name=namecheck)
print *,'iopened=',iopened
!print *,'namecheck=',namecheck

    open(unit_table,file=trim(bufrtab),action='read')

!print*,'check1'

    call openbf(lnbufr,'IN',unit_table)
    call datelen(10)

!print*,'check2'
    
    nscanl=0
    irec=0
    do while(ireadmg(lnbufr,subset,idate)>=0)
      do while (ireadsb(lnbufr)==0)
        irec=irec+1
        call ufbrep(lnbufr,SLNM,1,1,iret,'SLNM')
        if(nscanl < int(SLNM)) then
          nscanl=int(SLNM)
        endif
      enddo
    enddo
    call closbf(lnbufr)

    close(lnbufr)
    close(unit_table)
    
    nmeasure=irec
print *,'nmeasure,nscanl=',nmeasure,',',nscanl
!print *,'check3'

    open(lnbufr,file=trim(bufrfile),form='unformatted',action='read')
    open(unit_table,file=trim(bufrtab),action='read')

    call openbf(lnbufr,'IN',unit_table)
    call datelen(10)

    irec=0
    wr_header=0
    iline=0
    do while(ireadmg(lnbufr,subset,idate)>=0)
      if(wr_header .eq. 0) then
        call WriteHdrMeasurmts(MeasurFile,lnMF,nmeasure,nqc,nchan,nscan,cfreq,pol,nscanl)  
        wr_header = 1
        iline=iline+1
print *,'wr_header=',wr_header
print *,'iline=',iline
      endif
    do while (ireadsb(lnbufr)==0) 

!print*,'check4'

      irec=irec+1
      call ufbrep(lnbufr,SOLAZI,1,1,iret,'SOLAZI')      ! solar azimuth angle
      call ufbrep(lnbufr,SOEL,1,1,iret,'SOEL')          ! solar elivation
      call ufbrep(lnbufr,IANG,1,1,iret,'IANG')          ! incidence angle
      call ufbrep(lnbufr,AANG,1,1,iret,'AANG')          ! azimuth angle
      call ufbrep(lnbufr,FOVN,1,1,iret,'FOVN')          ! field of view number
      fov=int(FOVN)
!print *,'fov=',fov
      call ufbrep(lnbufr,ACQF,1,1,iret,'ACQF')          ! channel quality flag for ATOVS
                                                ! will be 131072 if ScanDataQuality > 0
      call ufbrep(lnbufr,SLNM,1,1,iret,'SLNM')          ! scan line number
      sln=int(SLNM)
!print *,'sln=',sln
! center frequency (in Hz), polarization, Tb, channel number (missing for AMSR2)
      call ufbrep(lnbufr,obsR,6,nchan,iret,'SCCF ANPO TMBR CHNM VIIRSQ ALFR')     
 
      SCCF=obsR(1,:)
      TMBR=obsR(3,:)
      do l=1,nchan
        ANPO(l)=int(obsR(2,l))
        VIIRSQ(l)=int(obsR(5,l))
      enddo
      relAz=AANG
      solarZen=SOEL

! get lat and lon
      call ufbrep(lnbufr,latlon,2,1,iret,'CLATH CLONH')
      
      CLATH=latlon(1)
      CLONH=latlon(2)

!print*,'check5'

! get time and date      
      call ufbrep(lnbufr,timedate,6,1,iret,'YEAR MNTH DAYS HOUR MINU SECO')

      YEAR=timedate(1)
      yr=int(YEAR)
      MNTH=timedate(2)
      mon=int(MNTH)
      DAYS=timedate(3)
      day=int(DAYS)
      HOUR=timedate(4)
      MINU=timedate(5)
      SECO=timedate(6)
      
      UTC= SECO + (MINU*60.0) + (HOUR*3600.0)
      call compJulDay(yr,mon,day,julday)

!print*,'UTC,julday=',UTC,',',julday

! assign channels to AMSR2 data based on channel frequency and polarization
! also assign frequencies in GHz to a new array      
      do l=1,nchan         
        obsC(2,l)=obsR(2,l)
        obsC(3,l)=obsR(3,l)
        obsC(5,l)=obsR(5,l)
!        obsTemp(l)=(obsR(1,l)*(10.0**(-9.0)))
        if(ABS(obsR(1,l)-6900000000.00000)<500.0 .and. int(obsR(2,l))==0)then
          obsC(1,l)=6.925
          obsC(4,l)=2.0
        else if(ABS(obsR(1,l)-6900000000.00000)<500.0 .and. int(obsR(2,l))==1)then
          obsC(1,l)=6.925
          obsC(4,l)=1.0
        else if(ABS(obsR(1,l)-7300000000.00000)<500.0 .and. int(obsR(2,l))==0)then
          obsC(1,l)=7.30
          obsC(4,l)=4.0
        else if(ABS(obsR(1,l)-7300000000.00000)<500.0 .and. int(obsR(2,l))==1)then
          obsC(1,l)=7.30
          obsC(4,l)=3.0
        else if(ABS(obsR(1,l)-10700000000.0000)<500.0 .and. int(obsR(2,l))==0)then
          obsC(1,l)=10.65
          obsC(4,l)=6.0
        else if(ABS(obsR(1,l)-10700000000.0000)<500.0 .and. int(obsR(2,l))==1)then
          obsC(1,l)=10.65
          obsC(4,l)=5.0
        else if(ABS(obsR(1,l)-18700000000.0000)<5000.0 .and. int(obsR(2,l))==0)then
          obsC(1,l)=18.7
          obsC(4,l)=8.0
        else if(ABS(obsR(1,l)-18700000000.0000)<5000.0 .and. int(obsR(2,l))==1)then
          obsC(1,l)=18.7
          obsC(4,l)=7.0
        else if(ABS(obsR(1,l)-23800000000.0000)<5000.0 .and. int(obsR(2,l))==0)then
          obsC(1,l)=23.8
          obsC(4,l)=10.0
        else if(ABS(obsR(1,l)-23800000000.0000)<5000.0 .and. int(obsR(2,l))==1)then
          obsC(1,l)=23.8
          obsC(4,l)=9.0
        else if(ABS(obsR(1,l)-36500000000.0000)<5000.0 .and. int(obsR(2,l))==0)then
          obsC(1,l)=36.5
          obsC(4,l)=12.0
        else if(ABS(obsR(1,l)-36500000000.0000)<5000.0 .and. int(obsR(2,l))==1)then
          obsC(1,l)=36.5
          obsC(4,l)=11.0
        else if(ABS(obsR(1,l)-89000000000.0000)<5000.0 .and. int(obsR(2,l))==0)then
          obsC(1,l)=89.0
          obsC(4,l)=14.0
        else if(ABS(obsR(1,l)-89000000000.0000)<5000.0 .and. int(obsR(2,l))==1)then
          obsC(1,l)=89.0
          obsC(4,l)=13.0
        endif
      enddo
      
      nread=nread+nchan

!print*,'check6'

!      call WriteMeasurmts(lnbufr,nqc,ACQF,nchan,angle,obsR(3,:),CLATH,CLONH,AD,***scanUTC,scanDAY***,YEAR,***iscanPos***,SLNM,AANG,**SolZenAngle**)

!print*,'AANG,SOEL=', AANG,SOEL
      call WriteMeasurmts(lnMF,nqc,VIIRSQ,nchan,angle,TMBR,CLATH,CLONH,AD,UTC,julday,yr,fov,sln,relAz,solarZen)

      enddo 
      enddo
!print*,'check7'

      call closbf(lnbufr)
    
      close(lnbufr)
      close(lnMF)
END PROGRAM IO_bufr2binary
