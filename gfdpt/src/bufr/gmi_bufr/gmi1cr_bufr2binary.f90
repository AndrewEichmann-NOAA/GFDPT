PROGRAM gmi_bufr2binary

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
    REAL               :: CLATH
    REAL               :: CLONH
    REAL, ALLOCATABLE  :: CLATH1(:,:,:)           !- LAT,
    REAL, ALLOCATABLE  :: CLONH1(:,:,:)            !- LON
    REAL               :: YEAR,MNTH,DAYS,HOUR,MINU,SECO
    REAL               :: UTC
    REAL(8)            :: OGCE,GSES,SIID,SCLF,ORBN,SLNM,FOVN,GMISQ
!   MAKE THESE ARRAYS?
    REAL(8)                 :: SOLAZI,SOEL,IANG,AANG,ACQF,MTYP,SAMA,SZA
    REAL                 :: relAz,solarZen
    REAL, ALLOCATABLE    :: CHNM1(:,:,:),TMBR1(:,:,:),SCCF1(:,:,:),ANPO1(:,:,:),VIIRSQ1(:,:,:),ALFR1(:,:,:)
    REAL, DIMENSION(13)  ::CHNM,TMBR,SCCF,ANPO,cfreq                             ! nchan dimension
    REAL(8), DIMENSION(6)::timedate
    REAL(8), DIMENSION(2)::latlon    
    REAL(8), DIMENSION(4,13):: obsR, obsC                              ! nchan dimension
    integer              :: str_id0,str_id1,str_id2,str_ida
    REAL, DIMENSION(13)  :: angle    ! correct?                       ! nchan dimension
    INTEGER              :: nqc        ! VIIRSQ and ACQF 
    INTEGER              :: chanm
    INTEGER              :: yr,mon,day,julday
    INTEGER              :: fov, sln
    INTEGER              :: wr_header
    INTEGER, DIMENSION(1)  :: GMISQ1
    INTEGER, DIMENSION(13) :: pol                                     ! nchan dimension
    INTEGER, DIMENSION(160):: AD_sln
!   INTEGER, DIMENSION(12) :: ANPO
    INTEGER, DIMENSION(14) :: VIIRSQ
    LOGICAL                :: iopened
    CHARACTER(LEN=200)      :: namecheck


! ------------------- unedited above this point

    n_arguments = iargc()
    if (n_arguments .eq. 1) then
      call getarg(1, bufrfile)
    endif

    str_ida = len(bufrfile)
    str_id0 = index(bufrfile(0:str_ida),'1C-R.GPM.GMI')

    str_id1 = index(bufrfile(0:str_ida),'1C-R.')
    str_id2 = index(bufrfile(0:str_ida),'_BUFR')
    MFroot_sub=bufrfile((str_id1+23):(str_id2-2))
    MeasurFile= 'FMSDR_GPM_GMI.L1CR_' // trim(MFroot_sub) // '.CR'

print*,'bufrfile=',bufrfile
print*,'str_id1=',str_id1
print*,'str_id2=',str_id2    
print*,'MeasurFile=',MeasurFile


    bufrtab='GMI_bufr_table_1CR'
    nread=0
    nchan = 13
    lnbufr=10
    unit_table=12
!    lnbufr=get_lun()
    nscan=221
    angle=(/52.821,52.821,52.821,52.821,52.821,52.821,52.821,52.821,52.821,49.495,49.495,49.495,49.495/)
    nqc=1                         ! 1 (VIIRSQ) or 2 (VIIRSQ and ACQF)?

    cfreq=(/10.65,10.65,18.7,18.7,23.8,36.5,36.5,89.0,89.0,166.0,166.0,183.31,183.31/)  
    pol=(/4,5,4,5,4,4,5,4,5,4,5,4,4/)  

    allocate (CLATH1(1,221,160))
    allocate (CLONH1(1,221,160))

    open(lnbufr,file=trim(bufrfile),form='unformatted',action='read')

inquire(lnbufr, opened=iopened, name=namecheck)
print *,'iopened=',iopened
print *,'namecheck=',namecheck

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
        sln = int(SLNM)
        call ufbrep(lnbufr,FOVN,1,1,iret,'FOVN')
        fov = int(FOVN)
        call ufbrep(lnbufr,latlon,2,1,iret,'CLATH CLONH')
!print*,'sln,fov=', sln,fov
        do l=1,160
          do n=1,221
            if (l==sln .and. n==fov ) then
              CLATH1(1,fov,sln)=latlon(1)
            endif
          enddo
        enddo

        if(nscanl < int(SLNM)) then
          nscanl=int(SLNM)
        endif
      enddo
    enddo
    call closbf(lnbufr)

    close(lnbufr)
    close(unit_table)

!    allocate (CLATH1(1,nscan,nscanl))
!    allocate (CLONH1(1,nscan,nscanl))
    
    do l=1,160
      if ( l > 1 ) then
        if (CLATH1(1,110,l) .gt. CLATH1(1,110,(l-1))) then 
          AD_sln(l) = 0
        else if (CLATH1(1,110,l) .lt. CLATH1(1,110,(l-1))) then
          AD_sln(l) = 1
        endif
      endif
    enddo
    AD_sln(1) = AD_sln(2)
!print *,'AD_sln=',AD_sln


    
    nmeasure=irec
print *,'nmeasure,nscanl=',nmeasure,',',nscanl
!print *,'check3'

    open(lnbufr,file=trim(bufrfile),form='unformatted',action='read')
    open(unit_table,file=trim(bufrtab),action='read')

!print *,'check4'

    call openbf(lnbufr,'IN',unit_table)
    call datelen(10)

!print *,'check5'

    irec=0
    wr_header=0
    iline=0
    do while(ireadmg(lnbufr,subset,idate)>=0)
      if(wr_header .eq. 0) then

!print *,'check6'

        call WriteHdrMeasurmts(MeasurFile,lnMF,nmeasure,nqc,nchan,nscan,cfreq,pol,nscanl)  
print *,'wr_header=',wr_header
print *,'iline=',iline
        wr_header = 1
        iline=iline+1

!Print *,'check7'
      endif
    do while (ireadsb(lnbufr)==0) 

!print*,'check8'

      irec=irec+1
      call ufbint(lnbufr,SZA,1,1,iret,'SZA')          ! incidence angle
!print*,'check8a'
      call ufbint(lnbufr,SAMA,1,1,iret,'SAMA')          ! azimuth angle
!print*,'check8b'
      relAz = SAMA
      solarZen = SZA
!print*,'SZA,SAMA=',solarZen,relAz

      call ufbrep(lnbufr,FOVN,1,1,iret,'FOVN')          ! field of view number
      fov=int(FOVN)
!print*,'check8c'
!print *,'fov=',fov
      call ufbrep(lnbufr,GMISQ,1,1,iret,'GMISQ')          ! channel quality flag for ATOVS
!print*,'check8d'
      GMISQ1(1)=int(GMISQ)                                     
!print*,'check8e'
      call ufbrep(lnbufr,SLNM,1,1,iret,'SLNM')          ! scan line number
      sln=int(SLNM)
!print*,'check8f'
!print *,'sln=',sln
! center frequency (in Hz), polarization, Tb, channel number (missing for AMSR2)
      call ufbrep(lnbufr,obsR,4,nchan,iret,'SCCF ANPO TMBR CHNM')     
!print*,'check8g'
      SCCF=obsR(1,:)
      TMBR=obsR(3,:)
      ANPO(l)=int(obsR(2,l))
! get lat and lon
      call ufbrep(lnbufr,latlon,2,1,iret,'CLATH CLONH')
!print*,'check8h'      
      CLATH=latlon(1)
      CLONH=latlon(2)
!print*,'check8i'
!      do l=1,nscanl
!       do n=1,nscan      
!        if(l==sln .and. n==fov )then 
!          CLATH1(1,fov,sln)=latlon(1)
!!          CLONH1(l,sln)=latlon(2)
!        endif
!       enddo
!      enddo
!print*,'check9'     
!      if ( fov > 1) then
!        if (CLATH1(1,(fov-1),sln) < CLATH1(1,fov,sln)) then
!          AD=0
!        else if (CLATH1(1,(fov-1),sln) > CLATH1(1,fov,sln)) then
!          AD=1
!        endif
!      else if (fov == 1) then
!        if (CLATH1(1,221,sln-1) < CLATH1(1,fov,sln)) then
!          AD=0
!        else if (CLATH1(1,221,sln-1) > CLATH1(1,fov,sln)) then
!          AD=1
!        endif
!      endif
!print*,'check10'
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

      nread=nread+nchan

!print*,'check11'

!      call WriteMeasurmts(lnbufr,nqc,ACQF,nchan,angle,obsR(3,:),CLATH,CLONH,AD,***scanUTC,scanDAY***,YEAR,***iscanPos***,SLNM,AANG,**SolZenAngle**)
if (fov >0  .and. sln > 0) then
  AD = AD_sln(sln)
!print*,'lnMF,nqc,GMISQ1,nchan,angle,TMBR,CLATH,CLONH,AD,UTC,julday,yr,fov,sln,SAMA,SZA'
print*,lnMF,nqc,GMISQ1,nchan,angle,TMBR,CLATH,CLONH,AD,UTC,julday,yr,fov,sln,SAMA,SZA
      call WriteMeasurmts(lnMF,nqc,GMISQ1,nchan,angle,TMBR,CLATH,CLONH,AD,UTC,julday,yr,fov,sln,relAz,solarZen)

endif
      enddo 
      enddo
!print*,'check12'

      call closbf(lnbufr)
    
      close(lnbufr)
      close(lnMF)
END PROGRAM 
