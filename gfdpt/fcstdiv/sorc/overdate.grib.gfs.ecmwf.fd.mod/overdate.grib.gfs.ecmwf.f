      program overdate
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: OVERDATE     REPLACE DATE ON A GRIB FILE
C   PRGMMR: IREDELL          ORG: NP23        DATE: 1998-01-01
C
C ABSTRACT: THIS PROGRAM READS AN ENTIRE GRIB FILE FROM UNIT 11
C   AND WRITES IT BACK OUT TO UNIT 51, REPLACING THE INTERNAL
C   DATE FROM A DATE READ IN FROM UNIT 5 IN YYYYMMDDHH FORMAT.
C
C PROGRAM HISTORY LOG:
C   1998-01-01  IREDELL
C   1999-05-24  Gilbert     - added calls to BAOPEN.
C   2011-04-01  Krishna Kumar - Modified to change the ECMWF header
C                               from a forecast hour to an analysis  
C                               hour
C
C INPUT FILES:
C   UNIT    5    10-DIGIT DATE IN YYYYMMDDHH FORMAT
C   UNIT   11    INPUT GRIB FILE = "fort.11"
C
C OUTPUT FILES:
C   UNIT   51    OUTPUT GRIB FILE = "fort.51"
C
C SUBPROGRAMS CALLED:
C   SKGB     - Find next grib record
C   BAREAD   - Read GRIB record
C   WRYTE    - Write GRIB record
C
C REMARKS:
C   ANY NON-GRIB INFORMATION IN THE INPUT GRIB FILE WILL BE LOST.
C   AN OUTPUT LINE WILL BE WRITTEN FOR EACH GRIB MESSAGE COPIED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      parameter(msk1=32000,msk2=4000,mgrib=3999999)
      character cgrib(mgrib)
      character * 6 envvar
      data          envvar/'FORT  '/
      character(len=80) :: gfilein,gfileout
C
C  Open input and output grib files
C
      IFL1=11
      IFL3=51
C
C     Read GRIB1 data  file names from the XLFUNIT_nn
C     environment variables, and open the files.
C
      write(envvar(5:6),fmt='(i2.2)') ifl1
      call getenv(envvar,gfilein)
      CALL BAOPENR(ifl1,gfilein,IOS)
      if (IOS.NE.0) then
         call errmsg('overdate.grib: cannot open input GRIB file '//
     &               gfilein)
         call errexit(3)
      endif
C
C     Write output GRIB1 file name from XLFUNIT_
C     environment variable, and open file.
C
      write(envvar(5:6),fmt='(I2)') ifl3
      call getenv(envvar,gfileout)
      CALL BAOPENW(ifl3,gfileout,IOS)
      if (IOS.NE.0) then
         call errmsg('overdate.grib: cannot open output GRIB file '//
     &               gfileout)
         call errexit(4)
      endif
C      read *,idate   ! input in yyyymmddhh form
      read(5,*)idate,imd,ifhr
C
      print*,'idate  = ',idate,'modelid = ',imdd,' forecast hour ',ifhrr
      i4=idate/1000000
      im=(idate-i4*1000000)/10000
      id=(idate-i4*1000000-im*10000)/100
      ih=(idate-i4*1000000-im*10000-id*100)
      ic=(i4-1)/100+1
      iy=(i4-(ic-1)*100)
      n=0
      iseek=0
      call skgb(11,iseek,msk1,lskip,lgrib)
      dowhile(lgrib.gt.0.and.lgrib.le.mgrib)
        call baread(11,lskip,lgrib,ngrib,cgrib)
        if(ngrib.ne.lgrib) call exit(2)
        n=n+1
C
C  PDS=   00 00 1c 02 | 07 60 03 80 | 07 64 00 0a | 0a 0a 06 0c | 00 01 00 78 | 0a 00 00 00 | 15 00 00 02:
C  PDS10=  0 0  28  2    7 96 3 128    7 100 0 10   10 10 6 12     0  1  0 120  10  0  0  0   21  0  0 2
C
        imd0=mova2i(cgrib(8+6))
        ic0=mova2i(cgrib(8+25))
        iy0=mova2i(cgrib(8+13))
        im0=mova2i(cgrib(8+14))
        id0=mova2i(cgrib(8+15))
        ih0=mova2i(cgrib(8+16))
C        ifhr0=mova2i(cgrib(8+20))  For GFS and operational ECMWF files
C        ifhr0=mova2i(cgrib(8+19))  For EMC's /global/stat ECMWF files
        ifhr0=mova2i(cgrib(8+19))   
        cgrib(8+6)=char(imd)
        cgrib(8+25)=char(ic)
        cgrib(8+13)=char(iy)
        cgrib(8+14)=char(im)
        cgrib(8+15)=char(id)
        cgrib(8+16)=char(ih)
C        cgrib(8+20)=char(ifhr)  For GFS and operational ECMWF files
C        cgrib(8+19)=char(ifhr)  For EMC's /global/stat ECMWF files
        cgrib(8+19)=char(ifhr)
        call wryte(51,lgrib,cgrib)
        print '("msg",i6,4x,"len",i8,4x,"was",7i4.2,4x,"now",7i4.2)',
     &   n,lgrib,ic0,iy0,im0,id0,ih0,imd0,ifhr0,ic,iy,im,id,ih,imd,ifhr
        iseek=lskip+lgrib
        call skgb(11,iseek,msk2,lskip,lgrib)
      enddo
      end
