PROGRAM CREATEGRADSSTATIONDATA
IMPLICIT NONE
   character*8 stnid
   character*10 gdate,ndate
   real  time
   integer yr,mm,dd,hh,nlev,nflag,ierr,oldd,olhh
   real lat,lon,rain,snow,ice,frz

 open(21, file='grads.dat', form='unformatted', status='unknown'&
 ,access='stream')
 open(11, file='test.txt', form='formatted', status='old',iostat=ierr)
 if (ierr .ne. 0 ) stop
 nflag = 0 
 do
   read(11,'(i4,x,i2,x,i2,x,i2,3x,a8,3x,f5.2,x,f7.2,x,4f5.2)',iostat=ierr) &
   yr,mm,dd,hh,stnid,lat,lon,rain,snow,ice,frz
   print *,dd,hh,stnid
   if (ierr .ne. 0 ) exit
   if (nflag.eq.0) then
     nflag=1
     oldd=dd
     olhh=hh
   endif

!   write(gdate,'(i10)') yr,mm,dd,hh
!   ndate=gdate
!   print *, 'ndate = ', ndate
!   print *, 'stnid = ', id
   if(oldd .ne. dd .or. olhh .ne. hh) then
     print *, 'new day or new hour so ',olhh,' not equal ', hh
     nlev=0
     write(21) stnid,lat,lon,time,nlev,nflag
     oldd=dd
     olhh=hh
   endif

   time=0.0
   nlev=1
   nflag=1
   write(21) stnid,lat,lon,time,nlev,nflag
   write(21) rain,snow,ice,frz

 enddo

close(11)

  

!   WRITE THE TERMINATE TAIL  
 nlev=0
 write(21) stnid,lat,lon,time,nlev,nflag
 stop
 end program CREATEGRADSSTATIONDATA
