SUBROUTINE troplocdist(ltest,ntrop,troplat,troplon,troprad,i1,j1)
!******************************************************
!
       dimension troplat(50),troplon(50),troprad(50)
       ltest=0
       pi=4.0*atan(1.0)
       fac=6370.0   ! is radius of earth in Km
       xlon1=pi*float(i1)/180.0
       phi1=pi*float(91-j1)/180.0

       if(ntrop .gt. 0) then
       do n=1,ntrop
       xlon2=pi*troplon(n)/180.0
       phi2=pi*troplat(n)/180.0
       cosang=sin(phi1)*sin(phi2)+cos(phi1)*cos(phi2)*cos(xlon1-xlon2)
       dist=fac*acos(cosang)   ! is spherical dist in  KM
       tdist=dist+troprad(n)
       if(tdist .le. 750.0) ltest=1
       enddo
       endif
       RETURN
       END SUBROUTINE troplocdist
