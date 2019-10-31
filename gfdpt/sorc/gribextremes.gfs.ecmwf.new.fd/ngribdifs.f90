SUBROUTINE ngribdifs(difs,dmax,dmin,num,nbig,hgt,wlat,play,np,i,j,k,nlevs,pressl,bigl)
!******************************************************
!
!Modified by B Ballish 7 Nov 2013 to add input arguement bigl
! where any grid point difference bigger than bigl is counted as a large difference in
! new output variable nbig
       dimension hgt(360,181,14)
       dimension pressl(360,181)
       dimension wlat(181),play(14),nlevs(14)
       difs=0.0
       dmax=-999.9
       dmin=999.9
       num=0
       nbig=0
       kmax=k+np
       kmin=k-np
       if(kmax .gt. 14) kmax=14
       if(kmin .lt. 1) kmin=1

       jmax=j+5
       jmin=j-5
       if(jmax .gt. 181) jmax=181
       if(jmin .lt. 1) jmin=1

       do kk=kmin,kmax
       plev=float(nlevs(k))
       do jj=jmin,jmax
       do ii=1,360
       if(plev .lt. pressl(ii,jj)) then
       call grib_dist(dist,ii,jj,i,j)
       if(dist .le. 500.0) then
       difs=difs+play(kk)*wlat(jj)*hgt(ii,jj,kk)*hgt(ii,jj,kk)
       if(hgt(ii,jj,kk) .gt. dmax) dmax=hgt(ii,jj,kk)
       if(hgt(ii,jj,kk) .lt. dmin) dmin=hgt(ii,jj,kk)
       if(abs(hgt(ii,jj,kk)) .gt. bigl) nbig=nbig+1
       num=num+1
       endif
       endif
       enddo
       enddo
       enddo
       difs=sqrt(difs)
       RETURN
       END SUBROUTINE ngribdifs
