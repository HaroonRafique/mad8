      subroutine twwmap(iunit, suml, elmnam, iocc,
     +rt, tt, orbit0, orbit)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save transfer map including kick.                                  *
*----------------------------------------------------------------------*
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer i,iunit,k,l,iocc
      double precision ek,orbit,orbit0,rt,sum1,sum2,suml,temp,tt
      character*(mcnam) elmnam
      dimension         rt(6,6), tt(6,6,6), orbit0(6), orbit(6)
 
      dimension         ek(6), temp(6)
 
*---- Track ORBIT0 using zero kick.
      do 30 i = 1, 6
        sum2 = orbit(i)
        do 20 k = 1, 6
          sum1 = 0.0
          do 10 l = 1, 6
            sum1 = sum1 + tt(i,k,l) * orbit0(l)
   10     continue
          sum2 = sum2 - (rt(i,k) - sum1) * orbit0(k)
          rt(i,k) = rt(i,k) - 2.0 * sum1
   20   continue
        temp(i) = sum2
   30 continue
 
*---- Kick is actual ORBIT minus result of track.
      do 80 i = 1, 6
         ek(i) = temp(i)
 80   continue
 
*---- Output.
      write (iunit, 920) suml, elmnam
      write (iunit, 930) ek
      write (iunit, 930) rt
      write (iunit, 930) tt
*---- Save new ORBIT0 and re-initialize map.
      call ucopy(orbit, orbit0, 6*mwflt)
      call m66one(rt)
      call uzero(tt, 1, 216*mwflt)
 
 920  format(g20.6,a20)
 930  format(6e16.8)
 
      end
