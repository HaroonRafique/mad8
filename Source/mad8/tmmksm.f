      subroutine tmmksm(fsec)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Make transfer map for full superperiod from half superperiod.      *
* Input:                                                               *
*   FSEC      (logical) True, if second order terms are desired.       *
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
      logical           fsec
      double precision ek,re,te
 
*---- Transfer map for current element.
      common /mapelm/   ek(6), re(6,6), te(6,6,6)
      save              /mapelm/
      double precision rt,rtp,tt
 
*---- Transfer map for complete turn.
      common /maptrn/   rt(6,6), tt(6,6,6), rtp(6,6)
      save              /maptrn/
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
 
*---- Second order desired?
      if (fsec) then
        call tmrefl(rt, tt, re, te)
        call tmcat(.true., re, te, rt, tt, rt, tt)
        suml = suml + suml
 
*---- First order only.
      else
        call m66ref(rt, re)
        call m66mpy(re, rt, rt)
        suml = suml + suml
      endif
 
      end
