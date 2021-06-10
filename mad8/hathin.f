      subroutine hathin(iprint)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print results of HARMON functions.                                 *
* Input:                                                               *
* IPRINT      (integer) Print flag.                                    *
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
      integer mhfun
      double precision hdes,hfac,hfun,hwei
 
*---- Data for minimization in HARMON.
      parameter         (mhfun = 21)
      common /hafbad/   hdes(mhfun), hfun(mhfun), hwei(mhfun),
     +                  hfac(mhfun)
      save              /hafbad/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer iprint
      double precision xix2,xix3,xiy2,xiy3
 
      dimension         xix2(2), xiy2(2), xix3(3), xiy3(3)
 
*---- Dispersion function of order two and three.
*     HADDSP must always be run first.
      call haddsp(iprint, hfun(10), hfun(11), hfun(16), hfun(17))
 
*---- (delta beta)/beta.
      call hadbet(iprint, hfun(12), hfun(13), hfun(18), hfun(19))
 
*---- Resonance parameters.
      call hareso(iprint, hfun(14), hfun(15), hfun(20), hfun(21))
 
*---- Tune shift with momentum.
      call hachcl(hfun(1), hfun(2))
      call hadtun(xix2, xiy2, xix3, xiy3)
      hfun(3) = xix2(1) + xix2(2)
      hfun(4) = xiy2(1) + xiy2(2)
      hfun(5) = xix3(1) + xix3(2) + xix3(3)
      hfun(6) = xiy3(1) + xiy3(2) + xiy3(3)
      if (iprint .gt. 0) then
        write (iqpr2, 910)
        if (iprint .gt. 1) then
          write (iqpr2, 920) hfun(1), xix2(1), xix3(1),
     +                       hfun(2), xiy2(1), xiy3(1)
          write (iqpr2, 930) xix2(2), xix3(2), xiy2(2), xiy3(2)
          write (iqpr2, 940) xix3(3), xiy3(3)
          write (iqpr2, 950) hfun(1), hfun(3), hfun(5),
     +                       hfun(2), hfun(4), hfun(6)
        else
          write (iqpr2, 960) hfun(1), hfun(3), hfun(5),
     +                       hfun(2), hfun(4), hfun(6)
        endif
      endif
 
*---- Tune shift with amplitude.
      call haatun(iprint, hfun(7), hfun(8), hfun(9))
 
  910 format(' '/' Derivatives of tune w.r.t. momentum:'/
     +       t19,'h o r i z o n t a l',t69,'v e r t i c a l'/
     +       t13,'first',t28,'second',t45,'third',
     +       t61,'first',t76,'second',t93,'third')
  920 format(' ',1p,6e16.6,' (simple integrals)')
  930 format(' ',1p,16x,2e16.6,16x,2e16.6,' (double integrals)')
  940 format(' ',1p,32x,e16.6,32x,e16.6,' (triple integrals)')
  950 format(' ',1p,6e16.6,' (total)'/' ')
  960 format(' ',1p,6e16.6/' ')
 
      end
