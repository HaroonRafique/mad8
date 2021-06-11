      subroutine haprnt(word2)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print results of minimization.                                     *
* Input:                                                               *
*   WORD      (char)    String "before" or "after".                    *
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
      integer i,nline
      double precision badtot,hcon,zero
      character*(*)     word2
      integer mhfun
      double precision hdes,hfac,hfun,hwei
 
*---- Data for minimization in HARMON.
      parameter         (mhfun = 21)
      common /hafbad/   hdes(mhfun), hfun(mhfun), hwei(mhfun),
     +                  hfac(mhfun)
      save              /hafbad/
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      character         word1*(*), word3*(*), title*24
      dimension         hcon(mhfun)
 
      parameter         (zero = 0.0d0)
      parameter         (word1 = 'HARMON ', word3 = ' matching.')
 
*---- Print page header.
      title = word1 // word2 // word3
      call prhead('HCELL', title, zero, 0, nline, 1)
 
*---- Print out results obtained.
      call hathin(1)
 
*---- Print page header.
      call prhead('HCELL', title, zero, 0, nline, 2)
 
*---- Output the functions of penalty.
      badtot = 0.0
      hfac(1) = 1.0
      hfac(2) = 1.0
      do 10 i = 1, mhfun
        hcon(i) = ((hfun(i) - hdes(i)) * hfac(i)) ** 2
        badtot = badtot + hcon(i)
   10 continue
      write (iqpr2, 910)
      write (iqpr2, 920)
     +  (hdes(i), hfun(i), hwei(i), hcon(i), i = 1, 9)
      write (iqpr2, 930) 'interaction',
     +  (hdes(i), hfun(i), hwei(i), hcon(i), i = 10, 15)
      if (symm) then
        write (iqpr2, 930) 'symmetry',
     +    (hdes(i), hfun(i), hwei(i), hcon(i), i = 16, 21)
      endif
      write (iqpr2, 940) badtot
 
  910 format(' '/' Penalty:'/7x,'design',11x,'value',10x,'weight',
     +       4x,'contribution',t68,'meaning')
  920 format(' '/' Tune shifts:'/1p,
     +       ' ',4e16.6,'  d(Qx)/d(delta)'/
     +       ' ',4e16.6,'  d(Qy)/d(delta)'/
     +       ' ',4e16.6,'  d**2(Qx)/d(delta)**2'/
     +       ' ',4e16.6,'  d**2(Qy)/d(delta)**2'/
     +       ' ',4e16.6,'  d**3(Qx)/d(delta)**3'/
     +       ' ',4e16.6,'  d**3(Qy)/d(delta)**3'/
     +       ' ',4e16.6,'  d(Qx)/d(Ex)'/
     +       ' ',4e16.6,'  d(Qy)/d(Ey)'/
     +       ' ',4e16.6,'  d(Qy)/d(Ex)')
  930 format(' '/' At ',a,' point:'/1p,
     +       ' ',4e16.6,'  d(Dx)/d(delta)'/
     +       ' ',4e16.6,'  d**2(Dx)/d(delta)**2'/
     +       ' ',4e16.6,'  d(betax)/d(delta) / betax'/
     +       ' ',4e16.6,'  d(betay)/d(delta) / betay'/
     +       ' ',4e16.6,'  x resonances',/
     +       ' ',4e16.6,'  y resonances')
  940 format(' '/' Total penalty function: ',1p,e16.6/' ')
 
      end
