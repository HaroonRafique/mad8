      subroutine twbtpr(iloc, elmnam, ipos, ienum, iocc)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print listing for Twiss parameters.                                *
* Input:                                                               *
*   ILOC      (integer) Position code:                                 *
*                       1 = Beginning of system.                       *
*                       2 = Entrance of misaligned element.            *
*                       3 = Exit of misaligned element.                *
*                       4 = After an element    (ICODE = 1).           *
*                       5 = Beginning of line   (ICODE = 2).           *
*                       6 = End of line         (ICODE = 3).           *
*                       7 = End of system.                             *
*   ELMNAM    (char)    Element associated with current element.       *
*   IPOS      (integer) Position counter.                              *
*   IENUM     (integer) Element counter.                               *
*   IOCC      (integer) Occurrence counter for current element.        *
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
      integer ietflg,ipnflg, liftbeam,currbeam
      double precision alfa,amass,arad,beta,betas,bunch,bxmax,bymax,
     +charge,cosmux,cosmuy,currnt,deltas,deltat,dtbyds,dxmax,dymax,
     +et,ex,exn,ey,eyn,freq0,gamma,gammas,gamtr,parnum,pc,pdamp,
     +qs,qx,qy,sigdx,sigdy,sige,sigt,sigx,sigxco,sigy,sigyco,sinmux,
     +sinmuy,u0,xcomax,xix,xiy,ycomax,en0,beambv,elkfact,elmfact
 
*---- Particles, emittances and sigmas.
      integer mfact, mbmult
      parameter (mfact = 50, mbmult = 20)
      common /beanam/   prtnam, bsequnam,
     +                  prtnames(mttact), bseqnames(mttact)
      common /beaflt/   amass, charge, en0, pc, gamma,
     +                  ex, exn, ey, eyn, et, sigt, sige,
     +                  bunch, parnum, currnt
      common /beaaux/   sigx, qx, xix, cosmux, sinmux, bxmax, dxmax,
     +                  xcomax, sigxco, sigdx,
     +                  sigy, qy, xiy, cosmuy, sinmuy, bymax, dymax,
     +                  ycomax, sigyco, sigdy,
     +                  qs, alfa, gamtr, deltas, dtbyds, deltat,
     +                  freq0, beta, u0, arad, beambv, pdamp(3),
     +                  gammas, betas,
     +                  elkfact(mfact), elmfact(0:mbmult)
      common /beaint/   ietflg, ipnflg, liftbeam, currbeam
      save   /beaint/
      common /bealog/   fbch, frad
      save              /beanam/, /beaflt/, /beaaux/, /bealog/
      logical           fbch, frad
      character*(mcnam) prtnam, bsequnam, prtnames, bseqnames
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
      integer lcali,lcatt,lccls,lccmd,lccom,lcdef,lcelm,lcexp,lcfld,
     +lckey,lcseq,lcspl,lcsrc,lcvar,ldbnk,ldkey,lref1,lref2,lsali,lscom,
     +lsdir,lsfld,lsflg,lsnum,lsspl,lbeam,lconsm,ldummy
 
*---- Global reference links.
      common /refer/    lref1,
     +                  lcali, lcatt, lccls, lccmd, lccom, lcdef, lcelm,
     +                  lcexp, lcfld, lckey, lcseq, lcspl, lcsrc, lcvar,
     +                  lbeam, lconsm, ldbnk(4), ldkey(4), ldummy(10),
     +                  lsali, lscom, lsdir, lsfld, lsflg, lsnum, lsspl,
     +                  lref2
      save              /refer/
      integer liftseq, currseq
      common /seqinfi/ liftseq, currseq
      character * (mcnam) sequnam, seqnames
      common /seqinfc/ sequnam, seqnames(mttact)
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer ienum,iloc,iocc,ipos,mhead,mline,mtail,nline,npage
      double precision ds,pxco,pyco,utwopi,xco,yco
      double precision zco, pzco
      logical prtacc
      logical anylcav
 
      character         elmnam*(mcnam), apos*5, title*(*)
 
      save              npage, nline
 
      parameter         (mhead = 3, mline = 1, mtail = 8)
      integer maxlin
 
*---- Number of lines per print page.
      parameter         (maxlin = 55)
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (utwopi = 1.0 / (2.0 * pi))
      parameter         (title = 'Linear lattice functions.')
 
*---  print format: if prtacc = true, include energy
      prtacc = anylcav()
*---- Scale closed orbit.
      xco = 1000.0 * orbit(1)
      pxco = 1000.0 * orbit(2)
      yco = 1000.0 * orbit(3)
      pyco = 1000.0 * orbit(4)
      zco = 1000.0 * orbit(5)
      pzco = 100.0 * orbit(6)
 
*---- Switch on position type.
      go to (10, 20, 30, 40, 50, 60, 70), iloc
 
*==== Begin of system: Initialize page layout.
   10 continue
        npage = 0
        nline = maxlin
      go to 9999
 
*==== Print w.r.t. displaced element at entrance.
   20 continue
 
*---- Reserve space on print page, for entrance and exit.
      nline = nline + 2 * mline
      if (nline .gt. maxlin) then
        npage = npage + 1
        call prhead('TWISS', title, deltas, 0, nline, npage)
        nline = nline + mhead + 2 * mline
        if (prtacc)  then
          write (iqpr2, 810)
        else
          write (iqpr2, 910)
        endif
        call prline(iqpr2)
      endif
 
*---- Print.
      write (apos, '(I5)') ienum
      if (prtacc)  then
        write (iqpr2, 820) apos, elmnam, iocc, suml, ener1, zco, pzco,
     +  betx, alfx, amux * utwopi, xco, pxco, disp(1), disp(2),
     +  bety, alfy, amuy * utwopi, yco, pyco, disp(3), disp(4)
      else
        write (iqpr2, 920) apos, elmnam, iocc, suml,
     +  betx, alfx, amux * utwopi, xco, pxco, disp(1), disp(2),
     +  bety, alfy, amuy * utwopi, yco, pyco, disp(3), disp(4)
      endif
      go to 9999
 
*==== Print w.r.t. displaced element at exit.
   30 continue
      if (prtacc)  then
        write (iqpr2, 830)
     +  betx, alfx, amux * utwopi, xco, pxco, disp(1), disp(2),
     +  bety, alfy, amuy * utwopi, yco, pyco, disp(3), disp(4)
      else
        write (iqpr2, 930)
     +  betx, alfx, amux * utwopi, xco, pxco, disp(1), disp(2),
     +  bety, alfy, amuy * utwopi, yco, pyco, disp(3), disp(4)
      endif
      go to 9999
 
*==== Print w.r.t. ideal orbit after an element.
   40 continue
   50 continue
   60 continue
 
*---- Reserve space on print page.
        nline = nline + mline
        if (nline .ge. maxlin) then
          npage = npage + 1
          call prhead('TWISS', title, deltas, 0, nline, npage)
          nline = nline + mhead + mline
          if (prtacc)  then
            write (iqpr2, 810)
          else
            write (iqpr2, 910)
          endif
          call prline(iqpr2)
        endif
 
*---- Print.
        if (iloc .eq. 4) then
          write (apos, '(I5)') ienum
        else if (iloc .eq. 5) then
          apos = 'begin'
        else
          apos = 'end'
        endif
      if (prtacc) then
        write (iqpr2, 820) apos, elmnam, iocc, suml, ener1, zco, pzco,
     +  betx, alfx, amux * utwopi, xco, pxco, disp(1), disp(2),
     +  bety, alfy, amuy * utwopi, yco, pyco, disp(3), disp(4)
      else
        write (iqpr2, 920) apos, elmnam, iocc, suml,
     +  betx, alfx, amux * utwopi, xco, pxco, disp(1), disp(2),
     +  bety, alfy, amuy * utwopi, yco, pyco, disp(3), disp(4)
      endif
      go to 9999
 
*==== Summary print-out at end of system.
   70 continue
 
*---- Reserve space on print page.
        call prline(iqpr2)
        nline = nline + mtail
        if (nline .gt. maxlin) then
          npage = npage + 1
          call prhead('TWISS', title, deltas, 0, nline, npage)
        endif
 
*---- Summary for non-periodic case.
        if (stabt) then
          ds = 1000.0 * deltat
        else
          ds = 1000.0 * (deltat + orbit0(5) - orbit(5))
        endif
        if (inval) then
          write (iqpr2, 940)
     +      circ, qx, qy, ds, xix, xiy,
     +      bxmax, bymax, dxmax, dymax, sigdx, sigdy
 
*---- Summary for periodic case.
        else
          write (iqpr2, 950)
     +      circ, qx, qy, ds, xix, xiy,
     +      alfa, bxmax, bymax, gamtr, dxmax, dymax, sigdx, sigdy,
     +      1000.0*xcomax, 1000.0*ycomax, 1000.0*sigxco, 1000.0*sigyco
        endif
        call prline(iqpr2)
      go to 9999
  810 format('      ELEMENT SEQUENCE                              ',
     +       'I                H O R I Z O N T A L                 ',
     +       'I                  V E R T I C A L'/
     +       ' pos.  element occ.    dist  energy   z(co)  dE(co) ',
     +       'I     betax    alfax   mux  x(co)  px(co) Dx    Dpx  ',
     +       'I     betay    alfay   muy  y(co)  py(co) Dy    Dpy'/
     +       ' no.   name    no.      [m]  [GeV]     [mm]    [%]  ',
     +       'I     [m]      [1]    [2pi]  [mm]  [.001] [m]   [1]  ',
     +       'I     [m]      [1]    [2pi]  [mm]  [.001] [m]   [1]')
  820 format(' ',a5,' ',a8,i4,f10.3,f9.3,2f7.3,
     +       2(f10.3,f10.3,f8.3,3f6.3,f6.3))
  830 format(' ',28x,2(f10.3,f10.3,f8.3,3f6.3,f6.3))
 
*---- Tor: FORMAT statements for synchrotron integrals ... not used
 
C 945 FORMAT( 1X,'Synch. Int 1 =',1PG18.6,10X,'Synch. int 2 =',G18.6,
C    +       10X,'Synch. Int 3 =',1PG18.6/
C    +        1X,'Synch. Int 4 =',1PG18.6,10X,'Synch. int 5 =',G18.6,
C    +       10X,'T566(approx) =',1PG18.6/)
 
  910 format('      ELEMENT SEQUENCE       ',
     +       'I                H O R I Z O N T A L               ',
     +       'I                  V E R T I C A L'/
     +       ' pos.  element occ.     dist ',
     +       'I   betax  alfax   mux    x(co)  px(co) Dx    Dpx  ',
     +       'I   betay   alfay    muy    y(co)  py(co)  Dy    Dpy'/
     +       ' no.   name    no.      [m]  ',
     +       'I   [m]    [1]     [2pi]  [mm]   [.001] [m]   [1]  ',
     +       'I   [m]     [1]      [2pi]  [mm]   [.001]  [m]   [1]')
  920 format(' ',a5,' ',a8,i4,f10.3,2(f9.3,f7.3,f8.3,f8.4,
     +2f7.3,f6.3))
  930 format(' ',28x,2(f9.3,f7.3,f8.3,f8.4,2f7.3,f6.3))
  940 format( 1x,'total length =',f18.6,10x,'mux          =',f18.6,
     +       10x,'muy          =',f18.6/
     +        1x,'delta(s)     =',f18.6,' mm',
     +        7x,'dmux         =',f18.6,10x,'dmuy         =',f18.6/
     +       43x,'betax(max)   =',f18.6,10x,'betay(max)   =',f18.6/
     +       43x,'Dx(max)      =',f18.6,10x,'Dy(max)      =',f18.6/
     +       43x,'Dx(r.m.s.)   =',f18.6,10x,'Dy(r.m.s.)   =',f18.6)
  950 format( 1x,'total length =',f18.6,10x,'Qx           =',f18.6,
     +       10x,'Qy           =',f18.6/
     +        1x,'delta(s)     =',f18.6,' mm',
     +        7x,'Qx''          =',f18.6,10x,'Qy''          =',f18.6/
     +        1x,'alfa         =',e22.6, 6x,'betax(max)   =',f18.6,
     +       10x,'betay(max)   =',f18.6/
     +        1x,'gamma(tr)    =',f18.6,10x,'Dx(max)      =',f18.6,
     +       10x,'Dy(max)      =',f18.6/
     +       43x,'Dx(r.m.s.)   =',f18.6,10x,'Dy(r.m.s.)   =',f18.6/
     +       43x,'xco(max)     =',f18.6,10x,'yco(max)     =',f18.6/
     +       43x,'xco(r.m.s.)  =',f18.6,10x,'yco(r.m.s.)  =',f18.6)
 9999 end
