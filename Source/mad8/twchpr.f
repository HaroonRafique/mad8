      subroutine twchpr(elmnam, iloc, ipos, ienum, iocc)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print listing for Twiss parameters, CHROM option.                  *
* Input:                                                               *
*   ELMNAM    (char)    Name associated with current element.          *
*   ILOC      (integer) Position code:                                 *
*                       1 = Beginning of system.                       *
*                       2 = Entrance of misaligned element.            *
*                       3 = Exit of misaligned element.                *
*                       4 = After an element    (ICODE = 1).           *
*                       5 = Beginning of line   (ICODE = 2).           *
*                       6 = End of line         (ICODE = 3).           *
*                       7 = End of system.                             *
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
      integer ienum,iloc,iocc,ipos,mhead,mline,nline,npage
      double precision utwopi
      character*(*)     elmnam
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      character*5       apos, title*(*)
      save              npage, nline
 
      integer maxlin
 
*---- Number of lines per print page.
      parameter         (maxlin = 55)
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (utwopi = 1.0 / (2.0 * pi))
      parameter         (mhead = 3, mline = 1)
      parameter         (title = 'Chromatic functions.')
 
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
          write (iqpr2, 910)
          call prline(iqpr2)
        endif
 
*---- Print.
        write (apos, '(I5)') ienum
        write (iqpr2, 920) apos, elmnam, iocc, suml,
     +    wx, phix * utwopi, dmux * utwopi, ddisp(1), ddisp(2),
     +    wy, phiy * utwopi, dmuy * utwopi, ddisp(3), ddisp(4)
      go to 9999
 
*==== Print w.r.t. displaced element at exit.
   30 continue
        write (iqpr2, 930)
     +    wx, phix * utwopi, dmux * utwopi, ddisp(1), ddisp(2),
     +    wy, phiy * utwopi, dmuy * utwopi, ddisp(3), ddisp(4)
      go to 9999
 
*==== Print w.r.t. ideal orbit after an element.
   40 continue
   50 continue
   60 continue
 
*---- Reserve space on print page.
        nline = nline + 1
        if (nline .gt. maxlin) then
          npage = npage + 1
          call prhead('TWISS', title, deltas, 0, nline, npage)
          nline = nline + mhead + mline
          write (iqpr2, 910)
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
        write (iqpr2, 920) apos, elmnam, iocc, suml,
     +    wx, phix * utwopi, dmux * utwopi, ddisp(1), ddisp(2),
     +    wy, phiy * utwopi, dmuy * utwopi, ddisp(3), ddisp(4)
      go to 9999
 
*==== No summary print-out.
   70 continue
        call prline(iqpr2)
      go to 9999
 
  910 format('      ELEMENT SEQUENCE       ',
     +       'I               H O R I Z O N T A L                ',
     +       'I                 V E R T I C A L'/
     +       ' pos.  element occ.     dist',
     +       '   I   Wx        Phix      Dmux      DDx       DDpx',
     +       '   I   Wy        Phiy      Dmuy      DDy       DDpy'/
     +       ' no.   name    no.      [m] ',
     +       '   I   [1]       [2pi]     [2pi]     [m]       [1] ',
     +       '   I   [1]       [2pi]     [2pi]     [m]       [1]')
  920 format(' ',a5,' ',a8,i4,f10.3,2(1x,5f10.3))
  930 format(' ',28x,2(1x,5f10.3))
 
 9999 end
