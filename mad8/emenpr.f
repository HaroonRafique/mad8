      subroutine emenpr(comand, iloc, elmnam, ienum, iocc, sigma)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print beam sizes for NORMAL command.                               *
*   Uses data from BEAM command and current beam line sequence.        *
* Input:                                                               *
*   COMAND    (char)    Name of command.                               *
*   ILOC      (integer) Position code:                                 *
*                       1 = Beginning of system.                       *
*                       4 = After an element.                          *
*                       5 = Beginning of line.                         *
*                       6 = End of line.                               *
*                       7 = Summary.                                   *
*   ELMNAM    (char)    Element name.                                  *
*   IENUM     (integer) Element number.                                *
*   IOCC      (integer) Occurrence number.                             *
*   SIGMA(6,6)(real)    Beam matrix in internal form.                  *
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
      integer ienum,iloc,iocc,j,k,mhead,mline,nline,npage
      double precision corr,eps,sigma
      character*(*)     comand
      character*(mcnam) elmnam
      dimension         sigma(6,6)
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
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
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
 
      character         apos*5, title*(*)
      dimension         corr(6,6)
 
      save              nline, npage
 
      parameter         (eps = 1.0d-8, mhead = 2, mline = 7)
      parameter         (title = 'Beam envelope.')
      integer maxlin
 
*---- Number of lines per print page.
      parameter         (maxlin = 55)
 
*---- Page layout control.
      if (iloc .eq. 0) then
        npage = 0
        nline = maxlin
 
*---- Print page header.
      else
        nline = nline + mline
        if (nline .ge. maxlin) then
          npage = npage + 1
          call prhead(comand, title, deltas, 0, nline, npage)
          nline = nline + mhead + mline
          if (stabt) then
            write (iqpr2, 910)
          else
            write (iqpr2, 920)
          endif
          call prline(iqpr2)
        endif
 
*---- Compute standard deviations and correlations.
        call emci2t(sigma, corr)
 
*---- Print output lines.
        if (iloc .eq. 1) then
          write (apos, '(I5)') ienum
        else if (iloc .eq. 2) then
          apos = 'begin'
        else
          apos = 'end'
        endif
        if (stabt) then
          write (iqpr2, 930) apos, elmnam, iocc, suml,
     +      (1000.0 * orbit(j), 1000.0 * corr(j,j),
     +       (corr(j,k), k = 1, j - 1), j = 1, 6)
        else
          write (iqpr2, 940) apos, elmnam, iocc, suml,
     +      (1000.0 * orbit(j), disp(j), 1000.0 * corr(j,j),
     +       (corr(j,k), k = 1, j - 1), j = 1, 4)
        endif
      endif
 
  910 format(t8,'Element sequence',t47,'orbit',t60,'sigma',t73,
     +       'c o r r e l a t i o n s')
  920 format(t8,'Element sequence',t47,'orbit',t60,'dispersion',
     +       t73,'sigma', t86, 'c o r r e l a t i o n s')
  930 format(' ',a5,' ',a8,i4,f10.3,
     +       t31,'x [mm]',t41,2f13.6/t31,'px [mrad]',t41,3f13.6/
     +       t31,'y [mm]',t41,4f13.6/t31,'py [mrad]',t41,5f13.6/
     +       t31,'t [mm]',t41,6f13.6/t31,'pt [mrad]',t41,7f13.6)
  940 format(' ',a5,' ',a8,i4,f10.3,
     +       t31,'x [mm]',t41,3f13.6/t31,'px [mrad]',t41,4f13.6/
     +       t31,'y [mm]',t41,5f13.6/t31,'py [mrad]',t41,6f13.6)
 
      end