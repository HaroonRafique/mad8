      subroutine emtwpr(comand, iloc, elmnam, ienum, iocc, em, amu)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print Mais-Ripken betatron functions for TWISS3 command.           *
* Input:                                                               *
*   COMAND    (char)    Name of command.                               *
*   ILOC      (integer) Position code:                                 *
*                       0 = Initialize.                                *
*                       1 = After an element.                          *
*                       2 = Beginning of line.                         *
*                       3 = End of line.                               *
*   ELMNAM    (char)    Element name, at beginning: command name.      *
*   IENUM     (integer) Element number.                                *
*   IOCC      (integer) Occurrence number.                             *
*   EM(6,6)   (real)    Eigenvector matrix.                            *
*   AMU(3)    (real)    Phases of eigenmodes.                          *
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
      integer ienum,ihead,iline,iloc,iocc,j,j1,j2,k,k1,k2,nline,npage
      double precision alf,amu,bet,em,gam
      character*(*)     comand
      character*(mcnam) elmnam
      dimension         em(6,6), amu(3)
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
 
      dimension         bet(3,3), gam(3,3), alf(3,3)
      character         apos*5, title*(*)
 
      save              ihead, iline, nline, npage
 
      parameter         (title = 'Mais-Ripken functions.')
      integer maxlin
 
*---- Number of lines per print page.
      parameter         (maxlin = 55)
 
*---- Page layout control.
      if (iloc .eq. 0) then
        npage = 0
        nline = maxlin
        iline = 2
        if (stabx) iline = iline + 1
        if (staby) iline = iline + 1
        ihead = 4
        if (.not. stabt) ihead = 6
 
*---- Compute functions to be printed.
      else
        do 20 j = 1, 3
          j1 = 2 * j -1
          j2 = 2 * j
          do 10 k = 1, 3
            k1 = 2 * k - 1
            k2 = 2 * k
            bet(j,k) = em(j1,k1) * em(j1,k1) + em(j1,k2) * em(j1,k2)
            gam(j,k) = em(j2,k1) * em(j2,k1) + em(j2,k2) * em(j2,k2)
            alf(j,k) = em(j1,k1) * em(j2,k1) + em(j2,k2) * em(j1,k2)
   10     continue
   20   continue
 
*---- Print page header.
        nline = nline + iline
        if (nline .ge. maxlin) then
          npage = npage + 1
          call prhead(comand, title, deltas, 0, nline, npage)
          nline = nline + ihead + iline
          if (stabt) then
            write (iqpr2, 910)
          else
            write (iqpr2, 920)
          endif
          call prline(iqpr2)
        endif
 
*---- Print output lines.
        if (iloc .eq. 1) then
          write (apos, '(I5)') ienum
        else if (iloc .eq. 2) then
          apos = 'begin'
        else
          apos = 'end  '
        endif
        if (stabt) then
          write (iqpr2, 930) apos, elmnam, iocc, suml,
     +      (1000.0 * orbit(j), j = 1, 6)
          if (stabx) write (iqpr2, 940)
     +      1, amu(1), (bet(j,1), gam(j,1), alf(j,1), j = 1, 3)
          if (staby) write (iqpr2, 940)
     +      2, amu(2), (bet(j,2), gam(j,2), alf(j,2), j = 1, 3)
          write (iqpr2, 940)
     +      3, amu(3), (bet(j,3), gam(j,3), alf(j,3), j = 1, 3)
        else
          write (iqpr2, 950) apos, elmnam, iocc, suml,
     +      (1000.0 * orbit(j), j = 1, 4), (disp(j), j = 1, 4)
          if (stabx) write (iqpr2, 960)
     +      1, amu(1), (bet(j,1), gam(j,1), alf(j,1), j = 1, 2)
          if (staby) write (iqpr2, 960)
     +      2, amu(2), (bet(j,2), gam(j,2), alf(j,2), j = 1, 2)
        endif
      endif
 
  910 format(' Element sequence',t50,'x',t60,'px',
     +       t80,'y',t90,'py',t110,'t',t120,'pt'/
     +       t50,'[mm]',t60,'[mrad]',t80,'[mm]',t90,'[mrad]',
     +       t110,'[mm]',t120,'[mrad]'/
     +       t28,'mode',t36,'mu',
     +       t46, 'betx',t56, 'gamx',t66, 'alfx',
     +       t76, 'bety',t86, 'gamy',t96, 'alfy',
     +       t106,'bett',t116,'gamt',t126,'alft'/
     +       t36,'[2pi]',
     +       t46, '[m]',t56, '[1/m]',t66, '[1]',
     +       t76, '[m]',t86, '[1/m]',t96, '[1]',
     +       t106,'[m]',t116,'[1/m]',t126,'[1]')
  920 format(' Element sequence',
     +       t55,'x',   t67,'px',    t91,'y',   t103,'py'/
     +       t55,'[mm]',t67,'[mrad]',t91,'[mm]',t103,'[mrad]'/
     +       t55,'Dx',  t67,'Dpx',   t91,'Dy',  t103,'Dpy'/
     +       t55,'[m]', t67,'[rad]', t91,'[m]', t103,'[rad]'/
     +       t28,'mode',t37,'mu',
     +       t49,'betx',t61,'gamx',t73,'alfx',
     +       t85,'bety',t97,'gamy',t109,'alfy'/
     +       t37,'[2pi]',
     +       t49,'[m]',t61,'[1/m]',t73,'[1]',
     +       t85,'[m]',t97,'[1/m]',t109,'[1]')
  930 format(' ',a5,' ',a8,i4,f10.3,t32,10x,3(5x,2f10.6,5x))
  940 format(t31,i1,10f10.5)
  950 format(' ',a5,' ',a8,i4,f10.3,t32,12x,2(6x,2f12.6,6x)/
     +       t32,12x,2(6x,2f12.6,6x))
  960 format(t31,i1,7f12.6)
 
      end
