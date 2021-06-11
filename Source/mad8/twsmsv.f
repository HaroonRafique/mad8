      subroutine twsmsv(iflag, idelta)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Build table of Twiss summary data (tunes, chromaticities etc.).    *
* Input:                                                               *
*   IFLAG     (integer) Operation desired:                             *
*                       1: Create new table for summary.               *
*                       2: Save one line.                              *
*                       3: Retrieve one line.                          *
*                       4: Close table.                                *
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
      integer memlen,memmin
      parameter         (memmin =  1600 000)
      parameter         (memlen = 16000 000)
      integer llump,lq,lroot
      double precision dq
 
*---- Memory pool definition.
      common //         fence, lq(mwflt*memlen)
      integer           iq(mwflt*memlen)
      real              fence(2), q(mwflt*memlen)
      dimension         dq(memlen)
      equivalence       (iq(1), q(1), dq(1), lq(9))
      equivalence       (lroot, lq(1)), (llump, lq(2))
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      double precision cofact,optflt
 
*---- Option flags.
      common /optflt/   optflt(10)
      equivalence       (cofact, optflt( 1))
      common /optint/   optint(10)
      integer           optint
      integer           icmdfl, ideffl, iexpfl, ikeyfl, ilinfl
      equivalence       (icmdfl, optint( 1)), (ideffl, optint( 2))
      equivalence       (iexpfl, optint( 3)), (ikeyfl, optint( 4))
      equivalence       (ilinfl, optint( 5))
      common /optlog/   optflg(20), optcon(5)
      logical           optflg, optcon
      logical           debug,  double, echo,   inter,  trace,  verify,
     +                  warn,   info,   sympl,  rbarc, ereset, bborbit
      logical           reset,  tell
      equivalence       (debug,  optflg( 1)), (double, optflg( 2))
      equivalence       (echo,   optflg( 3)), (inter,  optflg( 4))
      equivalence       (trace,  optflg( 5)), (verify, optflg( 6))
      equivalence       (warn,   optflg( 7)), (info,   optflg( 8))
      equivalence       (sympl,  optflg( 9)), (rbarc,  optflg(10))
      equivalence       (ereset,  optflg(11)),(bborbit,optflg(12))
      equivalence       (reset,  optcon( 1)), (tell,   optcon( 2))
      save              /optflt/, /optint/, /optlog/
      integer ndelta
 
*---- Common for Twiss module.
      common /twchar/   funnam, optnam, sumnam, betnam
      common /twdata/   ndelta, chrom, couple
      save              /twdata/, /twchar/
      character*(mcnam) funnam, optnam, sumnam, betnam
      logical           chrom, couple
      integer ltwbet,ltwbuf,ltwfun,ltwlin,ltwopt,ltwsum
 
*---- Reference links for lattice function tables.
      common /twlink/   ltwlin, ltwbet, ltwbuf, ltwfun, ltwopt, ltwsum
      save              /twlink/
      integer i,idelta,iflag,iform,maxtun,nb,nc,nr,ns
      double precision dummy
 
      parameter         (maxtun = 17)
      character*(mcnam) tuncol(maxtun)
      integer           icfrm(maxtun)
      data tuncol
     +                  / 'DELTAP', 'ALFA',   'GAMMATR',
     +                    'QX',     'QY',     'XIX',    'XIY',
     +                    'XRMS',   'YRMS',   'XMAX',   'YMAX',
     +                    'BXMAX',  'BYMAX',  'DXMAX',  'DYMAX',
     +                    'DXRMS',  'DYRMS' /
 
*---- Create internal table for global quantities.
      if (iflag .eq. 1) then
        ns = 1
        nr = ndelta
        nc = maxtun
        nb = 1
        iform = 3
        if (double) iform = mreal
        do 10 i = 1, maxtun
          icfrm(i) = iform
   10   continue
        call tbcrea(sumnam, ns, nr, nc, tuncol, icfrm, nb, ltwsum)
        call tbpdsc(ltwsum, 'TYPE', 5, 0, dummy, 'TUNES')
 
*---- Save one table line.
      else if (ltwsum .ne. 0) then
        if (iflag .eq. 2) then
          call tbset(ltwsum, idelta, 3, ltwbuf)
          if (double) then
            call ucopy(deltas, q(ltwbuf+1), mwflt)
            call ucopy(alfa, q(ltwbuf+mwflt+1), mwflt)
            call ucopy(gamtr, q(ltwbuf+2*mwflt+1), mwflt)
            call ucopy(qx, q(ltwbuf+3*mwflt+1), mwflt)
            call ucopy(qy, q(ltwbuf+4*mwflt+1), mwflt)
            call ucopy(xix, q(ltwbuf+5*mwflt+1), mwflt)
            call ucopy(xiy, q(ltwbuf+6*mwflt+1), mwflt)
            call ucopy(sigxco, q(ltwbuf+7*mwflt+1), mwflt)
            call ucopy(sigyco, q(ltwbuf+8*mwflt+1), mwflt)
            call ucopy(xcomax, q(ltwbuf+9*mwflt+1), mwflt)
            call ucopy(ycomax, q(ltwbuf+10*mwflt+1), mwflt)
            call ucopy(bxmax, q(ltwbuf+11*mwflt+1), mwflt)
            call ucopy(bymax, q(ltwbuf+12*mwflt+1), mwflt)
            call ucopy(dxmax, q(ltwbuf+13*mwflt+1), mwflt)
            call ucopy(dymax, q(ltwbuf+14*mwflt+1), mwflt)
            call ucopy(sigdx, q(ltwbuf+15*mwflt+1), mwflt)
            call ucopy(sigdy, q(ltwbuf+16*mwflt+1), mwflt)
          else
            q(ltwbuf+ 1) = deltas
            q(ltwbuf+ 2) = alfa
            q(ltwbuf+ 3) = gamtr
            q(ltwbuf+ 4) = qx
            q(ltwbuf+ 5) = qy
            q(ltwbuf+ 6) = xix
            q(ltwbuf+ 7) = xiy
            q(ltwbuf+ 8) = sigxco
            q(ltwbuf+ 9) = sigyco
            q(ltwbuf+10) = xcomax
            q(ltwbuf+11) = ycomax
            q(ltwbuf+12) = bxmax
            q(ltwbuf+13) = bymax
            q(ltwbuf+14) = dxmax
            q(ltwbuf+15) = dymax
            q(ltwbuf+16) = sigdx
            q(ltwbuf+17) = sigdy
          endif
 
*---- Retrieve one table line.
        else if (iflag .eq. 3) then
          if (double) then
            call ucopy(q(ltwbuf+1), deltas, mwflt)
            call ucopy(q(ltwbuf+mwflt+1), alfa, mwflt)
            call ucopy(q(ltwbuf+2*mwflt+1), gamtr, mwflt)
            call ucopy(q(ltwbuf+3*mwflt+1), qx, mwflt)
            call ucopy(q(ltwbuf+4*mwflt+1), qy, mwflt)
            call ucopy(q(ltwbuf+5*mwflt+1), xix, mwflt)
            call ucopy(q(ltwbuf+6*mwflt+1), xiy, mwflt)
            call ucopy(q(ltwbuf+7*mwflt+1), sigxco, mwflt)
            call ucopy(q(ltwbuf+8*mwflt+1), sigyco, mwflt)
            call ucopy(q(ltwbuf+9*mwflt+1), xcomax, mwflt)
            call ucopy(q(ltwbuf+10*mwflt+1), ycomax, mwflt)
            call ucopy(q(ltwbuf+11*mwflt+1), bxmax, mwflt)
            call ucopy(q(ltwbuf+12*mwflt+1), bymax, mwflt)
            call ucopy(q(ltwbuf+13*mwflt+1), dxmax, mwflt)
            call ucopy(q(ltwbuf+14*mwflt+1), dymax, mwflt)
            call ucopy(q(ltwbuf+15*mwflt+1), sigdx, mwflt)
            call ucopy(q(ltwbuf+16*mwflt+1), sigdy, mwflt)
          else
            deltas = q(ltwbuf+ 1)
            alfa   = q(ltwbuf+ 2)
            gamtr  = q(ltwbuf+ 3)
            qx     = q(ltwbuf+ 4)
            qy     = q(ltwbuf+ 5)
            xix    = q(ltwbuf+ 6)
            xiy    = q(ltwbuf+ 7)
            sigxco = q(ltwbuf+ 8)
            sigyco = q(ltwbuf+ 9)
            xcomax = q(ltwbuf+10)
            ycomax = q(ltwbuf+11)
            bxmax  = q(ltwbuf+12)
            bymax  = q(ltwbuf+13)
            dxmax  = q(ltwbuf+14)
            dymax  = q(ltwbuf+15)
            sigdx  = q(ltwbuf+16)
            sigdy  = q(ltwbuf+17)
          endif
 
*---- Close table file.
        else if (iflag .eq. 4) then
          call tbpdsc(ltwsum, 'CIRCUM', mreal, 0, circ, ' ')
          call tbclos(ltwsum)
          msg(1) = 'Tunes saved in table: ' // sumnam
          call aainfo('TWSMSV', 1, msg)
        endif
      endif
 
      end
