      subroutine twbtsv(iflag, ipos)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save Twiss parameters for plotting etc.                            *
* Input:                                                               *
*   IFLAG     (integer) Operation desired:                             *
*                       1: Create new table.                           *
*                       2: Save one line.                              *
*                       3: Retrieve one line.                          *
*                       4: Close table.                                *
*   IPOS      (integer) Current table row number.                      *
*----------------------------------------------------------------------*
* Modified: 28-DEC-1998, T. Raubenheimer (SLAC)                        *
*   Changed: MAXFUN = 26 --> MAXFUN = 27, MAXCPL = 30 --> MAXCPL = 31; *
*   added energy to output table                                       *
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
      integer msali,msbn,mscom,mscor,msdir,mselm,msf1,msf2,msfld,msflg,
     +mslie,mslnk,msmap,msmon,msnum,msr1,msr2,msref,msrn,mss,msspl,msup,
     +msym
 
*---- Bias for sequence description banks.
      parameter         (msf1 = 1, msr1 = 2, msr2 = 3, msym = 4,
     +                   msup = 5, msf2 = 6, msbn = 7,
     +                   msrn = msbn + mwnam, mss = msrn + 40 / mcwrd)
*     Links for sequence description banks.
      parameter         (msdir =  1, msflg =  2, msali =  3, msfld =  4,
     +                   msnum =  5, mscom =  6, msmap =  9, mslie = 10,
     +                   msspl = 11, mscor = 12, msmon = 13, mselm = 14)
      parameter         (mslnk = 11, msref = 14)
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer i,iflag,ipos,l,maxcpl,maxfun,nb,nc,nr,ns
      double precision dummy,twopi,utwopi
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0 * pi, utwopi = 1.0 / twopi)
 
      parameter         (maxfun = 27, maxcpl = 31)
      character*(mcnam) funcol(maxcpl)
      integer           icfrm(maxcpl)
      data funcol
     +  / 'DELTAP', 'S',
     +    'BETX',   'ALFX',   'MUX',    'BETY',   'ALFY',   'MUY',
     +    'X',      'PX',     'Y',      'PY',
     +    'DX',     'DPX',    'DY',     'DPY',
     +    'WX',     'PHIX',   'DMUX',   'WY',     'PHIY',   'DMUY',
     +    'DDX',    'DDPX',   'DDY',    'DDPY',   'ENER',
     +    'R(1,1)', 'R(2,1)', 'R(1,2)', 'R(2,2)'                   /
 
*---- Create new table for lattice functions.
*     Warning: L is local link. Be careful with Zebra calls.
      if (iflag .eq. 1) then
        lsnum = lq(lcseq-msnum)
        ns = ndelta
        nr = iq(lq(lcseq-msflg)-1)
        nc = maxfun
        if (couple) nc = maxcpl
        nb = 1
        icfrm(1) = 3
        if (double) icfrm(1) = mreal
        do 10 i = 2, maxcpl
          icfrm(i) = icfrm(1)
   10   continue
        call tbcrea(funnam, ns, nr, nc, funcol, icfrm, nb, ltwfun)
        call tbpdsc(ltwfun, 'TYPE', 5, 0, dummy, 'TWISS')
        call mzbook(2, l, ltwfun, -1, 'BRNG', 0, 0, mss, 7, 0)
        call ucopy(q(lcseq+1), q(l+1), mss)
 
*---- Save one complete table line.
      else if (ltwfun .ne. 0) then
        if (iflag .eq. 2) then
          call tbset(ltwfun, ipos, 3, ltwbuf)
          if (double) then
            call ucopy(deltas, q(ltwbuf+1), mwflt)
            call ucopy(suml, q(ltwbuf+mwflt+1), mwflt)
            call ucopy(betx, q(ltwbuf+2*mwflt+1), mwflt)
            call ucopy(alfx, q(ltwbuf+3*mwflt+1), mwflt)
            call ucopy(amux * utwopi, q(ltwbuf+4*mwflt+1), mwflt)
            call ucopy(bety, q(ltwbuf+5*mwflt+1), mwflt)
            call ucopy(alfy, q(ltwbuf+6*mwflt+1), mwflt)
            call ucopy(amuy * utwopi, q(ltwbuf+7*mwflt+1), mwflt)
            call ucopy(orbit, q(ltwbuf+8*mwflt+1), 4*mwflt)
            call ucopy(disp, q(ltwbuf+12*mwflt+1), 4*mwflt)
            call ucopy(wx, q(ltwbuf+16*mwflt+1), mwflt)
            call ucopy(phix * utwopi, q(ltwbuf+17*mwflt+1), mwflt)
            call ucopy(dmux * utwopi, q(ltwbuf+18*mwflt+1), mwflt)
            call ucopy(wy, q(ltwbuf+19*mwflt+1), mwflt)
            call ucopy(phiy * utwopi, q(ltwbuf+20*mwflt+1), mwflt)
            call ucopy(dmuy * utwopi, q(ltwbuf+21*mwflt+1), mwflt)
            call ucopy(ddisp, q(ltwbuf+22*mwflt+1), 4*mwflt)
            call ucopy(ener1, q(ltwbuf+26*mwflt+1), mwflt)
            if (couple) then
              call ucopy(rmat, q(ltwbuf+27*mwflt+1), 4*mwflt)
            endif
          else
            q(ltwbuf+ 1) = deltas
            q(ltwbuf+ 2) = suml
            q(ltwbuf+ 3) = betx
            q(ltwbuf+ 4) = alfx
            q(ltwbuf+ 5) = amux * utwopi
            q(ltwbuf+ 6) = bety
            q(ltwbuf+ 7) = alfy
            q(ltwbuf+ 8) = amuy * utwopi
            q(ltwbuf+ 9) = orbit(1)
            q(ltwbuf+10) = orbit(2)
            q(ltwbuf+11) = orbit(3)
            q(ltwbuf+12) = orbit(4)
            q(ltwbuf+13) = disp(1)
            q(ltwbuf+14) = disp(2)
            q(ltwbuf+15) = disp(3)
            q(ltwbuf+16) = disp(4)
            q(ltwbuf+17) = wx
            q(ltwbuf+18) = phix * utwopi
            q(ltwbuf+19) = dmux * utwopi
            q(ltwbuf+20) = wy
            q(ltwbuf+21) = phiy * utwopi
            q(ltwbuf+22) = dmuy * utwopi
            q(ltwbuf+23) = ddisp(1)
            q(ltwbuf+24) = ddisp(2)
            q(ltwbuf+25) = ddisp(3)
            q(ltwbuf+26) = ddisp(4)
            q(ltwbuf+27) = ener1
            if (couple) then
              q(ltwbuf+28) = rmat(1,1)
              q(ltwbuf+29) = rmat(2,1)
              q(ltwbuf+30) = rmat(1,2)
              q(ltwbuf+31) = rmat(2,2)
            endif
          endif
 
*---- Load one complete table line.
        else if (iflag .eq. 3) then
          call tbset(ltwfun, ipos, 1, ltwbuf)
          if (double) then
            call ucopy(q(ltwbuf+1), deltas, mwflt)
            call ucopy(q(ltwbuf+mwflt+1), suml, mwflt)
            call ucopy(q(ltwbuf+2*mwflt+1), betx, mwflt)
            call ucopy(q(ltwbuf+3*mwflt+1), alfx, mwflt)
            call ucopy(q(ltwbuf+4*mwflt+1), amux, mwflt)
            call ucopy(q(ltwbuf+5*mwflt+1), bety, mwflt)
            call ucopy(q(ltwbuf+6*mwflt+1), alfy, mwflt)
            call ucopy(q(ltwbuf+7*mwflt+1), amuy, mwflt)
            call ucopy(q(ltwbuf+8*mwflt+1), orbit(1), 4*mwflt)
            call ucopy(q(ltwbuf+12*mwflt+1), disp(1), 4*mwflt)
            call ucopy(q(ltwbuf+16*mwflt+1), wx, mwflt)
            call ucopy(q(ltwbuf+17*mwflt+1), phix, mwflt)
            call ucopy(q(ltwbuf+18*mwflt+1), dmux, mwflt)
            call ucopy(q(ltwbuf+19*mwflt+1), wy, mwflt)
            call ucopy(q(ltwbuf+20*mwflt+1), phiy, mwflt)
            call ucopy(q(ltwbuf+21*mwflt+1), dmuy, mwflt)
            call ucopy(q(ltwbuf+22*mwflt+1), ddisp(1), 4*mwflt)
            call ucopy(q(ltwbuf+26*mwflt+1), ener1, mwflt)
            amux = amux * twopi
            amuy = amuy * twopi
            phix = phix * twopi
            dmux = dmux * twopi
            phiy = phiy * twopi
            dmuy = dmuy * twopi
            if (couple) then
              call ucopy(q(ltwbuf+27*mwflt+1), rmat(1,1), 4*mwflt)
            endif
          else
            deltas   = q(ltwbuf+ 1)
            suml     = q(ltwbuf+ 2)
            betx     = q(ltwbuf+ 3)
            alfx     = q(ltwbuf+ 4)
            amux     = q(ltwbuf+ 5) * twopi
            bety     = q(ltwbuf+ 6)
            alfy     = q(ltwbuf+ 7)
            amuy     = q(ltwbuf+ 8) * twopi
            orbit(1) = q(ltwbuf+ 9)
            orbit(2) = q(ltwbuf+10)
            orbit(3) = q(ltwbuf+11)
            orbit(4) = q(ltwbuf+12)
            disp(1)  = q(ltwbuf+13)
            disp(2)  = q(ltwbuf+14)
            disp(3)  = q(ltwbuf+15)
            disp(4)  = q(ltwbuf+16)
            wx       = q(ltwbuf+17)
            phix     = q(ltwbuf+18) * twopi
            dmux     = q(ltwbuf+19) * twopi
            wy       = q(ltwbuf+20)
            phiy     = q(ltwbuf+21) * twopi
            dmuy     = q(ltwbuf+22) * twopi
            ddisp(1) = q(ltwbuf+23)
            ddisp(2) = q(ltwbuf+24)
            ddisp(3) = q(ltwbuf+25)
            ddisp(4) = q(ltwbuf+26)
            ener1 = q(ltwbuf+27)
            if (couple) then
              rmat(1,1) = q(ltwbuf+28)
              rmat(2,1) = q(ltwbuf+29)
              rmat(1,2) = q(ltwbuf+30)
              rmat(2,2) = q(ltwbuf+31)
            endif
          endif
 
*---- Close table file.
        else if (iflag .eq. 4) then
          call tbpdsc(ltwfun, 'CIRCUM', mreal, 0, circ, ' ')
          call tbclos(ltwfun)
          msg(1) = 'Lattice functions saved in table: ' // funnam
          call aainfo('TWBTSV', 1, msg)
        endif
      endif
 
      end
