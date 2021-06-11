      subroutine mtcond(fprt, nf, fvec, iflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Run through the matching conditions, compute (and print) values.   *
* Input:                                                               *
*   FPRT      (logical) Print flag.                                    *
*   NF        (integer) Number of matching functions.                  *
* Output:                                                              *
*   FVEC(NF)  (real)    Matching function values.                      *
*   IFLAG     (integer) Stability flag.                                *
*----------------------------------------------------------------------*
* Modified: 07-JAN-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 26 -> MAXVAL = 27; added ENERGY_VAL and           *
*   CHROM_VAL ... the latter is necessary to get the printing and      *
*   constraint summation correct; added ENER to DICT list; added       *
*   printing for energy constraint                                     *
* Modified: 04-MAR-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 27 -> MAXVAL = 28; added CIRC to DICT list        *
* Modified: 14-JUL-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 28 -> MAXVAL = 34; added /SYNCH/ common block;    *
*   reset BLEN to zero in tracking loop                                *
* Modified: 12-AUG-1999, T. Raubenheimer (SLAC)                        *
*   Changed MAXVAL = 34 -> MAXVAL = 36; added I5I2 and I5I1            *
*   constraints                                                        *
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
      integer i,icode,icon,ienum,iepos,iflag,iocc,ipos,j,jbyt,jcon,k,
     +kcon,kflag,last,nf,nline
      double precision el,fsum,fvec,rsum,tsum,utwopi,x,zero
      logical           fprt
      dimension         fvec(nf)
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
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
 
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
      integer maxlin,maxval,mconf1,mconf2,mconmn,mconmx,mcontp,mconvl,
     +mconwt
 
*---- Parameters for matching module.
      parameter         (maxlin = 16, maxval = 36)
      parameter         (mconf1 = 1, mcontp = 2, mconf2 = maxval + 2)
      parameter         (mconmn = mconf2 + 1)
      parameter         (mconmx = mconmn + maxval * mwflt)
      parameter         (mconvl = mconmx + maxval * mwflt)
      parameter         (mconwt = mconvl + maxval * mwflt)
      integer energy_val, chrom_val
      parameter         (energy_val = 27, chrom_val = 26)
      double precision ek,re,te
 
*---- Transfer map for current element.
      common /mapelm/   ek(6), re(6,6), te(6,6,6)
      save              /mapelm/
      double precision rt,rtp,tt
 
*---- Transfer map for complete turn.
      common /maptrn/   rt(6,6), tt(6,6,6), rtp(6,6)
      save              /maptrn/
      integer icall,icovar,ifirst,ilevel,imode,istrat,ncon,nfcn,nfcnmx,
     +nvar
      double precision edm,fmin,tol,up
 
*---- Communication area for routines derived from MINUIT.
      common /minchr/   crout, cstat
      common /mindbl/   edm, fmin, tol, up
      common /minint/   icall, icovar, ifirst, imode, ilevel, istrat,
     +                  ncon, nvar, nfcn, nfcnmx
      common /minflt/   time1, time2
      save              /minchr/, /mindbl/, /minint/, /minflt/
      character         crout*8, cstat*16
      real              time1, time2
      integer icc
      double precision cmax,cmin,cval,cwgt
 
*---- Working area for a single matching constraint.
      common /mtccon/   icc(maxval), cmin(maxval), cmax(maxval),
     +                  cwgt(maxval), cval(maxval)
      save   /mtccon/
 
*---- Flags for matching.
      common /mtcflg/   flbeta, florb, flrmat, fltmat, flchrm
      save              /mtcflg/
      logical           flbeta, florb, flrmat, fltmat, flchrm
      character *(mcnam)  sequd, betnm
      common / dmatchc / sequd(2), betnm(2)
      integer mtdbfl, imsequ
      common / dmatchi / mtdbfl, imsequ
      logical bdtflg
      common / dmatchl / bdtflg(2)
      integer lcon,lmcon,lmtbet,lmtlin,lmtseq,lmtsub,lmvar,lptr,lref,
     +lsmat,lvar,lbeta0
 
*---- Link area for matching.
      common /mtlink/   lsmat, lmcon, lmvar,
     +                  lmtlin, lmtseq, lmtbet, lbeta0(2), lmtsub,
     +                  lcon, lref, lvar, lptr
      save              /mtlink/
      double precision rtdes,rtsav,rtwgt,ttsav
 
*---- Working area for matrix constraints.
      common /mtcmtx/   rtsav(36), ttsav(216), rtdes(216), rtwgt(216)
      save   /mtcmtx/
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
*---- Synchrotron integrals, etc.
      common /synch/    synch_1, synch_2, synch_3, synch_4, synch_5,
     +                  blen, rhoinv
      double precision  synch_1, synch_2, synch_3, synch_4, synch_5,
     +                  blen, rhoinv
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      integer srange
      common / tmptmp/ srange(2,10)
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (utwopi = 1.0d0 / (2.0d0 * pi), zero = 0.0d0)
      character*4       dict(maxval)
      character*(mcnam) elmnam*(mcnam), title*(*)
      logical           fmap, fsec
      parameter         (title = 'Matching summary.')
      data dict
     +   / 'BETX', 'ALFX', 'MUX ', 'BETY', 'ALFY', 'MUY ',
     +     'X   ', 'PX  ', 'Y   ', 'PY  ', 'T',    'PT',
     +     'DX  ', 'DPX ', 'DY  ', 'DPY ',
     +     'WX  ', 'PHIX', 'DMUX', 'WY  ', 'PHIY', 'DMUY',
     +     'DDX ', 'DDPX', 'DDY ', 'DDPY', 'ENER', 'CIRC',
     +     'I1  ', 'I2  ', 'I3  ', 'I4  ', 'I5  ', 'I5I2',
     +     'I5I1', 'DUMM'  /
 
*--- total penalty value
      fsum = 0.0
*--- constraint counter
      icon = 0
*---- Initialize global user constraints.
      call mtufun(.false., 1, icon, fvec, fsum)
 
*--- loop over sequences
      do imsequ = 1, mtdbfl
      lmtbet = lbeta0(imsequ)
      if (sequd(imsequ) .ne. ' ' .and. sequd(imsequ) .ne. sequnam)
     +call get_active(sequd(imsequ), 'MTCOND')
*---- Initialize lattice functions
      fsec = florb .or. fltmat .or. flchrm
      call mtpini(iflag)
      if (iflag .ne. 0) go to 9999
*---- Initialize transfer map.
      if (flrmat .or. fltmat) then
        call m66one(rt)
        if (fltmat) call uzero(tt, 1, 216*mwflt)
      endif
 
*---- Initialize.
      iepos = 0
      suml = 0.0
 
*---- Print page header.
      if (fprt) then
        call prhead('ENDMATCH', title, deltas, 0, nline, 1)
        if (florb) then
          write (iqpr2, 800) 'ACTUAL'
        else
          write (iqpr2, 800) 'IDEAL'
        endif
        call prline(iqpr2)
        write (iqpr2, 810)
        call prline(iqpr2)
 
*---- Print begin of beam line.
        elmnam = ' '
        write (iqpr2, 820) iepos, elmnam, 0, suml,
     +                     'begin matching range'
        cval(1) = betx
        cval(2) = alfx
        cval(3) = amux * utwopi
        cval(4) = bety
        cval(5) = alfy
        cval(6) = amuy * utwopi
        call ucopy(orbit, cval(7), 6*mwflt)
        call ucopy(disp, cval(13), 4*mwflt)
        ener1 = ener0
        if (ener1 .eq. zero .or. .not. ereset) ener1 = en0
        cval(energy_val) = ener1
        cval(energy_val+1) = suml
        cval(energy_val+2) = synch_1
        cval(energy_val+3) = synch_2
        cval(energy_val+4) = synch_3
        cval(energy_val+5) = synch_4
        cval(energy_val+6) = synch_5
        cval(energy_val+7) = synch_5/max(synch_2,1.d-8)
        cval(energy_val+8) = synch_5/max(synch_1,1.d-8)
        cval(energy_val+9) = 0
        if (flchrm) then
          last = chrom_val
          cval(17) = wx
          cval(18) = phix
          cval(19) = dmux * utwopi
          cval(20) = wy
          cval(21) = phiy
          cval(22) = dmuy * utwopi
          call ucopy(ddisp, cval(23), 4*mwflt)
        else
          last = maxlin
        endif
 
*---- Print orbit only if computed.
        do 10 jcon = 1, last
          if (florb  .or.  jcon .lt. 7  .or.  jcon .gt. 12) then
            write (iqpr2, 830) dict(jcon), cval(jcon)
          endif
   10   continue
        jcon = energy_val
        write (iqpr2, 830) dict(jcon), cval(jcon)
        write (iqpr2, 830) dict(jcon+1), cval(jcon+1)
        write (iqpr2, 830) dict(jcon+2), cval(jcon+2)
        write (iqpr2, 830) dict(jcon+3), cval(jcon+3)
        write (iqpr2, 830) dict(jcon+4), cval(jcon+4)
        write (iqpr2, 830) dict(jcon+5), cval(jcon+5)
        write (iqpr2, 830) dict(jcon+6), cval(jcon+6)
        write (iqpr2, 830) dict(jcon+7), cval(jcon+7)
        write (iqpr2, 830) dict(jcon+8), cval(jcon+8)
        write (iqpr2, 830) dict(jcon+9), cval(jcon+9)
      endif
 
*---- Tracking loop.
      irg1 = srange(1,imsequ)
      irg2 = srange(2,imsequ)
      do 100 ipos = irg1, irg2
        call utelem(lcseq, ipos, kflag, elmnam, iocc, ienum)
        icode = jbyt(kflag,1,mcode)
 
*---- Tor: set BLEN to zero ... it may get reset in TMMAP
        blen = zero
 
*---- Misalignment at entrance.
        if (icode .ne. 3  .and.  florb  .and.  lcali .ne. 0) then
          call tmali1(ipos, fsec, orbit, orbit, re, te)
          if (flchrm) then
            call twbttk(.true., .false.)
          else
            call mtbttk(.false.)
          endif
          if (flrmat .or. fltmat) then
            call tmcat(fltmat, re, te, rt, tt, rt, tt)
          endif
        endif
 
*---- Advance through element.
        if (icode .eq. 1) then
          call tmmap(fsec, florb, orbit, fmap, el, ek, re, te)
          if (fmap) then
            if (flchrm) then
              call twbttk(.true., .true.)
            else
              call mtbttk(.true.)
            endif
            suml = suml + el
            if (flrmat .or. fltmat) then
              call tmcat(fltmat, re, te, rt, tt, rt, tt)
            endif
          endif
        endif
 
*---- Misalignment at exit.
        if (icode .ne. 2  .and.  florb  .and.  lcali .ne. 0) then
          call tmali2(ipos, fsec, orbit, orbit, re, te)
          if (flchrm) then
            call twbttk(.true., .false.)
          else
            call mtbttk(.false.)
          endif
          if (flrmat .or. fltmat) then
            call tmcat(fltmat, re, te, rt, tt, rt, tt)
          endif
        endif
 
*---- Return (and print) user constraints for current position.
        if (icode .eq. 1) call mtufun(fprt, 2, icon, fvec, fsum)
 
*---- Test for constraints in this position.
        lptr = lq(lsmat-imsequ)
        if (lptr .ne. 0) lptr = lq(lptr-ipos)
   20   if (lptr .ne. 0) then
          lcon = lq(lptr-1)
          kcon = iq(lptr+1)
 
*---- Coupling constraint (beginning).
          if (kcon .eq. 2) then
            call ucopy(q(lcon+mconmn), cmin, maxval*mwflt)
            call ucopy(q(lcon+mconvl), cval, maxval*mwflt)
            call ucopy(q(lcon+mconwt), cwgt, maxval*mwflt)
            cmin(1) = betx
            cmin(2) = alfx
            cval(3) = amux * utwopi
            cmin(4) = bety
            cmin(5) = alfy
            cval(6) = amuy * utwopi
            call ucopy(orbit, cmin(7), 6*mwflt)
            call ucopy(disp, cmin(13), 4*mwflt)
            cval(energy_val) = ener1
            cval(energy_val+1) = suml
            cval(energy_val+2) = synch_1
            cval(energy_val+3) = synch_2
            cval(energy_val+4) = synch_3
            cval(energy_val+5) = synch_4
            cval(energy_val+6) = synch_5
            cval(energy_val+7) = synch_5/max(synch_2,1.d-8)
            cval(energy_val+8) = synch_5/max(synch_1,1.d-8)
            cval(energy_val+9) = 0
            if (flchrm) then
              last = chrom_val
              cval(17) = wx
              cval(18) = phix
              cval(19) = dmux * utwopi
              cval(20) = wy
              cval(21) = phiy
              cval(22) = dmuy * utwopi
              call ucopy(ddisp, cval(23), 4*mwflt)
            else
              last = maxlin
            endif
            call ucopy(cmin, q(lcon+mconmn), maxval*mwflt)
            call ucopy(cval, q(lcon+mconvl), maxval*mwflt)
            if (fprt) then
              write (iqpr2, 820) ienum, elmnam, iocc, suml,
     +                           'begin COUPLE range'
              do 30 jcon = 1, last
                if (cwgt(jcon) .ne. 0.) then
                  if (jcon .eq. 3  .or.  jcon .eq. 6) then
                    write (iqpr2, 840) dict(jcon),cwgt(jcon),zero
                  else
                    write (iqpr2, 840) dict(jcon),cwgt(jcon),cmin(jcon)
                  endif
                endif
   30         continue
              jcon = energy_val
              write (iqpr2, 830) dict(jcon), cval(jcon)
              write (iqpr2, 830) dict(jcon+1), cval(jcon+1)
              write (iqpr2, 830) dict(jcon+2), cval(jcon+2)
              write (iqpr2, 830) dict(jcon+3), cval(jcon+3)
              write (iqpr2, 830) dict(jcon+4), cval(jcon+4)
              write (iqpr2, 830) dict(jcon+5), cval(jcon+5)
              write (iqpr2, 830) dict(jcon+6), cval(jcon+6)
              write (iqpr2, 830) dict(jcon+7), cval(jcon+7)
              write (iqpr2, 830) dict(jcon+8), cval(jcon+8)
              write (iqpr2, 830) dict(jcon+9), cval(jcon+9)
            endif
 
*---- CONSTRAINT or end of COUPLE.
          else if (kcon .eq. 1  .or.  kcon .eq. 3) then
            call ucopy(q(lcon+mcontp), icc, maxval)
            call ucopy(q(lcon+mconmn), cmin, maxval*mwflt)
            call ucopy(q(lcon+mconmx), cmax, maxval*mwflt)
            call ucopy(q(lcon+mconvl), cval, maxval*mwflt)
            call ucopy(q(lcon+mconwt), cwgt, maxval*mwflt)
            cval(1) = betx
            cval(2) = alfx
            cval(4) = bety
            cval(5) = alfy
            call ucopy(orbit, cval(7), 6*mwflt)
            call ucopy(disp, cval(13), 4*mwflt)
            cval(energy_val) = ener1
            cval(energy_val+1) = suml
            cval(energy_val+2) = synch_1
            cval(energy_val+3) = synch_2
            cval(energy_val+4) = synch_3
            cval(energy_val+5) = synch_4
            cval(energy_val+6) = synch_5
            cval(energy_val+7) = synch_5/max(synch_2,1.d-8)
            cval(energy_val+8) = synch_5/max(synch_1,1.d-8)
            cval(energy_val+9) = 0
            if (flchrm) then
              last = chrom_val
              cval(17) = wx
              cval(18) = phix
              cval(19) = dmux * utwopi
              cval(20) = wy
              cval(21) = phiy
              cval(22) = dmuy * utwopi
              call ucopy(ddisp, cval(23), 4*mwflt)
            else
              last = maxlin
            endif
            if (kcon .eq. 1) then
              cval(3) = amux * utwopi
              cval(6) = amuy * utwopi
              if (fprt) write (iqpr2, 820) ienum, elmnam, iocc, suml,
     +                                     'CONSTRAINT'
            else
              cval(3) = amux * utwopi - cval(3)
              cval(6) = amuy * utwopi - cval(6)
              if (fprt) write (iqpr2, 820) ienum, elmnam, iocc, suml,
     +                                     'end   COUPLE range'
            endif
 
*---- Contribution to penalty function.
            do 90 jcon = 1, last
              if (icc(jcon) .ne. 0) then
                icon = icon + 1
                if (icc(jcon) .eq. 1) then
                  fvec(icon) = cwgt(jcon) * dim(cmin(jcon),cval(jcon))
                  if (fprt) write (iqpr2, 880) dict(jcon), cwgt(jcon),
     +              cval(jcon), cmin(jcon), fvec(icon)**2
                else if (icc(jcon) .eq. 2) then
                  fvec(icon) = cwgt(jcon) * dim(cval(jcon),cmax(jcon))
                  if (fprt) write (iqpr2, 890) dict(jcon), cwgt(jcon),
     +              cval(jcon), cmax(jcon), fvec(icon)**2
                else if (icc(jcon) .eq. 3) then
                  fvec(icon) = cwgt(jcon) * dim(cmin(jcon),cval(jcon))
     +               +  cwgt(jcon) * dim(cval(jcon),cmax(jcon))
                  if (fprt) write (iqpr2, 840) dict(jcon), cwgt(jcon),
     +              cval(jcon), cmin(jcon), cmax(jcon), fvec(icon)**2
                else if (icc(jcon) .eq. 4) then
                  fvec(icon) = cwgt(jcon) * (cval(jcon) - cmin(jcon))
                  if (fprt) write (iqpr2, 840) dict(jcon), cwgt(jcon),
     +              cval(jcon), cmin(jcon), cmin(jcon), fvec(icon)**2
                endif
                fsum = fsum + fvec(icon)**2
              endif
   90       continue
 
*---- Tor: added to sum energy and circ
            do 92 jcon = energy_val, energy_val+9
              if (icc(jcon) .ne. 0) then
                icon = icon + 1
                if (icc(jcon) .eq. 1) then
                  fvec(icon) = cwgt(jcon) * dim(cmin(jcon),cval(jcon))
                  if (fprt) write (iqpr2, 880) dict(jcon), cwgt(jcon),
     +                cval(jcon), cmin(jcon), fvec(icon)**2
                else if (icc(jcon) .eq. 2) then
                  fvec(icon) = cwgt(jcon) * dim(cval(jcon),cmax(jcon))
                  if (fprt) write (iqpr2, 890) dict(jcon), cwgt(jcon),
     +                cval(jcon), cmax(jcon), fvec(icon)**2
                else if (icc(jcon) .eq. 3) then
                  fvec(icon) = cwgt(jcon) * dim(cmin(jcon),cval(jcon))
     +                 +  cwgt(jcon) * dim(cval(jcon),cmax(jcon))
                  if (fprt) write (iqpr2, 840) dict(jcon), cwgt(jcon),
     +                cval(jcon), cmin(jcon), cmax(jcon), fvec(icon)**2
                else if (icc(jcon) .eq. 4) then
                  fvec(icon) = cwgt(jcon) * (cval(jcon) - cmin(jcon))
                  if (fprt) write (iqpr2, 840) dict(jcon), cwgt(jcon),
     +                cval(jcon), cmin(jcon), cmin(jcon), fvec(icon)**2
                endif
                fsum = fsum + fvec(icon)**2
              endif
   92       continue
*---- End of Tor
 
*---- RMATRIX or TMATRIX (beginning of range):
*     Save inverted accumulated map.
          else if (kcon .eq. 4) then
            call m66inv(rt, rtsav)
            call ucopy(rtsav, q(lcon+1), 36*mwflt)
            if (fprt) write (iqpr2, 820)
     +        ienum, elmnam, iocc, suml, 'begin RMATRIX range'
          else if (kcon .eq. 6) then
            call tminv(rt, tt, rtsav, ttsav)
            call ucopy(rtsav, q(lcon+1), 36*mwflt)
            call ucopy(ttsav, q(lcon+36*mwflt+1), 216*mwflt)
            if (fprt) write (iqpr2, 820)
     +        ienum, elmnam, iocc, suml, 'begin TMATRIX range'
 
*---- RMATRIX (end of range).
          else if (kcon .eq. 5) then
            call ucopy(q(lcon+1), rtsav, 36*mwflt)
            call m66mpy(rt, rtsav, rtsav)
            call ucopy(q(lcon+36*mwflt+1), rtdes, 36*mwflt)
            call ucopy(q(lcon+(36+36)*mwflt+1), rtwgt, 36*mwflt)
            rsum = 0.0
            do 70 k = 1, 36
              if (rtwgt(k) .ne. 0.0) then
                icon = icon + 1
                fvec(icon) = rtwgt(k) * (rtsav(k) - rtdes(k))
                rsum = rsum + fvec(icon)**2
              endif
   70       continue
            fsum = fsum + rsum
            if (fprt) then
              write (iqpr2, 820) ienum, elmnam, iocc, suml,
     +                           'end   RMATRIX range'
              do 75 i = 1, 6
                write (iqpr2, 850) (rtsav(k), k = i, 36, 6)
   75         continue
              write (iqpr2, 870) rsum
            endif
 
*---- TMATRIX (end of range).
          else if (kcon .eq. 7) then
            call ucopy(q(lcon+1), rtsav, 36*mwflt)
            call ucopy(q(lcon+36*mwflt+1), ttsav, 216*mwflt)
            call tmcat(.true., rt, tt, rtsav, ttsav, rtsav, ttsav)
            call ucopy(q(lcon+(36+216)*mwflt+1), rtdes, 216*mwflt)
            call ucopy(q(lcon+(36+216+216)*mwflt+1), rtwgt, 216*mwflt)
            tsum = 0.0
            do 80 k = 1, 216
              if (rtwgt(k) .ne. 0.0) then
                icon = icon + 1
                fvec(icon) = rtwgt(k) * (ttsav(k) - rtdes(k))
                tsum = tsum + fvec(icon)**2
              endif
   80       continue
            fsum = fsum + tsum
            if (fprt) then
              write (iqpr2, 820)
     +        ienum, elmnam, iocc, suml, 'end   TMATRIX range'
              do 85 i = 1, 6
                write (iqpr2, 860) i,
     +            ((ttsav(k), k = j, 216, 36), j = i, 36, 6)
   85         continue
              write (iqpr2, 870) tsum
            endif
          endif
          lptr = lq(lptr)
          go to 20
        endif
  100 continue
 
*---- Return (and print) global user constraints.
      call mtufun(fprt, 3, icon, fvec, fsum)
*---- End of beam line.
      if (fprt) then
        write (iqpr2, 820) ienum, elmnam, iocc, suml,
     +                     'end   matching range'
        cval(1) = betx
        cval(2) = alfx
        cval(3) = amux * utwopi
        cval(4) = bety
        cval(5) = alfy
        cval(6) = amuy * utwopi
        call ucopy(orbit, cval(7), 6*mwflt)
        call ucopy(disp, cval(13), 4*mwflt)
        cval(energy_val) = ener1
        cval(energy_val+1) = suml
        cval(energy_val+2) = synch_1
        cval(energy_val+3) = synch_2
        cval(energy_val+4) = synch_3
        cval(energy_val+5) = synch_4
        cval(energy_val+6) = synch_5
        cval(energy_val+7) = synch_5/max(synch_2,1.d-8)
        cval(energy_val+8) = synch_5/max(synch_1,1.d-8)
        cval(energy_val+9) = 0
        if (flchrm) then
          last = chrom_val
          cval(17) = wx
          cval(18) = phix
          cval(19) = dmux * utwopi
          cval(20) = wy
          cval(21) = phiy
          cval(22) = dmuy * utwopi
          call ucopy(ddisp, cval(23), 4*mwflt)
        else
          last = maxlin
        endif
 
*---- Print orbit only if computed.
        do 110 jcon = 1, last
          if (florb  .or.  jcon .lt. 7  .or.  jcon .gt. 12) then
            write (iqpr2, 830) dict(jcon), cval(jcon)
          endif
  110   continue
        jcon = energy_val
        write (iqpr2, 830) dict(jcon), cval(jcon)
        write (iqpr2, 830) dict(jcon+1), cval(jcon+1)
        write (iqpr2, 830) dict(jcon+2), cval(jcon+2)
        write (iqpr2, 830) dict(jcon+3), cval(jcon+3)
        write (iqpr2, 830) dict(jcon+4), cval(jcon+4)
        write (iqpr2, 830) dict(jcon+5), cval(jcon+5)
        write (iqpr2, 830) dict(jcon+6), cval(jcon+6)
        write (iqpr2, 830) dict(jcon+7), cval(jcon+7)
        write (iqpr2, 830) dict(jcon+8), cval(jcon+8)
        write (iqpr2, 830) dict(jcon+9), cval(jcon+9)
      endif
*--- end of loop over sequences
      enddo
*---- Final value of penalty function.
      if (fprt)  then
        fmin = fsum
        write (iqpr2, 900) fmin
        if (ilevel .ge. 0) then
          crout = 'ENDMATCH'
          cstat = 'final values'
          call mtprnt(nvar, x)
        endif
        write (msg, 910) fsum
        call aainfo('MTCOND', 1, msg)
      endif
 
  800 format(' Matched with respect to the ',a,' orbit.')
  810 format(' Pos.  element occ.     dist.  condition    quantity',
     +       '   weight',10x,'actual',10x,'minimum',9x,
     +       'maximum',9x,'penalty'/
     +       ' no.   name    no.       m     type',9x,'name',23x,
     +       3('value',11x),'contribution')
  820 format(' ',i5,' ',a8,i4,f10.3,2x,a)
  830 format(t45,a4,3x,16x,1p,e16.6)
  840 format(t45,a4,3x,1p,5e16.6)
  850 format(2x,6f16.6)
  860 format(1x,i1,6e16.6/(2x,6e16.6))
  870 format(t45,'Contribution of this constraint:',t116,1p,e16.6)
  880 format(t45,a4,3x,1p,3e16.6,16x,e16.6)
  890 format(t45,a4,3x,1p,2e16.6,16x,2e16.6)
  900 format(' '/t45,'Total penalty function:',t116,1p,e16.6)
  910 format('Last value of the penalty function:',1p,e14.6)
 
 9999 end
