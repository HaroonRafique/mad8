      subroutine tmoct(fsec, ftrk, fract, orbit, fmap, el, ek, re, te)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TRANSPORT map for octupole element.                                *
* Input:                                                               *
*   FSEC      (logical) If true, return second order terms.            *
*   FTRK      (logical) If true, track orbit.                          *
*   FRACT     (real)    Fraction of length to be used.                 *
* Input/output:                                                        *
*   ORBIT(6)  (real)    Closed orbit.                                  *
* Output:                                                              *
*   FMAP      (logical) If true, element has a map.                    *
*   EL        (real)    Element length.                                *
*   EK(6)     (real)    Kick due to element.                           *
*   RE(6,6)   (real)    Transfer matrix.                               *
*   TE(6,6,6) (real)    Second-order terms.                            *
* Important common data:                                               *
*   LCELM     /REFER/   Current element bank.                          *
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
      double precision ci,cr,dl,dsk3,ek,el,four,fract,octi,octr,one,
     +orbit,posi,posr,pt,re,rfac,rw,six,sk3,sk3l,te,three,tilt,tilt4,tw,
     +two
      logical           fsec, ftrk, fmap
      dimension         orbit(6), ek(6), re(6,6), te(6,6,6)
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
      integer meangb,meangg,meangr,mechg,mee1b,mee1g,mee2b,mee2g,meflde,
     +mefrqc,megapb,megapg,meh1b,meh1g,meh2b,meh2g,mehrmc,meintb,meintg,
     +mek1b,mek1g,mek1q,mek2b,mek2s,mek3b,mek3o,mekick,meklm,meksg,
     +mekss,melagc,melen,mesigx,mesigy,metltb,metlte,metltg,metltm,
     +metlto,metltq,metlts,metyp,mevltc,mexcol,mexma,meycol,meyma
      integer meintbx,meintgx,meapro,mek0lm,met0m,mek1lm,met1m,
     +mek2lm,met2m,mek3lm,met3m,meaprm,meapss,melosc,meaprc,mee0l,
     +medel,mephil,mefrql,melosl,mevoll,melagl,meaprl
 
*---- Bias for element attribute values.
*     These statements MUST be consistent with the command dictionary.
*     Routines using this group must also include BANKHEAD and CMDGROUP.
*     Common to all elements: TYPE and L attributes.
      parameter    (metyp  = mbat   + mcval, melen  = metyp  + mcsiz)
*     Common to RBEND and SBEND.
      parameter    (meangb = melen  + mcsiz, mek1b  = meangb + mcsiz,
     +              mee1b  = mek1b  + mcsiz, mee2b  = mee1b  + mcsiz,
     +              metltb = mee2b  + mcsiz, mek2b  = metltb + mcsiz,
     +              meh1b  = mek2b  + mcsiz, meh2b  = meh1b  + mcsiz,
     +              megapb = meh2b  + mcsiz, meintb = megapb + mcsiz)
      parameter (meintbx = meintb + mcsiz, mek3b  = meintbx + mcsiz)
*     QUADRUPO.
      parameter    (mek1q  = melen  + mcsiz, metltq = mek1q  + mcsiz)
      integer meaprq
      parameter    (meaprq = metltq + mcsiz)
*     SEXTUPOL.
      parameter    (mek2s  = melen  + mcsiz, metlts = mek2s  + mcsiz)
      integer meaprs
      parameter    (meaprs = metlts + mcsiz)
*     OCTUPOLE.
      parameter    (mek3o  = melen  + mcsiz, metlto = mek3o  + mcsiz)
      parameter    (meapro = metlto + mcsiz)
*     MULTIPOL.
      parameter    (mek0lm = melen  + mcsiz, met0m  = mek0lm + mcsiz,
     +              mek1lm = met0m  + mcsiz, met1m  = mek1lm + mcsiz,
     +              mek2lm = met1m  + mcsiz, met2m  = mek2lm + mcsiz,
     +              mek3lm = met2m  + mcsiz, met3m  = mek3lm + mcsiz,
     +              meaprm = melen  + 21*mcsiz)
*     MULTIPOL.
      parameter    (meklm  = melen  + mcsiz, metltm = meklm  + mcsiz)
*     SOLENOID.
      parameter    (mekss  = melen  + mcsiz, meapss = mekss  + mcsiz)
*     RFCAVITY.
      parameter    (mevltc = melen  + mcsiz, melagc = mevltc + mcsiz,
     +              mefrqc = melagc + mcsiz, mehrmc = mefrqc + mcsiz)
      parameter    (melosc = mehrmc + 5*mcsiz,
     +              meaprc = melosc + 3*mcsiz)
*     ELSEPARA.
      parameter    (meflde = melen  + mcsiz, metlte = meflde + mcsiz)
*     Common to SROT and YROT.
      parameter    (meangr = melen  + mcsiz)
*     Common to KICK, HKICK, and VKICK.
      parameter    (mekick = melen  + mcsiz)
*     Common to ECOLLIMA and RCOLLIMA.
      parameter    (mexcol = melen  + mcsiz, meycol = mexcol + mcsiz)
*     BEAMBEAM.
      parameter    (mesigx = melen  + mcsiz, mesigy = mesigx + mcsiz,
     +              mexma  = mesigy + mcsiz, meyma  = mexma  + mcsiz,
     +              mechg  = meyma  + mcsiz)
*     GBEND.
      parameter    (meangg = melen  + mcsiz, mek1g  = meangg + mcsiz,
     +              mee1g  = mek1g  + mcsiz, mee2g  = mee1g  + mcsiz,
     +              metltg = mee2g  + mcsiz, meksg  = metltg + mcsiz,
     +              meh1g  = meksg  + mcsiz, meh2g  = meh1g  + mcsiz,
     +              megapg = meh2g  + mcsiz, meintg = megapg + mcsiz)
*     lcavity.
      parameter    (mee0l  = melen  + mcsiz, medel  = mee0l  + mcsiz,
     +              mephil = medel  + mcsiz, mefrql = mephil + mcsiz,
     +              melosl = mefrql + mcsiz, mevoll = melosl + mcsiz,
     +              melagl = mevoll + mcsiz, meaprl = melagl + mcsiz)
      parameter (meintgx = meintg + mcsiz)
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
      integer maxcpf,maxdof
 
*---- Status flags for TRANSPORT map module.
*     Set to consider everything before each executable command.
      parameter         (maxcpf = 10, maxdof = 10)
      common /stflag/   cpflag(maxcpf), doflag(maxdof)
      logical           cpflag, cplxy, cplxt
      logical           doflag, docav, dorad, doali, dofld, dokick
      logical           dodamp, dorand
      save              /stflag/
      equivalence       (cplxy,  cpflag( 1)), (cplxt,  cpflag( 2))
      equivalence       (docav,  doflag( 1)), (dorad,  doflag( 2))
      equivalence       (doali,  doflag( 3)), (dofld,  doflag( 4))
      equivalence       (dokick, doflag( 5)), (dodamp, doflag( 6))
      equivalence       (dorand, doflag( 7))
 
      dimension         rw(6,6), tw(6,6,6)
 
      parameter         (one    = 1.0d0)
      parameter         (two    = 2.0d0, three  = 3.0d0)
      parameter         (four   = 4.0d0, six    = 6.0d0)
 
*---- Initialize.
      call uzero(ek, 1, 6*mwflt)
      call m66one(re)
      if (fsec) call uzero(te, 1, 216*mwflt)
 
*---- Fetch data from pool.
      call ucopy(q(lcelm+melen), el, mwflt)
      dl = el * fract
      fmap = dl .ne. 0.0
      if (.not. fmap) return
 
*---- Set up half octupole strength.
      if (ftrk) then
        call ucopy(q(lcelm+mek3o), sk3, mwflt)
        call ucopy(q(lcelm+metlts), tilt, mwflt)
 
*---- Field error.
        if (lcfld .ne. 0) then
          call ucopy(q(lcfld+6*mwflt+1), dsk3, mwflt)
          sk3l = (dl * sk3 + fract * dsk3) / (one + deltas)
        else
          sk3l = (dl * sk3) / (one + deltas)
        endif
*--- apply inversion and scaling
        sk3l = sk3l * elkfact(7)
*---- Normal and skew components of octupole.
        tilt4 = four * tilt
        octr = sk3l * cos(tilt4)
        octi = sk3l * sin(tilt4)
 
*---- Half kick at entrance.
        posr = orbit(1) * (orbit(1)**2 - three*orbit(3)**2) / six
        posi = orbit(3) * (three*orbit(1)**2 - orbit(3)**2) / six
        cr = octr * posr - octi * posi
        ci = octr * posi + octi * posr
        orbit(2) = orbit(2) - cr / two
        orbit(4) = orbit(4) + ci / two
 
*---- Half radiation effects at entrance.
        if (dorad) then
          rfac = arad * gammas**3 * (cr**2 + ci**2) / (three * dl)
          pt = orbit(6)
          orbit(2) = orbit(2) - rfac * (one + pt) * orbit(2)
          orbit(4) = orbit(4) - rfac * (one + pt) * orbit(4)
          orbit(6) = orbit(6) - rfac * (one + pt) ** 2
        endif
 
*---- First-order terms w.r.t. orbit.
        call m66one(rw)
        posr = (orbit(1)**2 - orbit(3)**2) / four
        posi = orbit(1) * orbit(3) / two
        cr = octr * posr - octi * posi
        ci = octr * posi + octi * posr
        rw(2,1) = - cr
        rw(2,3) = + ci
        rw(4,1) = + ci
        rw(4,3) = + cr
        if (ci .ne. 0.0) cplxy = .true.
 
*---- Second-order terms w.r.t. orbit.
        if (fsec) then
          call uzero(tw, 1, 216*mwflt)
          cr = (octr * orbit(1) - octi * orbit(3)) / four
          ci = (octr * orbit(3) + octi * orbit(1)) / four
          tw(2,1,1) = - cr
          tw(2,1,3) = + ci
          tw(2,3,1) = + ci
          tw(2,3,3) = + cr
          tw(4,1,1) = + ci
          tw(4,1,3) = + cr
          tw(4,3,1) = + cr
          tw(4,3,3) = - ci
        endif
 
*---- Concatenate with drift map.
        call tmdrf(fsec, ftrk, fract, orbit, fmap, el, ek, re, te)
        call tmcat(fsec, re, te, rw, tw, re, te)
 
*---- Half kick at exit.
        posr = orbit(1) * (orbit(1)**2 - three*orbit(3)**2) / six
        posi = orbit(3) * (three*orbit(1)**2 - orbit(3)**2) / six
        cr = octr * posr - octi * posi
        ci = octr * posi + octi * posr
        orbit(2) = orbit(2) - cr / two
        orbit(4) = orbit(4) + ci / two
 
*---- Half radiation effects.
        if (dorad) then
          rfac = arad * gammas**3 * (cr**2 + ci**2) / (three * dl)
          pt = orbit(6)
          orbit(2) = orbit(2) - rfac * (one + pt) * orbit(2)
          orbit(4) = orbit(4) - rfac * (one + pt) * orbit(4)
          orbit(6) = orbit(6) - rfac * (one + pt) ** 2
        endif
 
*---- First-order terms w.r.t. orbit.
        call m66one(rw)
        posr = (orbit(1)**2 - orbit(3)**2) / four
        posi = orbit(1) * orbit(3) / two
        cr = octr * posr - octi * posi
        ci = octr * posi + octi * posr
        rw(2,1) = - cr
        rw(2,3) = + ci
        rw(4,1) = + ci
        rw(4,3) = + cr
        if (ci .ne. 0.0) cplxy = .true.
 
*---- Second-order terms w.r.t. orbit.
        if (fsec) then
          call uzero(tw, 1, 216*mwflt)
          cr = (octr * orbit(1) - octi * orbit(3)) / four
          ci = (octr * orbit(3) + octi * orbit(1)) / four
          tw(2,1,1) = - cr
          tw(2,1,3) = + ci
          tw(2,3,1) = + ci
          tw(2,3,3) = + cr
          tw(4,1,1) = + ci
          tw(4,1,3) = + cr
          tw(4,3,1) = + cr
          tw(4,3,3) = - ci
        endif
        call tmcat(fsec, rw, tw, re, te, re, te)
 
*---- Not orbit track requested, use drift map.
      else
        call tmdrf(fsec, ftrk, fract, orbit, fmap, el, ek, re, te)
      endif
 
      end
