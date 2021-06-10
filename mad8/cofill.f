      subroutine cofill
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Fill in orbit and dispersion for monitor positions.                *
*   Assumes closed orbit to be known beforehand.                       *
* Important common data:                                               *
*             /MAPELM/  Element transfer map.                          *
*             /MAPTRN/  One turn transfer map.                         *
*             /OPTIC0/  Initial values.                                *
*             /OPTIC1/  Current values.                                *
*   LCELM     /REFER/   Current element bank.                          *
*   LCALI     /REFER/   Current misalignment pointer.                  *
*   LCFLD     /REFER/   Current field error pointer.                   *
*   LCSEQ     /REFER/   Current beam line sequence bank.               *
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
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
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
      integer ncor,nmon
      double precision akl,amuxcm,amuycm,betxcm,betycm,dxcm,dycm,halfqx,
     +halfqy,qual,scm,weight,xcm,ycm
 
*---- Data for current corrector or monitor.
*     Order of variables is important for UCOPY calls.
      common /codata/   xcm, ycm, dxcm, dycm, scm, betxcm, betycm,
     +                  amuxcm, amuycm, akl, halfqx, halfqy,
     +                  qual, weight(2), ncor(2), nmon(2)
      save              /codata/
      double precision ek,re,te
 
*---- Transfer map for current element.
      common /mapelm/   ek(6), re(6,6), te(6,6,6)
      save              /mapelm/
      double precision rt,rtp,tt
 
*---- Transfer map for complete turn.
      common /maptrn/   rt(6,6), tt(6,6,6), rtp(6,6)
      save              /maptrn/
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
      integer icode,ienum,iflag,iocc,ipos,isp,jbyt
      double precision el,fact,ten
 
      parameter         (ten = 10.0d0)
 
      logical           fmap
      character*(mcnam) elmnam
 
*---- Initial orbit and dispersion.
      if (lcseq .eq. 0) go to 9999
      call ucopy(orbit0, orbit, 6*mwflt)
      call twdisp(rt, rt(1,6), disp)
      disp(5) = 0.0
      disp(6) = 1.0
      if (error) go to 9999
      suml = 0.0
 
*---- Loop through element sequence.
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
      do 90 ipos = irg1, irg2
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
        icode = jbyt(iflag,1,mcode)
 
*---- Physical element.
        if (icode .eq. 1) then
 
*---- Misalignent at entrance.
          if (lcali .ne. 0) then
            call tmali1(ipos, .false., orbit, orbit, re, te)
            call m66byv(re, disp, disp)
          endif
 
*---- Fill in values at beam position monitors.
          isp = iq(lcelm+mbsp)
          if (isp .ge. 17  .and. isp .le. 19) then
            call ucopy(q(lcelm+melen), el, mwflt)
            if (lccom .ne. 0) then
 
*---- Read errors.
              if (lcali .ne. 0) then
                call ucopy(q(lcali+6*mwflt+1), xcm, 4*mwflt)
              else
                xcm = 0.0
                ycm = 0.0
                dxcm = 0.0
                dycm = 0.0
              endif
 
*---- Actual position w.r.t. monitor centre plus read error.
*     Suppress if monitor reads only one plane.
              fact = (el / 2.0) * (1.0 - orbit(6) / betas)
              if (isp .le. 18) then
                xcm = orbit(1) + fact * orbit(2) + xcm
                dxcm = disp(1) + fact * disp(2) + dxcm
              else
                xcm = 0.0
                dxcm = 0.0
              endif
              if (isp .ge. 18) then
                ycm = orbit(3) + fact * orbit(4) + ycm
                dycm = disp(3) + fact * disp(4) + dycm
              else
                ycm = 0.0
                dycm = 0.0
              endif
              call ucopy(xcm, q(lccom+1), 4*mwflt)
            endif
          endif
 
*---- Transfer through element.
          call tmmap(.true., .true., orbit, fmap, el, ek, re, te)
          if (fmap) call m66byv(re, disp, disp)
 
*---- Misalignment at exit.
          if (lcali .ne. 0) then
            call tmali2(ipos, .false., orbit, orbit, re, te)
            call m66byv(re, disp, disp)
          endif
 
*---- Entrance of line.
        else if (icode .eq. 2) then
          if (lcali .ne. 0) then
            call tmali1(ipos, .false., orbit, orbit, re, te)
            call m66byv(re, disp, disp)
          endif
 
*---- Exit of line.
        else
          if (lcali .ne. 0) then
            call tmali2(ipos, .false., orbit, orbit, re, te)
            call m66byv(re, disp, disp)
          endif
        endif
   90 continue
 
 9999 end
