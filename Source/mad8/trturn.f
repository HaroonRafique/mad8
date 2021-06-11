      subroutine trturn(doelem, iorder, iturn, track, number, ktrack)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Track one complete turn in the machine (control routine).          *
* Input:                                                               *
*   DOELEM    (subr)    Routine to track through one element.          *
*   IORDER    (integer) Order for Lie algebraic tracking               *
*   ITURN     (integer) Turn number.                                   *
* Input/output:                                                        *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   NUMBER(*) (integer) Number of current track.                       *
*   KTRACK    (integer) number of surviving tracks.                    *
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
      integer i1,i2,iorder,ipos,isup,itrack,iturn,jbit,jbyt,ktrack
      double precision beti,bi2gi2,bihalf,eject,el,excurs,one,pt,px,
     +py,track,two,ttt
      external          doelem
      dimension         track(6,*)
      integer           number(*)
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
      integer bbd_max, bbd_cnt, bbd_pos, bbd_loc, bbd_flag
      parameter (bbd_max = 200)
      common / bbcommi / bbd_cnt, bbd_pos, bbd_flag, bbd_loc(bbd_max)
      double precision bb_kick
      common / bbcommr / bb_kick(2, bbd_max)
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
      integer mpcor,mpedi,mpelm,mpenv,mperr,mpfil,mphar,mpkey,mplie,
     +mplin,mpmat,mppar,mpplt,mppol,mpsrv,mpstr,mpsub,mpsur,mptrk,
     +mptws,mpdoom
 
*---- Standard process codes.
      parameter         (mpkey =  1, mppar =  2, mpstr =  3, mpelm =  5,
     +                   mplin =  6)
      parameter         (mpsub = 10)
      parameter         (mpsrv = 11, mpfil = 12, mpenv = 13, mpplt = 14,
     +                   mpsur = 15, mptws = 16, mpmat = 17, mptrk = 18,
     +                   mphar = 19, mperr = 20, mpcor = 21, mplie = 22,
     +                   mpedi = 23, mppol = 24, mpdoom = 25)
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
      integer iffreq,ipfreq,itrfil,npart,ntrack
 
*---- Common flags for tracking.
      common /trkchr/   trktitle
      character * 32    trktitle
      common /trkint/   ipfreq, iffreq, npart, ntrack, itrfil
      common /trktim/   time1, time2, dtime
      common /trkflg/   onepss, fdamp, frand, fdynap, fstart, fbelow,
     +                  oneshot
      save              /trkint/, /trktim/, /trkflg/
      real              time1, time2, dtime
      logical           onepss, fdamp, frand, fdynap, fstart, fbelow,
     +                  oneshot
      integer ltrbuf,ltrcur,ltrfbf,ltrnoi,ltrnum,ltrobs,ltrrbf,ltrstt,
     +ltrtab,ltrtmp
 
*---- Links used for tracking.
      common /trlink/   ltrnum, ltrstt, ltrnoi, ltrfbf, ltrrbf, ltrobs,
     +                  ltrcur, ltrtmp, ltrtab, ltrbuf
      save              /trlink/
      double precision aival,eigen,reval
 
*---- Initial conditions for optical functions for tracking.
      common /troptc/   eigen(6,6), reval(6), aival(6)
      save              /troptc/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      parameter         (one = 1.0d0, two = 2.0d0, eject = 1.0d0)
 
*---- Precompute a few constants.
      suml = 0.0
      beti = one / betas
      bi2gi2 = one / (betas * gammas)**2
      bihalf = one / (two * betas)
      lcali = 0
      lcfld = 0
      lccom = 0
*---- Loop over the superperiods.
      do 100 isup = 1, nsup
        i1 = irg1
        i2 = irg2
 
*---- Set up for first superperiod.
        if (isup .eq. 1  .and.  ltrobs .ne. 0) then
          ltrtmp = ltrobs
          i2 = iq(ltrtmp-5)
        else
          ltrtmp = 0
        endif
 
*---- Loop over track tables.
   10   continue
 
*---- Do first (or only) half of superperiod.
          do 40 ipos = i1, i2
            bbd_pos = ipos
            lcelm = lq(ldbnk(3)-iq(lsdir+ipos))
 
*---- Test for physical element.
            if (iq(lcelm+mbpr) .eq. mpelm) then
 
*---- Drift handled inline for speed.
              if (iq(lcelm+mbsp) .eq. 1) then
                call ucopy(q(lcelm+melen), el, mwflt)
                excurs = 0.d0
                do 20 itrack = 1, ktrack
                  px = track(2,itrack)
                  py = track(4,itrack)
                  pt = track(6,itrack)
                  ttt = one/sqrt(one+two*pt*beti+pt**2 - px**2 - py**2)
                  track(1,itrack) = track(1,itrack) + el*ttt*px
                  track(3,itrack) = track(3,itrack) + el*ttt*py
                  track(5,itrack) = track(5,itrack)
     +            + el*(beti - (beti+pt)*ttt) + el * pt * dtbyds
                  excurs = max(abs(track(1,itrack)),
     +            abs(track(3,itrack)), excurs)
   20           continue
                if (excurs .gt. eject) then
                  call trflow(iturn,isup,ipos,suml,track,number,ktrack)
                  if (ktrack .eq. 0) go to 200
                endif
                suml = suml + el
 
*---- Other elements: Alignment and field error pointers (skip markers).
              else if (iq(lcelm+mbsp) .ne. 25) then
                if (lsali .ne. 0) lcali = lq(lsali-ipos)
                if (lsfld .ne. 0) lcfld = lq(lsfld-ipos)
                if (lscom .ne. 0) lccom = lq(lscom-ipos)
                if (lcali .ne. 0) call trdsp1(ipos, track, ktrack)
                if (iq(lcelm+mbsp) .eq. 8) then
                  call ttmult(track, ktrack)
                else
                  call doelem(iturn, iorder, isup, ipos, suml,
     +                        track, number, ktrack)
                endif
                if (ktrack .eq. 0) go to 200
                if (lcali .ne. 0) call trdsp2(ipos, track, ktrack)
              endif
 
*---- Entrance or exit of beam line.
            else if (iq(lcelm+mbpr) .eq. mplin) then
              if (lsali .ne. 0) lcali = lq(lsali-ipos)
              if (lcali .ne. 0) then
                if (jbyt(iq(lsflg+ipos), 1, mcode) .eq. 2) then
                  call trdsp1(ipos, track, ktrack)
                else
                  call trdsp2(ipos, track, ktrack)
                endif
              endif
            endif
 
*---- Optional print.
            if (jbit(iq(lsflg+ipos), mtrck) .ne. 0) then
             call trpelm(iturn,isup,ipos,suml,ener1,track,number,ktrack)
            endif
   40     continue
 
*---- Put particles in table and set up tracking up to next position.
        if (ltrtmp .ne. 0) then
          call ucopy(q(lq(ltrtmp-1)+1), orbit, 6*mwflt)
          call trtbsv(lq(ltrtmp-2), iturn, orbit, track, number, ktrack)
          ltrtmp = lq(ltrtmp)
          i1 = i2 + 1
          if (ltrtmp .ne. 0) then
            i2 = iq(ltrtmp-5)
          else
            i2 = irg2
          endif
          go to 10
        endif
 
*---- If line is symmetric, move backward.
        if (symm) then
          do 90 ipos = irg2, irg1, -1
            lcelm = lq(ldbnk(3)-iq(lsdir+ipos))
 
*---- Test for physical element.
            if (iq(lcelm+mbpr) .eq. mpelm) then
 
*---- Drift handled inline for speed.
              if (iq(lcelm+mbsp) .eq. 1) then
                call ucopy(q(lcelm+melen), el, mwflt)
                excurs = 0.d0
                do 70 itrack = 1, ktrack
                  px = track(2,itrack)
                  py = track(4,itrack)
                  pt = track(6,itrack)
                  ttt = one/sqrt(one+two*pt*beti+pt**2 - px**2 - py**2)
                  track(1,itrack) = track(1,itrack) + el*ttt*px
                  track(3,itrack) = track(3,itrack) + el*ttt*py
                  track(5,itrack) = track(5,itrack)
     +            + el*(beti - (beti+pt)*ttt) + el * pt * dtbyds
                  excurs = max(abs(track(1,itrack)),
     +            abs(track(3,itrack)), excurs)
   70           continue
                if (excurs .gt. eject) then
                  call trflow(iturn,isup,ipos,suml,track,number,ktrack)
                  if (ktrack .eq. 0) go to 200
                endif
                suml = suml + el
 
*---- Other elements: Alignment and field error pointers (skip markers).
              else if (iq(lcelm+mbsp) .ne. 25) then
                if (lsali .ne. 0) lcali = lq(lsali-ipos)
                if (lsfld .ne. 0) lcfld = lq(lsfld-ipos)
                if (lscom .ne. 0) lccom = lq(lscom-ipos)
                if (lcali .ne. 0) call trdsp1(ipos, track, ktrack)
                if (iq(lcelm+mbsp) .eq. 8) then
                  call ttmult(track, ktrack)
                else
                  call doelem(iturn, iorder, isup, ipos, suml,
     +                        track, number, ktrack)
                endif
                if (ktrack .eq. 0) go to 200
                if (lcali .ne. 0) call trdsp2(ipos, track, ktrack)
              endif
 
*---- Entrance or exit of beam line.
            else if (iq(lcelm+mbpr) .eq. mplin) then
              if (lsali .ne. 0) lcali = lq(lsali-ipos)
              if (lcali .ne. 0) then
                if (jbyt(iq(lsflg+ipos), 1, mcode) .eq. 2) then
                  call trdsp1(ipos, track, ktrack)
                else
                  call trdsp2(ipos, track, ktrack)
                endif
              endif
            endif
 
*---- Optional print.
            if (jbit(iq(lsflg+ipos), mtrck) .ne. 0) then
             call trpelm(iturn,isup,ipos,suml,ener1,track,number,ktrack)
            endif
   90     continue
        endif
  100 continue
 
*---- End of turn.
  200 continue
      end
