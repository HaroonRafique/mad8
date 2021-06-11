      subroutine tmlump(fsec, ftrk, orbit, fmap, el, ek, re, te)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TRANSPORT map for a lump.                                          *
*   Assumes that the map has been set to identity.                     *
* Input:                                                               *
*   FSEC      (logical) If true, return second order terms.            *
*   FTRK      (logical) If true, track orbit.                          *
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
*   LCFLD     /REFER/   Current field error pointer.                   *
* Local links:                                                         *
*   LMAP                Transfer map for a lump.                       *
*   LSEQ                Beam line sequence for a lump.                 *
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
      integer ienum,iflag,ikl,il,ileng,iocc,irl,itl,jbit,lmap,lseq,ns
      double precision ek,el,elump,orbit,re,te,zero
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
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
 
      character*(mcnam) elmnam
      logical           radsav
 
*---- Initialize, test for validity.
      llump = 0
      radsav = dorad
      if (dorad) then
        dorad = .false.
        call diname(ldbnk, iq(lcelm+mbnam), elmnam)
        call utleng(elmnam, ileng)
        msg(1) = 'LUMP "' // elmnam(1:ileng)
     +  // '" will ignore radiation.'
        call aawarn('TMLUMP', 1, msg)
      endif
 
*---- Recursive procedure to build lumps.
*     Test for available map.
  100 lmap = lq(lcelm-iq(lcelm+mbat)-mbemap)
      if (lmap .ne. 0) then
        call ucopy(q(lmap         +1), ek,   6*mwflt)
        call ucopy(q(lmap+ 6*mwflt+1), re,  36*mwflt)
        call ucopy(q(lmap+42*mwflt+1), te, 216*mwflt)
        call ucopy(q(lcelm+melen), el, mwflt)
        fmap = .true.
        go to 200
      endif
 
*---- Avoid recursive call.
      if (jbit(iq(lcelm),mxlmp) .ne. 0) then
        call diname(ldbnk, iq(lcelm+mbnam), elmnam)
        call utleng(elmnam, ileng)
        msg(1) = 'LUMP "' // elmnam(1:ileng)
     +    // '" has a recursive definition,'
        msg(2) = 'Identity transformation used for inner map.'
        call aafail('TMLUMP', 2, msg)
        call uzero(ek, 1, 6*mwflt)
        call m66one(re)
        call uzero(te, 1, 216*mwflt)
        go to 200
      endif
 
*---- Lump map must be computed; stack data for element.
      call sbit1(iq(lcelm), mxlmp)
      call mzbook(2, llump, llump, 1, 'LSTK', 4, 1, 4, 2, 0)
      lq(llump-2) = lcelm
      lq(llump-3) = lcali
      lq(llump-4) = lcfld
 
*---- Expand the element sequence for this lump.
*     Avoid dropping of a previous sequence.
      call lnrefe(lcelm, 4, lseq, llump, -1)
      if (error) go to 200
 
*---- Clear accumulated length.
      call ucopy(zero, q(lcelm+melen), mwflt)
 
*---- Assign working space for accumulation of map.
      iq(llump+4) = iwork
      ikl = iwork
      irl = iwork +  6
      itl = iwork + 42
      iwork = iwork + (6 + 36 + 216)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Set initial map to identity.
      call uzero(dq(ikl+1), 1, 6*mwflt)
      call m66one(dq(irl+1))
      call uzero(dq(itl+1), 1, 216*mwflt)
 
*---- Simulated DO loop indices.
      iq(llump+1) = iq(lseq+msr1)
      iq(llump+2) = iq(lseq+msr2)
      iq(llump+3) = iq(lseq+msr1) - 1
 
*---- Loop over sequence.
*     Cannot use DO's and block IF's due to jump into this block!
  110 iq(llump+3) = iq(llump+3) + 1
      if (iq(llump+3) .gt. iq(llump+2)) go to 130
        lseq = lq(llump-1)
        call utelem(lseq, iq(llump+3), iflag, elmnam, iocc, ienum)
 
*---- Test for element.
        if (iq(lcelm+mbpr) .ne. mpelm) go to 110
 
*---- Test for inner lump.
        if (iq(lcelm+mbsp) .eq. 23) go to 100
 
*---- Compute map for a single element.
        call tmmap(.true., .false., orbit, fmap, el, ek, re, te)
        if (.not. fmap) go to 110
 
*---- Accumulate transfer matrix and kick for lump.
*     Come here after computation of an inner lump.
  120   ikl = iq(llump+4)
        irl = ikl +  6
        itl = irl + 36
        call uzero(ek, 1, 6*mwflt)
        call tmcat1(.true., ek, re, te, dq(ikl+1), dq(irl+1), dq(itl+1),
     +              dq(ikl+1), dq(irl+1), dq(itl+1))
 
*---- Accumulate length for lump.
        lcelm = lq(llump-2)
        call ucopy(q(lcelm+melen), elump, mwflt)
        elump = elump + el
        call ucopy(elump, q(lcelm+melen), mwflt)
 
*---- Advance to next component of lump.
      go to 110
 
*---- End of lump; unstack pointers.
  130 lcelm = lq(llump-2)
      lcali = lq(llump-3)
      lcfld = lq(llump-4)
 
*---- Clear call bit.
      call sbit0(iq(lcelm), mxlmp)
 
*---- Store transfer matrix for current lump.
      il = iq(lcelm+mbat) + mbemap
      ns = (6 + 36 + 216) * mwflt
      call mzbook(2, lmap, lcelm, -il, 'LMAP', 0, 0, ns, mreal, 0)
      ikl = iq(llump+4)
      irl = ikl +  6
      itl = irl + 36
      call ucopy(dq(ikl+1), q(lmap         +1),   6*mwflt)
      call ucopy(dq(irl+1), q(lmap+ 6*mwflt+1),  36*mwflt)
      call ucopy(dq(itl+1), q(lmap+42*mwflt+1), 216*mwflt)
 
*---- Return accumulated matrix.
      call ucopy(dq(ikl+1), ek,   6*mwflt)
      call ucopy(dq(irl+1), re,  36*mwflt)
      call ucopy(dq(itl+1), te, 216*mwflt)
      call ucopy(q(lcelm+melen), el, mwflt)
      fmap = .true.
 
*---- Release working space and unstack.
      iwork = iq(llump+4)
      call lndrop(lq(llump-1))
      call mzdrop(0, llump, '.')
 
*---- End of element calculation; check for outer LUMP.
  200 if (llump .ne. 0) go to 120
 
*---- Track orbit.
      if (ftrk) call tmtrak(ek, re, te, orbit, orbit)
 
*---- Restore radiation flag.
      dorad = radsav
 
      end
