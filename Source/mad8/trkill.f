      subroutine trkill(iflag, iturn, isup, ipos, suml, el,
     +                  track, number, ntrk)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Test for aperture limits.                                          *
* Input:                                                               *
*   IFLAG     (integer) Aperture type flag:                            *
*                       1: elliptic, 2: rectangular, 3: overflow.      *
*   ITURN     (integer) Current turn number.                           *
*   ISUP      (integer) Current superperiod number.                    *
*   IPOS      (integer) Position counter.                              *
*   SUML      (real)    Accumulated length.                            *
* Output:                                                              *
*   EL        (real)    Element length.                                *
* Input/output:                                                        *
*   TRACK(6,*)(real)    Track coordinates: (X, PX, Y, PY, T, PT).      *
*   NUMBER(*) (integer) Number of current track.                       *
*   NTRK      (integer) Number of surviving tracks.                    *
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
      integer i,icopy,iflag,iname,iocc,ipass,ipos,isup,itrack,iturn,
     +jbyt,npass,ntrk
      double precision apx,apy,eject,el,suml,track,xx,yy
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      character*(mcnam) elmnam
      character*25      text(3)
      logical           ellips, header, outsid, rectan
      parameter         (eject = 1.0)
      data text         / ' in elliptical aperture ',
     +                    ' in rectangular aperture ',
     +                    ' by overflow in element ' /
 
      ellips(xx,yy) = (xx / apx) ** 2 + (yy / apy) ** 2 .gt. 1.0
      rectan(xx,yy) = abs(xx) .gt. apx  .or.  abs(yy) .gt. apy
 
*---- Get aperture size.
      if (iflag .eq. 3) then
        el  = 0.0
        apx = eject
        apy = eject
        npass = 1
      else
        call ucopy(q(lcelm+melen), el, mwflt)
        call ucopy(q(lcelm+mexcol), apx, mwflt)
        call ucopy(q(lcelm+meycol), apy, mwflt)
        if (apx .eq. 0.0) apx = eject
        if (apy .eq. 0.0) apy = eject
        if (el .eq. 0.0) then
          npass = 1
        else
          npass = 2
        endif
      endif
 
*---- Do one or two passes through tests.
      header = .false.
      do 90 ipass = 1, npass
        icopy = 0
        do 80 itrack = 1, ntrk
 
*---- Is particle outside aperture?
          if (iflag .eq. 1) then
            outsid = ellips(track(1,itrack), track(3,itrack))
          else
            outsid = rectan(track(1,itrack), track(3,itrack))
          endif
          if (outsid) then
 
*---- Stop when loosing a particle in DYNAP mode.
            if (fdynap) then
              ntrk = 0
              return
 
*---- For first particle lost, print header.
            else
              if (.not. header) then
                iname = (iq(lsdir+ipos) - 1) * mwnam + 1
                call uhtoc(q(ldbnk(2)+iname), mcwrd, elmnam, mcnam)
                iocc = jbyt(iq(lsflg+ipos), mocc1, mocc2)
                write (iqpr2, 910) iturn, isup, suml, text(iflag),
     +                             elmnam, iocc
                header = .true.
              endif
 
*---- Give message for particle.
              write (iqpr2, 920) number(itrack), (track(i,itrack),i=1,6)
 
*---- Loosing closed orbit is fatal.
              if (number(itrack) .eq. 0) then
                call aafail('TRKILL', 1,
     +            'Closed orbit has been lost, ' //
     +            'cannot continue tracking.')
                ntrk = 0
                return
              endif
            endif
 
*---- If in aperture, compact track storage.
          else
            icopy = icopy + 1
            if (icopy .ne. itrack) then
              track(1,icopy) = track(1,itrack)
              track(2,icopy) = track(2,itrack)
              track(3,icopy) = track(3,itrack)
              track(4,icopy) = track(4,itrack)
              track(5,icopy) = track(5,itrack)
              track(6,icopy) = track(6,itrack)
              number(icopy) = number(itrack)
            endif
          endif
   80   continue
        ntrk = icopy
 
*---- If first pass and non-zero length, drift to end and repeat.
        if (ipass .lt. npass) call ttdrf(el, track, ntrk)
   90 continue
 
  910 format(' Particle(s) lost during turn ',i8,', superperiod ',i8,
     +  ', s = ',f12.6,a,a,'[',i8,']'/
     +  ' Number',6x,'x',15x,'px',14x,'y',15x,'py',14x,'t',15x,'pt')
  920 format(' ',i6,4x,1p,6e16.8)
 
      end
