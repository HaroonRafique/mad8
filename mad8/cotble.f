      subroutine cotble(eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Set up table of all correctors and monitors in the machine.        *
*   Reference optics ignores imperfections, but keeps radiation.       *
* Output:                                                              *
*   EFLAG     (logical) Error flag.                                    *
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
      integer i,iflag,ipos,isp,jbyt,mcom,nd,nl
      double precision denx,deny,el,el2,one,ta,tb,two,twopi,zero
      logical           eflag
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
      integer ncor,nmon
      double precision akl,amuxcm,amuycm,betxcm,betycm,dxcm,dycm,halfqx,
     +halfqy,qual,scm,weight,xcm,ycm
 
*---- Data for current corrector or monitor.
*     Order of variables is important for UCOPY calls.
      common /codata/   xcm, ycm, dxcm, dycm, scm, betxcm, betycm,
     +                  amuxcm, amuycm, akl, halfqx, halfqy,
     +                  qual, weight(2), ncor(2), nmon(2)
      save              /codata/
      integer lcobuf,lcocor,lcoelm,lcomon,lcotab
 
*---- Links for closed orbit correction module.
      common /colink/   lcotab, lcobuf, lcocor, lcomon, lcoelm
      save              /colink/
      double precision ek,re,te
 
*---- Transfer map for current element.
      common /mapelm/   ek(6), re(6,6), te(6,6,6)
      save              /mapelm/
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
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi)
      parameter         (zero  = 0.0d0, one = 1.0d0, two = 2.0d0)
      parameter         (mcom  = 9 * mwflt)
 
      logical           dosave(maxdof), fmap
 
*---- Set to ignore all effects but cavities and radiation.
      do 10 i = 1, maxdof
        dosave(i) = doflag(i)
        doflag(i) = .false.
   10 continue
 
*---- The flags have changed, drop precomputed maps.
      call aapdrp
 
*---- Test for presence of corrector and monitor table.
      eflag = .false.
      nd = 2 * mwflt + 4
      if (lq(lcseq-mscom) .ne. 0) then
        lscom = lq(lcseq-mscom)
        call ncopy(iq(lscom+1), ncor, 2)
        call ncopy(iq(lscom+3), nmon, 2)
        call ucopy(q(lscom+5), halfqx, 2*mwflt)
        go to 800
      endif
      call aainfo('COTBLE', 1,
     +            'Building tables for MICADO algorithm.')
 
*---- Initial conditions for ideal lattice functions.
      call tmrefe(lcseq)
      if (iq(lcseq+msym) .ne. 0) call tmmksm(.false.)
      call twbtin(lcseq, .false., eflag)
      if (eflag) go to 800
      halfqx = atan2(sinmux,cosmux) / two
      halfqy = atan2(sinmuy,cosmuy) / two
 
*---- Initialize.
      ncor(1) = 0
      ncor(2) = 0
      nmon(1) = 0
      nmon(2) = 0
 
*---- Copy initial optical functions.
      betx = betx0
      alfx = alfx0
      amux = zero
      bety = bety0
      alfy = alfy0
      amuy = zero
      call ucopy(disp0, disp, 6*mwflt)
 
*---- Book pointer bank.
      nl = iq(lsdir-1)
      lcocor = 0
      lcomon = 0
      lcoelm = 0
      call mzbook(2, lscom, lcseq, -mscom, 'COTB', nl, nl, nd, 2, 0)
 
*---- Loop on complete working beam line.
      suml = 0.0
      lcali = 0
      lcfld = 0
      do 90 ipos = 1, nl
        iflag = iq(lq(lcseq-msflg)+ipos)
        lcelm = lq(ldbnk(3)-iq(lq(lcseq-msdir)+ipos))
 
*---- Element: find transfer matrix.
        if (jbyt(iflag,1,mcode) .eq. 1) then
          lccom = 0
          call tmmap(.false., .false., orbit, fmap, el, ek, re, te)
          isp = iq(lcelm+mbsp)
          akl = 0.0
 
*---- Quadrupole or thin multipole.
          if (isp .eq. 5  .or.  isp .eq. 8) then
            call mzbook(0, lccom, lscom, -ipos, 'DELM', 2, 0,
     +                  mcom, mreal, 0)
            iq(lccom-5) = ipos
            lq(lccom-2) = lcelm
            if (lcoelm .ne. 0) then
              lq(lcoelm-1) = lccom
            else
              lq(lcseq-mselm) = lccom
            endif
            lcoelm = lccom
            xcm   = 0.0
            ycm   = 0.0
 
*---- Quadrupole: Interpolate functions at centre
*     (no need for dispersion).
            if (isp .eq. 5) then
              call ucopy(q(lcelm+melen), el, mwflt)
              el2    = el / 2.
              scm    = suml + el2
              tb     = (re(1,1) + one) * betx - re(1,2) * alfx
              denx   = re(1,1) + re(2,2) + two
              betxcm = (tb**2 + re(1,2)**2) / (denx * betx)
              amuxcm = amux + atan2(re(1,2), tb)
              tb     = (re(3,3) + one) * bety - re(3,4) * alfy
              deny   = re(3,3) + re(4,4) + two
              betycm = (tb**2 + re(3,4)**2) / (deny * bety)
              amuycm = amuy + atan2(re(3,4), tb)
 
*---- Multipole.
            else
              scm    = suml
              betxcm = betx
              betycm = bety
              amuxcm = amux
              amuycm = amuy
              dxcm   = disp(1)
              dycm   = disp(3)
            endif
            call ucopy(xcm, q(lccom+1), mcom)
 
*---- Sextupole.
          else if (isp .eq. 6) then
            call mzbook(0, lccom, lscom, -ipos, 'DELM', 2, 0,
     +                  mcom, mreal, 0)
            iq(lccom-5) = ipos
            lq(lccom-2) = lcelm
            if (lcoelm .ne. 0) then
              lq(lcoelm-1) = lccom
            else
              lq(lcseq-mselm) = lccom
            endif
            lcoelm = lccom
            xcm   = 0.0
            ycm   = 0.0
 
*---- Interpolate functions at centre.
            call ucopy(q(lcelm+melen), el, mwflt)
            el2    = el / 2.
            scm    = suml + el2
            betxcm = ((betx - el2 * alfx)**2 + el2**2) / betx
            betycm = ((bety - el2 * alfy)**2 + el2**2) / bety
            amuxcm = amux + atan2(el2, betx - el2 * alfx)
            amuycm = amuy + atan2(el2, bety - el2 * alfy)
            dxcm   = disp(1) + el2 * disp(2)
            dycm   = disp(3) + el2 * disp(4)
            call ucopy(xcm, q(lccom+1), mcom)
 
*---- Corrector or monitor?
          else if (isp .ge. 14  .and.  isp .le. 19) then
            call ucopy(q(lcelm+melen), el, mwflt)
            el2    = el / two
            scm    = suml + el2
            xcm    = 0.0
            ycm    = 0.0
            dxcm   = 0.0
            dycm   = 0.0
            betxcm = ((betx - el2 * alfx)**2 + el2**2) / betx
            betycm = ((bety - el2 * alfy)**2 + el2**2) / bety
            amuxcm = amux + atan2(el2, betx - el2 * alfx)
            amuycm = amuy + atan2(el2, bety - el2 * alfy)
 
*---- Element is corrector (ISP = 14 ... 16).
            if (isp .le. 16) then
              call mzbook(0, lccom, lscom, -ipos, 'DCOR', 1, 0,
     +                    mcom, mreal, 0)
              iq(lccom-5) = ipos
              if (lcocor .ne. 0) then
                lq(lcocor-1) = lccom
              else
                lq(lcseq-mscor) = lccom
              endif
              lcocor = lccom
              if (isp .le. 15) then
                ncor(1) = ncor(1) + 1
                call sbit1(iq(lccom), 1)
              endif
              if (isp .ge. 15) then
                ncor(2) = ncor(2) + 1
                call sbit1(iq(lccom), 2)
              endif
 
*---- Set corrector active and copy data.
              call sbyt(iq(lccom), iq(lccom), 5, 2)
              call ucopy(xcm, q(lccom+1), mcom)
 
*---- Element is monitor (ISP = 17 ... 19).
            else
              call mzbook(0, lccom, lscom, -ipos, 'DMON', 1, 0,
     +                    mcom, mreal, 0)
              iq(lccom-5) = ipos
              if (lcomon .ne. 0) then
                lq(lcomon-1) = lccom
              else
                lq(lcseq-msmon) = lccom
              endif
              lcomon = lccom
              if (isp .le. 18) then
                nmon(1) = nmon(1) + 1
                call sbit1(iq(lccom), 1)
              endif
              if (isp .ge. 18) then
                nmon(2) = nmon(2) + 1
                call sbit1(iq(lccom), 2)
              endif
 
*---- Set monitor active for orbit and dispersion and copy data.
              call sbyt(iq(lccom), iq(lccom), 3, 2)
              call sbyt(iq(lccom), iq(lccom), 5, 2)
              call ucopy(xcm, q(lccom+1), mcom)
            endif
          endif
 
*---- Advance through element.
          if (fmap) then
            tb   = re(1,1)*betx - re(1,2)*alfx
            ta   = re(2,1)*betx - re(2,2)*alfx
            alfx = - (tb*ta + re(1,2)*re(2,2)) / betx
            betx = (tb**2 + re(1,2)**2) / betx
            amux = amux + atan2(re(1,2),tb)
            tb   = re(3,3)*bety - re(3,4)*alfy
            ta   = re(4,3)*bety - re(4,4)*alfy
            alfy = - (tb*ta + re(3,4)*re(4,4)) / bety
            bety = (tb**2 + re(3,4)**2) / bety
            amuy = amuy + atan2(re(3,4),tb)
            call m66byv(re, disp, disp)
            suml = suml + el
          endif
        endif
   90 continue
      call ncopy(ncor, iq(lscom+1), 2)
      call ncopy(nmon, iq(lscom+3), 2)
      call ucopy(halfqx, q(lscom+5), 2*mwflt)
 
*---- Restore status flags.
  800 continue
      do 810 i = 1, maxdof
        doflag(i) = dosave(i)
  810 continue
      dokick = .true.
 
*---- The flags have changed, drop precomputed maps.
      call aapdrp
 
      end
