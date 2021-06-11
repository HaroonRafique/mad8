      subroutine enput
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Store complete BEAM common into BEAM bank.                         *
* 18 attributes:                                                       *
*   PARTICLE, MASS,     CHARGE,   ENERGY,   PC,       GAMMA,           *
*   EX,       EY,       EXN,      EYN,      ET,       SIGT,            *
*   SIGE,     KBUNCH,   NPART,    BCURRENT, BUNCHED,  RADIATE          *
* Additional quantities kept in BEAM bank:                             *
*   FREQ0     (real)    Revolution frequency in MHz.                   *
*   BETA      (real)    Relativistic parameter v/c.                    *
*   U0        (real)    Radiation loss per turn in GeV.                *
*   ARAD      (real)    Classical particle radius.                     *
*   PDAMP(3)  (real)    Damping partition numbers.                     *
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
      integer mbarad,mbbeta,mbbnum,mbchrg,mbcurr,mbdamp,mbdata,mbener,
     +mbet,mbex,mbexn,mbey,mbeyn,mbfbch,mbfrad,mbfreq,mbgamm,mbmass,
     +mbpart,mbpc,mbpnum,mbsige,mbsigt,mbu0,mbsequ,mbbv
 
*---- Attribute positions in BEAM bank.
      parameter         (mbpart =  1, mbsequ =  2,
     +                   mbmass =  3, mbchrg =  4,
     +                   mbener =  5, mbpc   =  6, mbgamm =  7,
     +                   mbex   =  8, mbexn  =  9, mbey   = 10,
     +                   mbeyn  = 11, mbet   = 12, mbsigt = 13,
     +                   mbsige = 14, mbbnum = 15, mbpnum = 16,
     +                   mbcurr = 17, mbfbch = 18, mbfrad = 19,
     +                   mbfreq = 20, mbbeta = 21, mbu0   = 22,
     +                   mbarad = 23, mbbv   = 24,
     +                   mbdamp = 25, mbdata = 27)
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
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
      integer i,k,ipr,isp,locpt,bsequ_number,idir
 
      character*(mcnam) label
      data label        / 'BEAM' /
 
*---- Make sure there is a BEAM bank.
      call difind(ldkey, label, idir, lckey)
      lbeam = lq(lckey-3)
      if (lbeam .eq. 0) then
        label = 'BEAM'
        ipr = iq(lckey+mbpr)
        isp = iq(lckey+mbsp)
        call aabook(lbeam, label, ipr, isp, lckey, 1)
        lq(lckey-3) = lbeam
        call didefi(ldbnk, label, lbeam)
        call sbit1(iq(lbeam), mxcls)
        iq(lbeam+mbln) = 0
      endif
 
*---- Store data into BEAM bank.
      call utpnam(lbeam, mbpart, mbpart, prtnam)
      call utpnam(lbeam, mbsequ, mbsequ, bsequnam)
      call utpflt(lbeam, mbmass, mbcurr, amass)
      call utplog(lbeam, mbfbch, mbfrad, fbch)
      call utpflt(lbeam, mbfreq, mbdata, freq0)
      iq(lbeam-5) = ietflg + 16 * ipnflg
*--- store in beam bank pool - either new, or overwrite old
      k = bsequ_number(bsequnam)
      if (k .eq. 0)  then
        if (liftbeam .eq. iq(lq(lroot-mbeam)-3))  then
          call mzpush(0, lq(lroot-mbeam), liftbeam, 0, 'I')
        endif
        liftbeam = liftbeam + 1
        currbeam = liftbeam
        call mzbook(2, locpt, lq(lroot-mbeam), -liftbeam, 'SBEA',
     +  iq(lbeam-3), iq(lbeam-2), iq(lbeam-1), 0, -1)
        bseqnames(liftbeam) = bsequnam
      else
        locpt = lq(lq(lroot-mbeam)-k)
      endif
*--- copy bank
      k = iq(lbeam-3)
      i = iq(lbeam-1)
      call ucopy(lq(lbeam-k-1), lq(locpt-k-1), k+1)
      call ucopy(iq(lbeam+1), iq(locpt+1), i)
      end
