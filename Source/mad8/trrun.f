      subroutine trrun
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initial tracking run; RUN command.                                 *
*   Build a new tracking table.                                        *
* Attributes:                                                          *
*   METHOD    (name)    TRANSPORT, LIE3, LIE4                          *
*   TABLE     (name)    Name for track table to be generated.          *
*   TURNS     (integer) Number of turns to be tracked.                 *
*   FPRINT    (integer) Print frequency.                               *
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
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
      integer mobs,morng,mosiz,motab
 
*---- Parameters for observation points.
      parameter         (mobs = 10)
      parameter         (motab = 1, morng = motab + mwnam)
      parameter         (mosiz = morng + mcrng / mcwrd - 1)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      double precision ek,re,te
 
*---- Transfer map for current element.
      common /mapelm/   ek(6), re(6,6), te(6,6,6)
      save              /mapelm/
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer i1,i2,ibias,icode,ienum,iflag,ileng,imeth,iocc,ipos,jbyt,
     +l,maxmet,mcmmnt,mffreq,mmeth,mpfreq,mtable,mturns,nturn
      double precision distvect,el,em,zn,zero
      common /turns/    nturn
      parameter (zero = 0.d0)
      parameter         (maxmet = 3)
      parameter         (mmeth  = 1, mtable = 2, mcmmnt = 3,
     +                   mturns = 4, mpfreq = 5, mffreq = 6)
 
      external          ttelem, lmelem
      character*(mcnam) dmeth(maxmet), trknam(2), tabnam, elmnam
      character*(mcrng) tabpos
      character*(mcstr) cmmnt
      dimension         em(6,6)
      integer           ival(3)
      logical           fmap
 
      data dmeth        / 'TRANSPORT', 'LIE3    ', 'LIE4    ' /
 
*---- Tracking method and table name.
      trknam(1) = 'TRANSPORT'
      trknam(2) = ' '
      call utgnam(lccmd, mmeth, mtable, trknam)
      call utlook(trknam(1), dmeth, maxmet, imeth)
      if (imeth .eq. 0) then
        call utleng(trknam(1), ileng)
        msg(1) = 'Unknown method "' // trknam(1)(1:ileng)
     +  // '" --- "TRANSPORT" assumed.'
        call aawarn('TRRUN', 1, msg)
        imeth = 1
      endif
*--- set print file title
      trktitle(10:) = 'method: ' // trknam(1)
*---- Get track file name and comments string.
      cmmnt = ' '
      call utgstr(lccmd, mcmmnt, mcmmnt, cmmnt)
 
*---- Number of turns and frequencies.
      ival(1) = 1
      ival(2) = 0
      ival(3) = 0
      call utgint(lccmd, mturns, mffreq, ival)
      nturn = max(1, ival(1))
      ipfreq = ival(2)
      iffreq = ival(3)
 
*---- Quit, if no particles.
      if (npart .le. 0) then
        call aawarn('TRRUN',1,'Cannot RUN --- no particles available.')
 
*---- Assign working storage for tracks.
      else
        ibias = 6 * mwflt * ntrack
        iwork = ibias + ntrack
        call mzwork(0, dq(1), iq(iwork+1), 2)
        call ucopy(q(ltrstt+1), dq(1), ibias)
        call ucopy(q(ltrnum+1), iq(ibias+1), ntrack)
 
*---- Order track tables according to their positions in beam.
        call zsorti(0, ltrobs, -5)
 
*---- List and open track tables.
        if (trknam(2) .ne. ' '  .or.  ltrobs .ne. 0) then
          write (iqlog, 910)
 
*---- Loop over track tables.
          ltrtmp = ltrobs
          if (ltrtmp .ne. 0) then
            i1 = irg1
            call ucopy(orbit0, orbit, 6*mwflt)
            call m66cpy(eigen, em)
            suml = 0.0
          endif
 
*---- Track up to next table position.
   10     if (ltrtmp .ne. 0) then
            i2 = iq(ltrtmp-5)
            do 20 ipos = i1, i2
              call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
              icode = jbyt(iflag, 1, mcode)
 
*---- Misalignment at entrance of element or line.
              if (icode .ne. 3  .and.  lcali .ne. 0) then
                call tmali1(ipos, .false., orbit, orbit, re, te)
                call m66mpy(re, em, em)
              endif
 
*---- Track through element.
              if (icode .eq. 1) then
                call tmmap(.true., .true., orbit, fmap, el, ek, re, te)
                if (fmap) then
                  suml = suml + el
                  call m66mpy(re, em, em)
                endif
              endif
 
*---- Misalignment at exit of element or line.
              if (icode .ne. 2  .and.  lcali .ne. 0) then
                call tmali2(ipos, .false., orbit, orbit, re, te)
                call m66mpy(re, em, em)
              endif
   20       continue
 
*---- Build table and store orbit.
            call uhtoc(q(ltrtmp+motab), mcwrd, tabnam, mcnam)
            call uhtoc(q(ltrtmp+morng), mcwrd, tabpos, mcrng)
            call trtbop(orbit, em, tabnam, nturn, ntrack)
            call mzbook(2,l,ltrtmp,-1,'TVAL',0,0,6*mwflt,mreal,0)
            if (onepss)  then
              call vzero(q(l+1), 6*mwflt)
            else
              call ucopy(orbit, q(l+1), 6*mwflt)
            endif
            write (iqlog, 920) tabpos, tabnam
            lq(ltrtmp-2) = ltrtab
 
*---- Go to next table.
            ltrtmp = lq(ltrtmp)
            i1 = i2 + 1
            go to 10
          endif
          ltrtab = 0
 
*---- Open main track table.
          if (trknam(2) .ne. ' ') then
            call trtbop(orbit0, eigen, trknam(2), nturn, ntrack)
            write (iqlog, 920) '#E', trknam(2)
          endif
        endif
*--- store beam-beam kicks
        if (.not. bborbit) call bbstore()
*---- Open track file.
        if (iffreq .ne. 0) then
          call trflop(cmmnt, npart, nturn)
          call trflsv(0, dq(1), iq(ibias+1), ntrack)
        endif
 
*---- Set initial energy.
        ener1 = ener0
        if (ener1 .eq. zero .or. .not. ereset) ener1 = en0
*---- Track according to selected method.
        fbelow = .false.
        if (imeth .eq. 1) then
          call trexec(ttelem, 3, nturn, dq(1), iq(ibias+1), ntrack,
     +    zn, distvect)
        else if (imeth .eq. 2) then
          call trexec(lmelem, 3, nturn, dq(1), iq(ibias+1), ntrack,
     +    zn, distvect)
        else
          call trexec(lmelem, 4, nturn, dq(1), iq(ibias+1), ntrack,
     +    zn, distvect)
        endif
        call ucopy(dq(1), q(ltrstt+1), 6 * mwflt * ntrack)
        call ucopy(q(ibias+1), q(ltrnum+1), ntrack)
 
*---- Close tracking tables.
        if (ltrtab .ne. 0) call trtbcl(ltrtab)
 
        if (ltrobs .ne. 0) then
          ltrtmp = ltrobs
   30     if (ltrtmp .ne. 0) then
            call trtbcl(lq(ltrtmp-2))
            lq(ltrtmp-1) = 0
            ltrtmp = lq(ltrtmp)
            go to 30
          endif
          call mzdrop(0, ltrobs, 'L')
        endif
 
*---- Close track file.
        if (iffreq .ne. 0) call trflcl
*---- Drop working storage.
        iwork = 0
        call mzwork(0, dq(1), dq(1), -1)
      endif
 
  900 format(/' Tracking with method: ', a)
  910 format(/' The following tables will be generated:'/
     +       ' Observation point',t51,'Table name')
  920 format(' ',a,t51,a)
 
      end
