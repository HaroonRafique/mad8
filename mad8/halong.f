      subroutine halong
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Set up "long" table of linear lattice functions for HARMON.        *
*   This routine assumes that the temporary links for HARMON are       *
*   already active.                                                    *
*   It ignores synchrotron radiation and imperfections, and considers  *
*   RF cavities only to compute synchrotron tunes.                     *
*----------------------------------------------------------------------*
* Modified: 19-JUL-1999, T. Raubenheimer [SLAC]                        *
*    Added /SYNCH/ common and reset BLEN before TMMAP call             *
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
      double precision ensige,ensigx,ensigy
 
*---- Communication area for HARMON module.
      common /harchr/   lngnam, shtnam
      common /harflt/   ensigx, ensigy, ensige
      save              /harchr/, /harflt/
      character*(mcnam) lngnam, shtnam
      double precision amux1,amux2,amuy1,amuy2,ax1,ax2,ay1,ay2,bx1,bx2,
     +by1,by2,ct1,ct2,delta1,delta2,dpx1,dpx2,dpy1,dpy2,dx1,dx2,dy1,dy2,
     +px1,px2,py1,py2,s1,s2,x1,x2,y1,y2
 
*---- Buffer for "long" HARMON table: Values at both ends of an element.
      common /halbuf/   bx1, ax1, amux1, by1, ay1, amuy1,
     +                  x1, px1, y1, py1, ct1, delta1,
     +                  dx1, dpx1, dy1, dpy1, s1,
     +                  bx2, ax2, amux2, by2, ay2, amuy2,
     +                  x2, px2, y2, py2, ct2, delta2,
     +                  dx2, dpx2, dy2, dpy2, s2
      save   /halbuf/
      integer lhalbf,lhaltb,lhasbf,lhastb,mlcol,mscol
 
*---- Link area for HARMON module.
      parameter         (mlcol = 17, mscol = 12)
      common /halink/   lhaltb, lhalbf, lhastb, lhasbf
      save              /halink/
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
*---- Synchrotron integrals, etc.
      common /synch/    synch_1, synch_2, synch_3, synch_4, synch_5,
     +                  blen, rhoinv
      double precision  synch_1, synch_2, synch_3, synch_4, synch_5,
     +                  blen, rhoinv
      integer i,ienum,iflag,iocc,ipos,nline,nrow
      double precision el,twopi,zero
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0 * pi, zero = 0.0)
      character*(mcnam) colnam(mlcol), elmnam
      character*(*)     title
      integer           icform(mlcol)
      logical           fmap
 
      parameter         (title = 'HARMON startup.')
 
      data colnam
     +   / 'BETX', 'ALFX', 'AMUX', 'BETY', 'ALFY', 'AMUY',
     +     'X',    'PX',   'Y',    'PY',   'T',    'PT',
     +     'DX',   'DPX',  'DY',   'DPY',  'S'              /
      data icform       / mlcol * mreal /
 
*---- Set status flags to ignore all effects but field errors.
      do 10 i = 1, maxdof
        doflag(i) = .false.
   10 continue
      dofld = .true.
 
*---- Fix up environment and find initial values for tracking.
      call prhead('HARMON', title, zero, 0, nline, 1)
      call enfix
      if (error) return
      call enfreq(zero)
      call tmturn(lcseq, zero, error)
      if (error) return
      if (iq(lcseq+msym) .ne. 0) call tmmksm(.true.)
      call twbtin(lcseq, .false., error)
      if (.not. (stabx .and. staby)) then
        call aafail('HALONG', 1,
     +    'One or both planes unstable, cannot run HARMON.')
      endif
      if (error) return
      call enprgl
      call enprem
      call enprrf
      amux0 = zero
      amuy0 = zero
      suml = zero
      call ucopy(betx0, betx, 18*mwflt)
 
*---- Set up table buffer for optical functions.
      nrow = iq(lq(lcseq-msdir)-1)
      call tbcrea(lngnam, 1, nrow, mlcol, colnam, icform, 2, lhaltb)
      if (error) return
 
*---- Save lattice parameters at start of system.
*     (In case that first position is an element).
      if (irg1 .gt. 1) then
        call tbset(lhaltb, irg1 - 1, 3, lhalbf)
        if (lhalbf .ne. 0) then
          call ucopy(betx, bx1, mwflt * mlcol)
          s1 = suml
          call ucopy(bx1, q(lhalbf+1), iq(lhalbf-1))
        endif
      endif
 
*---- Loop on beam range.
      cplxy = .false.
      cplxt = .false.
      do 90 ipos = irg1, irg2
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
 
*---- Tor: set BLEN to zero ... it may get reset in TMMAP
        blen = zero
 
*---- Track lattice parameters.
        if (iq(lcelm+mbpr) .eq. mpelm) then
          call tmmap(.false., .false., orbit, fmap, el, ek, re, te)
          if (fmap) then
            suml = suml + el
            call twbttk(.false., .true.)
          endif
        endif
 
*---- Save lattice parameters.
        call tbset(lhaltb, ipos, 3, lhalbf)
        if (lhalbf .ne. 0) then
          call ucopy(betx, bx1, mwflt * mlcol)
          s1 = suml
          call ucopy(bx1, q(lhalbf+1), iq(lhalbf-1))
        endif
   90 continue
 
*---- Check for no coupling and no synchrotron motion.
      if (cplxy) then
        call aawarn('HALONG', 1,
     +  'HARMON will ignore transverse coupling.')
      endif
      if (cplxt) then
        call aawarn('HALONG', 1,
     +  'HARMON will ignore RF cavities except to compute Qs.')
      endif
      if (dorad) then
        call aawarn('HALONG', 1,
     +  'HARMON will ignore synchrotron radiation.')
      endif
 
*---- Store summary data.
      qx = nsup * amux / twopi
      qy = nsup * amuy / twopi
      circ = suml * nsup
      if (symm) then
        qx = qx + qx
        qy = qy + qy
        circ = circ + circ
      endif
 
      end
