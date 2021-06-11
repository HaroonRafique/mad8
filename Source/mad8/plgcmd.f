      subroutine plgcmd (ierr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Returns unpacked PLOT command parameters.                          *
*                                                                      *
*+++ Input and output (except IERR) via common blocks in +CA PLCOMM    *
* Output:                                                              *
*   HAXIS        (char) variable on horizontal axis                    *
*   VAXIS        (char) variables on axes 1 - 4                        *
*   NVVAR     (integer) no. of vert. variables per axis                *
*   IVPAR     (integer) certain integer PLOT parameters                *
*   TYPE         (char) plot type for track plots                      *
*   HRANGE       (real) horizontal plot interval                       *
*   VRANGE       (real) vertical plot intervals                        *
*   TABLE        (char) table name                                     *
*   TITLE        (char) plot title                                     *
*   IRPOS     (integer) lower and upper range position                 *
*   SPARM        (char) frame selection parameter                      *
*   SORTFL, SPLIFL, MULTFL, FFTFL, DUMPFL (logical)   flags            *
*   USRV         (real) user provided ranges                           *
*   NNTV         (int)  no. of values in USRV                          *
*                                                                      *
*   IERR      (integer) =0: OK, >0 : error in command                  *
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
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
      integer maux,maxitp,maxppt,mdsv,mint,mksmax,mntmax,mnvar,mpanno,
     +mpascl,mpbars,mpbtit,mpcolr,mpfelm,mpfont,mpfram,mplscl,mplscw,
     +mpmax,mpmin,mpmxvr,mpname,mpparn,mppcrn,mpsclf,mpspli,
     +mpsscl,mpstyl,mpsymb,mpsymf,mptscl,mpttit,mpvaxr,mpxsiz,mpysiz,
     +mqadd,mtbv,mtitl,musrv,mxdep,mxipar,mxlabl,mxqbnk,mxqcnd
 
      real              pflmax
 
      parameter         (mpparn = 11, mppcrn = 170)
      parameter         (mpmxvr = 5,  mxipar = 8, mtitl  = 128)
      parameter         (mxlabl = 40, pflmax = 1.e20)
      parameter         (mtbv = 6, mdsv = 3, musrv = 3)
      parameter         (maxppt = 20000, mnvar = 74, mxdep = 2)
      parameter         (mint = 10, maux = mint + 1, maxitp = 5000)
      parameter         (mxqcnd = 10, mxqbnk = 1000, mqadd = 100000)
      parameter         (mntmax = 20, mksmax = 10)
 
      parameter         (mpfont = 1, mpxsiz = 3, mpysiz = 4)
      parameter         (mplscl = 6, mptscl = 8, mpascl = 5)
      parameter         (mplscw = 2, mpsscl = 7, mpfelm = 9)
      parameter         (mpfram = 2, mpmin  = 1, mpmax  = 2)
      parameter         (mpsclf = 3, mpvaxr = 4, mpname = 5)
      parameter         (mpstyl = 1, mpspli = 2, mpbars = 3)
      parameter         (mpsymf = 4, mpcolr = 5, mpsymb = 6)
      parameter         (mpanno = 7)
      parameter         (mpttit = mpname + mtitl / mcwrd)
      parameter         (mpbtit = mpttit + mtitl / mcwrd)
 
*--- preceding parameters: see LPMAIN description (routine PLPLOT)
      integer idsbis,idsfrm,ihpntr,iqrang,irg1,irg2,irpos,itbv,ivpar,
     +ivpntr,laux,lbias,lbuf,lcnt,lexpv,lform,lframe,lhval,lindx,lm1,
     +lm2,locc,lpint,lpmain,lpparl,lproc,lqv1,lrvc,lrvv,ltab,ltbr,ltmp,
     +lvcurv,lvrw,lvsg,lvval,lvvar,nexpvr,nform,nntv,nocc,ntmax,ntvvar,
     +nvvar
      double precision usrv
      common /plcomm/      lpmain, ltbr, lexpv, ltab, lvsg, lvrw, locc,
     +                     lcnt, lproc, lform, lbias, lpint, lm1, lm2,
     +                     ltmp, lframe, lvvar, lvcurv, lhval, lvval,
     +                     lindx, lpparl, lrvv(4), laux(maux), lqv1,
     +                     lrvc(4*mpmxvr), lbuf
      save   /plcomm/
      common /plcoms/ haxis, vaxis, type, table, sparm, title,
     +                plfnam, plpnam, qcond(mxqcnd)
      save   /plcoms/
      character*(mcnam) haxis, type, table, sparm,
     +                  vaxis(mpmxvr,4)
      character*(mtitl) title
      character*(mcstr) qcond, plfnam, plpnam
      common /plcomp/      nntv(musrv), ntvvar, ihpntr, nocc, nform,
     +                     idsbis(mtbv), idsfrm(mtbv), irg1, irg2, itbv,
     +                     ntmax, nexpvr,
     +                     sortfl, splifl, multfl, fftfl, dumpfl,
     +                     helpfl,
     +                     ivpntr(mpmxvr,4), nvvar(4), ivpar(mxipar),
     +                     irpos(2), iqrang(3,mxqcnd), hrange(2),
     +                     vrange(2,4), qsval
      save   /plcomp/
 
      real                 hrange, vrange, qsval
 
      logical              sortfl, splifl, multfl, fftfl, dumpfl, helpfl
 
      common /plcomd/      usrv(25, musrv)
      save   /plcomd/
 
*---- Page header information.
      common /header/   ctitle, cdate, ctime, nvers, cvers
      save              /header/
      character         ctitle*80, cdate*8, ctime*8, nvers*8, cvers*16
      integer icvref,iframe,ipar,ipxval,ipyval,ivnarw,nptval
      common /plotcl/   fpmach
      save   /plotcl/
 
      logical           fpmach
      common /plotcr/   yvtop, fdum, chh,
     +vpt(4), window(4,4), actwin(4,4), range(2), xax(2), yax(8)
      save   /plotcr/
 
      real              yvtop, fdum, chh
      real              vpt, window, actwin, range, xax, yax
 
      common /plotci/   iframe, ivnarw,
     +                  ipar(50), nptval(4), ipxval(4), ipyval(4),
     +                  icvref(4)
      save   /plotci/
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer i,ierr,ius,j,k,kk,l,nhor,nvert
      double precision tval
      dimension            tval(25)
 
      integer              itype(mppcrn)
 
      logical              eflag
 
      ierr     = 0
      irpos(1) = 0
      irpos(2) = 0
      call vzero (nvvar, 4)
      call vzero (nntv , musrv)
      call vfill (ivpar, mxipar, -1)
      ivnarw    = 0
      ntmax     = 0
      sortfl    = .false.
      splifl    = .false.
      multfl    = .false.
      helpfl    = .false.
      hrange(1) = pflmax
      hrange(2) = pflmax
      qsval = 0.
      call vfill (vrange, 8, pflmax)
      type  = ' '
      haxis = ' '
      table = ' '
      title = ' '
      sparm = ' '
      do 10 i = 1, mpmxvr
        do 10 j = 1, 4
          vaxis(i,j) = ' '
   10 continue
*--- set defaults for K_x etc. selection criteria (Q_y/Q_x plot)
      do 11  i = 1, mxqcnd
        iqrang(1,i) = 0
        iqrang(2,i) = 0
        iqrang(3,i) = 1
        qcond(i)    = ' '
   11 continue
 
      call utgtyp (lccmd, itype)
*--- take VAXIS parameters if given, else take VAXIS1...4
      if (itype(1) .ne. 0)  then
        do 20 i = 1, mpmxvr
          if (itype(i) .ne. 0)  then
            call utgnam (lccmd, i, i, vaxis(i,1))
            nvvar(1) = nvvar(1) + 1
          endif
   20   continue
        nvert = 1
      else
        nvert = 0
        k = mpmxvr
        do 30 i = 1,4
          do 30 j = 1, mpmxvr
            k = k + 1
            if (itype(k) .ne. 0)  then
              nvert = nvert + 1
              call utgnam (lccmd, k, k, vaxis(j,i))
              nvvar(i) = nvvar(i) + 1
            endif
   30   continue
      endif
      k = 5 * mpmxvr + 1
      if (itype(k) .ne. 0)  then
        call utgnam (lccmd, k, k, haxis)
        nhor = 1
      else
        nhor = 0
      endif
      do 40 i = 1, mxipar
        k = k + 1
        if (itype(k) .ne. 0)  call utgint (lccmd, k, k, ivpar(i))
   40 continue
*--- sort option
      k = k + 1
      if (itype(k) .ne. 0)  call utglog (lccmd, k, k, sortfl)
*--- spline option
      k = k + 1
      if (itype(k) .ne. 0)  call utglog (lccmd, k, k, splifl)
*--- multiple option
      k = k + 1
      if (itype(k) .ne. 0)  call utglog (lccmd, k, k, multfl)
*--- help flag (prints list of all variables, suppresses plot)
      k = k + 1
      if (itype(k) .ne. 0)  call utglog (lccmd, k, k, helpfl)
*--- Fast Fourier Transform option
      fftfl = ivpar(6) .ge. 0
*--- dump flag (dumps plot bank, suppresses plot)
      dumpfl = ivpar(8) .ge. 0
      if (nvert .eq. 0)  then
        call aawarn('PLGCMD', 1, 'No vertical variable.')
        ierr = 1
        goto 999
      endif
      if (nhor .eq. 0)  then
        call aawarn('PLGCMD', 1, 'No horizontal variable.')
        ierr = 2
        goto 999
      endif
      call utgflt (lccmd, k + 1, k + 2, tval)
      do 50 i = 1, 2
        k = k + 1
        if (itype(k) .ne. 0)  hrange(i) = tval(i)
   50 continue
      call utgflt (lccmd, k+1, k+8, tval)
      kk = 0
      do 70 j = 1, 2
        do 60 i = 1, 4
          kk = kk + 1
          k  = k + 1
          if (itype(k) .ne. 0)  vrange(j,i) = tval(kk)
   60   continue
   70 continue
      k = k + 1
      if (itype(k) .ne. 0)  call utgnam (lccmd, k, k, table)
      k = k + 1
      if (itype(k) .ne. 0)  then
        call utgstr (lccmd, k, k, title)
      else
        if (ctitle .eq. ' ')  then
          title = sequnam
        else
          title = ctitle
        endif
      endif
      k = k + 1
      if (itype(k) .ne. 0) call utgnam (lccmd, k, k, sparm)
      k = k + 1
      if (itype(k) .ne. 0)  then
        l = lq(lccmd - k)
        if (l .ne. 0) then
          call utgrng (l, lcseq, irpos(1), irpos(2), eflag)
          if (eflag) then
            ierr = 3
            goto 999
          endif
        else
          call aawarn('PLGCMD', 1, 'Currently no line exists.')
          ierr = 4
          goto 999
        endif
      endif
      do 90  ius = 1, musrv
        call utgflt (lccmd, k+1, k+25, tval)
        do 80 j = 1, 25
          k = k + 1
          if (itype(k) .ne. 0) then
            nntv(ius) = nntv(ius) + 1
            usrv(nntv(ius),ius) = tval(j)
          endif
   80   continue
   90 continue
      k = k + 1
      if (itype(k) .ne. 0)  call utgint (lccmd, k, k, ntmax)
      ntmax = min(ntmax, mntmax)
      do 100 i = 1, mxqcnd
        k = k + 1
        if (itype(k) .ne. 0)  then
          call utgstr (lccmd, k, k, qcond(i))
          ivnarw = 1
        endif
        do 110  j = 1, 3
          k = k + 1
          if (itype(k) .ne. 0)  then
            call utgint (lccmd, k, k, iqrang(j,i))
            ivnarw = 1
          endif
  110   continue
  100 continue
*--- temporary QS variable
      k = k + 1
      call utgflt (lccmd, k, k, tval)
      if (itype(k) .ne. 0)  qsval = tval(1)
*--- plot file name (HIGZ only)
      k = k + 1
      call utgstr (lccmd, k, k, plfnam)
      if (itype(k) .eq. 0)  plfnam = plpnam
  999 end
