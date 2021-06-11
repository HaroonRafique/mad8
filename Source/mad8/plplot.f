      subroutine plplot
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Plot all types of graphs in MAD.                                   *
*   Uses GXPLOT with underlying GKS.                                   *
*----------------------------------------------------------------------*
*                                                                      *
*               input tree structure                                   *
*                                                                      *
*   LPMAIN
*    | | |
*    | | LQV1 (struct. link MPFRAM+3)
*    | |
*    | LM1,LM2 (struct. links MPFRAM+1,MPFRAM+2)
*    |
*   LFRAME (up to MPFRAM structural links)
*    | | |
*    | | (possibly index for sequence of hor. coord., structure link 3)
*    | |
*    | (possibly bank with hor. coordinates, structure link 2)
*    |
*   LVVAR --- next --- next --- ...     (structure link 1)
*    |
*    |
*   LVCURV --- next --- next --- ...    (structure link 1)
*    || ||
*    || ||
*    || |LVVAL (structural link 1)
*    || |
*    || (possibly structural link 2 to hor. coor. bank)
*    ||
*    |LHVAL (ref. link 1 = total link 3)
*    |
*    LINDX  (ref. link 2 = total link 4)
*
*   bank contents (data part):
*
*   LPMAIN:
*
*   MPFONT      font position in top bank
*   MPXSIZ      x size    - " -
*   MPYSIZ      y    - " -
*   MPASCL      annotation   - " -
*   MPLSCW      line width scale factor pos.
*   MPLSCL      label scale factor position
*   MPSSCL      symbol scale factor position
*   MPTSCL      text    - " -
*   MPFELM      start of first element in banks LM1, LM2 below
*
*   LM1:        types of elements for machine plot
*               0: drift
*               1: bend
*               2: focussing quad.
*               3: defocussing quad.
*
*   LM2:        lengths of elements
*
*   LQV1:       bank for Arnold web plot (from routine PLGARW):
*                       number of superperiods
*                       number of constraints  N
*                       N constraints:
*                       minimum
*                       maximum
*                       step
*                       Length L
*                       expression in polish notation of length L,
*                       coded as 1+, 2-, 3*, 4/, 1 KX, 2 KY, 3 KS
*                       + MQADD + 4, e.g. 100006 = KY
*
*   LFRAME:
      integer i,idum,ierr,ivax,k1dum,k2dum,k3dum,kf,kf1,kf2,kl,kl1,kl2,
     +l,masize,mlsize,mtsize,nframe,np,npar,ntcurv,nvax
 
*   MPMIN       hor. range minimum position in bank
*   MPMAX       hor.   "   maximum     - " -
*   MPSCLF      scale flag        - " -
*               meaning of flag: 0 = choose optimum range
*                                1 = start or end range at 0.
*
*                                2 = centre range around 0.
*                                3 = use range as is
*   MPNAME      hor. variable name position in bank
*   MPTTIT      top title position in bank
*   MPBTIT      bottom  - " -
*
*
*   LVVAR:   one bank per vertical variable axis (compound name)
*
*   MPMIN       vert. range minimum position in bank
*   MPMAX       vert.   "   maximum     - " -
*   MPSCLF      scale flag              - " -
*               meaning of flag: 0 = choose optimum range
*                                1 = start or end range at 0.
*
*                                2 = centre range around 0.
*                                3 = use range as is
*   MPVAXR      vert. axis ref. no. in bank
*   MPNAME      vert. variable name position in bank
*
*   LVCURV:  one bank per curve belonging to the "mother" axis
*
*   MPSTYL      curve style flag: 0 = no polyline
*                                 1 = solid line
*                                 2 = dashed
*                                 3 = dotted
*                                 4 = dot-dashed
*                               100 = 1,2,3,4,1,2,.... for succ. curves
*   MPSPLI      spline interpolation flag: 0 no, 1 yes
*   MPBARS      connect points to x axis (vertical bars): 0 no, 1 yes
*   MPSYMF      plot a symbol at point position:
*               0 = none
*               1 = dot, 2 = +, 3 = *, 4 = o, 5 = x
*               100 = current curve count (1, 2, etc.) modulo 10
*               200 = use first character of string at MPSYMB
*   MPCOLR      colour (same for line, symbol, and annotation)
*   MPSYMB      plot symbol position
*   MPANNO      symbol annotation position (= complete variable name)
*
*   LVVAL:
*
*   vert. curve coordinates
*
*   LHVAL (ref. link):
*
*   horizontal coordinates
*
*   LINDX (ref. link):
*
*   sequence index for hor. coord.s
*
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
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
 
      parameter            (mlsize = 13,mtsize = 13,masize = 20)
*--- character sizes:
*   MLSIZE    label character height
*   MTSIZE    text   - " -
*   MASIZE    annotation - " -
 
      character         svar*(mcnam), sname*(mxlabl), tname*(mcnam)
      character         slocn*(mtitl)
      character         stemp*(mtitl), stext*300, sform*20
      character         sdum(mxdep)*(mcnam), symb*1
      real                 prmach, symch
*--- strings:
*   SVAR     buffer for variable names etc.
*   SNAME    local name buffer
*   SLOCN    local name buffer (without leading "_")
*   STEMP    temporary buffer for titles
*   STEXT    buffer for labels etc.
*   SFORM    format buffer
*   SYMB     symbol buffer
*--- reals:
*   PRMACH fraction of viewport taken by machine plot
*   SYMCH  preset symbol character height
 
      data prmach /0.1/, symch /0.01/
 
*--- check whether main plot bank exists
      if (lpmain .le. 0) goto 80
*--- count number of frames
      nframe = 0
*--- allow max. of two frames (on top of each other)
      do 10 i = 1, 2
        l = lq(lpmain - i)
        if (l .ne. 0) nframe = nframe + 1
   10 continue
   20 if (nframe .eq. 0) goto 999
*--- reset axis and curve defaults
      call gxsdef ('AXIS', 0)
      call gxsdef ('CURVE', 0)
*--- set "new line" character (change default = '/')
      call gxsvar ('SDEFNL', idum, fdum, '%')
*--- set top of viewport - leave space to plot machine if required
      if (fpmach)  then
        yvtop = 1. - prmach
      else
        yvtop = 1.
      endif
*--- set line width scale factor
      call jslwsc (q(lpmain + mplscw))
*--- loop over frames
      do 70 iframe = 1, nframe
*--- set viewport
        vpt(1) = 0.
        vpt(2) = 1.
        vpt(3) = (iframe - 1) * yvtop / nframe
        vpt(4) = iframe * yvtop / nframe
        call gxsvpt (vpt)
*--- get link of frame bank
        lframe = lq(lpmain - iframe)
*--- find variable name in list
        call uhtoc (q(lframe + mpname), mcnam, svar, mcnam)
        call plgetn (1, svar, itbv, idum, sdum, sname)
        slocn = ' '
        call gxpnbl(sname, k1dum, k2dum)
        k3dum = 0
        do 71 idum = k1dum, k2dum
          if (sname(idum:idum) .ne. '_')  then
            k3dum = k3dum + 1
            slocn(k3dum:k3dum) = sname(idum:idum)
          endif
   71   continue
*--- prepare horizontal axis
        do 30 i = 1, 4, 3
          call gxqaxs ('X', i, npar, ipar, range, stext, sform)
*--- set character sizes for labels and text including user requests
          ipar(7)  = max (mlsize * q(lpmain + mplscl) + .01, 1.1)
          ipar(13) = max( mtsize * q(lpmain + mptscl) + .01, 1.1)
*--- text left adjusted
          ipar(10) = 1
*--- font
          ipar(11) = iq(lpmain + mpfont)
*--- axis ref. number
          ipar(21) = 1
*--- range centre etc.
          if (iq(lframe + mpsclf) .le. 0)  then
*--- automatic scaling
            ipar(22) = iq(lframe + mpsclf)
          else
*--- use range as is
            ipar(23) = 0
            range(1) = q(lframe + mpmin)
            range(2) = q(lframe + mpmax)
          endif
          if (i .eq. 1) then
*--- bottom title
            stemp = ' '
            call uhtoc (q(lframe + mpbtit), mtitl, stemp, mtitl)
*--- find first and last non-blank
            call gxpnbl (stemp, kf, kl)
            call tbgnam (ltab, tname)
            if (kf .ne. 0) then
              stext = '<#>' // slocn // '%' // stemp(kf:kl) // '%%' //
     +                'Table name = ' // tname // '%'
            else
              stext= '<#>' // slocn // '%%' // 'Table name = ' //
     +               tname // '%'
            endif
          else
*--- suppress labels on upper axis
            ipar(3) = 0
*--- ticks below axis
            ipar(4) = 1
*--- top title
            call uhtoc (q(lframe + mpttit), mcwrd, stemp, mtitl)
            call gxpnbl (stemp, kf1, kl1)
            kl1 = max (1, kl1)
*--- set top title to local picture title;
*--- if local title is different from main TITLE, add main TITLE
            if (stemp(:kl1) .ne. ctitle) then
              call gxpnbl (ctitle, kf2, kl2)
              stext = stemp(:kl1) // '%' // ctitle(:kl2)
              kl1 = kl1 + kl2 + 1
              stemp = stext
            endif
            call gxpnbl (cvers, kf2, kl2)
            kl2= max (1, kl2)
            stext = stemp(:kl1) // '%'
     +      // cvers(:kl2) // ' version ' //
     +      nvers // '<#>' // cdate // '  ' // ctime
          endif
*--- set axis parameters
          call gxsaxs ('X', i, npar, ipar, range, stext, sform)
   30   continue
*--- set min. and max. for horizontal axis
        xax(1) = q(lframe + mpmin)
        xax(2) = q(lframe + mpmax)
*--- get link to first vertical compound variable
        lvvar = lq(lframe - 1)
        if (lvvar .eq. 0) goto 100
*--- loop over compound variables
        nvax = 0
   40   nvax = nvax + 1
        ivax = iq(lvvar + mpvaxr)
        call gxqaxs ('Y', ivax, npar, ipar, range, stext, sform)
*--- set character sizes for labels and text including user requests
        ipar(7)  = max (mlsize * q(lpmain + mplscl) + .01, 1.1)
        ipar(13) = max (mtsize * q(lpmain + mptscl) + .01, 1.1)
*--- right adjusted label
        ipar(10) = 3
*--- font
        ipar(11) = iq(lpmain + mpfont)
*--- range centre etc.
        if (iq(lvvar + mpsclf) .le. 2)  then
*--- automatic scaling
          ipar(22) = iq(lvvar + mpsclf)
        else
*--- use range as is
          ipar(23) = 0
          range(1) = q(lvvar + mpmin)
          range(2) = q(lvvar + mpmax)
        endif
*--- get axis annotation
        call uhtoc (q(lvvar + mpname), mcwrd, slocn, mtitl)
        stemp = ' '
        call gxpnbl(slocn, k1dum, k2dum)
        k3dum = 0
        do 41 idum = k1dum, k2dum
          if (slocn(idum:idum) .ne. '_')  then
            k3dum = k3dum + 1
            stemp(k3dum:k3dum) = slocn(idum:idum)
          endif
   41   continue
        if (nvax .eq. 1)  then
          stext = '%' // stemp
        else
          stext = stemp
        endif
        call gxsaxs ('Y', ivax, npar, ipar, range, stext, sform)
*--- set curve parameters for frame call
        call gxqcrv (nvax, npar, ipar, symb)
        ipar(2) = ivax
        call gxscrv (nvax, npar, ipar, symb)
*--- store y values for frame scaling
        yax(2 * nvax - 1) = q(lvvar + mpmin)
        yax(2 * nvax)     = q(lvvar + mpmax)
        nptval(nvax)      = 2
        ipxval(nvax)      = 1
        ipyval(nvax)      = 2 * nvax - 1
        icvref(nvax)      = nvax
*--- loop if necessary and possible
        lvvar = lq(lvvar)
        if (lvvar .gt. 0 .and. nvax .lt. 4) goto 40
*--- if only one y axis, plot right axis with ticks only
        if (nvax .eq. 1) then
          ivax = 4
          call gxqaxs ('Y', ivax, npar, ipar, range, stext, sform)
          ipar(2)  = 0
          ipar(3)  = 0
          ipar(4)  = 1
          ipar(21) = 1
          call gxsaxs ('Y', ivax, npar, ipar, range, stext, sform)
        endif
*--- plot frame, keep windows for curves + clipping
        call gxfrm1 (nvax, nptval, ipxval, ipyval, icvref, xax, yax,
     +  window, actwin, ierr)
        if (ierr .ne. 0) goto 110
*--- now loop over vertical variables for real curve plotting
*--- get link to first vertical compound variable
        lvvar = lq(lframe - 1)
*--- loop over compound variables, count them + the curves
        nvax   = 0
        ntcurv = 0
   50   nvax   = nvax + 1
*--- get link to first curve
        lvcurv = lq(lvvar - 1)
        if (lvcurv .eq. 0) goto 120
*--- loop over curves to each vert. variable
   60   ntcurv = ntcurv + 1
*--- find variable name in list for annotation
        call uhtoc (q(lvcurv + mpanno), mcnam, svar, mcnam)
        call plgetn (2, svar, itbv, idum, sdum, sname)
        slocn = ' '
        call gxpnbl(sname, k1dum, k2dum)
        k3dum = 0
        do 61 idum = k1dum, k2dum
          if (sname(idum:idum) .ne. '_')  then
            k3dum = k3dum + 1
            slocn(k3dum:k3dum) = sname(idum:idum)
          endif
   61   continue
*--- bank containing the y values
        lvval = lq(lvcurv - 1)
        if (lvval .eq. 0) goto 130
*--- no. of points to plot
        np = iq(lvval - 1)
*--- ref. link to hor. coordinates
        lhval = lq(lvcurv - 3)
        if (lhval .eq. 0) goto 90
*--- ref. link to hor. sequence index
        lindx = lq(lvcurv - 4)
        if (lindx .eq. 0) goto 90
*--- character height including user request
        chh = 0.001 * masize * q(lpmain + mpascl)
*--- plot symbol
        call uhtoc (q(lvcurv + mpsymb), mcnam, symb, 1)
*--- call curve plot routine with simple arrays and flags
        call plcurv (ntcurv, slocn, chh, q(lpmain + mpascl), symb,
     +  symch * q(lpmain + mpsscl), iq(lvcurv + 1), np, q(lhval+1),
     +  q(lvval + 1), iq(lindx + 1), window(1,nvax), actwin(1,nvax),
     +  ierr)
        if (ierr .ne. 0) goto 140
*--- loop if necessary
        lvcurv = lq(lvcurv)
        if (lvcurv .gt. 0) goto 60
*--- loop if necessary and possible
        lvvar = lq(lvvar)
        if (lvvar .gt. 0 .and. nvax .lt. 4) goto 50
   70 continue
      if (fpmach)  then
        lm1 = lq(lpmain - mpfram - 1)
        lm2 = lq(lpmain - mpfram - 2)
        if (lm1 .ne. 0 .and. lm2 .ne. 0) then
          vpt(1) = 0.
          vpt(2) = 1.
          vpt(3) = yvtop
          vpt(4) = 1.
          call gxsvpt (vpt)
          window(3,1) = -1.
          window(4,1) = 1.
          call gxswnd (window)
          call plschm (iq(lm1 - 1), q(lpmain + mpfelm), iq(lm1 + 1),
     +    q(lm2 + 1), actwin)
        endif
      endif
*--- plot Arnold web if requested
      if (ivnarw .ne. 0)  then
        lqv1 = lq(lpmain - mpfram - 3)
        if (lqv1 .ne. 0)  call plarwe(iq(lqv1+1), actwin(1,ivnarw))
      endif
      goto 999
 
*--- no top graph bank given
   80 continue
*--- no hor. coord. bank
   90 continue
*--- no vert. compound banks
  100 continue
*--- GXFRM1 error
  110 continue
*--- curve for vert. var. missing
  120 continue
*--- y values missing
  130 continue
*--- PLCURV error
  140 continue
  999 end
