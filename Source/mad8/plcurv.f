      subroutine plcurv (ncc, sname, annh, usex, symb, sych, ippar, np,
     +xval, yval, indx, window, actwin, ierr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Plot one curve                                                     *
* Input:                                                               *
*   NCC      (integer)  current curve count (1,2, etc.)                *
*   SNAME    (char)     curve annotation string                        *
*   ANNH     (real)     character height                               *
*   USEX     (real)     user character height expansion                *
*   SYMB     (char)     plot symbol if code = 200                      *
*   SYCH     (real)     symbol character height                        *
*   IPPAR    (integer)  array containing the plot parameters           *
*   NP       (integer)  no. of points to plot                          *
*   XVAL     (real)     x values                                       *
*   YVAL     (real)     y values                                       *
*   INDX     (integer)  sequence index for x values                    *
*   WINDOW   (real)     array containing the window to use             *
*   ACTWIN   (real)     active (inside frame) window                   *
* Output:                                                              *
*   IERR     (integer)  0 if OK, else GXPLOT error                     *
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
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
      integer icolr,iecub,ierr,ilb,ipbar,ispli,istyl,isymb,j,kact,kf,
     +kft,kl,klt,maxpnt,ncc,np,npt,irow,kkt
      double precision xmd
      parameter            (maxpnt = 500)
 
      real                 annh, sych, xval(*), yval(*), window(*)
      real                 actwin(*), gxcubv, usex
      real                 xpos, ypos, xwpos, ywpos, xf, wsclx, wscly
      real                 xaux, yaux
      real                 rsave(20), act(4), xpl(2), ypl(2), winnor(4)
      real                 xreal(maxpnt), yreal(maxpnt), yy1d(maxpnt)
      real                 yy2d(maxpnt)
 
      integer              ippar(*), indx(*)
      integer              isave(20)
 
      character         sname*(*), symb*1, symloc*1
 
      save                 act
 
      data winnor /0., 1., 0., 1./
 
*--- save GKS settings
      call gxsave (isave, rsave, ierr)
      call gxswnd (window)
      wsclx = 1. / (window(2) - window(1))
      wscly = 1. / (window(4) - window(3))
      xmd = 1.e-8 * (window(2) - window(1))**2
      if (ncc .eq. 1)  then
*--- first curve in frame - reset label position array, get act.
*    window in NDC
        act(1) = (actwin(1) - window(1)) * wsclx
        act(2) = (actwin(2) - window(1)) * wsclx
        act(3) = (actwin(3) - window(3)) * wscly
        act(4) = (actwin(4) - window(3)) * wscly
      endif
      istyl = ippar(mpstyl)
      ipbar = ippar(mpbars)
      isymb = ippar(mpsymf)
      icolr = ippar(mpcolr)
      if(icolr .eq. 100)  icolr = mod(ncc-1,6) + 1
      icolr = max(1, min(icolr, 7))
      ilb = -1
      if (istyl .ne. 0)  then
*--- polyline requested
        if (np .lt. 2) goto 999
        if(istyl .eq. 100)  istyl = mod(ncc-1,4) + 1
        ispli = ippar(mpspli)
*--- get first and last blank in annotation
        call gxpnbl (sname, kft, klt)
        if (kft .ne. 0) then
*--- annotation exists
          ilb    = 0
        endif
*--- set line style
        call jsln (max (1, min (4, istyl)))
*--- set line colour
        call jsplci(icolr)
        kact = 1
    1   continue
*--- get first and last point inside
        call pliact(kact, np, indx, xval, yval, actwin, kf, kl)
*--- quit if no points inside
        if (kf .eq. 0)  goto 21
        kf = max(1, kf - 1)
        kl = min(np, kl + 1)
        npt      = 1
        xreal(1) = xval(indx(kf))
        yreal(1) = yval(indx(kf))
        do 10 j = kf + 1, kl
*--- avoid identical points
          if ((xreal(npt) - xval(indx(j)))**2 +
     +    (yreal(npt) - yval(indx(j)))**2 .gt. xmd) then
            npt        = npt + 1
            xreal(npt) = xval(indx(j))
            yreal(npt) = yval(indx(j))
          endif
          if ((j .eq. kl .and. npt .ge. 2) .or. npt .eq. maxpnt) then
*--- plot - get first curve annotation position
            if (ilb .eq. 0)
     +      call plgacn(ncc, window, act, xreal, yreal, npt, usex,
     +      xwpos, xpos, ypos, ilb)
            if (npt .eq. 2 .or. ispli .eq. 0) then
*--- no spline
              call gxpl (npt, xreal, yreal, actwin)
              if (ilb .gt. 0) then
*--- get y pos. on curve for label
                ywpos = yreal(ilb - 1) + (yreal(ilb) - yreal(ilb-1))
     +          * (xwpos  - xreal(ilb - 1))
     +          / (xreal(ilb) - xreal(ilb - 1))
                ilb = -2
              endif
            else
*--- spline
              call gxplt1 (npt, xreal, yreal, actwin)
              if (ilb .gt. 0) then
*--- get y pos. on curve for label
                call gxcubi (npt, xreal, yreal, yy1d, yy2d, iecub)
                ywpos = gxcubv (xwpos, npt, xreal, yreal, yy1d, yy2d)
                ilb   = -2
              endif
            endif
            xreal(1) = xreal(npt)
            yreal(1) = yreal(npt)
            npt = 1
          endif
   10   continue
        if (kl .lt. np)  then
          kact = kl + 1
          goto 1
        endif
      else
*--- no polyline
        if (np .eq. 0) goto 999
      endif
*--- plot symbols or bars if requested
      if (ipbar .ne. 0)  then
        call jsln (1)
*--- set line colour
        call jsplci(icolr)
        do 20 j = 1, np
          xpl(1) = xval(indx(j))
          xpl(2) = xval(indx(j))
          ypl(1) = yval(indx(j))
          ypl(2) = actwin(3)
          call gvpl (2, xpl, ypl)
   20   continue
      endif
   21 continue
      if (isymb .ne. 0)  then
*--- set marker colour
        call jspmci(icolr)
        if (isymb .le. 5)  then
          call jsmk (isymb)
          if (iq(ltbr+3) .eq. 0)  then
*--- set colour per particle
            kkt = iq(ltbr+2)
            do irow = 1, iq(ltbr+1)
              if (ippar(mpcolr) .eq. 100) icolr = mod(irow-1,6) + 1
              call jspmci(icolr)
              call gxpmsw (kkt, xval((irow-1)*kkt+1),
     +        yval((irow-1)*kkt+1), actwin)
            enddo
          else
            call gxpmsw (np, xval, yval, actwin)
          endif
        elseif (isymb .eq. 100 .or. isymb .eq. 200)  then
          if (istyl .ne. 0)  then
            if (isymb .eq. 100)  then
*--- use current curve count
              write (symloc, '(I1)')  mod (ncc, 10)
            else
              symloc= symb
            endif
          endif
*--- plot one character symbol
*    switch to normalized window
          call gxswnd (winnor)
*--- set character height
          call jschh (sych)
*--- text alignment
          call jstxal (2, 3)
*--- text expansion factor - mind distorted viewports
          call gxqrvp (xf)
          call jschxp (xf)
          do 30 j = 1, np
            if (isymb .eq. 100 .and. istyl .eq. 0)  then
*--- use current point number
              write (symloc, '(I1)')  mod (indx(j), 10)
            endif
            xaux = wsclx * (xval(indx(j)) - window(1))
            yaux = wscly * (yval(indx(j)) - window(3))
            if (xaux .gt. act(1) .and. xaux .lt. act(2)
     +      .and. yaux .gt. act(3) .and. yaux .lt. act(4))
     +      call gxtx (xaux, yaux, symloc)
 
   30     continue
        endif
      endif
      if (ilb .eq. -2)  then
*--- plot annotation
*    switch to normalized window
        call gxswnd (winnor)
*--- set character height
        call jschh (annh)
*--- text alignment
        call jstxal (2, 5)
*--- text expansion factor - mind distorted viewports
        call gxqrvp (xf)
        call jschxp (xf)
*--- set marker colour
          call jstxci(icolr)
*--- plot annotation string
        call gxtx (xpos, ypos, sname(kft:klt))
*--- connect to curve
        xpl(1) = xpos
        xpl(2) = xpos
        ypl(1) = ypos
        ypl(2) = (ywpos - window(3)) * wscly
        if (ypl(2) .gt. ypl(1))  ypl(1) = ypl(1) + .02
*--- set dotted line
        call jsln (3)
*--- set line colour
        call jsplci(icolr)
*--- plot line
        call gxpl (2, xpl, ypl, act)
      endif
*--- restore
      call gxrest (isave, rsave)
  999 end
