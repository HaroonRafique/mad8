      subroutine plarwe (ibk, actwin)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Plots Arnold web                                                   *
*                                                                      *
*--- Input                                                             *
*   IBK       (integer) array containing:                              *
*                       number of superperiods                         *
*                       number of constraints  N                       *
*                       N constraints:                                 *
*                       minimum                                        *
*                       maximum                                        *
*                       step                                           *
*                       Length L                                       *
*                       expression in polish notation of length L,     *
*                       coded as 1+, 2-, 3*, 4/, 1 KX, 2 KY, 3 KS      *
*                       + MQADD + 4, e.g. 100006 = KY                  *
*   ACTWIN       (real) active user window (WC)                        *
*                                                                      *
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
      integer ierr,ifp,ip,ipm,ipp,iselct,j,k1,k2,k3,kax,kay,klwid,ksmax,
     +kx,ky
      integer    ibk(*)
      real       actwin(4)
      integer    k(3), l(2), lstyl(0:3)
      real       s, qs, v1, v2, vm, vp, sclwid
      real       ql(2), qu(2), x(2), y(2)
      data lstyl / 1, 2, 4, 3 /
 
*--- set flag for first error message (following suppressed)
      ifp = 0
      if (haxis(:2) .eq. 'QX')  then
        kx = 1
      else
        kx = 2
      endif
      ky  = 3 - kx
      kax = 2 * kx -1
      kay = 2 * ky -1
      ql(kx) = actwin(kax)
      qu(kx) = actwin(kax+1)
      ql(ky) = actwin(kay)
      qu(ky) = actwin(kay+1)
      qs = qsval
      s   = ibk(1)
      if (qs .eq. 0.)  then
        ksmax = 0
      else
        ksmax = mksmax
      endif
      do 10  k1 = -ntmax, ntmax
        k(kx) = k1
        l(kx) = ntmax - abs(k1)
        do 20  k2 = -l(kx), l(kx)
          if (k1 .ne. 0 .or. k2 .ne. 0)  then
            k(ky) = k2
            l(ky) = min(l(kx) - abs(k2), ksmax)
            do 30  k3 = -l(ky), l(ky)
              k(3) = k3
              call plqcon(kx, ky, k, ibk, iselct, ierr)
              if (iselct .ne. 0 .or. ierr .ne. 0)  then
                call jsln(lstyl(min(abs(k3),3)))
                if (ierr .ne. 0 .and. ifp .eq. 0)  then
                  ifp = 1
                  call aawarn ('PLARWE', 1,
     +            'Illegal constraint --- all constraints ignored.')
                endif
*--- set line width scale factor
                klwid = max(1, 6 - abs(k1) - abs(k2))
                sclwid = klwid
                call jslwsc(sclwid)
                if (k1*k2 .lt. 0)  then
                  v1 = k(kx) * ql(kx) + k(ky) * qu(ky) + k3 * qs
                  v2 = k(kx) * qu(kx) + k(ky) * ql(ky) + k3 * qs
                else
                  v1 = k(kx) * qu(kx) + k(ky) * qu(ky) + k3 * qs
                  v2 = k(kx) * ql(kx) + k(ky) * ql(ky) + k3 * qs
                endif
                vm = min(v1, v2)
                vp = max(v1, v2)
                ipm = vm / s + 0.499 * (1. + sign(1., vm))
                ipp = vp / s - 0.499 * (1. - sign(1., vm))
                do 40  ip = max(0, ipm), ipp
                  if (k(2) .eq. 0)  then
*--- vertical line
                    y(1) = ql(2)
                    y(2) = qu(2)
                    x(1) = (ip * s - k3 * qs) / k(1)
                    x(2) = x(1)
                  else
                    x(1) = ql(1)
                    x(2) = qu(1)
                    y(1) = (ip * s - k(1) * ql(1) - k3 * qs) / k(2)
                    y(2) = (ip * s - k(1) * qu(1) - k3 * qs) / k(2)
                    if (k(1) .ne. 0)  then
                      do 50  j = 1, 2
                        if (y(j) .lt. ql(2))  then
                          x(j) = (ip * s - k(2) * ql(2) - k3 * qs)
     +                           / k(1)
                          y(j) = ql(2)
                        elseif (y(j) .gt. qu(2))  then
                          x(j) = (ip * s - k(2) * qu(2) - k3 * qs)
     +                           / k(1)
                          y(j) = qu(2)
                        endif
   50                 continue
                    endif
                  endif
                  call gvpl(2,x,y)
   40           continue
              endif
   30       continue
          endif
   20   continue
   10 continue
*--- reset line width scale factor
      call jslwsc(1.)
      end
