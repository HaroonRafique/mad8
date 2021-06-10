      subroutine plfft
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Replace plot by plot of Fourier spectrum of hvar + i vvar          *
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer i,idum,ivcurv,ivvar,k,l,m,maxpft,mp,n,nm,nselc,nskip,nup,
     +nwarn
      double precision eps,hs,riv,rmax,rrv,stp
      parameter (maxpft = 14, eps = 1.e-20)
      character *(mxlabl) sanno
      character *(mcnam)  sname, svar, sdum
      character *(mtitl)  stitl, stit1, stit2, stit3
      if (lpmain .ne. 0)  then
        nselc = ivpar(6)
        nskip = max(0, ivpar(7))
        do 30 iframe = 1, mpfram
          lframe = lq(lpmain - iframe)
          if (lframe .ne. 0) then
            lvvar = lq(lframe - 1)
            if (lvvar .eq. 0) then
              call aawarn('PLFFT', 1, 'No vertical variable bank.')
              goto 999
            endif
            ivvar = 0
   10       continue
            ivvar = ivvar + 1
            lvcurv = lq(lvvar - 1)
            if (lvcurv .eq. 0) then
              call aawarn('PLFFT', 1, 'No vertical variable bank.')
              goto 999
            endif
            ivcurv = 0
   20       continue
            ivcurv = ivcurv + 1
            lvval = lq(lvcurv - 1)
            if (lvval .eq. 0) then
              call aawarn('PLFFT', 1, 'No vertical variable bank.')
              goto 999
            endif
            lhval = lq(lvcurv - 3)
            if (lhval .eq. 0) then
              call aawarn('PLFFT', 1, 'No horizontal variable bank.')
              goto 999
            endif
            if (iq(lvval-1) .lt. iq(lhval-1))  then
              call aawarn('PLFFT', 1, 'Not enough vertical values.')
              goto 999
            endif
*--- find number of tracking points (ampl**2 > eps)
            do 1  n = 1, iq(lhval-1)
              if ((q(lhval+n)**2 + q(lvval+n)**2) .lt. eps)  goto 2
    1       continue
            n = iq(lhval-1)
    2       continue
            if (n .le. 4) then
              stitl = 'only $$ points (all values may be = zero)'
              write(stitl(6:7), '(I2)') n
              call aawarn('PLFFT', 1, stitl)
            endif
            if (n - nskip .le. 0) then
              stitl = 'skip request ignored: only $$$$$ points.'
              write(stitl(28:32), '(I5)') n
              call aawarn('PLFFT', 1, stitl)
              nskip = 0
            else
              n = n - nskip
            endif
*--- find nearest power of two for FFT
            nwarn = 0
            mp = maxpft
            m = 2**mp
    3       continue
            if (n .gt. m) then
              nwarn = n
              n = m
            elseif (n .lt. m)  then
              m = m / 2
              mp = mp - 1
              goto 3
            endif
            if (nwarn .gt. 0)  then
              stitl = 'no. of points reduced from $$$$$ to $$$$$.'
              write(stitl(28:32), '(I5)') nwarn
              write(stitl(37:41), '(I5)') n
              call aawarn('PLFFT', 1, stitl)
            endif
*--- book bank for temporary storage
            call mzbook(1, ltmp, ltbr, -10, 'TMP ', 0, 0, 2 * n, 0, -1)
            do 4  i = 1, n
              if (nselc .eq. 0)  then
                rrv = q(lhval+nskip+i)
                riv = q(lvval+nskip+i)
              elseif (nselc .eq. 1)  then
                rrv =  q(lhval+nskip+i)
                riv = 0.
              else
                rrv =  q(lvval+nskip+i)
                riv = 0.
              endif
              q(ltmp+2*i-1) = rrv
              q(ltmp+2*i) = riv
    4       continue
            call cfft(q(ltmp+1), mp)
*--- restore, get min. + max.
            stp = 1. / n
            rmax = 0.
            nm = 1
            if (nselc .eq. 0)  then
              nup = n
            else
              nup = max(1, n / 2)
            endif
            do 5  i = 1, nup
              if (i .eq. 1)  then
                q(lhval+i) = 0.2 * stp
              else
                q(lhval+i) = stp * (i - 1)
              endif
              q(lvval+i) = sqrt(q(ltmp+2*i-1)**2 + q(ltmp+2*i)**2)
              if (q(lvval+i) .gt. rmax)  then
                rmax =  q(lvval+i)
                nm = i
              endif
    5       continue
            call mzdrop(0, ltmp, ' ')
            call uhtoc (q(lframe + mpname), mcnam, svar, mcnam)
            call plgetn (1, svar, itbv, idum, sdum, sname)
            call uhtoc (q(lvvar + mpname), mxlabl, sanno, mxlabl)
            call gxpnbl(sname, i, k)
            if (nselc .eq. 0)  then
              stit1 = 'FFT of: ' // sname(:k) // ' + i ' // sanno(4:)
              hs = 1.
            elseif (nselc .eq. 1)  then
              stit1 = 'FFT of: ' // sname
              hs = 0.5
            else
              stit1 = 'FFT of: ' // sanno(4:)
              hs = 0.5
            endif
            stit2 = ' ($$$$$ points)'
            write(stit2(3:7), '(I5)')  n
            stit3 = 'max. at f = '
            write(stit3(13:20), '(F8.4)')  stp * (nm - 1)
            call gxpnbl(stit1, i, k)
            stitl = stit1(:k) // stit2(:15) // '<#>' // stit3
            call uctoh (stitl, q(lframe + mpbtit), mcwrd, mtitl)
            sname = 'norm. frequency'
            call uctoh (sname, q(lframe + mpname), mcwrd, mcnam)
            iq(lhval - 1) = nup
            iq(lvval - 1) = nup
            q(lframe + mpmin) = 0.
            q(lframe + mpmax) = hs
            iq(lframe + mpsclf) = 0
            q(lvvar + mpmin) = 0.
            q(lvvar + mpmax) = rmax
            iq(lvvar + mpsclf) = 0
            sanno = 'amplitude'
            call uctoh (sanno, q(lvvar + mpname), mcwrd, mxlabl)
            l = lvcurv
            iq(l + mpstyl) = 0
            iq(l + mpcolr) = 1
            iq(l + mpspli) = 0
            iq(l + mpbars) = 1
            iq(l + mpsymf) = 0
            sname = ' '
            call uctoh (sname, q(l + mpanno), mcwrd, mcnam)
*--- loop
            lvcurv = lq(lvcurv)
            if (lvcurv .ne. 0) goto 20
            lvvar = lq(lvvar)
            if (lvvar .ne. 0) goto 10
          endif
   30   continue
      endif
  999 end
