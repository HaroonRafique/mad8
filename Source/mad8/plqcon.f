      subroutine plqcon (kx, ky, kxys, ibk, iselct, ierr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Checks user constraints on Arnold web                              *
*                                                                      *
*--- Input                                                             *
*   KX        (integer) position of K_x in KXYS                        *
*   KY        (integer) position of K_y in KXYS                        *
*   KXYS      (integer) array containing K_x at KX, K_y at KY, KS at 3 *
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
*--- Output                                                            *
*   ISELCT    (integer) 1 if combination of KX, KY, KS accepted,       *
*                       0 if not accepted                              *
*   IERR                0 if OK, 1 if expression illegal, 2 if / 0     *
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
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
      integer i,ic,ierr,ipt,iselct,k,kc,kop,kx,ky,low,lsp,lup,mqadd4,
     +nstack,nw
      integer    ibk(*), kxys(3)
      integer    kloc(3), istack(100)
      ierr = 0
      kloc(1) = kx
      kloc(2) = ky
      kloc(3) = 3
      iselct = 1
      ipt = 2
      mqadd4 = mqadd + 4
      do 10  ic = 1, ibk(2)
        low = ibk(ipt+1)
        lup = ibk(ipt+2)
        lsp = max(1, ibk(ipt+3))
        nw  = ibk(ipt+4)
        ipt = ipt + 4
*--- calculate expression given in inverse Polish
        nstack = 0
        do 20  i = 1, nw
          kc = ibk(ipt+i)
          if (kc .le. mqadd)  then
*--- simple integer
            nstack = nstack + 1
            istack(nstack) = kc
          elseif (kc .gt. mqadd4)  then
*--- K_x, K_y, or K_s
            k = kc - mqadd4
            nstack = nstack + 1
            istack(nstack) = kxys(kloc(k))
          else
*--- operator
            if (nstack .lt. 2)  then
              ierr = 1
              goto 999
            endif
            kop = kc - mqadd
            if (kop .eq. 1)  then
              istack(nstack-1) =
     +        istack(nstack-1) + istack(nstack)
            elseif (kop .eq. 2)  then
              istack(nstack-1) =
     +        istack(nstack-1) - istack(nstack)
            elseif (kop .eq. 3)  then
              istack(nstack-1) =
     +        istack(nstack-1) * istack(nstack)
            elseif (kop .eq. 4)  then
              if (istack(nstack) .eq. 0)  then
                ierr = 2
                goto 999
              endif
              istack(nstack-1) =
     +        istack(nstack-1) / istack(nstack)
            endif
            nstack = nstack - 1
          endif
   20   continue
        if (nstack .ne. 1)  then
          ierr = 1
          goto 999
        endif
        do 30  i = low, lup, lsp
          if (istack(1) .eq. i)  goto 40
   30   continue
*--- test failed
        iselct = 0
        goto 999
   40   ipt = ipt + nw
   10 continue
  999 end
