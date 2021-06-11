      subroutine plgaxn (nax, vax, sax, ns)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Returns compound vertical axis annotation                          *
*                                                                      *
*--- Input                                                             *
*   NAX       (integer) no. of vert. var. names in VAX                 *
*   VAX          (char) vert. var. names                               *
*---Output                                                             *
*   SAX          (char) remaining (possibly truncated) names           *
*   NS        (integer) no. of names in SAX                            *
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
      integer i,index,j,k,k1,k1f,k2,k2f,nax,ns
      character*(mcnam) vax(*), sax(*), scut, saloc
 
      ns = 0
      if (nax .le. 0)  then
        sax(1) = ' '
      else
        do 10  i = 1, nax
          saloc = vax(i)
          call gxpnbl(saloc, k1, k2)
          if (k2 .gt. 1 .and. index('XY', saloc(k2:k2)) .ne. 0)  then
            scut = saloc(:k2-1)
            do 30  j = 1, ns
               if (scut .eq. sax(j))  goto 10
   30       continue
            do 20 j = i + 1, nax
              call gxpnbl(vax(j), k1f, k2f)
              if (k2 .eq. k2f)  then
                if (index('XY', vax(j)(k2:k2)) .ne. 0)  then
                  if (saloc(:k2-1) .eq. vax(j)(:k2-1))  then
                    saloc = scut
                    do 40  k = 1, ns
                      if (saloc .eq. sax(k))  goto 10
   40               continue
                  endif
                endif
              endif
   20       continue
          endif
          ns      = ns + 1
          sax(ns) = saloc
   10   continue
      endif
      end
