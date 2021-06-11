      subroutine plschm (nel, start, ityp, slelm, actwin)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Plot schema                                                        *
* Input:                                                               *
*   NEL      (integer)  no. of elements                                *
*   START       (real)  start of first element                         *
*   ITYP     (integer)  array with element types:                      *
*                       0: drift                                       *
*                       1: bend, zero tilt                             *
*                       2: focussing quad                              *
*                       3: defocussing quad                            *
*                       4: monitor                                     *
*                       5: collimator                                  *
*                       6: electrostatic separator                     *
*                       7: bend, non-zero tilt                         *
*                       8: multipole                                   *
*                       9: RF cavity                                   *
*                       10: positive sext                              *
*                       11: negative sext                              *
*                       12: positive oct                               *
*                       13: negative oct                               *
*                       14: lcavity                                    *
*   SLELM       (real)  array with element lengths                     *
*   ACTWIN      (real)  active window for curve plot (array of 4)      *
*----------------------------------------------------------------------*
* Modified: 13-JAN-1999, M. Woodley (SLAC)                             *
*   Add sextupoles and octupoles to machine layout                     *
* Modified: 17-MAR-1999, M. Woodley (SLAC)                             *
*   Add lcavity elements to machine layout                             *
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
      integer i,it,j,mparr,mptp,nel
      double precision spos
      integer              ityp(*)
 
      parameter            (mparr = 88, mptp = 14)
      real                 start, slelm(*), actwin(4)
      real                 shapex(mparr), shapey(mparr)
      real                 txp(2), typ(2)
      integer              npst(mptp), npnd(mptp), npsl(mparr)
 
      data npst   / 1,  6, 11, 16, 21,
     +              33, 43, 48,
     +              50,
     +              64, 69, 74, 79, 84 /
      data npnd   / 5, 10, 15, 20, 32,
     +              42, 47, 49,
     +              63,
     +              68, 73, 78, 83, 88 /
      data npsl   /5 * 1, 5 * 1, 5 * 1, 5 * 3, 5 * 1, 0, 4 * 1, 0, 1,
     +             1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 5 * 1, 2 * 1,
     +             6 * 1, 0, 5 * 1, 0, 1,
     +             5 * 1, 5 * 1, 5 * 1, 5 * 1, 5 * 1 /
      data shapex /0., 1., 1., 0., 0.,
     +             0., 1., 1., 0., 0.,
     +             0., 1., 1., 0., 0.,
     +             0., 1., 1., 0., 0.,
     +             0., 1., 1., 0., 0., 0., 1., 1., 0., 0., 0., 1.,
     +             0., 1., 0.5, 0.5, 0., 1., 0.5, 0.5, 0., 1.,
     +             0., 1., 1., 0., 0.,
     +             0., 0.,
     +             0., 0.25, 0.25, 0.75, 0.75, 1.,
     +             0., 0.25, 0.25, 0.75, 0.75, 1., 0., 1.,
     +             0., 1., 1., 0., 0.,
     +             0., 1., 1., 0., 0.,
     +             0., 1., 1., 0., 0.,
     +             0., 1., 1., 0., 0.,
     +             0., 1., 1., 0., 0. /
      data shapey /0.6, 0.6, -0.6, -0.6, 0.6,
     +             0., 0., 0.8, 0.8, 0.,
     +             0., 0., -0.8, -0.8, 0.,
     +             0.6, 0.6, -0.6, -0.6, 0.6,
     + 0.8, 0.8, 0.4, 0.4, 0.8, -0.8, -0.8, -0.4, -0.4, -0.8, 0., 0.,
     + 0.4, 0.4, 0.8, 0.4, -0.4, -0.4, -0.8, -0.4, 0., 0.,
     + 0.5, 0.5, -0.5, -0.5, 0.5,
     + 0.5, -0.5,
     + 0.2, 0.2, 0.8, 0.8, 0.2, 0.2,
     +-0.2, -0.2, -0.8, -0.8, -0.2, -0.2, 0., 0.,
     +             0., 0., 0.5, 0.5, 0.,
     +             0., 0., -0.5, -0.5, 0.,
     +             0., 0., 0.25, 0.25, 0.,
     +             0., 0., -0.25, -0.25, 0.,
     +             0.2, 0.2, -0.2, -0.2, 0.2 /
 
      spos = start
      do 10 i = 1, nel
        it = ityp(i)
        if (it .eq. 0) then
          txp(1) = spos
          txp(2) = spos + slelm(i)
          typ(1) = 0.
          typ(2) = 0.
          call gvpl (2, txp, typ)
        else
          txp(1) = spos + shapex(npst(it)) * slelm(i)
          typ(1) = shapey(npst(it))
          do 20  j = npst(it)+1, npnd(it)
            txp(2) = spos + shapex(j) * slelm(i)
            typ(2) = shapey(j)
            if (npsl(j) .gt. 0)  then
              call jsln(npsl(j))
              call gvpl(2, txp, typ)
            endif
            txp(1) = txp(2)
            typ(1) = typ(2)
   20     continue
        endif
        spos = spos + slelm(i)
   10 continue
      end
