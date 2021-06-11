      subroutine aadrop(lbank)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Drop a data bank.                                                  *
*   Clean out expression and variable directories.                     *
* Input:                                                               *
*   LBANK(1) (pointer)  Pointer to bank to be dropped.                 *
* Output:                                                              *
*   LBANK(1) (pointer)  Zero.                                          *
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
      integer jbit,jexp,jlast,jvar,lexp,lvar,nexp,nvar
      integer           lbank(*)
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
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
 
*---- Mark data structure to be dropped.
      if (lbank(1) .eq. 0) go to 9999
      if (jbit(iq(lbank(1)),mxcls) .ne. 0) go to 9999
      call mzflag(0, lbank, mxdrp, '.')
 
*---- Remove marked expression banks.
*     Warning: LEXP is local link. No Zebra calls allowed in DO loop!
      nexp = iq(lq(lroot-mdexp)+1)
      jlast = 0
      do 90 jexp = 1, nexp
        lexp = lq(lq(lroot-mdexp)-jexp)
        if (jbit(iq(lexp),mxdrp) .eq. 0) then
          jlast = jlast + 1
          lq(lq(lroot-mdexp)-jlast) = lexp
        endif
   90 continue
      if (jlast .lt. nexp) then
        iq(lq(lroot-mdexp)+1) = jlast
        iq(lq(lroot-mdexp)+2) = 0
      endif
 
*---- Remove marked variable references.
*     Warning: LVAR is local link. No Zebra calls allowed in DO loop!
      nvar = iq(lq(lroot-mdvar)+1)
      jlast = 0
      do 190 jvar = 1, nvar
        lvar = lq(lq(lroot-mdvar)-jvar)
        if (jbit(iq(lvar),mxdrp) .eq. 0) then
          jlast = jlast + 1
          lq(lq(lroot-mdvar)-jlast) = lvar
        endif
  190 continue
      if (jlast .lt. nvar) then
        iq(lq(lroot-mdvar)+1) = jlast
      endif
 
*---- Now drop data structure.
      call mzdrop(0, lbank, '.')
      lbank(1) = 0
 
 9999 end
