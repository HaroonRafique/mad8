      subroutine exordr
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Order the chained expressions for evaluation in correct order.     *
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
      integer mxf1,mxf2,mxop,mxsiz,mxval
 
*---- Bias for expression banks.
      parameter         (mxf1 = 1, mxop = 2, mxf2 = 3, mxval = 4,
     +                   mxsiz = mwflt + 3)
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
      integer mvattr,mvbank,mvbias,mvf1,mvf2,mvind1,mvind2,mvind3,
     +mvseen,mvsiz
 
*---- Bias for variable reference group.
      parameter         (mvf1   = 1,
     +                   mvbank = 2,
     +                   mvattr = mvbank + mwnam,
     +                   mvf2   = mvattr + mwnam,
     +                   mvseen = mvf2 + 1,
     +                   mvind1 = mvseen + 1,
     +                   mvind2 = mvind1 + 1,
     +                   mvind3 = mvind2 + 1,
     +                   mvbias = mvind3 + 1,
     +                   mvsiz = mvbias)
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
      integer lexbnk,lexexp,lexpar,lexsub,lexvar
 
*---- Local links for expression handler.
      common /exlink/   lexbnk, lexexp, lexpar, lexsub, lexvar
      save              /exlink/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer ibias,iopr,jbit,jdef,jexp,jop,nexp
 
      logical           known
 
*---- Clear markers for defined expressions.
      nexp = iq(lq(lroot-mdexp)+1)
      do 10 jexp = 1, nexp
        lexexp = lq(lq(lroot-mdexp)-jexp)
        call sbit0(iq(lexexp), mxord)
   10 continue
 
*==== Reorder expression directory.
      do 90 jdef = 1, nexp
 
*---- Try to find another expression which can be evaluated.
        do 80 jexp = jdef, nexp
          lexexp = lq(lq(lroot-mdexp)-jexp)
 
*---- Expression should not be deferred.
          known = jbit(iq(lexexp),mxdef) .eq. 0
          if (known) then
 
*---- All operands should be defined previously.
            ibias = 0
            do 50 jop = 1, iq(lexexp-2)
              iopr = iq(lexexp+ibias+mxop)
 
*---- Fetch operation: Get variable reference bank.
              if (iopr .eq. -3  .or.  iopr .eq. -2) then
                lexvar = lq(lexexp-jop)
 
*---- Data bank containing operand
                lexbnk = lq(lexvar-1)
                if (lexbnk .ne. 0) then
 
*---- Is there an expression bank?
                  lexsub = lq(lexbnk-iq(lexvar+mvbias))
                  if (lexsub .ne. 0) then
 
*---- Defined ordinary expression?
                    if (jbit(iq(lexsub),mxord) .eq. 0) then
                      known = .false.
                      go to 60
                    endif
                  endif
                else
                  known = .false.
                  go to 60
                endif
              endif
              ibias = ibias + mxsiz
   50       continue
   60       continue
          endif
 
*---- Exchange this expression with current expression.
          if (known) then
            lq(lq(lroot-mdexp)-jexp) = lq(lq(lroot-mdexp)-jdef)
            lq(lq(lroot-mdexp)-jdef) = lexexp
            call sbit1(iq(lexexp), mxord)
            go to 90
          endif
   80   continue
 
*---- No expression found, quit ordering pass.
        go to 100
   90 continue
      jdef = nexp
 
*==== End of ordering pass.
*     Last expression to be evaluated.
  100 continue
      iq(lq(lroot-mdexp)+2) = jdef
 
*---- Is everything defined?
      do 190 jexp = jdef + 1, nexp
        lexexp = lq(lq(lroot-mdexp)-jexp)
 
*---- If deferred expression, check that operands are defined.
        if (jbit(iq(lexexp),mxdef) .ne. 0) then
          known = .true.
          ibias = 0
          do 150 jop = 1, iq(lexexp-2)
            iopr = iq(lexexp+ibias+mxop)
 
*---- Fetch operation: Get variable reference bank.
            if (iopr .eq. -3  .or.  iopr .eq. -2) then
              lexvar = lq(lexexp-jop)
 
*---- Data bank containing operand.
              lexbnk = lq(lexvar-1)
              if (lexbnk .ne. 0) then
 
*---- Is there an expression bank?
                lexsub = lq(lexbnk-iq(lexvar+mvbias))
                if (lexsub .ne. 0) then
 
*---- Defined ordinary expression?
                  if (jbit(iq(lexsub),mxord) .eq. 0) then
                    known = .false.
                    go to 160
                  endif
                endif
              else
                known = .false.
                go to 160
              endif
            endif
            ibias = ibias + mxsiz
  150     continue
  160     continue
 
*---- Ordinary expression could not be defined.
        else
          known = .false.
        endif
 
*---- Message, if not known.
        if (.not. known) then
          call aafail('EXORDR', 1,
     +    'Unable to define expression (maybe circular definitions?):')
          call exdump(lexexp)
        endif
  190 continue
 
      end
