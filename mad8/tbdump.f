      subroutine tbdump(ltab)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Dump table structure and release space used.                       *
* Input:                                                               *
*   LTAB(1)   (pointer) Table manager bank (reference pointer).        *
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
      integer jbit
      integer           ltab(*)
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
      integer icurr,itabun,itbbuf,itbfil,itbspc,koff,ltable,ltbbuf,
     +ltbcol,ltbcur,ltbdsc,ltbsav,ltbspc,ltbsum,ltbtab,ltbtmp,nblock,
     +nbout,ncmax,nrbmod
 
*---- Communication area for table manager routines.
      integer mleng,mnblck,mstep
      parameter         (mnblck=10, mleng=512*mnblck, mstep=100)
      common /tbcomm/   ltable, ltbcol, ltbsum,
     +                  ltbbuf, ltbspc, ltbdsc, ltbtab, ltbcur, ltbsav,
     +                  ltbtmp,
     +                  nblock, nbout, nrbmod, icurr, ncmax, itbspc,
     +                  koff, itbfil, itabun, itbbuf(mleng,2)
      save              /tbcomm/
      integer mtbact,mtbbky,mtbbuf,mtbcfm,mtbcnm,mtbcol,mtbcps,mtbdsc,
     +mtbf1,mtbf2,mtbfst,mtblst,mtbmod,mtbnam,mtbrow,mtbseg,mtbsiz,
     +mtbsky,mtbwid
 
*---- Parameters for table manager bank structure.
      parameter         (mtbact = 1, mtbbuf = 2, mtbmod = 1)
      parameter         (mtbf1  = 1,
     +                   mtbseg = 2, mtbrow = 3, mtbcol = 4, mtbwid = 5,
     +                   mtbf2  = 6,
     +                   mtbnam = 7, mtbsiz = mtbnam + mwnam - 1)
      parameter         (mtbsky = 2, mtbbky = 3, mtbcnm = 4,
     +                   mtbcfm = 5, mtbcps = 6, mtbdsc = 7,
     +                   mtbfst = 8, mtblst = 9)
 
      logical           ok
 
*---- Check validity of table.
      call tbchck('TBDUMP', ltab, ok)
      if (ok) then
 
*---- Write modified buffers.
        ltbtmp = lq(ltab(1)-mtbfst)
   10   if (ltbtmp .gt. 0) then
          if (jbit(iq(ltbtmp),mtbmod) .ne. 0) then
            call tbwrit(ltbtmp, lq(ltab(1)-mtbbky), iq(ltbtmp-5))
          endif
          ltbtmp = lq(ltbtmp)
          go to 10
        endif
 
*---- Write buffer key bank, if modified.
        ltbtmp = lq(ltab(1)-mtbbky)
        if (jbit(iq(ltbtmp),mtbmod) .ne. 0) then
          call tbwrit(ltbtmp, lq(ltab(1)-mtbsky), iq(ltbtmp-5))
        endif
 
*---- Drop buffer queue and buffer key bank.
        lq(ltab(1)-mtblst) = 0
        call mzdrop(0, lq(ltab(1)-mtbfst), 'L')
        call mzdrop(0, lq(ltab(1)-mtbbky), '.')
 
*---- Table has no buffers.
        call sbit0(iq(ltab(1)), mtbbuf)
      endif
 
      end
