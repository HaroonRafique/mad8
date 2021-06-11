      subroutine tbbuff(nbuf)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Lift table buffer banks, organized as a queue.                     *
*   Internal routine for table manager, not to be called directly.     *
* Input:                                                               *
*   LTBSAV    /TBCOMM/  Table manager bank (reference pointer).        *
*   NBUF      (integer) Minimum number of buffers in memory.           *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer iquest
 
*---- ZEBRA system block: Returns system information.
      common /quest/    iquest(100)
      save              /quest/
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
      integer ikey,ileng,jbit,jbuf,last,ltbbky,nbuf,
     +need,nrow,nwid,nwrd,nzbank
 
      character*(mcnam) tnam
 
*---- Drop buffer queue, if it exists, but too small.
      if (jbit(iq(ltbsav),mtbbuf) .ne. 0  .and.
     +    nzbank(0, lq(ltbsav-mtbfst)) .lt. nbuf) then
        call tbdump(ltbsav)
      endif
 
*---- Lift bank for buffer keys and pointers.
      if (jbit(iq(ltbsav),mtbbuf) .eq. 0) then
        nwid = iq(ltbsav+mtbwid)
        nrow = iq(ltbsav+mtbrow)
        call mzbook(2, ltbbky, ltbsav, -mtbbky, 'BKEY', nrow, 0, nrow,
     +              2, 0)
 
*---- If first segment exists, load its keys.
        iq(ltbbky-5) = 1
        ikey = iq(lq(ltbsav-mtbsky)+1)
        if (ikey .ne. 0) call tbread(ltbbky, ikey)
 
*---- Find amount of free space without garbage collect.
        call mzneed(2, 0, ' ')
        nwrd = iquest(11)
 
*---- If space is getting low, try to free some more.
        need = 2 * nrow * (nwid + 10)
        if (need .ge. nwrd) then
          ltbcur = ltable
   10     if (ltbcur .gt. 0) then
            if (jbit(iq(ltbcur),mtbact) .eq. 0) call tbdump(ltbcur)
            ltbcur = lq(ltbcur)
            go to 10
          endif
 
*---- Force garbage collection.
          call mzneed(2, need, 'G')
          nwrd = iquest(11) + need
        endif
 
*---- Memory space insufficient even for minimum number of buffers.
        if (nwrd .lt. nbuf * (nwid + 10)) then
          call uhtoc(q(ltbsav+mtbnam), mcwrd, tnam, mcnam)
          call utleng(tnam, ileng)
          msg(1) = 'Insufficient memory for loading table "'
     +    // tnam(1:ileng) // '".'
          call aafail('TBBUFF', 1, msg)
        else
 
*---- Can we keep complete table in memory?
          if (nwrd .ge. need) then
            last = nrow
 
*---- Otherwise keep only the minimum number of buffers.
          else
            last = nbuf
          endif
 
*---- Build buffer queue.
          call mzbook(2, ltbtmp, ltbsav, -mtbfst, 'BUFF', 0, 0, nwid,
     +                0, 0)
          iq(ltbtmp-5) = 0
          lq(ltbsav-mtblst) = ltbtmp
          do 20 jbuf = 2, last
            call mzbook(2, ltbtmp, ltbsav, -mtbfst, 'BUFF', 0, 0, nwid,
     +                  0, 0)
            iq(ltbtmp-5) = 0
   20     continue
 
*---- Table is ready for use.
          call sbit1(iq(ltbsav), mtbbuf)
        endif
      endif
 
      end
