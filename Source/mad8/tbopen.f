      subroutine tbopen(tnam, nbuf, ltab)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Re-open table buffer structure for reading or updating.            *
* Input:                                                               *
*   TNAM      (char)    Table name.                                    *
*   NBUF      (integer) Minimum number of buffers in memory.           *
* Output:                                                              *
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
      integer ileng,lzlong,nbuf
      character*(mcnam) tnam
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
 
      integer           iname(mwnam)
 
*---- Blank name returns last table created.
*     Keep table pointer in common to avoid trouble when booking.
      if (tnam .eq. ' ') then
        ltbsav = ltable
        call uhtoc(q(ltbsav+mtbnam), mcwrd, tnam, mcnam)
 
*---- Search for definition.
      else
        call uctoh(tnam, iname, mcwrd, mcnam)
        ltbsav = lzlong(0, ltable, mwnam, iname, mtbnam)
      endif
 
*---- Message when no table available.
      if (ltbsav .eq. 0) then
        call utleng(tnam, ileng)
        msg(1) = 'Table "' // tnam(1:ileng) // '" not found.'
        call aafail('TBOPEN', 1, msg)
 
*---- Mark table as in use and create buffer queue.
      else
        call sbit1(iq(ltbsav), mtbact)
        call tbbuff(nbuf)
      endif
      ltab(1) = ltbsav
 
      end
