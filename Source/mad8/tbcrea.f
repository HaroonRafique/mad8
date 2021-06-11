      subroutine tbcrea(tnam, nseg, nrow, ncol, cnam, icfrm, nbuf, ltab)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Create table buffer structure for writing.                         *
* Input:                                                               *
*   TNAM      (char)    Table name.                                    *
*   NSEG      (integer) Number of table segments.                      *
*   NROW      (integer) Number of table rows per segment.              *
*   NCOL      (integer) Number of table columns.                       *
*   CNAM(*)   (char)    Column names.                                  *
*   ICFRM(*)  (integer) Zebra format codes for columns.                *
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
      integer ileng,jcol,l,lcfrm,lcnam,lzlong,nbuf,
     +ncol,nd,nr,nrow,ns,nseg,nwid
      double precision dummy
      character*(mcnam) tnam, cnam(*)
      integer           ltab(*), icfrm(*)
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
 
*---- Page header information.
      common /header/   ctitle, cdate, ctime, nvers, cvers
      save              /header/
      character         ctitle*80, cdate*8, ctime*8, nvers*8, cvers*16
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
 
*---- Look for previous definition.
*     Keep table pointer in common to avoid trouble when booking.
      call uctoh(tnam, iname, mcwrd, mcnam)
      ltbsav = lzlong(0, ltable, mwnam, iname, mtbnam)
      if (ltbsav .ne. 0) then
        call tbdrop(ltbsav)
        call utleng(tnam, ileng)
        msg(1) = 'Table buffer "' // tnam(1:ileng)
     +  // '" already exists --- previous version deleted.'
        call aawarn('TBCREA', 1, msg)
      endif
 
*---- Lift new table banks.
      nr = mtblst
      ns = mtbfst
      call mzbook(2, ltbsav, ltable, 1, 'TAB ', nr, ns, mtbsiz, 7, 0)
      iq(ltbsav+mtbf1)  = 4 * 16 + 2
      iq(ltbsav+mtbseg) = nseg
      iq(ltbsav+mtbrow) = nrow
      iq(ltbsav+mtbcol) = ncol
      iq(ltbsav+mtbf2)  = mwnam * 16 + 5
      call ncopy(iname, iq(ltbsav+mtbnam), mwnam)
 
*---- Store column names.
      nd = mwnam * ncol
      call mzbook(2, lcnam, ltbsav, -mtbcnm, 'CNAM', 0, 0, nd, 5, 0)
      do 10 jcol = 1, ncol
        call uctoh(cnam(jcol), iq(lcnam+1), mcwrd, mcnam)
        lcnam = lcnam + mwnam
   10 continue
 
*---- Store column formats.
      call mzbook(2, lcfrm, ltbsav, -mtbcfm, 'CFRM', 0, 0, ncol, 2, 0)
      call ncopy(icfrm, iq(lcfrm+1), ncol)
 
*---- Store column bias.
      call mzbook(2, l, ltbsav, -mtbcps, 'CPOS', 0, 0, ncol + 1, 2, 0)
      nwid = 0
      do 20 jcol = 1, ncol
 
*---- Integer or single precision real
        if (icfrm(jcol) .le. 3) then
          nwid = nwid + 1
 
*---- Double precision real.
        else if (icfrm(jcol) .eq. 4) then
          nwid = nwid + 2
 
*---- Hollerith.
        else if (icfrm(jcol) .eq. 5) then
          nwid = nwid + mwnam
        endif
        iq(l+jcol+1) = nwid
   20 continue
 
*---- Store buffer length.
      iq(ltbsav+mtbwid) = nwid
 
*---- Lift bank for segment storage keys.
      call mzbook(2, l, ltbsav, -mtbsky, 'SKEY', 0, 0, nseg, 2, 0)
 
*---- Mark table as in use (before lifting buffers, to avoid dumping).
      call sbit1(iq(ltbsav), mtbact)
 
*---- Add standard descriptors.
      call tbpdsc(ltbsav, 'TIME', 5, 0, dummy, ctime)
      call tbpdsc(ltbsav, 'DATE', 5, 0, dummy, cdate)
      call tbpdsc(ltbsav, 'ORIGIN', 5, 0, dummy, 'MAD '//nvers//cvers)
      call tbpdsc(ltbsav, 'COMMENT', 5, 0, dummy, ctitle)
 
*---- Build buffer queue.
      call tbbuff(nbuf)
      ltab(1) = ltbsav
 
      end
