      subroutine tbwrit(lbuf, lkey, ipos)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Write out a table line buffer.                                     *
*   Internal routine for table manager, not to be called directly.     *
* Input:                                                               *
*   LBUF(1)   (pointer) Buffer bank to be written.                     *
*   LKEY(1)   (pointer) Key bank.                                      *
*   IPOS      (integer) Position of key in key bank.                   *
*                       If non-zero on input, overwriting occurs.      *
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
      integer ikey,ileng,ipos,iret,nwid
      integer           lbuf(*), lkey(*)
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
 
      character*(mcnam) tnam
 
*---- Write buffer.
      nwid = iq(lbuf(1)-1)
      ikey = iq(lkey(1)+ipos)
      call tboutp(ikey, nwid, iq(lbuf(1)+1), iret)
 
*---- Error message required?
      if (iret .ne. 0) then
        call uhtoc(q(lq(lbuf(1)+1)+mtbnam), mcwrd, tnam, mcnam)
        call utleng(tnam, ileng)
        msg(1)(1:33) = 'Writing "' // tnam(1:ileng) // '",'
        if (iret .eq. 1) then
          msg(1)(34:) = 'Invalid key.'
        else if (iret .eq. 2) then
          msg(1)(34:) = 'Unable to write.'
        else if (iret .eq. 3) then
          msg(1)(34:) = 'Write error.'
        else
          msg(1)(34:) = 'Record length error.'
        endif
        call aafail('TBWRIT', 2, msg)
 
*---- Mark key bank as modified.
      else if (iq(lkey(1)+ipos) .eq. 0) then
        iq(lkey(1)+ipos) = ikey
        call sbit1(iq(lkey(1)),mtbmod)
      endif
 
      end
