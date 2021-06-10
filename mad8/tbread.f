      subroutine tbread(lbuf, ikey)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read a table buffer.                                               *
*   Internal routine for table manager, not to be called directly.     *
* Input:                                                               *
*   LBUF(1)   (pointer) Buffer bank to be loaded.                      *
*   IKEY      (integer) Key for data retrieval.                        *
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
      integer ifrst,ikey,ilast,ileng,iret,nwid
      integer           lbuf(*)
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
 
*---- Record exists --- read.
      nwid = iq(lbuf(1)-1)
      if (ikey .ne. 0) then
        call tbinpt(ikey, nwid, iq(lbuf(1)+1), iret)
        if (iret .eq. 0) then
          call sbit0(iq(lbuf(1)), mtbmod)
        else
          call uhtoc(q(lq(lbuf(1)+1)+mtbnam), mcwrd, tnam, mcnam)
          call utleng(tnam, ileng)
          msg(1)(1:34) = 'Reading table "' // tnam(1:ileng) // '",'
          if (iret .eq. 1) then
            msg(1)(35:) = 'Unknown key.'
          else if (iret .eq. 2) then
            msg(1)(35:) = 'Unable to read.'
          else if (iret .eq. 3) then
            msg(1)(35:) = 'Read error.'
          else
            msg(1)(35:) = 'Record length error.'
          endif
          call aafail('TBREAD', 1, msg)
        endif
 
*---- No record exists --- clear.
      else
        ifrst = lbuf(1) + 1
        ilast = lbuf(1) + nwid
        call uzero(q, ifrst, ilast)
      endif
 
      end
