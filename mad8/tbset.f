      subroutine tbset(ltab, irow, iflag, lbuf)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Make sure that a table line is accessible.                         *
* Input:                                                               *
*   LTAB(1)   (pointer) Table manager bank.                            *
*   IROW      (integer) Number of table line desired.                  *
*   IFLAG     (integer) Operation type flag:                           *
*                       1: read, 2: update, 3: write.                  *
* Output:                                                              *
*   LBUF(1)   (pointer) Buffer bank.                                   *
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
      integer iflag,ifrst,ikey,ilast,ileng,iold,irow,jbit
      integer           ltab(*), lbuf(*)
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
 
      logical           ok
      character*(mcnam) tnam
 
*---- Check validity of table.
      call tbchck('TBSET ', ltab, ok)
      if (ok) then
 
*---- Is table open?
        call uhtoc(q(ltab(1)+mtbnam), mcwrd, tnam, mcnam)
        call utleng(tnam, ileng)
        if (jbit(iq(ltab(1)),mtbact) .eq. 0) then
          msg(1) = 'Table buffer "' // tnam(1:ileng)
     +    // '" is not active.'
          call aafail('TBSET', 1, msg)
          lbuf(1) = 0
 
*---- Is position number in range?
        else if (irow .le. 0  .or.  irow .gt. iq(ltab(1)+mtbrow)) then
          call uhtoc(q(ltab(1)+mtbnam), mcwrd, tnam, mcnam)
          write (msg, 910) tnam(1:ileng), irow
  910     format('Table buffer "',a,'" has no row no. ',i10)
          call aafail('TBSET', 1, msg)
          lbuf(1) = 0
 
*---- Do we have to allocate a buffer?
        else
          lbuf(1) = lq(lq(ltab(1)-mtbbky)-irow)
          if (lbuf(1) .eq. 0) then
 
*---- Does the row exist for read?
            ikey = iq(lq(ltab(1)-mtbbky)+irow)
            if (iflag .ne. 1  .or.  ikey .ne. 0) then
 
*---- Take first buffer in queue and link it to end of queue.
              lbuf(1) = lq(ltab(1)-mtbfst)
              if (lbuf(1) .ne. lq(ltab(1)-mtblst)) then
                call zshunt(0, lbuf, lq(ltab(1)-mtblst), 0, 0)
                lq(ltab(1)-mtblst) = lbuf(1)
              endif
 
*---- If buffer is modified, write it out.
              if (jbit(iq(lbuf(1)),mtbmod) .ne. 0) then
                call tbwrit(lbuf, lq(ltab(1)-mtbbky), iq(lbuf(1)-5))
              endif
 
*---- Relink buffer to proper position in buffer pointer bank.
              iold = iq(lbuf(1)-5)
              if (iold .ne. 0) lq(lq(ltab(1)-mtbbky)-iold) = 0
              lq(lq(ltab(1)-mtbbky)-irow) = lbuf(1)
              iq(lbuf(1)-5) = irow
 
*---- Initialize buffer.
              if (iflag .le. 2  .and.  ikey .ne. 0) then
                call tbread(lbuf, ikey)
              else
                ifrst = lbuf(1) + 1
                ilast = lbuf(1) + iq(lbuf(1)-1)
                call uzero(q, ifrst, ilast)
              endif
            endif
          endif
 
*---- Update or write: Mark buffer as modified.
          if (iflag .ge. 2) then
            call sbit1(iq(lbuf(1)), mtbmod)
 
*---- Write: Clear buffer.
            if (iflag .eq. 3) then
              ifrst = lbuf(1) + 1
              ilast = lbuf(1) + iq(lbuf(1)-1)
              call uzero(q, ifrst, ilast)
            endif
          endif
        endif
      endif
 
      end
