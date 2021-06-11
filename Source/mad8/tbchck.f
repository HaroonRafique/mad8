      subroutine tbchck(rnam, ltab, ok)
      implicit none
 
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Check validity of a table pointer.                                 *
*   Internal routine for table manager, not to be called directly.     *
* Input:                                                               *
*   RNAM      (char*6)  Name of calling routine.                       *
*   LTAB(1)   (pointer) Table pointer to be checked.                   *
* Output:                                                              *
*   OK        (logical) Flag for success.                              *
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
      character*6       rnam
      integer           ltab(*)
      logical           ok
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
 
      character*4       bank
 
*---- Is pointer in range?
      ok = .false.
      if (ltab(1) .le. 0  .or.  ltab(1) .gt. mwflt*memlen) then
        write (msg, 910) ltab(1)
        call aawarn(rnam, 1, msg)
 
*---- Is LTAB a valid table bank?
      else
        call uhtoc(q(ltab(1)-4), mcwrd, bank, 4)
        if (bank .eq. 'TAB ') then
          ok = .true.
        else
          write (msg, 910) ltab(1)
          call aawarn(rnam, 1, msg)
        endif
      endif
 
  910 format('Invalid table bank pointer',i10,'.')
 
      end
