      subroutine tbseg(ltab, iseg, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Change active segment of a table.                                  *
*   This routine uses the fact that TBWRIT does not call Zebra.        *
* Input:                                                               *
*   LTAB(1)   (pointer) Table manager bank.                            *
*   ISEG      (integer) Number of segment desired.                     *
* Output:                                                              *
*   EFLAG     (logical) Error flag.                                    *
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
      integer ifrst,ikey,ilast,ileng,iseg,jbit
      integer           ltab(*)
      logical           eflag
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
 
      logical           ok
      character*(mcnam) tnam
 
*---- Check validity of table.
      call tbchck('TBSEG ', ltab, ok)
      eflag = .not. ok
      if (ok) then
 
*---- Is table open?
        call uhtoc(q(ltab(1)+mtbnam), mcwrd, tnam, mcnam)
        call utleng(tnam, ileng)
        if (jbit(iq(ltab(1)),mtbact) .eq. 0) then
          msg(1) = 'Table buffer "' // tnam(1:ileng)
     +    // '" is not active.'
          call aafail('TBSEG', 1, msg)
          eflag = .true.
 
*---- Is desired segment in range?
        else if (iseg .le. 0  .or.  iseg .gt. iq(ltab(1)+mtbseg)) then
          write (msg, 910) tnam(1:ileng), iseg
  910     format('Table buffer "',a,'" has no segment no. ',i10)
          call aafail('TBSEG', 1, msg)
          eflag = .true.
 
*---- Is the desired segment already active?
        else if (iq(lq(ltab(1)-mtbbky)-5) .ne. iseg) then
 
*---- Write modified buffers.
          ltbtmp = lq(ltab(1)-mtbfst)
   10     if (ltbtmp .gt. 0) then
            if (jbit(iq(ltbtmp),mtbmod) .ne. 0) then
              call tbwrit(ltbtmp, lq(ltab(1)-mtbbky), iq(ltbtmp-5))
              call sbit0(iq(ltbtmp),mtbmod)
              iq(ltbtmp-5) = 0
            endif
            ltbtmp = lq(ltbtmp)
            go to 10
          endif
 
*---- Write buffer key bank, if modified.
          ltbtmp = lq(ltab(1)-mtbbky)
          if (jbit(iq(ltbtmp),mtbmod) .ne. 0) then
            call tbwrit(ltbtmp, lq(ltab(1)-mtbsky), iq(ltbtmp-5))
          endif
 
*---- Set up buffer key bank for new segement.
          iq(ltbtmp-5) = iseg
          ikey = iq(lq(ltab(1)-mtbsky)+iseg)
          if (ikey .ne. 0) then
            call tbread(ltbtmp, ikey)
          else
            call uzero(q, ltbtmp + 1, ltbtmp + iq(ltab(1)+mtbrow))
          endif
          ifrst = ltbtmp - iq(ltab(1)+mtbrow)
          ilast = ltbtmp - 1
          call uzero(lq, ifrst, ilast)
        endif
      endif
 
      end
