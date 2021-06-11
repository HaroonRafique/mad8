      subroutine tbgdsc(ltab, dname, iform, ival, rval, sval)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find a descriptor for a table.                                     *
* Input:                                                               *
*   LTAB(1)   (pointer) Table bank pointer.                            *
*   DNAME     (char)    Descriptor name.                               *
* Output:                                                              *
*   IFORM     (integer) Zebra format code.                             *
*   IVAL      (integer) Value to be stored, if integer.                *
*   RVAL      (real)    Value to be stored, if real.                   *
*   SVAL      (char)    Value to be stored, if string or name.         *
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
      integer iform,ival,l,len,ll,lzlong
      double precision rval
      integer           ltab(*)
      character*(*)     dname, sval
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
 
      character*(mcnam) tname
      integer           iname(mwnam)
 
*---- Find descriptor bank.
      ival = 0
      rval = 0.0
      sval = ' '
      tname = dname
      call uctoh(tname, iname, mcwrd, mcnam)
      ltbdsc = lzlong(0, lq(ltab(1)-mtbdsc), mwnam, iname, 2)
      if (ltbdsc .eq. 0) then
        call uhtoc(q(ltab(1)+mtbnam), mcwrd, tname, mcnam)
        call utleng(tname, l)
        msg(1) = 'Table descriptor "' // dname
     +  // '" not found in table "' // tname(1:l) // '".'
        call aawarn('TBGDSC', 1, msg)
      else
        iform = mod(iq(ltbdsc+mwnam+2),16)
 
*---- Integer.
        if (iform .le. 2) then
          ival = iq(ltbdsc+mwnam+3)
 
*---- Single precision real.
        else if (iform .eq. 3) then
          rval = q(ltbdsc+mwnam+3)
 
*---- Double precision real.
        else if (iform .eq. 4) then
          call ucopy(q(ltbdsc+mwnam+3), rval, 2)
 
*---- String or name.
        else if (iform .eq. 5) then
          ll = min(len(sval), mcwrd * (iq(ltbdsc+mwnam+2) / 16))
          call uhtoc(q(ltbdsc+mwnam+3), mcwrd, sval, ll)
        endif
      endif
 
      end
