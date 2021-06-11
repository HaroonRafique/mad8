      subroutine tbpdsc(ltab, dname, iform, ival, rval, sval)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Add a descriptor to a table.                                       *
* Input:                                                               *
*   LTAB(1)   (pointer) Table bank pointer.                            *
*   DNAME     (char)    Descriptor name.                               *
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
      integer iform,ileng,ival,nd,nword
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
 
      character*100     tval
      character*(mcnam) tname
      logical           ok
 
*---- Check for valid table.
      call tbchck('TBPDSC', ltab, ok)
      if (ok) then
 
*---- Integer or bit string.
        if (iform .le. 2) then
          nd = mwnam + 3
          call mzbook(2, ltbdsc, ltab, -mtbdsc, 'DESC', 0, 0, nd, 7, 0)
          iq(ltbdsc+mwnam+2) = 16 * 1 + iform
          iq(ltbdsc+mwnam+3) = ival
 
*---- Single precision real.
        else if (iform .eq. 3) then
          nd = mwnam + 3
          call mzbook(2, ltbdsc, ltab, -mtbdsc, 'DESC', 0, 0, nd, 7, 0)
          iq(ltbdsc+mwnam+2) = 16 * 1 + 3
          q(ltbdsc+mwnam+3) = rval
 
*---- Double precision real.
        else if (iform .eq. 4) then
          nd = mwnam + 4
          call mzbook(2, ltbdsc, ltab, -mtbdsc, 'DESC', 0, 0, nd, 7, 0)
          iq(ltbdsc+mwnam+2) = 16 * 2 + 4
          call ucopy(rval, q(ltbdsc+mwnam+3), 2)
 
*---- String or name.
        else if (iform .eq. 5) then
          tval = sval
          call utleng(tval, ileng)
          nword = max((ileng - 1) / mcwrd, 0) + 1
          ileng = mcwrd * nword
          nd = mwnam + 2 + nword
          call mzbook(2, ltbdsc, ltab, -mtbdsc, 'DESC', 0, 0, nd, 7, 0)
          iq(ltbdsc+mwnam+2) = 16 * nword + 5
          call uctoh(tval, iq(ltbdsc+mwnam+3), mcwrd, ileng)
        endif
 
*---- Store name and type.
        iq(ltbdsc+1) = 16 * mwnam + 5
        tname = dname
        call uctoh(tname, iq(ltbdsc+2), mcwrd, mcnam)
      endif
 
      end
