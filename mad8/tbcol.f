      subroutine tbcol(ltab, colnam, iform, ibias)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find bias of a given table column.                                 *
* Input:                                                               *
*   LTAB(1)   (pointer) Pointer to open table.                         *
*   COLNAM    (char)    Name of desired column.                        *
* Output:                                                              *
*   IFORM     (integer) Zebra format code.                             *
*   IBIAS     (integer) Bias of column in buffer banks.                *
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
      integer ibias,iform,jcol,leng,lnam,n
      integer           ltab(*)
      character*(*)     colnam
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
 
      logical           ok
      character*(mcnam) name, temp
 
*---- Check validity.
      call tbchck('TBCOL', ltab, ok)
      iform = 0
      ibias = 0
 
*---- Search for desired column; try unabbreviated form.
      if (ok) then
        lnam = lq(ltab(1)-mtbcnm)
        temp = colnam
        do 10 jcol = 1, iq(ltab(1)+mtbcol)
          call uhtoc(q(lnam+1), mcwrd, name, mcnam)
          if (name .eq. temp) then
            iform = iq(lq(ltab(1)-mtbcfm)+jcol)
            ibias = iq(lq(ltab(1)-mtbcps)+jcol)
            go to 9999
          endif
          lnam = lnam + mwnam
   10   continue
 
*---- COLNAM not found:  Try abbreviations,  N counts ambiguities.
        call utleng(colnam, leng)
        lnam = lq(ltab(1)-mtbcnm)
        n = 0
        do 20 jcol = 1, iq(ltab(1)+mtbcol)
          call uhtoc(q(lnam+1), mcwrd, name, mcnam)
          if (name(1:leng) .eq. colnam) then
            iform = iq(lq(ltab(1)-mtbcfm)+jcol)
            ibias = iq(lq(ltab(1)-mtbcps)+jcol)
            n = n + 1
          endif
          lnam = lnam + mwnam
   20   continue
        if (n .ne. 1) then
          iform = 0
          ibias = 0
        endif
      endif
 
 9999 end
