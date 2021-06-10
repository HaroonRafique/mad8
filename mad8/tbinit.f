      subroutine tbinit(iunit)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initialize table manager.                                          *
* Input.                                                               *
*   IUNIT     (integer) Logical unit for direct access file.           *
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
      integer itbflg,iunit
 
      data itbflg       / 0 /
 
*---- First initialization?
      if (itbflg .eq. 0) then
        call mzlink(0, '/TBCOMM/', ltable, ltbbuf, ltbtmp)
        itbflg = 1
        itbfil = 0
      endif
 
*---- Logical unit number.
      if (iunit .ne. 0) itabun = iunit
 
*---- Reset various counters.
      koff = 0
      nblock = 0
      icurr = - 1
      nbout = - 1
      ncmax = 0
      nrbmod = 0
 
      end
