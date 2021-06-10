      subroutine tbfile
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Open direct-access file for table manager.                         *
*   Machine dependent routine, internal to table manager.              *
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
      integer irecl
 
      character*(mcfil) ftabl
 
      irecl = 4 * 512
*WNT  IRECL = 512
      irecl = 4 * 512 ! *AIX
      call flnset('table', ftabl)
      open (unit = itabun, file = ftabl, access = 'DIRECT',
     +      status = 'UNKNOWN', recl = irecl)
      itbfil = 123456
 
      end
