      subroutine tbget(iwords, nbl, nwid, iret)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read a block from Cray SSD or direct access file.                  *
*   Machine dependent routine.                                         *
* Input:                                                               *
*    IWORDS(*)    Block to be read.                                    *
*    NBL          Block number to read.                                *
*    NWID         Number of words to read.                             *
* Output:                                                              *
*    IRET         Return code:                                         *
*                 0 = OK                                               *
*                 3 = Error occurred during read.                      *
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
      integer i,iret,istat,j,k,nbl,nwid
      integer           iwords(*)
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
 
*---- Open direct access file.
      if (itbfil .eq. 0) call tbfile
 
*---- Read block.
      i = nbl
      do 10  j = 1, nwid, 512
        i = i + 1
        read (itabun, rec=i, iostat=istat) (iwords(k), k = j, j+511)
   10 continue
 
*---- Test for error.
      iret = 0
      if (istat .ne. 0) iret = 3
 
      end
