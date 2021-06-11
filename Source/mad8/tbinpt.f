      subroutine tbinpt(key, nwid, iwords, iret)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read a record from Cray SSD or direct access file.                 *
*   For usage see TBOUTP.                                              *
* Input:                                                               *
*    KEY          Record identifier (an integer returned by TBOUTP).   *
* Output:                                                              *
*    NWID         Number of words read.                                *
*    IWORDS       The NWID words read.                                 *
*                      -1 = Read, but record length changed.           *
*                 0 = OK.                                              *
*                 1 = Key KEY is not valid.                            *
*                 2 = Package not initialized.                         *
*                 3 = Error occurred during transfer.                  *
*   Author: H. Grote    CERN / LEP                   nov. 14, 1988     *
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
      integer i,ibl,irb,iret,ist,jbyt,k,key,n,nt,
     +nwid,nwin
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
 
*---- Check validity of key.
      iret = 0
      ibl = jbyt(key, 17, 16)
      k = jbyt(key, 1, 16) - 1
      if (key.lt.0 .or. k.gt.mleng .or. ibl.gt.nbout+mnblck) then
        iret = 1
        go to 9999
 
*---- Still in output buffer.
      else
        irb = 1
        if (ibl .gt. nbout) then
          irb = 2
 
*---- Not in input buffer.
        else if (ibl .ne. icurr) then
 
*---- Make sure read buffer is free.
          if (nrbmod .ne. 0) then
            call tbput(itbbuf(1,1), icurr, mleng, iret)
            if (iret .ne. 0) go to 9999
            nrbmod = 0
          endif
 
*---- Load new block to read buffer.
          call tbget(itbbuf(1,1), ibl, mleng, iret)
          if (iret .ne. 0) go to 9999
          icurr = ibl
        endif
      endif
 
*---- Transfer.
      nwin = itbbuf(k+1,irb)
      k = k + 1
      nt = min(nwin,nwid)
      ist = 0
 
   10 continue
      n = min(nt, mleng-k)
 
      do 20 i = 1, n
        iwords(ist+i) = itbbuf(k+i,irb)
   20 continue
 
      nt = nt - n
 
*---- Do we need another block?
      if (nt .gt. 0) then
        ist = ist + n
        ibl = ibl + mnblck
 
*---- Block needed resides in write buffer.
        if (ibl .gt. nbout) then
          irb = 2
 
*---- Make sure read buffer is free.
        else
          if (nrbmod .ne. 0) then
            call tbput(itbbuf(1,1), icurr, mleng, iret)
            if (iret .ne. 0) go to 9999
            nrbmod = 0
          endif
 
*---- Load new block.
          call tbget(itbbuf(1,1), ibl, mleng, iret)
          if (iret .ne. 0) go to 9999
          icurr = ibl
        endif
        k = 0
        go to 10
      endif
 
*---- Check record length.
      if (nwid .ne. nwin) iret = -1
      nwid = min(nwid,nwin)
 
 9999 end
