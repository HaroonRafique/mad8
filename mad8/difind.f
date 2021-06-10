      subroutine difind(ldir, label, idir, lbank)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Return bank pointer stored for LABEL in directory structure LDIR.  *
*   If LABEL is less than MCNAM characters, it may be abbreviation.    *
* Input:                                                               *
*   LDIR(4)  (pointer)  Directory links.                               *
*   LABEL    (char)     Label to be found.                             *
* Output:                                                              *
*   IDIR     (integer)  Directory index.                               *
*   LBANK(1) (pointer)  Pointer to data bank.                          *
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
      integer idir,index
      integer           ldir(4), lbank(*)
      character*(*)     label
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
 
*---- Return directory index.
      call dilook(ldir, label, index, idir)
 
*---- Return bank pointer.
      if (idir .eq. 0) then
        lbank(1) = 0
      else
        lbank(1) = lq(ldir(3)-idir)
      endif
 
      end
