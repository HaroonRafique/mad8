      subroutine dimake(mi, iroot, ldir)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Build a directory structure.                                       *
* Input:                                                               *
*   MI        (integer) Initial capacity of directory, and increment.  *
*   IROOT     (integer) Bias in master bank.                           *
*   LDIR(4)   (pointer) Directory links.                               *
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
      integer idiflg,iroot,mi,nd
      integer           ldir(4)
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
      integer ldinew,ldiold
 
*---- Local links for decoder.
      common /dilink/   ldinew, ldiold
      save              /dilink/
      data idiflg       / 0 /
 
      if (idiflg .eq. 0) then
        call mzlink(0, '/DILINK/', ldinew, ldinew, ldiold)
        idiflg = 1
      endif
 
*---- Index for binary search.
      call mzbook(2, ldir(1), lroot, -iroot, 'INDX', 0, 0, mi, 2, 0)
 
*---- Bank names.
      nd = mwnam * mi
      call mzbook(2, ldir(2), lroot, -iroot-1, 'NAME', 0, 0, nd, 5, 0)
 
*---- Bank pointers.
      call mzbook(2, ldir(3), lroot, -iroot-2, 'PNTR', mi, 0, 3, 2, 0)
      iq(ldir(3)+2) = mi
 
*---- Occurrence counts.
      call mzbook(2, ldir(4), lroot, -iroot-3, 'OCCR', 0, 0, mi, 2, 0)
      end
