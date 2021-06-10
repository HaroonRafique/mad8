      subroutine diadd(ldir, label, index, idir)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Add entry for LABEL in directory structure LDIR.                   *
* Input:                                                               *
*   LDIR(4)  (pointer)  Directory links.                               *
*   LABEL    (char)     Label to be found.                             *
*   INDEX    (integer)  Index for lexicographical insertion.           *
* Output:                                                              *
*   IDIR     (integer)  Directory index.                               *
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
      integer ibias,idir,index,j,last,mi
      integer           ldir(4)
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
 
      character*(mcnam) word
 
*---- Add LABEL to directory.
      word = label
      last = iq(ldir(3)+1)
      if (last .ge. iq(ldir(1)-1)) then
        mi = iq(ldir(3)+2)
        call mzpush(0, ldir(1),  0,       mi, 'I')
        call mzpush(0, ldir(2),  0, mwnam*mi, 'I')
        call mzpush(0, ldir(3), mi,        0, 'I')
        call mzpush(0, ldir(4),  0,       mi, 'I')
      endif
      do 10 j = last, index, -1
        iq(ldir(1)+j+1) = iq(ldir(1)+j)
   10 continue
      idir = last + 1
      ibias = mwnam * last + 1
      iq(ldir(3)+1) = idir
      iq(ldir(1)+index) = idir
      word = label
      call uctoh(word, iq(ldir(2)+ibias), mcwrd, mcnam)
 
      end
