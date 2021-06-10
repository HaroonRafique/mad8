      subroutine diname(ldir, idir, label)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Get LABEL for entry IDIR in directory structure LDIR.              *
* Input:                                                               *
*   LDIR(4)  (pointer)  Directory links.                               *
*   IDIR     (integer)  Directory index.                               *
* Output:                                                              *
*   LABEL    (char)     Label to be found.                             *
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
      integer ibias,idir
      integer           ldir(4)
      character*(mcnam) label
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
 
      if (idir .ne. 0) then
        ibias = mwnam * (idir - 1) + 1
        call uhtoc(q(ldir(2)+ibias), mcwrd, label, mcnam)
      else
        label = ' '
      endif
 
      end
