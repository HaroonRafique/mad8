      subroutine direfe(ldir, label, idir)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Return index for LABEL in directory structure LDIR.                *
*   If not found, a new entry is created and its index returned.       *
*   If LABEL is less than MCNAM characters, it may be abbreviation.    *
* Input:                                                               *
*   LDIR(4)  (pointer)  Directory links.                               *
*   LABEL    (char)     Label to be found.                             *
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
      integer idir,index
      integer           ldir(4)
      character*(*)     label
 
      call dilook(ldir, label, index, idir)
      if (idir .eq. 0) call diadd(ldir, label, index, idir)
 
      end
