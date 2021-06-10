      subroutine dilook(ldir, label, index, idir)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Return index for LABEL in directory structure LDIR.                *
*   If LABEL is less than MCNAM characters, it may be abbreviation.    *
* Input:                                                               *
*   LDIR(4)  (pointer)  Directory links.                               *
*   LABEL    (char)     Label to be found.                             *
* Output:                                                              *
*   INDEX    (integer)  Index for lexicographic insertion.             *
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
      integer ibias,idir,imax,imin,index,labl,last,len
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
 
      character*(mcnam) test, word
 
*---- Initial values.
      test = label
      labl = len(label)
      imin = 1
      last = iq(ldir(3)+1)
      imax = last
      idir = 0
*---- Search loop.
   10 if (imax .ge. imin) then
        index = (imin + imax) / 2
        idir = iq(ldir(1)+index)
        ibias = mwnam * (idir - 1) + 1
        call uhtoc(q(ldir(2)+ibias), mcwrd, word, mcnam)
        if (word .gt. test) then
          imax = index - 1
          go to 10
        else if (word .lt. test) then
          imin = index + 1
          go to 10
        endif
 
*---- If there is no exact match, try abbreviation.
      else
        index = imin
        idir = 0
        if (labl .ge. 4) then
          if (labl .lt. mcnam  .and.  index .le. last) then
            idir = iq(ldir(1)+index)
            ibias = mwnam * (idir - 1) + 1
            call uhtoc(q(ldir(2)+ibias), mcwrd, word, mcnam)
            if (word(1:labl) .eq. label) then
              if(index .lt. last) then
                ibias = mwnam * (iq(ldir(1)+index+1) - 1) + 1
                call uhtoc(q(ldir(2)+ibias), mcwrd, word, mcnam)
                if (word(1:labl) .eq. label) idir = 0
              endif
            else
              idir = 0
            endif
          endif
        endif
      endif
 
      end
