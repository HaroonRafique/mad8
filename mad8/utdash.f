      subroutine utdash(patt, init, last, ltok)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Build pattern entry for a character class.                         *
* Input:                                                               *
*   PATT      (char)    Input pattern.                                 *
*   INIT      (integer) Current position in pattern.                   *
*   LAST      (integer) Length of pattern in characters.               *
*   LTOK      (pointer) Points to current token bank.                  *
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
      integer i,ichar,init,j,j1,j2,jt,last
      character*(*)     patt
      integer           ltok(1)
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
 
      character*1       bslash
      parameter         (bslash = '\')
      character*1       c
 
*---- Is pattern exhausted?
      i = init
      if (i .gt. last) go to 200
 
*---- Loop until ']' is seen.
  100 if (patt(i:i) .ne. ']') then
        c = patt(i:i)
        if (c .eq. '-') then
          if (i .le. init  .or.  i .ge. last) then
            call setbit(ichar(c), iq(ltok(1)+3), 1)
          else
            i = i + 1
            j1 = ichar(patt(i-2:i-2))
            j2 = ichar(patt(i:i))
            if (j1 .gt. j2) then
              jt = j1
              j1 = j2
              j2 = jt
            endif
            do 110 j = j1, j2
              call setbit(j, iq(ltok(1)+3), 1)
  110       continue
          endif
        else if (c .eq. bslash) then
          i = i + 1
          if (i .gt. last) go to 200
          c = patt(i:i)
        endif
        call setbit(ichar(c), iq(ltok(1)+3), 1)
        i = i + 1
        if (i .le. last) go to 100
      endif
 
*---- Check.
  200 if (i .gt. last  .or.  patt(i:i) .ne. ']') then
        ltok(1) = 0
        init = last + 1
      else
        init = i + 1
      endif
 
      end
