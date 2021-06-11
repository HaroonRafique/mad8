      subroutine utpatt(patt, lpatt)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Transform wildcard pattern to a useful form.                       *
* Input:                                                               *
*   PATT      (char)    Input pattern.                                 *
* Output:                                                              *
*   LPATT     (pointer) Points to transformed pattern.                 *
* Warning:                                                             *
*   Routine uses local link LTOK. Don't add other Zebra calls.         *
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
      integer i,ichar,last,ltok
      character*(*)     patt
      integer           lpatt(1)
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
 
      character*1       c
      character*1       bslash
      parameter         (bslash = '\')
      integer many,mccl,mclos,mlit,mnccl
 
*---- Parameter group used for pattern matching.
      parameter         (many = 1, mccl = 2, mnccl = 3, mclos = 4,
     +                   mlit = 5)
 
*---- Test for invalid input.
      lpatt(1) = 0
      if (patt .eq. ' ') go to 9999
      call utleng(patt, last)
      if (patt(1:1) .eq. '*') go to 800
 
*---- Loop over pattern.
      i = 1
  100 if (i .le. last) then
        ltok = 0
        call mzbook(2, ltok, ltok, 1, 'PATT', 1, 1, 10, 2, 0)
        c = patt(i:i)
 
*---- Arbitrary character '.'.
        if (c .eq. '.') then
          iq(ltok+1) = many
          i = i + 1
 
*---- Character class character '['.
        else if (c .eq. '[') then
          if (i .ge. last) go to 800
          if (patt(i+1:i+1) .eq. '^') then
            iq(ltok+1) = mnccl
            i = i + 2
          else
            iq(ltok+1) = mccl
            i = i + 1
          endif
          call utdash(patt, i, last, ltok)
          if (ltok .eq. 0) go to 800
 
*---- Closure character '*'.
        else if (c .eq. '*') then
          iq(ltok+1) = mclos
          i = i + 1
 
*---- Escape character '\'.
        else if (c .eq. bslash) then
          if (i .ge. last) go to 800
          iq(ltok+1) = mlit
          iq(ltok+2) = ichar(patt(i+1:i+1))
          i = i + 2
 
*---- Litteral character.
        else
          iq(ltok+1) = mlit
          iq(ltok+2) = ichar(c)
          i = i + 1
        endif
 
*---- Link bank in proper position.
*     Put closure bank after the repeated token, any other at front.
*     Closure cannot be first bank.
        if (iq(ltok+1) .eq. mclos) then
          call zshunt(0, ltok, lpatt, 0, 0)
        else
          call zshunt(0, ltok, lpatt, 1, 0)
        endif
        go to 100
      endif
 
*---- Invert list of tokens.
      call ztopsy(0, lpatt)
      go to 9999
 
*---- Error: Delete pattern and return.
  800 call mzdrop(0, lpatt, 'L')
      msg(1) = 'Invalid pattern: ' // patt
      msg(2) = '".*" used.'
      call aawarn('UTPATT', 2, msg)
      lpatt(1) = 0
 
 9999 end
