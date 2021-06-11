      subroutine utmtpt(lpatt, name, found)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Match NAME with the wildcard pattern at LPATT.                     *
* Input:                                                               *
*   LPATT     (pointer) Input pattern.                                 *
*   NAME      (char)    Name to be matched with pattern.               *
* Output:                                                              *
*   FOUND     (logical) Success flag.                                  *
* Warning:                                                             *
*   Routine uses local links. No Zebra calls allowed.                  *
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
      integer i,k,l,last,ltok
      integer           lpatt(1)
      character*(*)     name
      logical           found
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
      integer many,mccl,mclos,mlit,mnccl
 
*---- Parameter group used for pattern matching.
      parameter         (many = 1, mccl = 2, mnccl = 3, mclos = 4,
     +                   mlit = 5)
      integer           istak(20), lstak(20), kstak(20)
 
*---- Initialize.
      call utleng(name, last)
      i = 1
      ltok = lpatt(1)
      l = 0
      if (ltok .eq. 0) i = last + 1
 
*---- Begin of procedure "match".
*     While we are not at end of pattern ...
  100 if (ltok .eq. 0) go to 200
 
*---- Closure.
        if (iq(ltok+1) .ne. mclos) go to 150
          ltok = lq(ltok)
          k = i
  110     if (i .gt. last) go to 120
            call utmtch(name, i, last, ltok, found)
            if (.not. found) go to 120
            i = i + 1
          go to 110
 
*---- Match remaining pattern.
  120     continue
          ltok = lq(ltok)
  130     if (i .lt. k) go to 100
            l = l + 1
            istak(l) = i
            lstak(l) = ltok
            kstak(l) = k
            go to 100
  140       i = istak(l) - 1
            ltok = lstak(l)
            k = kstak(l)
            l = l - 1
          go to 130
 
*---- Arbitrary single character.
  150   continue
          call utmtch(name, i, last, ltok, found)
          if (.not. found) go to 210
          i = i + 1
          ltok = lq(ltok)
        go to 100
 
*---- End of procedure "match".
*     Success, if name and pattern both exhausted.
  200 continue
      found = i .gt. last  .and.  ltok .eq. 0
 
*---- If failed, restart at closure with next shorter string.
      if (found) return
  210 if (l .gt. 0) go to 140
 
      end
