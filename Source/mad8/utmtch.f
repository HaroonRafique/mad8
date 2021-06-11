      subroutine utmtch(name, i, last, lpatt, found)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Match NAME with the pattern token at LPATT.                        *
* Input:                                                               *
*   NAME      (char)    Name to be matched with pattern.               *
*   INIT      (integer) Current character number.                      *
*   LAST      (integer) Last character in name.                        *
*   LPATT     (pointer) Input pattern.                                 *
* Output:                                                              *
*   FOUND     (logical) Success flag.                                  *
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
      integer i,ichar,k,last,n
      character*80      name
      integer           lpatt(1)
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
 
      found = .false.
      if (i .gt. last) return
      k = ichar(name(i:i))
      go to (10, 20 , 30 , 40 , 50), iq(lpatt(1)+1)
 
*---- Arbitrary character.
   10 found = .true.
      go to 9999
 
*---- Character class.
   20 call getbit(k, iq(lpatt(1)+3), n)
      found = n .ne. 0
      go to 9999
 
*---- Complemented character class.
   30 call getbit(k, iq(lpatt(1)+3), n)
      found = n .eq. 0
      go to 9999
 
*---- Closure cannot occur here.
   40 go to 9999
 
*---- Litteral character.
   50 found = k .eq. iq(lpatt(1)+2)
 
 9999 end
