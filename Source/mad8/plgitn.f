      subroutine plgitn(n, sin, nf, sout)
      implicit none
************************************************************************
*
*   Purpose: get items (blank separated) from line
*
*-- Input:
*   N        max. number of item looked for
*   SIN      input string
*-- Output:
*   nf       no .of items found
*   SOUT     item list, left adjusted
*
************************************************************************
      integer is,itog,js,jtog,l,lastnb,n,nf
 
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      character * (*)  sin, sout(*)
      l = lastnb(sin)
      nf = 0
      itog = 0
      jtog = 0
      sout(1) = ' '
      if (sin .ne. ' ')  then
        do 10  js = 1, l
          if (sin(js:js) .eq. '"')  jtog = 1 - jtog
          if  (itog .eq. 0 .and. sin(js:js) .eq. ' '
     +    .or. itog .ne. 0 .and. (sin(js:js) .ne. ' ' .or. jtog .ne. 0))
     +    goto 10
          if (itog .eq. 0)  then
            is = js
            nf = nf + 1
          else
            sout(nf) = sin(is:js-1)
            if (nf .eq. n)  goto 20
          endif
          itog = 1 - itog
   10   continue
        if (itog .ne. 0)  sout(nf) = sin(is:l)
   20   continue
      endif
  999 end
