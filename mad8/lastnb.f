      function lastnb(string)
      implicit none
*-----------------------------------------------------------------------
*
*--- find alst non-blank in string
*--- input
*    STRING
*--- output
*    function value = last non-blank (at least 1)
*-----------------------------------------------------------------------
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer i,lastnb,len
      character *(*) string
      do 10  i = len(string), 1, -1
        if (string(i:i) .ne. ' ')  goto 20
   10 continue
      i = 1
   20 lastnb = i
      end
