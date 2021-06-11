      subroutine dcinit
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initialize decoder.                                                *
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
      integer ldcatt,ldcbnk,ldcfrm,ldckey,ldclin
 
*---- Local links for decoder.
      common /dclink/   ldcatt, ldcbnk, ldcfrm, ldckey, ldclin
      save              /dclink/
      integer idcflg
 
      data idcflg       / 0 /
 
      if (idcflg .eq. 0) then
        call mzlink(0, '/DCLINK/', ldcatt, ldcatt, ldclin)
        idcflg = 1
      endif
 
      end
