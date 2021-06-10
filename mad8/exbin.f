      subroutine exbin(iopr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Generate binary instruction on top of stack.                       *
* Input:                                                               *
*   IOPR     (integer) Operation code.                                 *
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
      integer lexbnk,lexexp,lexpar,lexsub,lexvar
 
*---- Local links for expression handler.
      common /exlink/   lexbnk, lexexp, lexpar, lexsub, lexvar
      save              /exlink/
      integer isopr,isval,level,maxstk
      double precision rsval
 
*---- Stack for expression decoding and evaluation.
      parameter         (maxstk = 100)
      common /exstki/   level, isopr(maxstk), isval(maxstk)
      common /exstkr/   rsval(maxstk)
      save              /exstki/, /exstkr/
      integer ixopr,ixsub1,ixsub2,ixsub3,maxexp,nxopr
      double precision rxval
 
*---- Expression description.
      parameter         (maxexp = 100)
      common /exprsa/   nxopr, ixopr(maxexp),
     +                  ixsub1(maxexp), ixsub2(maxexp), ixsub3(maxexp)
      common /exprsc/   axbank(maxexp), axattr(maxexp)
      common /exprsr/   rxval(maxexp)
      save              /exprsa/, /exprsc/, /exprsr/
      character*(mcnam) axbank, axattr
      integer iopr
 
      call exoper(iopr)
      isval(level) = max(isval(level), isval(level+1))
 
*---- At least one operand variable: build subtract instruction.
      if (isval(level) .ne. 0) then
        nxopr = nxopr + 1
        ixopr(nxopr) = iopr
        rxval(nxopr) = 0.0
 
*---- Otherwise fold constants.
      else
        nxopr = nxopr - 1
        rxval(nxopr) = rsval(level)
      endif
 
      end
