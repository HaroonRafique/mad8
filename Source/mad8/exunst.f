      subroutine exunst(ieval)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Unstack parameter operation with any number of operands.           *
* Input:                                                               *
*   IEVAL    (integer)  Evaluation flag:                               *
*                       1: Constant, 2: Normal, 3: Deferred.           *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer ifun,ipre,narg,nfun
 
*---- Function definitions for expressions.
      parameter         (nfun = 26)
      common /funnam/   funnam(nfun)
      common /fundat/   ipre(-8:nfun), ifun(nfun), narg(nfun)
      save              /funnam/, /fundat/
      character*(mcnam) funnam
      integer ieval,ileng,isw,itype,j
 
*---- Get operation code.
      itype = isopr(level)
 
*---- Is any operand not a constant?
      do 10 j = 1, narg(itype)
        if (isval(level-j+1) .ne. 0) go to 100
   10 continue
 
*---- All operands are constant: Test for random function.
      if (ifun(itype) .eq. 3) then
        if (ieval .eq. 3) go to 100
        call utleng(funnam(itype), ileng)
        if (ieval .eq. 1) then
          msg(1) = 'Random function "' // funnam(itype)(1:ileng)
     +    // '" evaluated immediately in constant expression.'
          call rdwarn('EXUNST', 1, msg)
        else if (ieval .eq. 2) then
          msg(1) = 'Random function "' // funnam(itype)(1:ileng)
     +    // '" evaluated immediately in non-deferred expression.'
          call rdwarn('EXUNST', 1, msg)
        endif
      endif
 
*---- Reduce constant expression.
      call exoper(itype)
*     Discard operand fetches and construct fetch for result.
      nxopr = nxopr - narg(itype) + 1
      ixopr(nxopr) = - 1
      rxval(nxopr) = rsval(level)
*     Correct stack pointer and move result to place.
      level = level - 2 + narg(itype)
      rsval(level) = rxval(nxopr)
      isval(level) = 0
      go to 9999
 
*---- Operands not constant, or deferred random function.
  100 continue
*     Unstack operator.
      level = level - 1
*     Determine type of result.
      isw = ifun(itype)
      do 110 j = 1, narg(itype)
        isw = max(isw, isval(level+j-1))
  110 continue
      isval(level) = isw
*     Generate operation.
      nxopr = nxopr + 1
      ixopr(nxopr) = itype
 
 9999 end
