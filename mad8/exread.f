      subroutine exread(ieval, rval, iseen)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode parameter expression.                                       *
* Input:                                                               *
*   IEVAL    (integer)  Evaluation flag:                               *
*                       1: Constant, 2: Normal, 3: Deferred.           *
* Output:                                                              *
*   RVAL     (real)     Value, if constant expression.                 *
*   ISEEN    (integer)  Result flag:                                   *
*                       0: Error,                                      *
*                       1: Constant,                                   *
*                       2: Normal expression                           *
*                       3: Deferred expression.                        *
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
      integer ichtyp
 
*---- Character code translation tables.
      common /chcode/   ch2low(0:255), ch2upp(0:255)
      common /chtype/   ichtyp(0:255)
      save              /chcode/, /chtype/
      character*1       ch2low, ch2upp
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
      integer isopr,isval,level,maxstk
      double precision rsval
 
*---- Stack for expression decoding and evaluation.
      parameter         (maxstk = 100)
      common /exstki/   level, isopr(maxstk), isval(maxstk)
      common /exstkr/   rsval(maxstk)
      save              /exstki/, /exstkr/
      integer ifun,ipre,narg,nfun
 
*---- Function definitions for expressions.
      parameter         (nfun = 26)
      common /funnam/   funnam(nfun)
      common /fundat/   ipre(-8:nfun), ifun(nfun), narg(nfun)
      save              /funnam/, /fundat/
      character*(mcnam) funnam
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer ichar,ieval,ileng,iseen,jfun,leng
      double precision pval,rval
 
      character*(mcnam) bnknam
      logical           eflag
 
      iseen = 0
      eflag = .false.
 
*---- Initialize stack.
      nxopr = 0
      level = 1
      isopr(1) = 100
      isval(1) = 0
      rsval(1) = 0.
 
*---- Expression. Left parenthesis?
      if (token(jtok) .eq. '(') then
        jtok = jtok + 1
        level = level + 1
        isopr(level) = 101
      endif
 
*---- Unary "+" or "-"?
  100 continue
      if (token(jtok) .eq. '+') then
        jtok = jtok + 1
      else if (token(jtok) .eq. '-') then
        jtok = jtok + 1
        level = level + 1
        isopr(level) = 7
      endif
 
*---- Factor or term.
  200 continue
 
*---- Expression in parentheses?
      if (token(jtok) .eq. '(') then
        jtok = jtok + 1
        level = level + 1
        isopr(level) = 101
        go to 100
 
*---- Function or parameter name?
      else if (ichtyp(ichar(token(jtok))) .eq. 10  .or.
     +         token(jtok) .eq. ''''  .or.  token(jtok) .eq. '"') then
        call rdword(bnknam, leng)
 
*---- Function?
        if (token(jtok) .eq. '(') then
          call utlook(bnknam(1:leng), funnam, nfun, jfun)
          if (jfun .gt. 0) then
            jtok = jtok + 1
 
*---- Function without arguments.
            if (narg(jfun) .eq. 0) then
              if (token(jtok) .eq. ')') then
                jtok = jtok + 1
                level = level + 1
                isopr(level) = jfun
                call exunst(ieval)
              else
                call rdfail('EXREAD', 1, 'Invalid expression.')
                go to 800
              endif
 
*---- Function with one or more arguments.
            else
              level = level + 1
              isopr(level) = jfun
              level = level + 1
              isopr(level) = 100 + narg(jfun)
              go to 100
            endif
          else
            call utleng(bnknam, ileng)
            msg(1) = 'Unknown function"' // bnknam(1:ileng) // '".'
            call rdfail('EXREAD', 1, msg)
            go to 800
          endif
 
*---- Reference to bank attribute, parameter, or constant.
        else
          call exrefe(ieval, bnknam, eflag)
          if (eflag) go to 800
        endif
 
*---- Numeric value?
      else if (ichtyp(ichar(token(jtok))) .le. 9  .or.
     +         token(jtok) .eq. '.') then
        call rdnumb(pval, eflag)
        if (eflag) go to 800
        call excons(pval)
 
*---- Anything else is error.
      else
        call rdfail('EXREAD', 1, 'Invalid operand.')
        go to 800
      endif
 
*---- Unstack function calls.
  300 continue
      if (isopr(level) .lt. 100  .and.  ifun(isopr(level)) .gt. 1) then
        call exunst(ieval)
        go to 300
      endif
 
*---- Unstack power operator.
      if (isopr(level) .eq. 5) then
        call exunst(ieval)
      endif
 
*---- Test for power operator.
      if (token(jtok) .eq. '^') then
        jtok = jtok + 1
        level = level + 1
        isopr(level) = 5
        go to 200
      endif
 
*---- Unstack unary operators.
      if (isopr(level) .lt. 100  .and.  narg(isopr(level)) .eq. 1) then
        call exunst(ieval)
        go to 300
      endif
 
*---- Unstack multiply operators.
      if (isopr(level) .eq. 3 .or. isopr(level) .eq. 4) then
        call exunst(ieval)
      endif
 
*---- Test for multiply operators.
      if (token(jtok) .eq. '*') then
        jtok = jtok + 1
        level = level + 1
        isopr(level) = 3
        go to 200
      else if (token(jtok) .eq. '/') then
        jtok = jtok + 1
        level = level + 1
        isopr(level) = 4
        go to 200
      endif
 
*---- Unstack adding operators.
      if (isopr(level) .eq. 1 .or. isopr(level) .eq. 2) then
        call exunst(ieval)
      endif
 
*---- Test for adding operators.
      if (token(jtok) .eq. '+') then
        jtok = jtok + 1
        level = level + 1
        isopr(level) = 1
        go to 200
      else if (token(jtok) .eq. '-') then
        jtok = jtok + 1
        level = level + 1
        isopr(level) = 2
        go to 200
      endif
 
*---- Unstack comma.
      if (isopr(level) .gt. 101  .and.  token(jtok) .eq. ',') then
        jtok = jtok + 1
        isopr(level) = isopr(level) - 1
        isval(level-2) = isval(level)
        rsval(level-2) = rsval(level)
        go to 100
 
*---- Unstack parenthesis.
      else if (isopr(level) .eq. 101  .and.  token(jtok) .eq. ')') then
        jtok = jtok + 1
        level = level - 1
        isval(level) = isval(level+1)
        rsval(level) = rsval(level+1)
        go to 300
      else if (isopr(level) .ne. 100) then
        call rdfail('EXREAD', 1, 'Invalid expression.')
        go to 800
      endif
 
*---- If constant expression, return value.
      if (isval(1) .eq. 0) then
        rval = rsval(1)
        iseen = 1
 
*---- Otherwise build expression bank.
      else
        rval = 0.
 
*---- Normal expression.
        if (isval(1) .lt. 3) then
          iseen = 2
 
*---- Deferred expression.
        else
          iseen = 3
        endif
      endif
      go to 9999
 
*---- Error exit --- assume zero value.
  800 continue
      iseen = 0
      rval = 0.
 
 9999 end
