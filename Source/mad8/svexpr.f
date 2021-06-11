      subroutine svexpr(lexp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save complete expression.                                          *
* Input:                                                               *
*   LEXP(1)   (pointer) Pointer to expression bank.                    *
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
      integer i,ioplev,iopr,iopr1,ip,jp,lp
      double precision rval
      integer           lexp(1)
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
      integer mxf1,mxf2,mxop,mxsiz,mxval
 
*---- Bias for expression banks.
      parameter         (mxf1 = 1, mxop = 2, mxf2 = 3, mxval = 4,
     +                   mxsiz = mwflt + 3)
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
      integer lcali,lcatt,lccls,lccmd,lccom,lcdef,lcelm,lcexp,lcfld,
     +lckey,lcseq,lcspl,lcsrc,lcvar,ldbnk,ldkey,lref1,lref2,lsali,lscom,
     +lsdir,lsfld,lsflg,lsnum,lsspl,lbeam,lconsm,ldummy
 
*---- Global reference links.
      common /refer/    lref1,
     +                  lcali, lcatt, lccls, lccmd, lccom, lcdef, lcelm,
     +                  lcexp, lcfld, lckey, lcseq, lcspl, lcsrc, lcvar,
     +                  lbeam, lconsm, ldbnk(4), ldkey(4), ldummy(10),
     +                  lsali, lscom, lsdir, lsfld, lsflg, lsnum, lsspl,
     +                  lref2
      save              /refer/
      integer liftseq, currseq
      common /seqinfi/ liftseq, currseq
      character * (mcnam) sequnam, seqnames
      common /seqinfc/ sequnam, seqnames(mttact)
 
      integer           istack(maxstk), lstack(maxstk)
      character*4       cstack(maxstk)
      character*1       c1
 
*---- Expression limits excluding 'put'.
      level = 1
      istack(level) = 1
      lstack(level) = iq(lexp(1)-2)
      cstack(level) = '    '
 
*==== Procedure "save expression".
  100 continue
        ip = istack(level)
        lp = lstack(level)
        iopr = iq(lexp(1)+(lp-1)*mxsiz+mxop)
 
*---- Single operand?
        if (ip .eq. lp) then
 
*---- Reference.
          if (iopr .le. -2) then
            lcvar = lq(lexp(1)-ip)
            call svvref(lcvar)
 
*---- Constant.
          else if (iopr .eq. -1) then
            call ucopy(q(lexp(1)+(lp-1)*mxsiz+mxval), rval, mwflt)
            call svreal(rval)
 
*---- Argument-less function.
          else if (narg(iopr) .eq. 0) then
            call svname(funnam(iopr))
            call svlitt('()')
 
*---- Illegal item.
          else
            call aafail('SVEXPR', 1, 'Invalid expression seen:')
            call exdump(lexp)
            go to 9999
          endif
 
*---- The last item (LP) must be an operator.
        else if (ip .lt. lp) then
          if (iopr .le. 0) then
            call aafail('SVEXPR', 1, 'Invalid expression seen:')
            call exdump(lexp)
            go to 9999
 
*---- Unary operator: Operand is (IP to LP-1).
          else if (narg(iopr) .eq. 1) then
            istack(level) = lp + 1
            if (level .ge. maxstk) go to 800
            level = level + 1
            istack(level) = ip
            lstack(level) = lp - 1
            cstack(level) = '    '
            c1 = funnam(iopr)(1:1)
 
*---- Unary plus or minus sign.
            if (c1 .eq. '-'  .or. c1 .eq. '+') then
              call svlitt(c1)
 
*---- Function name.
            else
              call svname(funnam(iopr))
            endif
 
*---- Write parentheses only if required.
            iopr1 = iq(lexp(1)+(lp-2)*mxsiz+mxop)
            if (ipre(iopr1) .le. ipre(iopr)) then
              call svlitt('(')
              cstack(level) = ')   '
            endif
            go to 100
 
*---- Binary operator: Search backward to find end of first operand.
          else if (narg(iopr) .eq. 2) then
            ioplev = 0
            do 110 jp = lp, ip, -1
              iopr = iq(lexp(1)+(jp-1)*mxsiz+mxop)
              if (iopr .le. 0) then
                ioplev = ioplev - 1
                if (ioplev .eq. 0) go to 120
              else if (narg(iopr) .eq. 2) then
                ioplev = ioplev + 1
              endif
  110       continue
 
*---- Operands not found.
            call aafail('SVEXPR', 1, 'Invalid expression seen:')
            call exdump(lexp)
            go to 9999
 
*---- Operands found: (IP to JP-1) and (JP to LP-1).
  120       continue
            istack(level) = lp + 1
            if (level .ge. maxstk) go to 800
            level = level + 1
            istack(level) = jp
            lstack(level) = lp - 1
            cstack(level) = '    '
            if (level .ge. maxstk) go to 800
            level = level + 1
            istack(level) = ip
            lstack(level) = jp - 1
            cstack(level) = '    '
 
*---- Binary operator: Deal with operator precedence to decide about ().
            iopr = iq(lexp(1)+(lp-1)*mxsiz+mxop)
            if (ifun(iopr) .eq. 1) then
              cstack(level)(2:2) = funnam(iopr)(1:1)
              iopr1 = iq(lexp(1)+(jp-2)*mxsiz+mxop)
              if (ipre(iopr1) .lt. ipre(iopr)) then
                call svlitt('(')
                cstack(level)(1:1) = ')'
              endif
              iopr1 = iq(lexp(1)+(lp-2)*mxsiz+mxop)
              call ucopy(q(lexp(1)+(lp-2)*mxsiz+mxval), rval, mwflt)
              if (ipre(iopr1) .le. ipre(iopr)  .or.
     +            iopr1 .eq. -1  .and.  rval .lt. 0.0) then
                cstack(level)(3:3) = '('
                cstack(level-1) = ')   '
              endif
 
*---- Function with two arguments.
            else
              call svname(funnam(iopr))
              call svlitt('(')
              cstack(level) = ',   '
              cstack(level-1) = ')   '
            endif
            go to 100
          endif
        endif
 
*---- End of expression: Unstack parentheses and/or operators.
        do 130 i = 1, 4
          c1 = cstack(level)(i:i)
          if (c1 .ne. ' ') then
            call svlitt(c1)
          endif
  130   continue
        level = level - 1
      if (level .gt. 0) go to 100
      go to 9999
 
*---- Stack overflow.
  800 continue
      call aafail('SVEXPR', 1, 'Expression stack overflow.')
 
 9999 end
