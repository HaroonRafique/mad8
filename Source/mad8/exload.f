      subroutine exload(label, attrib, lbank, ilink, idata)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Generate instruction to half load real value to stack.             *
* Input:                                                               *
*   LABEL     (char)    Label of bank to be accessed.                  *
*   LBANK(1)  (pointer) Pointer to bank to be accessed.                *
*   ILINK     (integer) Number of attribute to be loaded.              *
*   IDATA     (integer) Bias of data word in bank.                     *
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
      integer idata,ilink
      character*(*)     label, attrib
      integer           lbank(1)
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
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
 
      level = level + 1
      call ucopy(q(lbank(1)+idata), rsval(level), mwflt)
      nxopr = nxopr + 1
      if (lq(lbank(1)-ilink) .eq. 0) then
        isval(level)  = 0
        ixopr(nxopr)  = -1
        rxval(nxopr)  = rsval(level)
      else
        isval(level)  = 1
        if (attrib .eq. ' ') then
          ixopr(nxopr)  = -4
          ixsub1(nxopr) = idata
        else
          ixopr(nxopr)  = -3
          ixsub1(nxopr) = 1
        endif
        ixsub2(nxopr) = 1
        ixsub3(nxopr) = 1
        rxval(nxopr)  = 0.0
        axbank(nxopr) = label
        axattr(nxopr) = attrib
      endif
 
      end
