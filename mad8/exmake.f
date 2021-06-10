      subroutine exmake(lbank, ilink, idata, rval, iexpr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Generate bank for expression.                                      *
* Input:                                                               *
*   LBANK(1) (pointer)  Data bank pointer.                             *
*   ILINK    (integer)  Bias for expression bank pointer.              *
*   IDATA    (integer)  Bias for data storage.                         *
*   RVAL     (real)     Value, if constant expression.                 *
*   IEXPR    (integer)  Expression flag returned by EXREAD.            *
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
      integer ibias,idata,iexpr,ilink,jx,nd,nl
      double precision rval
      integer           lbank(*)
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
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
      integer mxf1,mxf2,mxop,mxsiz,mxval
 
*---- Bias for expression banks.
      parameter         (mxf1 = 1, mxop = 2, mxf2 = 3, mxval = 4,
     +                   mxsiz = mwflt + 3)
      integer mvattr,mvbank,mvbias,mvf1,mvf2,mvind1,mvind2,mvind3,
     +mvseen,mvsiz
 
*---- Bias for variable reference group.
      parameter         (mvf1   = 1,
     +                   mvbank = 2,
     +                   mvattr = mvbank + mwnam,
     +                   mvf2   = mvattr + mwnam,
     +                   mvseen = mvf2 + 1,
     +                   mvind1 = mvseen + 1,
     +                   mvind2 = mvind1 + 1,
     +                   mvind3 = mvind2 + 1,
     +                   mvbias = mvind3 + 1,
     +                   mvsiz = mvbias)
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
      integer lexbnk,lexexp,lexpar,lexsub,lexvar
 
*---- Local links for expression handler.
      common /exlink/   lexbnk, lexexp, lexpar, lexsub, lexvar
      save              /exlink/
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
      double precision cofact,optflt
 
*---- Option flags.
      common /optflt/   optflt(10)
      equivalence       (cofact, optflt( 1))
      common /optint/   optint(10)
      integer           optint
      integer           icmdfl, ideffl, iexpfl, ikeyfl, ilinfl
      equivalence       (icmdfl, optint( 1)), (ideffl, optint( 2))
      equivalence       (iexpfl, optint( 3)), (ikeyfl, optint( 4))
      equivalence       (ilinfl, optint( 5))
      common /optlog/   optflg(20), optcon(5)
      logical           optflg, optcon
      logical           debug,  double, echo,   inter,  trace,  verify,
     +                  warn,   info,   sympl,  rbarc, ereset, bborbit
      logical           reset,  tell
      equivalence       (debug,  optflg( 1)), (double, optflg( 2))
      equivalence       (echo,   optflg( 3)), (inter,  optflg( 4))
      equivalence       (trace,  optflg( 5)), (verify, optflg( 6))
      equivalence       (warn,   optflg( 7)), (info,   optflg( 8))
      equivalence       (sympl,  optflg( 9)), (rbarc,  optflg(10))
      equivalence       (ereset,  optflg(11)),(bborbit,optflg(12))
      equivalence       (reset,  optcon( 1)), (tell,   optcon( 2))
      save              /optflt/, /optint/, /optlog/
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
*---- Initial operations.
      lexbnk = lbank(1)
      lcexp = lq(lexbnk-ilink)
      if (lcexp .ne. 0) call aadrop(lcexp)
      call ucopy(rval, q(lexbnk+idata), mwflt)
 
*---- Book expression bank.
      if (iexpr .ge. 2) then
        nl = nxopr
        nd = (nxopr + 1) * mxsiz
        call mzbook(2, lcexp, lbank, -ilink, 'EXPR', nl, nl, nd, 7, 0)
 
*---- Mark deferred expression.
        if (iexpr .eq. 3) call sbit1(iq(lcexp), mxdef)
 
*---- Fill in operations.
        ibias = 0
        do 90 jx = 1, nxopr
 
*---- Bank format and operation code.
          iq(lcexp+ibias+mxf1) = 16 * 1 + 2
          iq(lcexp+ibias+mxop) = ixopr(jx)
          iq(lcexp+ibias+mxf2) = 16 * mwflt + mreal
 
*---- Load variable: Lift variable reference bank (linked in EXLKEX).
          if (ixopr(jx) .le. -2) then
            call mzbook(2, lcvar, lcexp, -jx, 'VREF', 1, 0, mvsiz, 7, 0)
 
*---- Fill in variable reference bank.
            iq(lcvar+mvf1) = 16 * 2 * mwnam + 5
            call uctoh(axbank(jx), iq(lcvar+mvbank), mcwrd, mcnam)
            call uctoh(axattr(jx), iq(lcvar+mvattr), mcwrd, mcnam)
            iq(lcvar+mvf2) = 16 * 6 + 2
            if (ixopr(jx) .eq. -3) then
              iq(lcvar+mvseen) = 2
            else
              iq(lcvar+mvseen) = 1
            endif
            iq(lcvar+mvind1) = ixsub1(jx)
            iq(lcvar+mvind2) = ixsub2(jx)
            iq(lcvar+mvind3) = ixsub3(jx)
 
*---- Load constant: Store constant value.
          else if (ixopr(jx) .eq. -1) then
            call ucopy(rxval(jx), q(lcexp+ibias+mxval), mwflt)
          endif
          ibias = ibias + mxsiz
   90   continue
 
*---- Append store operation.
        iq(lcexp+ibias+mxf1) = 16 * 1 + 2
        iq(lcexp+ibias+mxop) = 0
        iq(lcexp+ibias+mxf2) = 16 * mwflt + 2
        iq(lcexp+ibias+mxval) = idata
 
*---- Link to expression table.
        call exlkex
 
*---- Dump expression.
        if (iexpfl .eq. 1  .or.  iexpfl .eq. 3) then
          call exdump(lcexp)
        endif
        if (iexpfl .eq. 2  .or.  iexpfl .eq. 3) then
          call dzshow('expression', 0, lcexp, 'V', 0, 0, 0, 0)
        endif
      endif
 
      end
