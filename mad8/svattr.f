      subroutine svattr(lbank, ilink, itype)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Format one attribute value for SAVE or VIEW command.               *
* Input:                                                               *
*   LBANK(1)  (pointer) Pointer to data bank.                          *
*   ILINK     (integer) Attribute number.                              *
*   ITYPE     (integer) Attribute type code.                           *
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
      integer icase,icode,idata,idir,ilen,ilink,itype,ival,ls
      double precision rval
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
 
      character*(mcnam) name
      character*(mcstr)   string
      character*(mcrng) rngnam
      logical           lval
 
*---- Branch on data type.
*            NAM  INT  FLT  DEF  LOG  STR  LIN  RNG  CST  VAR
      go to (10,  20,  30,  30,  40,  50,  60,  70,  80,  90), itype
      go to 100
 
*---- Name.
   10 continue
        call utgnam(lbank, ilink, ilink, name)
        call svname(name)
      go to 100
 
*---- Integer.
   20 continue
        call utgint(lbank, ilink, ilink, ival)
        call svint(ival)
      go to 100
 
*---- Real or deferred expression.
   30 continue
        lcexp = lq(lbank(1)-ilink)
        if (lcexp .ne. 0) then
          call svexpr(lcexp)
        else
          call utgflt(lbank, ilink, ilink, rval)
          call svreal(rval)
        endif
      go to 100
 
*---- Logical.
   40 continue
        call utglog(lbank, ilink, ilink, lval)
        if (lval) then
          call svlitt('.TRUE.')
        else
          call svlitt('.FALSE.')
        endif
      go to 100
 
*---- String.
   50 continue
        call utgstr(lbank, ilink, ilink, string)
        idata = mbat + (ilink - 1) * mcsiz
        ilen = min(iq(lbank(1)+idata+mcval), mcstr)
        call svstrg(string(1:ilen))
      go to 100
 
*---- Beam line.
   60 continue
        idata = mbat + (ilink - 1) * mcsiz
        icase = iq(lbank(1)+idata+mctyp)
        idir  = iq(lbank(1)+idata+mcval)
 
*---- Anonymous line in parentheses.
        if (icase .eq. 10 * mtlin + 3) then
          lcatt = lq(ldbnk(3)-idir)
          call svlist(lcatt)
 
*---- Named beam line.
        else
          call diname(ldbnk, idir, name)
          call svname(name)
 
*---- Actual argument list.
          if (icase .eq. 10 * mtlin + 2) then
            lcatt = lq(lbank(1)-ilink)
            call svlist(lcatt)
          endif
        endif
      go to 100
 
*---- Place (range).
   70 continue
        call enrang(lq(lbank(1)-ilink), rngnam)
        call utleng(rngnam, ls)
        call svlitt(rngnam(1:ls))
      go to 100
 
*---- Constraint.
   80 continue
        lcatt = lq(lbank(1)-ilink)
        icode = iq(lcatt+2)
        if (icode .ne. 2) then
          if (icode .eq. 4) then
            call svlitt('=')
          else
            call svlitt('>')
          endif
          lcexp = lq(lcatt-1)
          if (lcexp .ne. 0) then
            call svexpr(lcexp)
          else
            call ucopy(q(lcatt+2+mcval), rval, mwflt)
            call svreal(rval)
          endif
        endif
        if (icode .eq. 2  .or.  icode .eq. 3) then
          call svlitt('<')
          lcexp = lq(lcatt-2)
          if (lcexp .ne. 0) then
            call svexpr(lcexp)
          else
            call ucopy(q(lcatt+2+mcsiz+mcval), rval, mwflt)
            call svreal(rval)
          endif
        endif
      go to 100
 
*---- Variable.
   90 continue
        lcvar = lq(lbank(1)-ilink)
        call svvref(lcvar)
 
*---- Common exit point.
  100 continue
 
      end
