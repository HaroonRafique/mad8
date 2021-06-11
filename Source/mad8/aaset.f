      subroutine aaset
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Execute SET command.                                               *
* Input:                                                               *
* Attributes:                                                          *
*   VARIABLE  (vari)    Variable to be set.                            *
*   DEFAULE   (real)    Expression to be evaluted and stored.          *
* Local links (no Zebra operations called):                            *
*   LEXP                Expression.                                    *
*   LTAR                Target bank.                                   *
*   LVAR                Variable reference.                            *
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
      integer mxals,mxcls,mxdef,mxdrp,mxknw,mxlmp,mxmod,mxord
 
*---- Expression marker bits.
      parameter         (mxdrp = 1, mxdef = 2, mxord = 3,
     +                   mxcls = 4, mxals = 5, mxlmp = 6,
     +                   mxmod = 7, mxknw = 8)
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer ileng,itar,ival,ivar,jbit,jtar,lexp,ltar,lvar
      double precision vval
 
      character*(mcnam) label
 
*---- Check for all defined.
      ivar = mbat
      ival = mbat + mcsiz
      lvar = lq(lccmd-1)
      ltar = lq(lvar-1)
 
*---- Check presence of variable name.
      if (iq(lccmd+ivar+mctyp) .eq. 10 * mtvar) then
        call aawarn('AASET', 1,
     +  'No variable name found in SET command --- Command ignored.')
 
*---- Check presence of value.
      else if (iq(lccmd+ival+mctyp) .eq. 10 * mtflt) then
        call aawarn('AASET', 1,
     +  'No value found in SET command --- Command ignored.')
 
*---- Update not allowed for an alias.
      else if (jbit(iq(ltar),mxals) .ne. 0) then
        call diname(ldbnk, iq(ltar+mbnam), label)
        call utleng(label, ileng)
        msg(1) = 'Cannot SET name "' // label(1:ileng)
     +  // '", it occurs in a sequence.'
        call aafail('AASET', 2, msg)
 
*---- Check variable type.
      else
        jtar = iq(lvar+mvbias)
        itar = mbat + (jtar - 1) * mcsiz
        if (iq(ltar+itar+mctyp) / 10 .ne. mtflt) then
          call aawarn('AASET', 1,
     +    'Variable to be SET is not real --- command ignored.')
 
*---- Replace value.
        else
          lexp = lq(ltar-jtar)
          if (lexp .ne. 0) call aadrop(lexp)
          call utgflt(lccmd, 2, 2, vval)
          call utpflt(ltar, jtar, jtar, vval)
        endif
      endif
 
      end
