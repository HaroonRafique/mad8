      subroutine dcvref(ilink, idata, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode variable data type for "VARY", "SET", "ASK", etc.           *
* Input:                                                               *
*   LDCBNK   /DCLINK/   Data bank pointer.                             *
*   ILINK    (integer)  Bias for pointer to sub-bank.                  *
*   IDATA    (integer)  Bias for data block.                           *
* Output:                                                              *
*   EFLAG    (logical)  Error flag.                                    *
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
      integer idata,ilink,index1,index2,index3,iseen,leng
      logical           eflag
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
      integer ldcatt,ldcbnk,ldcfrm,ldckey,ldclin
 
*---- Local links for decoder.
      common /dclink/   ldcatt, ldcbnk, ldcfrm, ldckey, ldclin
      save              /dclink/
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
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      character*(mcnam) bnknam, atrnam
 
*---- Drop any previous variable reference.
      eflag = .false.
      lcvar = lq(ldcbnk-ilink)
      if (lcvar .ne. 0) call aadrop(lcvar)
 
*---- Initialize.
      bnknam = ' '
      atrnam = ' '
 
*---- Read bank name.
      call rdword(bnknam, leng)
      if (leng .eq. 0) then
        call rdfail('DCVREF', 1, 'Variable name expected.')
        eflag = .true.
        go to 9999
      endif
 
*---- "[" marks bank attribute.
      if (token(jtok) .eq. '[') then
        jtok = jtok + 1
 
*---- Read attribute name.
        call rdword(atrnam, leng)
        if (leng .eq. 0) then
          call rdfail('DCVREF', 1, 'Attribute name expected.')
          eflag = .true.
          go to 9999
        endif
        call dcindx(index1, index2, index3, eflag)
        if (eflag) go to 9999
        if (token(jtok) .ne. ']') then
          call rdfail('DCVREF', 1, 'Closing bracket "]" missing.')
          eflag = .true.
          go to 9999
        endif
        jtok = jtok + 1
        iseen = 2
 
*---- Parameter reference.
      else
        iseen = 1
        index1 = 0
        index2 = 0
        index3 = 0
      endif
 
*---- Lift variable reference bank and link to variable table.
      call mzbook(2, lcvar, ldcbnk, -ilink, 'VREF', 1, 0, mvsiz, 7, 0)
      call exlkvr
 
*---- Fill in data.
      iq(lcvar+mvf1) = 16 * 2 * mwnam + 5
      call uctoh(bnknam, iq(lcvar+mvbank), mcwrd, mcnam)
      call uctoh(atrnam, iq(lcvar+mvattr), mcwrd, mcnam)
      iq(lcvar+mvf2) = 16 * 6 + 2
      iq(lcvar+mvseen) = iseen
      iq(lcvar+mvind1) = index1
      iq(lcvar+mvind2) = index2
      iq(lcvar+mvind3) = index3
 
*---- Return reference type.
      iq(ldcbnk+idata+mctyp) = 10 * mtvar + iseen
 
 9999 end
