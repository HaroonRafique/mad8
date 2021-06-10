      subroutine aacopy(lsrc, ilink, ltar)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Copy one attribute of a command bank and link expressions etc.     *
* Input:                                                               *
*   LSRC(1)   (pointer) Source bank.                                   *
*   ILINK     (integer) Attribute number.                              *
*   LTAR(1)   (pointer) Target bank.                                   *
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
      integer idata,idsrc,idtar,ilink,ilsrc,iltar,itype
      integer           lsrc(*), ltar(*)
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
      integer laabnk,laacur,laadef,laakey,laanew,laaold,laaprc,laasrc,
     +laastk,laatar
 
*---- Local links for control module.
      common /aalink/   laabnk, laadef, laasrc, laatar, laakey, laacur,
     +                  laaprc, laastk, laanew, laaold
      save              /aalink/
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
 
*---- Both banks must exist.
      laasrc = lsrc(1)
      laatar = ltar(1)
      if (laasrc .eq. 0  .or.  laatar .eq. 0) go to 9999
 
*---- Banks must be large enough.
      idata = mbat + mcsiz * (ilink - 1)
      idsrc = iq(laasrc-1)
      idtar = iq(laatar-1)
      ilsrc = iq(laasrc-2)
      iltar = iq(laatar-2)
      if (idata .ge. idsrc  .or.  ilink .gt. ilsrc) go to 9999
      if (idata .ge. idtar  .or.  ilink .gt. iltar) go to 9999
 
*---- Copy only if this field has been set.
      itype = iq(laasrc+idata+mctyp)
      if (mod(itype,10) .eq. 0) go to 9999
 
*---- Copy data block.
      call ucopy(q(laasrc+idata+1), q(laatar+idata+1), mcsiz)
 
*---- Drop previous dependent bank(s).
      if (lq(laatar-ilink) .ne. 0) then
        lcatt = lq(laatar-ilink)
        call aadrop(lcatt)
      endif
 
*---- Duplicate new dependent bank(s).
      if (lq(laasrc-ilink) .ne. 0) then
        lcatt = lq(laasrc-ilink)
        call mzcopy(2, lcatt, 2, ltar, -ilink, 'Z')
 
*---- Expressions for real data.
        lcatt = lq(laatar-ilink)
        if (itype .eq. 10 * mtflt + 2  .or.
     +      itype .eq. 10 * mtflt + 3) then
          lcexp = lcatt
          call exlkex
 
*---- Expressions for constraints.
        else if (itype .eq. 10 * mtcon + 1) then
          lcexp = lq(lcatt-1)
          if (lcexp .ne. 0) then
            call exlkex
          endif
          lcexp = lq(lcatt-2)
          if (lcexp .ne. 0) then
            call exlkex
          endif
 
*---- Variable references.
        else if (itype .eq. 10 * mtvar + 1) then
          lcvar = lcatt
          call exlkvr
        endif
      endif
 
 9999 end
