      subroutine utpflt(lcmd, icat1, icat2, data)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Store command attributes of real type.                             *
*   This routine destroys expressions present for these attributes,    *
*   and it marks the bank as modified.                                 *
* Input:                                                               *
*   LCMD(1)   (pointer) Command to be used.                            *
*   ICAT1     (integer) First attribute number.                        *
*   ICAT2     (integer) Last attribute number.                         *
*   DATA(*)   (real)    Vector to be stored.                           *
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
      integer ibias,icat,icat1,icat2,isrce
      double precision data,rval
      integer           lcmd(*)
      dimension         data(*)
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
 
      logical modflg
 
      ibias = mbat + (icat1 - 1) * mcsiz
      isrce = 1
      modflg = .false.
 
      do 90 icat = icat1, icat2
        call ucopy(q(lcmd(1)+ibias+mcval), rval, mwflt)
        modflg = modflg .or. rval .ne. data(isrce)
 
        if (iq(lcmd(1)+ibias+mctyp) / 10 .eq. mtflt) then
          lcexp = lq(lcmd(1)-icat)
          if(lcexp .ne. 0) call aadrop(lcexp)
          call ucopy(data(isrce), q(lcmd(1)+ibias+mcval), mwflt)
          iq(lcmd(1)+ibias+mctyp) = 10 * mtflt + 1
        endif
 
        ibias = ibias + mcsiz
        isrce = isrce + 1
   90 continue
 
      if (modflg) call aamark('UTPFLT', lcmd)
 
      end
