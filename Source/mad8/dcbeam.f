      subroutine dcbeam(ilink, idata, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode reference to beam line.                                     *
* Input:                                                               *
*   NREPT    (integer)  Repeat count.                                  *
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
      integer idata,idir,ikey,ilink,leng,nseq
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
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
 
      character*(mcnam) label
      data nseq         / 0 /
 
*---- Drop previous reference, if any.
      eflag = .false.
      ldcatt = lq(ldcbnk-ilink)
      if (ldcatt .ne. 0) call aadrop(ldcatt)
      iq(ldcbnk+idata+mctyp) = 10 * mtlin
 
*---- List in parentheses.
      if (token(jtok) .eq. '(') then
 
*---- Lift dummy line bank and link it to LINE keyword.
        call difind(ldkey, 'LINE', ikey, ldckey)
        nseq = nseq + 1
        write (label, '(''*LIN.'',I2.2,''*'')') nseq
        call lnmake(ldcatt, ldckey)
 
*---- Decode beam line list.
        call dclist(ldcatt, eflag)
 
*---- If error detected, drop dummy line bank.
        if (eflag) then
          call aadrop(ldcatt)
 
*---- If all OK, link dummy line bank to data bank.
        else
          call direfe(ldbnk, label, idir)
          lq(ldbnk(3)-idir) = ldcatt
          iq(ldcatt+mbnam) = idir
          iq(ldcbnk+idata+mctyp) = 10 * mtlin + 3
          iq(ldcbnk+idata+mcval) = idir
        endif
 
*---- Read beam line name.
      else
        call rdword(label, leng)
        if (leng .eq. 0) then
          call rdfail('DCBEAM', 1, 'Beam line or list expected.')
          eflag = .true.
 
*---- Directory index for name reference.
        else
          call direfe(ldbnk, label, idir)
          iq(ldcbnk+idata+mcval) = idir
 
*---- Actual argument list.
          if (token(jtok) .eq. '(') then
            call difind(ldkey, 'LINE', ikey, ldckey)
            call lnmake(ldcatt, ldckey)
            call dclist(ldcatt, eflag)
 
*---- If all OK, link actual argument list to data bank.
            if (.not. eflag) then
              call zshunt(0, ldcatt, ldcbnk, -ilink, 0)
              iq(ldcbnk+idata+mctyp) = 10 * mtlin + 2
            endif
 
*---- No actual argument list.
          else
            iq(ldcbnk+idata+mctyp) = 10 * mtlin + 1
          endif
        endif
      endif
 
      end
