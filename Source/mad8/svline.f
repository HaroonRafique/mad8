      subroutine svline(lbank)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Save beam line or replacement list definition.                     *
* Input:                                                               *
*   LBANK(1)  (pointer) Pointer to data bank.                          *
*   ILINK     (integer) Attribute number.                              *
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
      integer isp,jform,lform
      double precision half,one,zero
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
 
      character*(mcnam) frmnam, linnam, refnam
      parameter         (zero = 0.0d0, half = 0.5d0, one = 1.0d0)
 
*---- Name of line or list.
      call diname(ldbnk, iq(lbank(1)+mbnam), linnam)
      call svbegn
      call svname(linnam)
 
*---- Beam line: Formal arguments, if present.
*     Skip dummy line names created for anonymous lists.
      isp = iq(lbank(1)+mbsp)
      if (isp .eq. 1  .and.  linnam(1:5) .ne. '*LIN.') then
        lform = lq(lbank(1)-1)
        if (lform .ne. 0) then
          do 10 jform = 1, iq(lform-1), mwnam
            if (jform .eq. 1) then
              call svlitt('(')
            else
              call svlitt(', ')
            endif
            call uhtoc(q(lform+jform), mcwrd, frmnam, mcnam)
            call svname(frmnam)
   10     continue
          call svlitt(')')
        endif
 
*---- LINE keyword.
        call svlitt(': LINE=')
        call svlist(lbank)
        call svdump
 
*---- Beam sequence: SEQUENCE keyword.
      else if (isp .eq. 2) then
        call svlitt(': SEQUENCE, REFER=')
        refnam = 'CENTRE'
        call utgnam(lbank, 1, 1, refnam)
        call svname(refnam)
        call svdump
        call svseq(lbank)
 
*---- Replacement list: LIST keyword.
      else if (isp .eq. 3) then
        call svlitt(': LIST=')
        call svlist(lbank)
        call svdump
      endif
 
      end
