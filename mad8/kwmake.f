      subroutine kwmake(label, ipr, isp, nkat)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Create new keyword bank.                                           *
* Input:                                                               *
*   LABEL    (char)     Label for new keyword bank.                    *
*   IPR      (integer)  Process code.                                  *
*   ISP      (integer)  Subprocess code.                               *
*   NKAT     (integer)  Number of attributes.                          *
* Output:                                                              *
*   LCCMD    /REFER/    Pointer to new keyword bank.                   *
*   LCKEY    /REFER/    Pointer to master keyword.                     *
*   LCCLS    /REFER/    Pointer to class bank.                         *
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
      integer ipr,isp,l,nd,nkat
      character*(*)     label
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
      integer mkdim1,mkdim2,mkdim3,mkf1,mkf2,mkname,mksiz,mktype
 
*---- Bias for keyword attribute groups.
      parameter         (mkf1 = 1, mktype = 2, mkdim1 = 3, mkdim2 = 4,
     +                   mkdim3 = 5, mkf2 = 6, mkname = 7,
     +                   mksiz = mwnam + 6)
      integer mpcor,mpedi,mpelm,mpenv,mperr,mpfil,mphar,mpkey,mplie,
     +mplin,mpmat,mppar,mpplt,mppol,mpsrv,mpstr,mpsub,mpsur,mptrk,
     +mptws,mpdoom
 
*---- Standard process codes.
      parameter         (mpkey =  1, mppar =  2, mpstr =  3, mpelm =  5,
     +                   mplin =  6)
      parameter         (mpsub = 10)
      parameter         (mpsrv = 11, mpfil = 12, mpenv = 13, mpplt = 14,
     +                   mpsur = 15, mptws = 16, mpmat = 17, mptrk = 18,
     +                   mphar = 19, mperr = 20, mpcor = 21, mplie = 22,
     +                   mpedi = 23, mppol = 24, mpdoom = 25)
      integer mbeam,mcseq,md,mdbnk,mdexp,mdkey,mdvar,minit,mlr,mls,
     +mrkey,msrseq,mdmtrk,mpparl,mconsm
 
*---- Link bias in "Great Master Bank".
      parameter         (mls   = 20, mlr   = mls + 20, md = 20)
      parameter         (mdkey =  1, mdbnk =  5, mdexp =  9, mdvar = 10,
     +                   mrkey = 11, mcseq = 12, minit = 13, mbeam = 14,
     +                   mdmtrk = 15, mpparl = 16, mconsm = 17)
      parameter         (msrseq = 1)
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
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
 
      character*4       name
      character*(mcnam) defalt
 
*---- Lift bank and link it to KEYWORD chain.
      nd = mbat + nkat * mksiz
      lckey = lq(lroot-mrkey)
      name = label
      call mzbook(2, lccmd, lckey, -1, name, 3, 2, nd, 7, 0)
 
*---- Store bank description.
      iq(lccmd+mbfrm) = 16 * 5 + 2
      iq(lccmd+mbln) = lintok
      iq(lccmd+mbpr) = ipr
      iq(lccmd+mbsp) = isp
      iq(lccmd+mbat) = nkat
 
*---- Link to directory.
      call didefi(ldkey, label, lccmd)
      if (error) go to 9999
 
*---- Banks for current values.
      defalt = 'DEFAULTS'
      call aabook(l, defalt, ipr, isp, lccmd, 2)
 
*---- Class element (for element definitions only).
      if (ipr .eq. mpelm) then
        call aabook(lccls, label, ipr, isp, lccmd, 1)
        lq(lccmd-3) = lccls
        call didefi(ldbnk, label, lccls)
      endif
 
 9999 end
