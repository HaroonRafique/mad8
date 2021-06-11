      subroutine kwmain(label)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode keyword definition.                                         *
*   Uses the pointers to current keyword and current command.          *
* Input:                                                               *
*   LABEL    (char)     Label for keyword definition.                  *
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
      integer icat,ikat,ileng,iln,isp,isrc,ival,jmark,kpr,ksp,leng,nkat
      character*(mcnam) label
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer iadim1,iadim2,iadim3,iatype,maxat
 
*---- Block for current keyword.
      parameter         (maxat = 100)
      common /keywdi/   iatype(maxat),
     +                  iadim1(maxat), iadim2(maxat), iadim3(maxat)
      common /keywdc/   katnam(maxat)
      save              /keywdi/, /keywdc/
      character*(mcnam) katnam
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      character*(mcnam) atrnam, key, source
      logical           eflag
      integer           ia1(maxat)
 
*==== PHASE 1: Initialize according to command type.
      kpr = 0
      ksp = 0
      nkat = 0
 
*---- ISP = 2: Edit existing keyword.
      isp = iq(lckey+mbsp)
      if (isp .eq. 2) then
        if (token(jtok) .eq. ',') then
          jtok = jtok + 1
          call rdword(source, leng)
        else
          leng = 0
        endif
        if (leng .eq. 0) then
          call diname(ldkey, iq(lckey+mbnam), key)
          msg(1) = 'Source missing for "KEYDIT" command.'
          call rdfail('KWMAIN', 1, msg)
          go to 9999
        endif
 
*---- Find source keyword.
        call difind(ldkey, source, isrc, lcsrc)
        if (lcsrc .eq. 0) then
          call utleng(source, ileng)
          msg(1) =
     +    'Source keyword "' // source(1:ileng) // '" not found.'
          call rdfail('KWMAIN', 1, msg)
          go to 9999
        endif
 
*---- Copy source keyword to local store.
        call kwget(lcsrc, iln, kpr, ksp, nkat)
        do 10 ikat = 1, nkat
          ia1(ikat) = 0
   10   continue
      endif
 
*==== PHASE 2: Process code, subprocess code, attribute groups.
  100 if (token(jtok) .eq. ',') then
        jtok = jtok + 1
 
*---- Attribute name and equals sign.
        call rdword(atrnam, leng)
        if (leng .eq. 0) then
          call rdfail('KWMAIN', 1, 'Attribute name expected.')
          go to 200
        else if (token(jtok) .ne. '=') then
          call rdfail('KWMAIN', 1, 'Equals sign "=" expected.')
          go to 200
        else
          jtok = jtok + 1
 
*---- Attribute group, type part.
          if (token(jtok) .eq. '(') then
            call utlook(atrnam(1:leng), katnam, nkat, ikat)
            if (ikat .eq. 0) then
              nkat = nkat + 1
              ikat = nkat
              katnam(ikat) = atrnam
              iatype(ikat) = mtflt
              iadim1(ikat) = 1
              iadim2(ikat) = 1
              iadim3(ikat) = 1
            else if (isp .eq. 1) then
              call rdwarn('KWMAIN', 1, 'Duplicate attribute.')
            endif
 
*---- Mark begin of attribute group, decode data type and dimensions.
            ia1(ikat) = jtok
            call kwdim(ikat, eflag)
            if (eflag) go to 200
 
*---- Process or subprocess code.
          else
            call rdint(ival, eflag)
            if (eflag) go to 200
            if (atrnam .eq. 'PR') then
              kpr = ival
            else if (atrnam .eq. 'SP') then
              ksp = ival
            else
              call rdfail('KWMAIN', 1, 'Invalid attribute name.')
              go to 200
            endif
          endif
        endif
        if (token(jtok) .eq. ','  .or.  token(jtok) .eq. ';') go to 100
        msg(1) =
     +  'Character "' // token(jtok) // '" is not allowed here.'
        call rdfail('KWMAIN', 1, msg)
 
*---- Error recovery.
  200   continue
        call rdfind(',;')
        error = .true.
        go to 100
      endif
 
*==== PHASE 3: Decode template and default data.
      if (error) go to 9999
 
*---- Build keyword, default, and template banks.
      call kwmake(label, kpr, ksp, nkat)
      if (error) go to 9999
 
*---- Decode default data and template.
      jmark = jtok
      icat = 1
      do 290 ikat = 1, nkat
        if (ia1(ikat) .ne. 0) then
          jtok = ia1(ikat)
          call kwgrp(ikat, icat, eflag)
          error = error .or. eflag
        endif
        icat = icat + iadim1(ikat) * iadim2(ikat) * iadim3(ikat)
  290 continue
      jtok = jmark
 
*---- Store attributes.
      call kwput(lccmd, nkat)
 
*---- KEYWORD dump.
      if (ikeyfl .eq. 1  .or.  ikeyfl .eq. 3) then
        call kwdump(lccmd)
      endif
      if (ikeyfl .eq. 2  .or.  ikeyfl .eq. 3) then
        call dzshow('keyword', 0, lccmd, 'V', 0, 0, 0, 0)
      endif
 
 9999 end
