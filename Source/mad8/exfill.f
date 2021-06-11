      subroutine exfill
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Fill in variable references.                                       *
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
      integer lexbnk,lexexp,lexpar,lexsub,lexvar
 
*---- Local links for expression handler.
      common /exlink/   lexbnk, lexexp, lexpar, lexsub, lexvar
      save              /exlink/
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer icat,idir,ikat,ikey,ileng,iln,ind1,ind2,ind3,ipr,isp,jk,
     +jleng,jvar,nkat,iaux
 
      character*(mcnam) atrnam, bnknam
 
*---- Find PARAMETER keyword.
      call difind(ldkey, 'PARAMETE', ikey, lexpar)
 
*---- Loop for all variable references.
      do 90 jvar = 1, iq(lq(lroot-mdvar)+1)
        lexvar = lq(lq(lroot-mdvar)-jvar)
        lexbnk = lq(lexvar-1)
        if (lexbnk .eq. 0)  then
          iaux = -1
        else
          idir = iq(lexbnk+mbnam)
          iaux = lq(ldbnk(3)-idir)
        endif
        if (lexbnk .eq. 0 .or. iaux .ne. lexbnk) then
 
*---- Try to find bank name.
          call uhtoc(q(lexvar+mvbank), mcwrd, bnknam, mcnam)
          call utleng(bnknam, ileng)
          call difind(ldbnk, bnknam(1:ileng), idir, lexbnk)
          lq(lexvar-1) = lexbnk
 
*---- Parameter bank: Create with zero value, if not existent.
          if (iq(lexvar+mvseen) .eq. 1) then
            if (lexbnk .eq. 0) then
              call kwget(lexpar, iln, ipr, isp, nkat)
              call aabook(lexbnk, bnknam, ipr, isp, lexpar, 1)
              iq(lexbnk+mbln) = 0
              iq(lexbnk+mbat+mctyp) = 10 * mtflt + 1
              call didefi(ldbnk, bnknam, lexbnk)
              lq(lexvar-1) = lexbnk
              iq(lexvar+mvbias) = 1
 
*---- Check legal use of parameter.
            else if (iq(lexbnk+mbpr) .eq. mppar) then
              iq(lexvar+mvbias) = 1
            else if (iq(lexbnk+mbpr) .eq. mplin  .and.
     +               iq(lexbnk+mbsp) .eq. 2) then
              iq(lexvar+mvbias) = iq(lexvar+mvind1)
            else
              write (msg, 910) bnknam(1:ileng)
              call aafail('EXFILL', 1, msg)
              go to 9999
            endif
 
*---- Data bank: Must be known.
          else if (iq(lexvar+mvseen) .eq. 2) then
            if (lexbnk .eq. 0) then
              write (msg, 920) bnknam
              call aafail('EXFILL', 1, msg)
 
*---- Find attribute.
            else
              ind1 = iq(lexvar+mvind1)
              ind2 = iq(lexvar+mvind2)
              ind3 = iq(lexvar+mvind3)
              call kwget(lq(lexbnk+1), iln, ipr, isp, nkat)
              call uhtoc(q(lexvar+mvattr), mcwrd, atrnam, mcnam)
              call utleng(atrnam, jleng)
              call utlook(atrnam(1:jleng), katnam, nkat, ikat)
              if (ikat .eq. 0) then
                write (msg, 930) bnknam(1:ileng), atrnam(1:jleng)
                call aafail('EXFILL', 1, msg)
              else if (iatype(ikat) .ne. mtflt) then
                write (msg, 940) bnknam(1:ileng), atrnam(1:jleng)
                call aafail('EXFILL', 1, msg)
              else if (ind1 .gt. iadim1(ikat)  .or.
     +                 ind2 .gt. iadim2(ikat)  .or.
     +                 ind3 .gt. iadim3(ikat)) then
                write (msg, 950)
     +          bnknam(1:ileng), atrnam(1:jleng), ind1, ind2, ind3
                call aafail('EXFILL', 1, msg)
              else
 
*---- Compute position number in bank.
                icat = 0
                do 10 jk = 1, ikat
                  icat = icat + iadim1(jk) * iadim2(jk) * iadim3(jk)
   10           continue
                iq(lexvar+mvbias) = icat + (ind1 - iadim1(ikat)) +
     +            (ind2 - iadim2(ikat)) * iadim1(ikat) +
     +            (ind3 - iadim3(ikat)) * iadim1(ikat) * iadim2(ikat)
              endif
            endif
          endif
        endif
   90 continue
 
*---- VERIFY option: Check if all parameters are defined.
      if (verify) then
        lexbnk = lq(lexpar-1)
  100   if (lexbnk .ne. 0) then
          if (iq(lexbnk+mbln) .eq. 0) then
            idir = iq(lexbnk+mbnam)
            call diname(ldbnk, idir, bnknam)
            call utleng(bnknam, ileng)
            write (msg, 960) bnknam(1:ileng)
            call aawarn('EXFILL', 1, msg)
          endif
          lexbnk = lq(lexbnk)
          go to 100
        endif
      endif
 
  910 format('Expression refers to name "',a,'" --- not a parameter.')
  920 format('Expression refers to name "',a,'" --- unable to find it.')
  930 format('Expression refers to attribute "',a,'[',a,
     +       ']" --- unable to find it.')
  940 format('Expression refers to attribute "',a,'[',a,
     +       '" --- not of real type.')
  950 format('Expression refers to attribute "',a,'[',a,'(',i5,',',i5,
     +       ',',i5,')]" --- index out of range.')
  960 format('VERIFY option found undefined parameter: "',a,/
     +       '" --- value set to zero.')
 
 9999 end
