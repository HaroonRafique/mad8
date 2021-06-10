      subroutine aaread(label, key, if1, if2)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Read and decode a command.                                         *
*   Returns pointer to current keyword,                                *
*   and optionally to current command.                                 *
* Output:                                                              *
*   LABEL    (char)     Command label.                                 *
*   KEY      (char)     Command keyword.                               *
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
      integer icmd,icmnt,if1,if2,ikey,isrc,jcase,jform,leng1,leng2
      character*(mcnam) label,  key
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
      character*8       toks
      equivalence       (toks, token(1))
 
      logical           eflag
      character*(mcnam) word1, word2
 
      data icmnt / 0 /
      save icmnt
 
*---- Read statement into buffer and skip empty statements.
   10 continue
        call rdstat(eflag)
        if (eflag) go to 9999
      if (token(1) .eq. ';') go to 10
 
*---- Skip comment blocks.
      if (toks(1:4) .eq. 'COMM') then
        icmnt = icmnt + 1
        go to 10
      else if (icmnt .gt. 0) then
        if (toks(1:4) .eq. 'ENDC') then
          icmnt = icmnt - 1
          go to 10
        else if (toks .eq. 'END_FILE') then
          call aawarn('AAREAD', 1, 'Unclosed "COMMENT" at end of file.')
          icmnt = 0
        else
          go to 10
        endif
      endif
 
*---- Initialize.
      label = ' '
      key = ' '
      if1 = 0
      if2 = 0
      jcase = 0
      jform = 0
 
*==== PHASE 1. Decode possible label and keyword.
*---- Command must begin with an identifier.
      call rdword(word1, leng1)
      if (leng1 .eq. 0) then
        call rdfail('AAREAD', 1,
     +  'Command should begin with a label or a keyword.')
        go to 9999
      endif
 
*---- Formal argument list.
      if (token(jtok) .eq. '(') then
        call rdform(if1, if2, eflag)
        if (eflag) go to 9999
        jform = 1
      endif
 
*---- ":" marks first name as label.
      if (token(jtok) .eq. ':') then
        jtok = jtok + 1
 
*---- ":=" marks statement as a parameter definition.
        if (token(jtok) .eq. '=') then
          jcase = 3
        else
 
*---- A keyword must follow the ":".
          call rdword(word2, leng2)
          if (leng2 .eq. 0) then
            call rdfail('AAREAD', 1, 'Keyword expected after ":".')
          else
            jcase = 2
          endif
        endif
 
*---- "=" marks statement as a parameter definition.
      else if (token(jtok) .eq. '=') then
        jcase = 3
 
*---- "," or ';': Keyword only or label only.
      else if (token(jtok) .eq. ','  .or.  token(jtok) .eq. ';') then
        jcase = 1
 
*---- All other characters are illegal at this point.
      else
        call rdfail('AAREAD', 1, 'End of statement ";" expected.')
        go to 9999
      endif
 
*==== PHASE 2. Find command keyword.
      lckey = 0
      lccmd = 0
      lccls = 0
 
*---- JCASE = 1. One word only; decide if keyword or label.
      if (jcase .eq. 1) then
 
*---- Exact match with keyword or stored command.
        call difind(ldkey, word1, ikey, lckey)
        if (lckey .ne. 0) then
          if (iq(lckey+mbpr) .eq. mpelm) lccmd = lq(lckey-3)
        else
          call difind(ldbnk, word1, icmd, lccmd)
          if (lccmd .ne. 0) then
            lckey = lq(lccmd+1)
 
*---- Approximate match with keyword or stored command.
          else
            call difind(ldkey, word1(1:leng1), ikey, lckey)
            if (lckey .ne. 0) then
              if (iq(lckey+mbpr) .eq. mpelm) lccmd = lq(lckey-3)
            else
              call difind(ldbnk, word1(1:leng1), icmd, lccmd)
              if (lccmd .ne. 0) then
                lckey = lq(lccmd+1)
              endif
            endif
          endif
        endif
 
*---- JCASE = 2. Two words; decide if second is keyword or class.
      else if (jcase .eq. 2) then
        label = word1
 
*---- Exact match with keyword or class name.
        call difind(ldkey, word2, ikey, lckey)
        if (lckey .ne. 0) then
          if (iq(lckey+mbpr) .eq. mpelm) lccls = lq(lckey-3)
        else
          call difind(ldbnk, word2, isrc, lccls)
          if (lccls .ne. 0) then
            lckey = lq(lccls+1)
 
*---- Approximate match with keyword or class name.
          else
            call difind(ldkey, word2(1:leng2), ikey, lckey)
            if (lckey .ne. 0) then
              if (iq(lckey+mbpr) .eq. mpelm) lccls = lq(lckey-3)
            else
              call difind(ldbnk, word2(1:leng2), isrc, lccls)
              if (lccls .ne. 0) then
                lckey = lq(lccls+1)
              endif
            endif
          endif
        endif
 
*---- JCASE = 3. Find parameter keyword.
      else if (jcase .eq. 3) then
        label = word1
        call difind(ldkey, 'PARAMETE', ikey, lckey)
      endif
 
*---- Check if a keyword was recognized.
      if (lckey .eq. 0) then
        call rdfail('AAREAD', 1, 'No command keyword found.')
 
*---- Test for redundant formal argument list.
      else
        if (jform .ne. 0) then
          if (iq(lckey+mbpr).ne.mplin .or. iq(lckey+mbsp).ne.1) then
            call rdwarn('AAREAD', 1, 'Redundant formal argument list.')
          endif
        endif
        call diname(ldkey, ikey, key)
        if (lccmd .ne. 0) call diname(ldbnk, icmd, label)
      endif
 
 9999 end
