      subroutine lnbeam(label, if1, if2)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode beam line definition.                                       *
* Input:                                                               *
*   LABEL    (char)     Label for new definition (required).           *
*   IF1      (integer)  First character of formals list (optional).    *
*   IF2      (integer)  Last character of formals list.                *
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
      integer if1,if2
      character*(mcnam) label
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
 
*---- Build LINE bank and link it to LINE keyword.
      call lnmake(lccmd, lckey)
 
*---- Decode formals list.
      if (if1 .ne. 0) then
        call dcform(lccmd, if1, if2, error)
      endif
 
*---- Skip separator.
      if (token(jtok) .eq. ','  .or.  token(jtok) .eq. '=') then
        jtok = jtok + 1
      endif
 
*---- Decode beam line list.
      call dclist(lccmd, error)
 
*---- If error detected, drop line bank.
      if (error) then
        call aadrop(lccmd)
 
*---- If all OK, link line bank to directory.
      else
        call didefi(ldbnk, label, lccmd)
      endif
 
      end
