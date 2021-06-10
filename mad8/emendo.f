      subroutine emendo
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   ENVELOPE command: Track beam envelope.                             *
* Attributes:                                                          *
*   SAVE      (name)    SAVE option: Table name.                       *
*   SIGMA0    (name)    Bank for initial conditions.                   *
*   LINE      (line)    Line for initial conditions.                   *
*   TAPE      (string)  TAPE option: File name.                        *
*----------------------------------------------------------------------*
* Modified: 30-NOV-1998, M. Woodley (SLAC)                             *
*   Add support for tape file output                                   *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer idisk
      integer mlin,msav,msig,mtape
 
      parameter         (msav = 1, msig = 2, mlin = 3, mtape = 4)
 
      character*(mcnam) twsnam(2), signam, savnam
      equivalence       (savnam, twsnam(1))
      equivalence       (signam, twsnam(2))
      integer           itype(3)
      character*(mcfil) strnam, filnam
      logical           tape, eflag
*---- Check main beam line.
      call lnchck('EMENDO', error)
      if (.not. error) then
 
*---- File name for TAPE option.
        strnam = ' '
        call utgstr(lccmd, mtape, mtape, strnam)
        tape = strnam .ne. ' '
 
*---- Open file for "TAPE" option.
        if (tape) then
          call flopen(strnam, 'SWFD', 0, 0, idisk, eflag)
          tape = .not. eflag
        endif
 
*---- Retrieve attributes.
        call utgtyp(lccmd, itype)
        signam = ' '
        savnam = ' '
        call utgnam(lccmd, msav, msig, twsnam)
        call emengo(itype, savnam, signam, mlin, tape, idisk)
      endif
 
*---- Close disk file.
      if (tape) then
        call flclos(idisk, error)
        if (.not. error) then
          call flname(idisk, filnam)
          msg(1) = 'Envelope functions written on file: ' // filnam
          call aainfo('emendo', 1, msg)
        endif
      endif
 
      end
