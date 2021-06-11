      subroutine emtwdo
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TWISS3 command: Track Mais-Ripken lattice functions.               *
* Attributes:                                                          *
*   SAVE      (name)    SAVE option: Table name.                       *
*   BETA0     (name)    Initial values for functions (ignored).        *
*   LINE      (line)    Line for initial conditions.                   *
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
      integer mbet,mlin,msav
 
      parameter         (msav = 1, mbet = 2, mlin = 3)
 
      character*(mcnam) twsnam(2), savnam, betnam
      equivalence       (savnam, twsnam(1)), (betnam, twsnam(2))
      integer           itype(3)
 
*---- Check main beam line.
      call lnchck('EMTWDO', error)
 
*---- Retrieve attributes.
      call utgtyp(lccmd, itype)
      if (.not. error) then
        savnam = ' '
        call utgnam(lccmd, msav, mbet, twsnam)
        call emtwgo(itype, savnam, betnam, mlin)
      endif
 
      end
