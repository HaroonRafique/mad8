      subroutine eropt
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Set options for error definitions and applications.                *
*   EOPT command.                                                      *
* Attributes:                                                          *
*   SEED      (integer) Seed for random generator.                     *
*   ADD       (logical) Superposition flag.                            *
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
 
*---- Option for additive error components.
      common /erdata/   adderr
      logical           adderr
      save              /erdata/
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
      double precision seed
 
*---- SEED option.
      seed = 0.0
      call utgflt(lccmd, 1, 1, seed)
      if (seed .ne. 0.0) call init55(int(seed + 0.5))
 
*---- ADD option.
      call utglog(lccmd, 2, 2, adderr)
 
      end
