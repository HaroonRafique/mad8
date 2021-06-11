      subroutine aaopts
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Set command options.                                               *
* Attributes, must be given in this order in the dictionary:           *
* Real attribute:                                                      *
*   COFACT    (real)    Factor to stabilize closed orbit search.       *
* Integer flags:                                                       *
*   COMMAND   (integer) Dump new command definitions.                  *
*   DEFINE    (integer) Dump new element definitions.                  *
*   EXPRESS   (integer) Dump new expression banks.                     *
*   KEYWORD   (integer) Dump new keyword definitions.                  *
*   LINE      (integer) Dump new line definitions.                     *
* Logical flags:                                                       *
*   DEBUG     (logical) Debug output about bank manipulation.          *
*   DOUBLE    (logical) TFS tables are in double precision.            *
*   ECHO      (logical) Print input echo.                              *
*   INTER     (logical) Interactive mode.                              *
*   TRACE     (logical) Trace command execution.                       *
*   VERIFY    (logical) Verify undefined parameters.                   *
*   WARN      (logical) Switch for warning messages.                   *
* Logical flags controlling this command:                              *
*   RESET     (logical) Reset all options to default.                  *
*   TELL      (logical) List all options.                              *
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer i1,i2,mopcon,mopflg,mopflt,mopint
 
      parameter         (mopcon =  2)
      parameter         (mopflt =  1)
      parameter         (mopint =  5)
      parameter         (mopflg =  12)
      logical           intrac
 
*---- Retrieve options controlling OPTION command.
      call utglog(lccmd, 1, mopcon, optcon)
 
*---- RESET option.
      if (reset) then
        cofact = 1.0
        icmdfl = 0
        ideffl = 0
        iexpfl = 0
        ikeyfl = 0
        ilinfl = 0
        debug  = .false.
        double = .false.
        echo   = .true.
        inter  = intrac()
        trace  = .false.
        verify = .false.
        warn   = .true.
        info   = .true.
        sympl  = .true.
        rbarc  = .true.
      endif
 
*---- Real option values.
      i1 = mopcon + 1
      i2 = mopcon + mopflt
      call utgflt(lccmd, i1, i2, optflt)
 
*---- Integer option values.
      i1 = i2 + 1
      i2 = i2 + mopint
      call utgint(lccmd, i1, i2, optint)
 
*---- Logical flags.
      i1 = i2 + 1
      i2 = i2 + mopflg
      call utglog(lccmd, i1, i2, optflg)
 
*---- TELL option.
      if (tell) then
        write (iqlog, 910) cofact,
     +    icmdfl, ideffl, iexpfl, ikeyfl, ilinfl,
     +    debug, double, echo, inter, trace, verify, warn, info,
     +    sympl, rbarc
      endif
 
  910 format(' '/' AAOPTS.  Command options in effect:'/
     +       t11,'COFACT (orbit factor):       ',f8.6/
     +       t11,'COMMAND dump level:          ',i1/
     +       t11,'DEFINE dump level:           ',i1/
     +       t11,'EXPRESSion dump level:       ',i1/
     +       t11,'KEYWORD dump level:          ',i1/
     +       t11,'LINE dump level:             ',i1/
     +       t11,'DEBUG modification flags:    ',l1/
     +       t11,'DOUBLE precision tables:     ',l1/
     +       t11,'ECHO file log:               ',l1/
     +       t11,'INTERactive execution:       ',l1/
     +       t11,'TRACE command execution:     ',l1/
     +       t11,'VERIFY undefined params:     ',l1/
     +       t11,'WARNing messages:            ',l1/
     +       t11,'INFOrmation messages:        ',l1/
     +       t11,'SYMPLECtify matrices:        ',l1/
     +       t11,'convert RBEND length to arc: ',l1/' ')
 
      end
