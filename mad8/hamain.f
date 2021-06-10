      subroutine hamain(ipr, isp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Control routine for HARMON commands.                               *
* Input:                                                               *
*   IPR      (integer)  Process code.                                  *
*   ISP      (integer)  Subprocess code.                               *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer lhalbf,lhaltb,lhasbf,lhastb,mlcol,mscol
 
*---- Link area for HARMON module.
      parameter         (mlcol = 17, mscol = 12)
      common /halink/   lhaltb, lhalbf, lhastb, lhasbf
      save              /halink/
      integer lcon,lmcon,lmtbet,lmtlin,lmtseq,lmtsub,lmvar,lptr,lref,
     +lsmat,lvar,lbeta0
 
*---- Link area for matching.
      common /mtlink/   lsmat, lmcon, lmvar,
     +                  lmtlin, lmtseq, lmtbet, lbeta0(2), lmtsub,
     +                  lcon, lref, lvar, lptr
      save              /mtlink/
      integer icall,icovar,ifirst,ilevel,imode,istrat,ncon,nfcn,nfcnmx,
     +nvar
      double precision edm,fmin,tol,up
 
*---- Communication area for routines derived from MINUIT.
      common /minchr/   crout, cstat
      common /mindbl/   edm, fmin, tol, up
      common /minint/   icall, icovar, ifirst, imode, ilevel, istrat,
     +                  ncon, nvar, nfcn, nfcnmx
      common /minflt/   time1, time2
      save              /minchr/, /mindbl/, /minint/, /minflt/
      character         crout*8, cstat*16
      real              time1, time2
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
      integer ihaflg,ipr,isp
 
      data ihaflg       / 0 /
 
*---- Initialize link area for HARMON module.
      if (ihaflg .eq. 0) then
        call mzlink(0, '/HALINK/', lhaltb, lhaltb, lhasbf)
        ihaflg = 1
      endif
 
*---- Check valid use of command.
      if (isp .ne. 1  .and.  imodul .ne. ipr) then
        msg(1) =
     +  'Cannot run HARMON subcommand outside a HARMON process,'
        msg(2) = 'HARMON command required first.'
        call aafail('HAMAIN', 2, msg)
      else if (isp .eq. 1  .and.  imodul .ne. 0) then
        msg(1) =
     +  'Cannot initiate HARMON while previous process not complete,'
        msg(2) = 'Enter proper ENDxxxx command first.'
        call aafail('HAMAIN', 2, msg)
 
*---- HARMON --- Enter HARMON mode.
      else if (isp .eq. 1) then
        call habegn
 
*---- Timing information.
        if (.not. error) then
          call timex(time1)
          if (trace) then
            write (msg, 910) time1
  910       format('Begin HARMON mode, time = ',f12.3)
            call aainfo('HAMAIN', 1, msg)
          endif
          imodul = ipr
        endif
 
*---- ENDHARMON --- End of harmon command sequence.
      else if (isp .eq. 2) then
 
*---- Drop HARMON tables.
        if (lsmat .ne. 0) call mzdrop(0, lsmat, ' ')
        call tbdrop(lhaltb)
        call tbdrop(lhastb)
 
*---- Timing information.
        call timex(time2)
        if (trace) then
          write (msg, 920) time2, time2 - time1
  920     format('End HARMON mode, time = ',f12.3,
     +           ', time for HARMON = ',f12.3,' seconds')
          call aainfo('HAMAIN', 1, msg)
        endif
        imodul = 0
 
*---- HRESONANCE --- Resonance calculation.
      else if (isp .eq. 3) then
        call haresc
 
*---- HCHROMATICITY --- Chromaticity calculation.
      else if (isp .eq. 4) then
        call hachrm
 
*---- HFUNCTION --- Calculate resonance functions.
      else if (isp .eq. 5) then
        call hafunc
 
*---- HTUNE --- Tune sextupoles for desired chromaticity.
      else if (isp .eq. 6) then
        call hatune
 
*---- HVARY --- Vary sextupole.
      else if (isp .eq. 7) then
        call havary
 
*---- HCELL --- Minimize for periodic system.
      else if (isp .eq. 8) then
        call hacell
 
*---- HWEIGHT --- Set matching weights.
      else if (isp .eq. 9) then
        call haweig
 
*---- HLEVEL --- Set printing level.
      else if (isp .eq. 10) then
        call utgint(lccmd, 1, 1, ilevel)
 
*---- User-defined code.
      else
        call usercm(ipr, isp)
      endif
 
      end
