      subroutine mtmain(ipr, isp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Control routine for matching.                                      *
* Input:                                                               *
*   IPR       (integer) Process code.                                  *
*   ISP       (integer) Subprocess code.                               *
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
      integer ipr,isp
 
*---- Check valid use of command.
      if (isp .le. 2  .and.  imodul .ne. 0) then
        msg(1) =
     +  'Cannot initiate matching while previous process not complete,'
        msg(2) = 'Enter proper ENDxxxx command first.'
        call aafail('MTMAIN', 2, msg)
      else if (isp .gt. 2  .and.  imodul .ne. ipr) then
        msg(1) =
     +  'Cannot run match subcommand outside matching process,'
        msg(2) = 'CELL or MATCH command required first.'
        call aafail('MTMAIN', 2, msg)
 
*---- MATCH: Initiate insertion-matching process.
      else if (isp .eq. 1) then
        call mtmtch
        if (.not. error) then
          imodul = ipr
        endif
 
*---- CELL: Initiate cell-matching process.
      else if (isp .eq. 2) then
        call mtcell
        if (.not. error) then
          imodul = ipr
        endif
 
*---- ENDMATCH: End cell or insertion matching process.
      else if (isp .eq. 3) then
        call mtend
        imodul = 0
 
*---- MIGRAD: Gradient minimization.
      else if (isp .eq. 4) then
        call mtmigr
 
*---- SIMPLEX: Simplex minimization.
      else if (isp .eq. 5) then
        call mtsimp
 
*---- CONSTRAINT: Constraint.
      else if (isp .eq. 6) then
        call mtcons
 
*---- COUPLE: Sub-period constraint.
      else if (isp .eq. 7) then
        call mtcple
 
*---- FIX: Fix a variable parameter.
      else if (isp .eq. 8) then
        call mtfix
 
*---- LEVEL: Set print level.
      else if (isp .eq. 9) then
        call utgint(lccmd, 1, 1, ilevel)
 
*---- VARY: Variable parameter.
      else if (isp .eq. 10) then
        call mtvary
 
*---- WEIGHT: Set matching weights.
      else if (isp .eq. 11) then
        call mtweig
 
*---- LMDIF: Minimize by LMDIF method.
      else if (isp .eq. 12) then
        call mtlmdf
 
*---- RMATRIX: Match R matrix.
      else if (isp .eq. 13) then
        call mtrmat
 
*---- TMATRIX: Match T matrix.
      else if (isp .eq. 14) then
        call mttmat
 
*---- GLOBAL: User constraint.
      else if (isp .eq. 15) then
        call mtucon
 
*---- GWEIGHT: Global User weights.
      else if (isp .eq. 16) then
        call mtgwgt
 
*---- Unassigned subprocess codes.
      else
        call usercm(ipr, isp)
      endif
 
      end
