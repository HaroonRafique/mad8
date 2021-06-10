      subroutine mtmigr
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   MIGRAD command.                                                    *
* Attributes:                                                          *
*   TOLERANCE (real)    Final tolerance for match.                     *
*   CALLS     (integer) Call limit to penalty function.                *
*   STRATEGY  (integer) Strategy selection (see MINUIT manual).        *
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer icov,idx,ifvec,iwa,ixvec,ncalls
 
      external          mtfcn
      logical           skip
 
*---- Insert parameters.
      tol = 1.0d-8
      call utgflt(lccmd, 1, 1, tol)
      ncalls = 1000
      call utgint(lccmd, 2, 2, ncalls)
      istrat = 1
      call utgint(lccmd, 3, 3, istrat)
 
*---- Any constraints?
      skip = .false.
      if (ncon .eq. 0) then
        call aawarn('MTMIGR', 1, 'No constraints seen.')
        skip = .true.
      endif
 
*---- Any variable parameters?
      if (nvar .eq. 0) then
        call aawarn('MTMIGR', 1, 'No variables seen.')
        skip = .true.
      endif
      if (skip .or. error .or. scan) go to 9999
 
*---- Too many variable parameters?
      if (nvar .gt. ncon) then
        msg(1) = 'More variables than constraints seen,'
        msg(2) = 'MIGRAD may not converge to optimal solution.'
        call aawarn('MTMIGR', 2, msg)
      endif
 
*---- Assign working space.
      ixvec = iwork
      idx   = ixvec + nvar
      ifvec = idx   + nvar
      icov  = ifvec + ncon
      iwa   = icov  + nvar * nvar
      iwork = iwa   + nvar * 7
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Call minimization routine.
      nfcnmx = nfcnmx + ncalls
      call mtgeti(nvar, dq(ixvec+1), dq(idx+1))
      call mtmig1(mtfcn, ncon, nvar, dq(ixvec+1), dq(idx+1),
     +            dq(ifvec+1), dq(icov+1), dq(iwa+1))
 
*---- Release working space.
      iwork = ixvec
 
 9999 end
