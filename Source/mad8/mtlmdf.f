      subroutine mtlmdf
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   LMDIF command.                                                     *
* Attributes:                                                          *
*   TOLERANCE (real)    Final tolerance for match.                     *
*   CALLS     (integer) Call limit to penalty function.                *
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
      integer idiag,idx,ifjac,ifvec,ipvt,iqtf,iwa1,iwa2,iwa3,iwa4,ixvec,
     +ncalls
 
      external          mtfcn
      logical           skip
 
*---- Insert parameters.
      tol = 1.0d-8
      call utgflt(lccmd, 1, 1, tol)
      ncalls = 1000
      call utgint(lccmd, 2, 2, ncalls)
 
*---- Any constraints?
      skip = .false.
      if (ncon .eq. 0) then
        call aawarn('MTLMDF', 1, 'No constraints seen.')
        skip = .true.
      endif
 
*---- Any variable parameters?
      if (nvar .eq. 0) then
        call aawarn('MTLMDF', 1, 'No variables seen.')
        skip = .true.
      endif
 
*---- Too many variable parameters.
      if (nvar .gt. ncon) then
        call aawarn('MTLMDF', 1, 'LMDIF command cannot handle'
     +  // ' problems with more parameters than constraints.')
        skip = .true.
      endif
      if (skip .or. error .or. scan) go to 9999
 
*---- Allocate working space.
      ixvec  = iwork
      idx    = ixvec  + nvar
      ifvec  = idx    + nvar
      idiag  = ifvec  + ncon
      ifjac  = idiag  + nvar
      ipvt   = ifjac  + nvar * ncon
      iqtf   = ipvt   + nvar
      iwa1   = iqtf   + nvar
      iwa2   = iwa1   + nvar
      iwa3   = iwa2   + nvar
      iwa4   = iwa3   + nvar
      iwork  = iwa4   + ncon
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- CALL Minimization routine.
      nfcnmx = nfcnmx + ncalls
      call mtgeti(nvar, dq(ixvec+1), dq(idx+1))
      call lmdif(mtfcn, ncon, nvar, dq(ixvec+1), dq(ifvec+1),
     +           tol, dq(idiag+1), 1.0d0,
     +           dq(ifjac+1), ncon, dq(ipvt+1), dq(iqtf+1),
     +           dq(iwa1+1), dq(iwa2+1), dq(iwa3+1), dq(iwa4+1))
 
 9999 end
