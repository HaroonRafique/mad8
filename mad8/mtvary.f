      subroutine mtvary
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   VARY command.                                                      *
* Attributes:                                                          *
*   NAME      (name)    Parameter name.                                *
*   STEP      (real)    Initial step size.                             *
*   LOWER     (real)    Lower limit.                                   *
*   UPPER     (real)    Upper limit.                                   *
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
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
      integer mmbias,mmbnam,mmcode,mmdata,mmf1,mmf2,mmf3,mmold,mmsiz,
     +mmvnam
 
*---- Bias for variable parameters group.
      parameter         (mmf1   = 1, mmcode = 2, mmbias = 3,
     +                   mmf2   = 4, mmold = 5, mmdata = mmold+mwflt,
     +                   mmf3   = mmdata+4*mwflt, mmbnam = mmf3+1,
     +                   mmvnam = mmbnam+mwnam, mmsiz = mmf3+2*mwnam)
      integer mvattr,mvbank,mvbias,mvf1,mvf2,mvind1,mvind2,mvind3,
     +mvseen,mvsiz
 
*---- Bias for variable reference group.
      parameter         (mvf1   = 1,
     +                   mvbank = 2,
     +                   mvattr = mvbank + mwnam,
     +                   mvf2   = mvattr + mwnam,
     +                   mvseen = mvf2 + 1,
     +                   mvind1 = mvseen + 1,
     +                   mvind2 = mvind1 + 1,
     +                   mvind3 = mvind2 + 1,
     +                   mvbias = mvind3 + 1,
     +                   mvsiz = mvbias)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer lcon,lmcon,lmtbet,lmtlin,lmtseq,lmtsub,lmvar,lptr,lref,
     +lsmat,lvar,lbeta0
 
*---- Link area for matching.
      common /mtlink/   lsmat, lmcon, lmvar,
     +                  lmtlin, lmtseq, lmtbet, lbeta0(2), lmtsub,
     +                  lcon, lref, lvar, lptr
      save              /mtlink/
      integer ivcode
      double precision vmax,vmin,vold,vstep
 
*---- Working area for a single matching variable.
      common /mtcvar/   vold, vstep, vmin, vmax, ivcode
      save   /mtcvar/
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer idata,iref,jref
      double precision temp
 
      character*(mcnam) atrnam
      logical           isleng, skip
 
*---- Variable name is required.
      call mtvfnd('VARY', skip)
      if (.not. (skip .or. error)) then
 
*---- Give message if an element length is varied.
        isleng = .false.
        if (iq(lref+mbpr) .eq. mpelm) then
          call uhtoc(q(lptr+mvattr), mcwrd, atrnam, mcnam)
          isleng = atrnam .eq. 'L'
        endif
 
*---- Parameter should be real.
        lref = lq(lptr-1)
        iref = iq(lptr+mvbias)
        jref = mbat + (iref - 1) * mcsiz
        if (iq(lref+jref+mctyp) / 10 .ne. mtflt) then
          call rdfail('MTVARY', 1, 'Parameter is not real value.')
          go to 9999
        endif
 
*---- Dependent parameter becomes independent, if varied.
        lcexp = lq(lref-iref)
        if (lcexp .ne. 0) then
          call rdwarn('MTVARY', 1,
     +    'Parameter depends on other values --- dependence ignored.')
          call aadrop(lcexp)
          iq(lref+jref+mctyp) = 10*mtflt + 1
 
*---- Should not vary an element length directly.
        else if (isleng) then
          msg(1) = 'You are varying an element length,'
          msg(2) = 'this may cause the system length to change.'
          call rdwarn('MTVARY', 2, msg)
        endif
 
*---- Get old value before matching.
        call utgflt(lref, iref, iref, vold)
 
*---- Check step size and limits.
        vstep = 0.0
        vmin = 0.0
        vmax = 0.0
        call utgflt(lccmd, 2, 4, vstep)
        if (vstep .le. 0.0) then
          msg(1) = 'VARY command expects a positive STEP,'
          msg(2) = 'omitting such a value may cause trouble later.'
          call rdwarn('MTVARY', 2, msg)
        endif
        ivcode = 0
        idata = mbat + 2*mcsiz
        if (mod(iq(lccmd+idata+mctyp),10) .ne. 0) ivcode = ivcode + 1
        idata = idata + mcsiz
        if (mod(iq(lccmd+idata+mctyp),10) .ne. 0) ivcode = ivcode + 2
        if (ivcode .eq. 3  .and.  vmin .gt. vmax) then
          call rdwarn('MTVARY', 1,
     +    'Upper and lower limits interchanged.')
          temp = vmin
          vmin = vmax
          vmax = temp
        endif
 
*---- Previously variable parameter?
        if (lvar .ne. 0) then
          call rdwarn('MTVARY', 1,
     +    'Parameter is already variable --- new data will be used.')
        else
          call mzbook(2, lvar, lmvar, 1, 'BVAR', 1, 0, mmsiz, 7, 0)
          nvar = nvar + 1
        endif
 
*---- Copy data to variable bank.
        lq(lvar-1) = lref
        iq(lvar+mmf1) = 16*2 + 2
        iq(lvar+mmcode) = ivcode
        iq(lvar+mmbias) = iref
        iq(lvar+mmf2) = 16*8*mwflt + mreal
        call ucopy(vold, q(lvar+mmold), 4*mwflt)
        iq(lvar+mmf3) = 16*2*mwnam + 5
        call ucopy(q(lptr+mvbank), q(lvar+mmbnam), 2*mwnam)
 
*---- Mark working space as outdated.
        iwork = 0
        nwork = 0
        icovar = 0
      endif
 
 9999 end
