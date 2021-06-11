      subroutine trnois
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Define noise for an element parameter, NOISE command.              *
* Attributes:                                                          *
*   VARIABLE  (variable)Variable to be affected.                       *
*   AMPLITUDE (real)    Amplitudes for perturbations.                  *
*   FREQUENCY (real)    Frequencies for perturbations.                 *
*   PHASE     (real)    Phases for perturbations.                      *
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
      integer iadim1,iadim2,iadim3,iatype,maxat
 
*---- Block for current keyword.
      parameter         (maxat = 100)
      common /keywdi/   iatype(maxat),
     +                  iadim1(maxat), iadim2(maxat), iadim3(maxat)
      common /keywdc/   katnam(maxat)
      save              /keywdi/, /keywdc/
      character*(mcnam) katnam
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
      integer ltrbuf,ltrcur,ltrfbf,ltrnoi,ltrnum,ltrobs,ltrrbf,ltrstt,
     +ltrtab,ltrtmp
 
*---- Links used for tracking.
      common /trlink/   ltrnum, ltrstt, ltrnoi, ltrfbf, ltrrbf, ltrobs,
     +                  ltrcur, ltrtmp, ltrtab, ltrbuf
      save              /trlink/
      integer maxnoi
      double precision ampl,freq,orig,phas
 
*---- Data for noise banks.
      parameter         (maxnoi = 20)
      common /trndat/   orig, ampl(maxnoi), freq(maxnoi), phas(maxnoi)
      save   /trndat/
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer iamp,ibias,idata,idim,ifrq,iln,iphs,ipr,isp,nd,ndim,nkat
      double precision amp,frq,phs
 
*---- Variable name is required (attribute 2).
      if (mod(iq(lccmd+mbat+mctyp),10).eq.0 .or. lq(lccmd-1).eq.0) then
        call aafail('TRNOIS', 1, 'NOISE command requires a variable.')
 
*---- Parameter must be known (error message in EXFILL).
      else if (lq(lq(lccmd-1)-1) .eq. 0) then
        error = .true.
 
*---- Parameter must refer to a physical element.
      else if (iq(lq(lq(lccmd-1)-1)+mbpr) .ne. mpelm) then
        call aafail('TRNOIS', 1,
     +  'NOISE command must refer to an element.')
 
*---- Find dimension of noise bank.
      else
        call kwget(lckey, iln, ipr, isp, nkat)
        ndim = 0
        iamp = mbat + mcsiz
        ifrq = iamp + mcsiz * iadim1(2)
        iphs = ifrq + mcsiz * iadim1(3)
        do 10 idim = 1, min(iadim1(2),iadim1(3),iadim1(4),maxnoi)
          call ucopy(q(lccmd+iamp+mcval), amp, mwflt)
          call ucopy(q(lccmd+ifrq+mcval), frq, mwflt)
          call ucopy(q(lccmd+iphs+mcval), phs, mwflt)
          if (amp .ne. 0) then
            ndim = ndim + 1
            ampl(ndim) = amp
            freq(ndim) = frq
            phas(ndim) = phs
          endif
          iamp = iamp + mcsiz
          ifrq = ifrq + mcsiz
          iphs = iphs + mcsiz
   10   continue
 
*---- Book a noise bank.
        if (ndim .gt. 0) then
          nd = mwflt * (3 * ndim + 1)
          call mzbook(2, ltrnoi, ltrnoi, 1, 'NOIS', 1, 1, nd, mreal, 0)
 
*---- Copy variable reference.
          lcvar = lq(lccmd-1)
          lcelm = lq(lcvar-1)
          call mzcopy(2, lcvar, 2, ltrnoi, -1, '.')
          lcvar = lq(ltrnoi-1)
          call exlkvr
 
*---- Fetch original value of this parameter.
          ibias = mbat + (iq(lcvar+mvbias) - 1) * mcsiz
          call ucopy(q(lcelm+ibias+mcval), orig, mwflt)
          call ucopy(orig, q(ltrnoi+1), mwflt)
          idata = mwflt + 1
 
*---- Dependent parameter becomes independent, if varied.
          lcexp = lq(lcelm-iq(lcvar+mvbias))
          if (lcexp .ne. 0) then
            call rdwarn('TRNOIS', 1,
     +      'Parameter depends on other values --- dependence deleted.')
            call aadrop(lcexp)
            iq(lcelm+ibias+mctyp) = 10*mtflt + 1
          endif
 
*---- Copy amplitudes and frequencies.
          iq(ltrnoi-5) = ndim
          call ucopy(ampl, q(ltrnoi+idata), mwflt*ndim)
          idata = idata + mwflt*ndim
          call ucopy(freq, q(ltrnoi+idata), mwflt*ndim)
          idata = idata + mwflt*ndim
          call ucopy(phas, q(ltrnoi+idata), mwflt*ndim)
        endif
      endif
 
      end
