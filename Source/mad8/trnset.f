      subroutine trnset(time)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Apply all noise definitions to perturbed elements.                 *
* Input:                                                               *
*   TIME      (real)    Value of time to be applied.                   *
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
      integer ibias,idata,idim,ndim
      double precision angle,time,twopi
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi)
 
*---- Loop over noise records to apply noise.
      ltrcur = ltrnoi
  100 if (ltrcur .ne. 0) then
        ndim = iq(ltrcur-5)
        call ucopy(q(ltrcur+1), orig, mwflt)
        idata = mwflt + 1
        call ucopy(q(ltrcur+idata), ampl, mwflt*ndim)
        idata = idata + mwflt*ndim
        call ucopy(q(ltrcur+idata), freq, mwflt*ndim)
        idata = idata + mwflt*ndim
        call ucopy(q(ltrcur+idata), phas, mwflt*ndim)
 
*---- Apply noise.
        do 110 idim = 1, ndim
          angle = twopi * (freq(idim) * time + phas(idim))
          orig = orig + ampl(idim) * cos(angle)
  110   continue
        lcatt = lq(ltrcur-1)
        lcelm = lq(lcatt-1)
        ibias = iq(lcatt+mvbias)
        call utpflt(lcelm, ibias, ibias, orig)
        ltrcur = lq(ltrcur)
        go to 100
      endif
 
      end
