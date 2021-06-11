      subroutine trrobs
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Define observation point for tracking.                             *
* Attributes:                                                          *
*   PLACE     (place)   Observation point.                             *
*   TABLE     (name)    Name of track table to be filled.              *
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
      integer mobs,morng,mosiz,motab
 
*---- Parameters for observation points.
      parameter         (mobs = 10)
      parameter         (motab = 1, morng = motab + mwnam)
      parameter         (mosiz = morng + mcrng / mcwrd - 1)
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
      integer ipos
      double precision eflag
 
      character*(mcrng) tabrng
      character*(mcnam) tabnam
 
*---- Recover attributes.
      lcatt = lq(lccmd-1)
      call utgpos(lcatt, lcseq, 0, ipos, eflag)
      call enrang(lcatt, tabrng)
      tabnam = tabrng
      call utgnam(lccmd, 2, 2, tabnam)
 
*---- Build list bank and bank for orbit and eigenvectors.
      if (.not. error) then
        call mzbook(2, ltrtmp, ltrobs, 1, 'TOBS', 2, 1, mosiz, 5, -1)
        iq(ltrtmp-5) = ipos
        call uctoh(tabnam, iq(ltrtmp+motab), mcwrd, mcnam)
        call uctoh(tabrng, iq(ltrtmp+morng), mcwrd, mcrng)
      endif
 
      end
