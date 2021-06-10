      subroutine mtrmat
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Match R matrix; RMATRIX command.                                   *
* Attributes:                                                          *
*   RANGE    (range)   Where to apply the constraint.                  *
*   RM(6,6)  (real)    Desired values for matrix elements.             *
*   W(6,6)   (real)    Matching weights.                               *
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
      integer mxf1,mxf2,mxop,mxsiz,mxval
 
*---- Bias for expression banks.
      parameter         (mxf1 = 1, mxop = 2, mxf2 = 3, mxval = 4,
     +                   mxsiz = mwflt + 3)
 
*---- Flags for matching.
      common /mtcflg/   flbeta, florb, flrmat, fltmat, flchrm
      save              /mtcflg/
      logical           flbeta, florb, flrmat, fltmat, flchrm
      character *(mcnam)  sequd, betnm
      common / dmatchc / sequd(2), betnm(2)
      integer mtdbfl, imsequ
      common / dmatchi / mtdbfl, imsequ
      logical bdtflg
      common / dmatchl / bdtflg(2)
      integer lcon,lmcon,lmtbet,lmtlin,lmtseq,lmtsub,lmvar,lptr,lref,
     +lsmat,lvar,lbeta0
 
*---- Link area for matching.
      common /mtlink/   lsmat, lmcon, lmvar,
     +                  lmtlin, lmtseq, lmtbet, lbeta0(2), lmtsub,
     +                  lcon, lref, lvar, lptr
      save              /mtlink/
      double precision rtdes,rtsav,rtwgt,ttsav
 
*---- Working area for matrix constraints.
      common /mtcmtx/   rtsav(36), ttsav(216), rtdes(216), rtwgt(216)
      save   /mtcmtx/
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
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer idata,idest,ilink,ipos1,ipos2,irmat,irwgt,jcon,nd,nfun
 
*---- Valid range required.
      idata = mbat
      if (iq(lccmd+idata+mctyp) .ne. 10*mtrng + 1) then
        call aafail('MTRMAT', 1, 'RMATRIX command requires a RANGE.')
        go to 9999
      endif
      lcatt = lq(lccmd-1)
      call utgrng(lcatt, lcseq, ipos1, ipos2, error)
      if (error) go to 9999
 
*---- Lift constraint bank.
*     NOTE. First link must always be zero (see MTPINI).
      nd = 3 * 36 * mwflt
      call mzbook(2, lcon, lmcon, 1, 'RMAT', 4, 2, nd, mreal, 0)
      nfun = 0
 
*---- Fill in constraint data and copy expressions.
      ilink = 2
      irmat = mbat + mcsiz
      irwgt = irmat + 36 * mcsiz
      idest = 36 * mwflt + 1
      do 10 jcon = 1, 36
        if (mod(iq(lccmd+irmat+mctyp),10) .ne. 0) then
          lcexp = lq(lccmd-ilink)
          if (lcexp .ne. 0) then
            call mzcopy(2, lcexp, 2, lcon, -2, 'Z')
            lcexp = lq(lcon-2)
            iq(lcexp+mxsiz*iq(lcexp-3)+mxval) = idest
            call exlkex
          else
            call ucopy(q(lccmd+irmat+mcval), rtdes(jcon), mwflt)
          endif
          if (mod(iq(lccmd+irwgt+mctyp),10) .ne. 0) then
            call ucopy(q(lccmd+irwgt+mcval), rtwgt(jcon), mwflt)
          else
            rtwgt(jcon) = 1.0
          endif
          nfun = nfun + 1
        else
          rtdes(jcon) = 0.0
          rtwgt(jcon) = 0.0
        endif
        ilink = ilink + 1
        irmat = irmat + mcsiz
        irwgt = irwgt + mcsiz
        idest = idest + mwflt
   10 continue
 
*---- Copy data to constraint bank.
*     Leave space for inverted accumulated matrix.
      call ucopy(rtdes, q(lcon+36*mwflt+1), 36*mwflt)
      call ucopy(rtwgt, q(lcon+(36+36)*mwflt+1), 36*mwflt)
 
*---- Link constraint to proper places.
      imsequ = 1
      call mtacon(ipos1, 4, 0, error)
      call mtacon(ipos2, 5, nfun, error)
      ifirst = 0
      icovar = 0
      flrmat = .true.
 
 9999 end
