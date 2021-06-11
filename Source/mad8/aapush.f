      subroutine aapush
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Push up to MAXCOL variables and/or expressions to SPECIAL table.   *
*   Determine the number of columns in the special table.              *
* Attributes:                                                          *
*   up to MAXCOL expressions.                                          *
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
      integer isave,isvbuf
 
*---- Buffer for SAVE and VIEW commands.
      common /svbuff/   savbuf
      common /svinfo/   isave, isvbuf
      save              /svbuff/, /svinfo/
      character*80      savbuf
      integer icurr,itabun,itbbuf,itbfil,itbspc,koff,ltable,ltbbuf,
     +ltbcol,ltbcur,ltbdsc,ltbsav,ltbspc,ltbsum,ltbtab,ltbtmp,nblock,
     +nbout,ncmax,nrbmod
 
*---- Communication area for table manager routines.
      integer mleng,mnblck,mstep
      parameter         (mnblck=10, mleng=512*mnblck, mstep=100)
      common /tbcomm/   ltable, ltbcol, ltbsum,
     +                  ltbbuf, ltbspc, ltbdsc, ltbtab, ltbcur, ltbsav,
     +                  ltbtmp,
     +                  nblock, nbout, nrbmod, icurr, ncmax, itbspc,
     +                  koff, itbfil, itabun, itbbuf(mleng,2)
      save              /tbcomm/
      integer icfrm,iclen,maxcol
 
*---- Table header information.
      parameter         (maxcol = 100)
      common /tbhedc/   cname(maxcol), cform(maxcol)
      common /tbhedf/   icfrm(maxcol), iclen(maxcol)
      save              /tbhedc/, /tbhedf/
      character*20      cname, cform
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
      integer ibias,icat,idata,nb,nc,ncat,nr,ns
      double precision dummy
 
      character*(mcnam) tabnam, colhed(maxcol)
      integer           icform(maxcol)
 
*---- Create new SPECIAL table, if it does not exist yet.
      if (ltbspc .eq. 0) then
        tabnam = 'SPECIAL'
        nb = 1
        nr = 1000
        ns = 1
        nc = 0
        ncat = iq(lccmd+mbat)
        idata = mbat
        do 10 icat = 1, ncat
          if (mod(iq(lccmd+idata+mctyp),10) .ne. 0) then
            isvbuf = 0
            call svattr(lccmd, icat, mtflt)
            nc = nc + 1
            colhed(nc) = savbuf(1:min(isvbuf,mcnam))
            icform(nc) = mreal
          endif
          idata = idata + mcsiz
   10   continue
 
        call tbcrea(tabnam, ns, nr, nc, colhed, icform, nb, ltbspc)
        call tbpdsc(ltbspc, 'TYPE', 5, 0, dummy, 'SPECIAL')
        itbspc = 0
      endif
 
*---- Store the expression values in next table row.
      itbspc = itbspc + 1
      call tbset(ltbspc, itbspc, 3, ltbbuf)
      ncat = iq(lccmd+mbat)
      idata = mbat
      ibias = 1
      do 20 icat = 1, ncat
        if (mod(iq(lccmd+idata+mctyp),10) .ne. 0) then
          call ucopy(q(lccmd+idata+mcval), q(ltbbuf+ibias), mwflt)
          ibias = ibias + mwflt
        endif
        idata = idata + mcsiz
   20 continue
 
      end
