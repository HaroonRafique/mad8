      subroutine svdict(lkey)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Encode a keyword bank for SAVE or VIEW command.                    *
* Input:                                                               *
*   LKEY(1)   (pointer) Pointer to keyword bank.                       *
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
      integer idata,ikat,iln,ipr,isp,itype,nkat
      integer           lkey(1)
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
 
      character*10      types
      character*(mcnam) label
 
      data types        / 'NIRDLSBPCV' /
 
*---- Label and "keyword" to first line.
      call svbegn
      call diname(ldkey, iq(lkey(1)+mbnam), label)
      call svname(label)
      call svlitt(': ')
      call svname('KEYWORD')
 
*---- Retrieve keyword data.
      call kwget(lkey, iln, ipr, isp, nkat)
 
*---- Process and subprocess.
      call svlitt(', PR = ')
      call svint(ipr)
      call svlitt(', SP = ')
      call svint(isp)
 
*---- Loop over attributes.
      idata = mbat
      do 90 ikat = 1, nkat
        call svlitt(', ')
        call svcont
        call svname(katnam(ikat))
        call svlitt(' = (')
        itype = iatype(ikat)
        call svlitt(types(itype:itype))
        call svlitt('(')
        call svint(iadim1(ikat))
 
*---- Treat extra dimensions.
        if (iadim2(ikat) .gt. 1  .or.  iadim3(ikat) .gt. 1) then
          call svlitt(',')
          call svint(iadim2(ikat))
          if (iadim3(ikat) .gt. 1) then
            call svlitt(',')
            call svint(iadim3(ikat))
          endif
        endif
        call svlitt(')')
 
*---- Default value, if present.
        if (mod(iq(lq(lkey(1)-2)+idata+mctyp),10) .ne. 0) then
          call svlitt(', = ')
          lcdef = lq(lkey(1)-2)
          call svattr(lcdef, idata, iatype(ikat))
        endif
 
*---- Close group, continue if more.
        call svlitt(')')
   90 continue
      call svdump
 
      end
