      subroutine kwdump(lkey)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Dump a complete keyword bank.                                      *
* Input:                                                               *
*   LKEY(1)  (pointer)  Bank pointer.                                  *
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
      integer iln,ipr,isp,j,nkat
      integer           lkey(*)
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      character*(mcnam) label
      character*1       atype(10)
      data atype / 'N', 'I', 'R', 'D', 'L', 'S', 'B', 'R', 'C', 'V' /
 
*---- Copy data to local store.
      call diname(ldkey, iq(lkey(1)+mbnam), label)
      call kwget(lkey, iln, ipr, isp, nkat)
 
*---- Print header information.
      write (iqlog, 910) label, iln, ipr, isp
 
*---- Print attributes.
      if (nkat .gt. 0) then
        write (iqlog, 920)
        do 90 j = 1, nkat
          write (iqlog, 930) katnam(j), atype(iatype(j)),
     +      iadim1(j), iadim2(j), iadim3(j)
   90   continue
      endif
      write (iqlog, 940)
 
  910 format(' '/' KWDUMP.',t11,'Keyword name:',t31,a/
     +       t11,'Definition line:',t31,i8/
     +       t11,'Process code:',t31,i8/
     +       t11,'Subprocess code:',t31,i8)
  920 format(' '/t11,'Attribute',t23,'Type',t36,'Dim 1',t48,'Dim 2',
     +       t60,'Dim 3')
  930 format(t11,a,t24,a1,t36,i5,t48,i5,t60,i5)
  940 format(' ')
 
      end
