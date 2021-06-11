      subroutine aadump(lbank)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Dump a complete command or parameter bank.                         *
* Input:                                                               *
*   LBANK(1) (pointer)  Bank pointer.                                  *
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
      integer i,ibdata,ic,idim1,idim2,idim3,ikat,ikdata,ilen,itype,ival,
     +j1,j2,j3,jtype,l,lbnk,lkey,nkat
      double precision rval
      integer           lbank(*)
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
      integer mkdim1,mkdim2,mkdim3,mkf1,mkf2,mkname,mksiz,mktype
 
*---- Bias for keyword attribute groups.
      parameter         (mkf1 = 1, mktype = 2, mkdim1 = 3, mkdim2 = 4,
     +                   mkdim3 = 5, mkf2 = 6, mkname = 7,
     +                   mksiz = mwnam + 6)
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
 
      character*(mcnam) atrnam, label
      character*1       atype(10)
      character*(mcstr) string
      logical           lval
      data atype / 'N', 'I', 'R', 'D', 'L', 'S', 'B', 'R', 'C', 'V' /
 
*---- Print header information.
      call diname(ldbnk, iq(lbank(1)+mbnam), label)
      write (iqlog, 910) label,
     +  iq(lbank(1)+mbln), iq(lbank(1)+mbpr), iq(lbank(1)+mbsp)
 
*---- Get keyword attributes.
*     Warning: LKEY and LBNK are local links.
*              No ZEBRA calls should be made after this point.
      lbnk = lbank(1)
      lkey = lq(lbnk+1)
      if (iq(lkey+mbat) .gt. 0) then
        write (iqlog, 920)
        ikdata = mbat
        ibdata = mbat
        ic = 0
        nkat = iq(lkey+mbat)
        do 190 ikat = 1, nkat
 
*---- Data from keyword bank.
          itype = iq(lkey+ikdata+mktype)
          idim1 = iq(lkey+ikdata+mkdim1)
          idim2 = iq(lkey+ikdata+mkdim2)
          idim3 = iq(lkey+ikdata+mkdim3)
          call uhtoc(q(lkey+ikdata+mkname), mcwrd, atrnam, mcnam)
          ikdata = ikdata + mksiz
          write (iqlog, 930) atrnam, atype(itype), idim1, idim2, idim3
 
*---- Data from command or definition bank.
          do 180 j3 = 1, idim3
          do 180 j2 = 1, idim2
          do 180 j1 = 1, idim1
            ic = ic + 1
            jtype = iq(lbnk+ibdata+mctyp)
*                  NAM INT FLT DEF LOG STR LIN RNG CON VAR
            go to (10, 20, 30, 40, 50, 60, 70, 80, 90, 100), itype
            go to 170
   10       continue
              call utgnam(lbnk, ic, ic, atrnam)
              write (iqlog, 940) ic, j1, j2, j3, jtype, atrnam
            go to 170
   20       continue
              ival = iq(lbnk+ibdata+mcval)
              write (iqlog, 950) ic, j1, j2, j3, jtype, ival
            go to 170
   30       continue
   40       continue
              call ucopy(q(lbnk+ibdata+mcval), rval, mwflt)
              write (iqlog, 960) ic, j1, j2, j3, jtype, rval
            go to 170
   50       continue
              lval = iq(lbnk+ibdata+mcval) .ne. 0
              write (iqlog, 970) ic, j1, j2, j3, jtype, lval
            go to 170
   60       continue
              call utgstr(lbnk, ic, ic, string)
              ilen = min(iq(lbnk+ibdata+mcval), mcstr)
              write (iqlog, 980) ic, j1, j2, j3, jtype, ilen
              do 65 i = 1, ilen, 80
                l = min(ilen, i + 79)
                write (iqlog, 985) string(i:l)
   65         continue
            go to 170
   70       continue
   80       continue
   90       continue
  100       continue
  170       continue
            ibdata = ibdata + mcsiz
  180     continue
  190   continue
        write (iqlog, 990)
      endif
 
  910 format(' '/' AADUMP.  Command name:     ',a/
     +       t11,'Definition line:  ',i8/
     +       t11,'Process code:     ',i8/
     +       t11,'Subprocess code:  ',i8)
  920 format(' '/t11,'Attribute',t23,'Type',t36,'Dim 1',t48,'Dim 2',
     +       t60,'Dim 3')
  930 format(t11,a,t24,a1,t36,i5,t48,i5,t60,i5)
  940 format(t11,5i5,5x,a)
  950 format(t11,5i5,5x,i8)
  960 format(t11,5i5,5x,1pe16.8)
  970 format(t11,5i5,5x,l8)
  980 format(t11,6i5,5x,i8)
  985 format(t11,a)
  990 format(' ')
 
      end
