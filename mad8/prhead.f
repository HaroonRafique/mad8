      subroutine prhead(cmdnam, title, deltap, nord, nline, npage)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print global page header on print file.                            *
* Input:                                                               *
*   TITLE     (char)    Header text.                                   *
*   CMDNAM    (char)    Command name.                                  *
*   DELTAP    (real)    Delta(p)/p.                                    *
*   NORD      (integer) Order of the map (not printed if zero).        *
* Input/output:                                                        *
*   NLINE     (integer) Current line number.                           *
*   NPAGE     (integer) Current page number.                           *
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
      integer mhead,nline,nord,npage
      double precision deltap
        character*(*)   cmdnam, title
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
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
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
 
      parameter         (mhead = 4)
 
      call prpage(iqpr2)
      write (iqpr2, 910) title, cmdnam, linnam, rngnam
      if (nord .lt. 0) then
        write (iqpr2, 920) symm, nsup, npage
      else if (nord .eq. 0) then
        write (iqpr2, 930) deltap, symm, nsup, npage
      else
        write (iqpr2, 940) deltap, symm, nsup, nord, npage
      endif
      call prline(iqpr2)
      nline = mhead
 
  910 format(' ',a,t31,a,t51,'line: ',a,t84,'range: ',a)
  920 format(t31,'symm: ',l1,t51,'super:',i4,
     +       t122,'page',i6)
  930 format(' Delta(p)/p: ',f12.6,t31,'symm: ',l1,t51,'super:',i4,
     +       t122,'page',i6)
  940 format(' Delta(p)/p: ',f12.6,t31,'symm: ',l1,t51,'super:',i4,
     +       t84,'nord:',i4,t122,'page',i6)
 
      end
