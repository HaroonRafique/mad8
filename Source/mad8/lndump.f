      subroutine lndump(lline, label)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Dump a line definition.                                            *
* Input:                                                               *
*   LLINE(1)  (pointer) Pointer to line module.                        *
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
      integer i,ilist,j,jform,lform,nform,nlist
      integer           lline(*)
      character*(mcnam) label
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
      integer mlact,mlf1,mlf2,mlfm,mlfree,mlhd,mlnxt,mlprv,mlref,mlrep,
     +mlsiz,mltyp
 
*---- Bias for beam line list information.
      parameter         (mlfm = mbat + 1, mlhd = mbat + 2,
     +                   mlf1 = mbat + 3, mlf2 = mbat + 4,
     +                   mlfree = mbat + 4)
 
*---- Bias for beam line list cells.
      parameter         (mltyp = 1, mlprv = 2, mlnxt = 3, mlrep = 4,
     +                   mlref = 5, mlact = 6, mlsiz = 6)
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      character*(mcnam) frmnam(10)
 
*---- Print header information.
      write (iqlog, 910) label,
     +  iq(lline(1)+mbln), iq(lline(1)+mbpr), iq(lline(1)+mbsp)
 
*---- Formal arguments.
      lform = lq(lline(1)-1)
      if (lform .ne. 0) then
        nform = iq(lform-1)
        write (iqlog, 920)
        i = 0
        do 10 jform = 1, nform, mwnam
          i = i + 1
          call uhtoc(q(lform+jform), mcwrd, frmnam(i), mcnam)
          if (i .eq. 10) then
            write (iqlog, 930) (frmnam(j), j = 1, 10)
            i = 0
          endif
   10   continue
        if (i .ne. 0) then
          write (iqlog, 930) (frmnam(j), j = 1, i)
        endif
      endif
 
*---- List data.
      nlist = iq(lline(1)-1) - 1
      write (iqlog, 940) iq(lline(1)+mlhd)
      do 20 ilist = mlfree, nlist, mlsiz
        write (iqlog, 950) ilist, (iq(lline(1)+ilist+j), j = 1, mlsiz)
   20 continue
      write (iqlog, 960)
 
  910 format(' '/' LNDUMP.  Command name:     ',a/
     +       t11,'Definition line:  ',i8/
     +       t11,'Process code:     ',i8/
     +       t11,'Subprocess code:  ',i8)
  920 format(' '/t11,'Formal arguments:')
  930 format(t11,10(a,' '))
  940 format(' '/t11,'Beam line list (header at',i8,'):'/
     +       t11,'Position      Type  Previous      Next    Repeat',
     +       ' Reference   Actuals')
  950 format(t11,i8,6i10)
  960 format(' ')
 
      end
