      subroutine enprrf
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print data of RF system.                                           *
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
      integer iharm,irfk
      double precision data
 
      character*(mcnam) cavnam
      dimension         data(4)
 
*---- Loop over all RF cavities.
      write (iqpr2, 910)
      call difind(ldkey, 'RFCAVITY', irfk, lckey)
      lcelm = lq(lckey-1)
 
*---- Fetch data for next cavity and print.
   10 if (lcelm .ne. 0) then
        call diname(ldbnk, iq(lcelm+mbnam), cavnam)
        data(1) = 0.0
        data(2) = 0.0
        data(3) = 0.0
        data(4) = 0.0
        iharm = 0
        call utgflt(lcelm, 2, 5, data)
        call utgint(lcelm, 6, 6, iharm)
        write (iqpr2, 920) cavnam, data, iharm, iq(lcelm-5)
        lcelm = lq(lcelm)
        go to 10
      endif
 
  910 format(' '/' RF system:'/' '/' Cavity',t40,'L',t57,'volt',
     +       t78,'lag',t92,'frequency',t109,'harmon',t119,'occur.'/
     +       ' name',t40,'m',t59,'MV',t98,'MHz')
  920 format(' ',a16,t21,4f20.6,i14,i10)
 
      end
