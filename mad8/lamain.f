      subroutine lamain(ipr, isp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Switch routine for Lie algebraic analysis.                         *
* Input:                                                               *
*   IPR       (integer) Process code.                                  *
*   ISP       (integer) Subprocess code.                               *
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
      double precision dq1de1,dq1de2,dq2de2,q1,q2,q3,xi1,xi2,xi3,xin1,
     +xin2,xin3
 
*---- Global quantities computed by STATIC and DYNAMIC.
      common /largo/    q1, q2, q3, xi1, xi2, xi3, xin1, xin2, xin3,
     +                  dq1de1, dq1de2, dq2de2
      save              /largo/
 
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer i,ipr,isp
      double precision deltap,dq1de3,dq2de3
 
      logical           flag(20)
 
*---- ISP = 1, "DYNAMIC".
      if (isp .eq. 1) then
 
*---- Retrieve parameters.
        deltap = 0.0
        call utgflt(lccmd, 1, 1, deltap)
        do 10 i = 1, 9
          flag(i) = .false.
   10   continue
        call utglog(lccmd, 2, 10, flag)
 
*---- Execute.
        flag(10) = .true.
        call ladyna(deltap, flag)
 
*---- Make parameters accessible to VALUE and PUSH.
        call aasetp('Q1',     q1)
        call aasetp('Q2',     q2)
        call aasetp('Q3',     q3)
        call aasetp('Q1''',   xi1)
        call aasetp('Q2''',   xi2)
        call aasetp('Q3''',   xi3)
        call aasetp('Q1''''', xin1)
        call aasetp('Q2''''', xin2)
        call aasetp('Q3''''', xin3)
        call aasetp('DQ1DE1', dq1de1)
        call aasetp('DQ2DE2', dq2de2)
        call aasetp('DQ3DE3', dq2de2)
        call aasetp('DQ1DE2', dq1de2)
        call aasetp('DQ1DE3', dq1de3)
        call aasetp('DQ2DE3', dq2de3)
 
*---- ISP = 2, "STATIC".
      else if (isp .eq. 2) then
 
*---- Retrieve parameters.
        deltap = 0.0
        call utgflt(lccmd, 1, 1, deltap)
        do 20 i = 1, 14
          flag(i) = .false.
   20   continue
        call utglog(lccmd, 2, 15, flag)
 
*---- Execute.
        flag(15) = .true.
        call lastat(deltap, flag)
 
*---- Make parameters accessible to VALUE and PUSH.
        call aasetp('Q1',     q1)
        call aasetp('Q2',     q2)
        call aasetp('Q1''',   xi1)
        call aasetp('Q2''',   xi2)
        call aasetp('Q1''''', xin1)
        call aasetp('Q2''''', xin2)
        call aasetp('DQ1DE1', dq1de1)
        call aasetp('DQ2DE2', dq2de2)
        call aasetp('DQ1DE2', dq1de2)
      endif
 
      end
