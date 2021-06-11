      subroutine tmturn(lseq, deltap, eflag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Compute closed orbit and one turn TRANSFORM map.                   *
* Input:                                                               *
*   LSEQ(1)   (pointer) Beam line expansion to be used.                *
*   DELTA     (real)    delta(p) / p.                                  *
* Output:                                                              *
*   EFLAG     (logical) Error flag.                                    *
*   ORBIT0(6) /OPTIC0/  Closed orbit, initial conditions.              *
*   RT(6,6)   /MAPTRN/  Linear map for one turn.                       *
*   TT(6,6,6) /MAPTRN/  Quadratic terms for one turn.                  *
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
      integer i,iflag,lmap,nd
      double precision deltap
      logical           eflag
      integer           lseq(1)
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
      integer msali,msbn,mscom,mscor,msdir,mselm,msf1,msf2,msfld,msflg,
     +mslie,mslnk,msmap,msmon,msnum,msr1,msr2,msref,msrn,mss,msspl,msup,
     +msym
 
*---- Bias for sequence description banks.
      parameter         (msf1 = 1, msr1 = 2, msr2 = 3, msym = 4,
     +                   msup = 5, msf2 = 6, msbn = 7,
     +                   msrn = msbn + mwnam, mss = msrn + 40 / mcwrd)
*     Links for sequence description banks.
      parameter         (msdir =  1, msflg =  2, msali =  3, msfld =  4,
     +                   msnum =  5, mscom =  6, msmap =  9, mslie = 10,
     +                   msspl = 11, mscor = 12, msmon = 13, mselm = 14)
      parameter         (mslnk = 11, msref = 14)
      double precision rt,rtp,tt
 
*---- Transfer map for complete turn.
      common /maptrn/   rt(6,6), tt(6,6,6), rtp(6,6)
      save              /maptrn/
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
      integer maxcpf,maxdof
 
*---- Status flags for TRANSPORT map module.
*     Set to consider everything before each executable command.
      parameter         (maxcpf = 10, maxdof = 10)
      common /stflag/   cpflag(maxcpf), doflag(maxdof)
      logical           cpflag, cplxy, cplxt
      logical           doflag, docav, dorad, doali, dofld, dokick
      logical           dodamp, dorand
      save              /stflag/
      equivalence       (cplxy,  cpflag( 1)), (cplxt,  cpflag( 2))
      equivalence       (docav,  doflag( 1)), (dorad,  doflag( 2))
      equivalence       (doali,  doflag( 3)), (dofld,  doflag( 4))
      equivalence       (dokick, doflag( 5)), (dodamp, doflag( 6))
      equivalence       (dorand, doflag( 7))
      double precision cofact,optflt
 
*---- Option flags.
      common /optflt/   optflt(10)
      equivalence       (cofact, optflt( 1))
      common /optint/   optint(10)
      integer           optint
      integer           icmdfl, ideffl, iexpfl, ikeyfl, ilinfl
      equivalence       (icmdfl, optint( 1)), (ideffl, optint( 2))
      equivalence       (iexpfl, optint( 3)), (ikeyfl, optint( 4))
      equivalence       (ilinfl, optint( 5))
      common /optlog/   optflg(20), optcon(5)
      logical           optflg, optcon
      logical           debug,  double, echo,   inter,  trace,  verify,
     +                  warn,   info,   sympl,  rbarc, ereset, bborbit
      logical           reset,  tell
      equivalence       (debug,  optflg( 1)), (double, optflg( 2))
      equivalence       (echo,   optflg( 3)), (inter,  optflg( 4))
      equivalence       (trace,  optflg( 5)), (verify, optflg( 6))
      equivalence       (warn,   optflg( 7)), (info,   optflg( 8))
      equivalence       (sympl,  optflg( 9)), (rbarc,  optflg(10))
      equivalence       (ereset,  optflg(11)),(bborbit,optflg(12))
      equivalence       (reset,  optcon( 1)), (tell,   optcon( 2))
      save              /optflt/, /optint/, /optlog/
      double precision coest,cotol
 
*---- Estimate for closed orbit search.
      common /coesti/ coest(6), cotol
      save            /coesti/
 
*---- Search for precomputed map with same DELTAP and flags.
      iflag = 0
      do 10 i = 1, maxdof
        if (doflag(i)) then
          iflag = 2 * iflag + 1
        else
          iflag = 2 * iflag
        endif
   10 continue
      lmap = lq(lseq(1)-msmap)
   20 if (lmap .ne. 0) then
        if (abs(q(lmap+1)-real(deltap)) .lt. 1.0e-6  .and.
     +      iq(lmap+2) .eq. iflag) then
          call ucopy(q(lmap+3), orbit0, 6*mwflt)
          if (optflg(20))  call ucopy(orbit0, coest, 6*mwflt)
          call ucopy(q(lmap+6*mwflt+3), rt, 36*mwflt)
          call ucopy(q(lmap+42*mwflt+3), tt, 216*mwflt)
          return
        endif
        lmap = lq(lmap)
        go to 20
      endif
 
*---- Not found, compute and store map.
      call tmclor(lseq, deltap, .true., eflag)
      if (.not. eflag) then
        call tmscnd(lseq)
        nd = mwflt * (6 + 36 + 216) + 2
        call mzbook(2, lmap, lseq, -msmap, 'TMAP', 0, 0, nd, mreal, -1)
        q(lmap+1) = real(deltap)
        iq(lmap+2) = iflag
        call ucopy(orbit0, q(lmap+3), 6*mwflt)
        if (optflg(20))  call ucopy(orbit0, coest, 6*mwflt)
        call ucopy(rt, q(lmap+6*mwflt+3), 36*mwflt)
        call ucopy(tt, q(lmap+42*mwflt+3), 216*mwflt)
      endif
 
      end
