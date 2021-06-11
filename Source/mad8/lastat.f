      subroutine lastat(deltap, flag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Analysis of static maps; STATIC command execution.                 *
* Attributes:                                                          *
*   DELTAP    (real)    Average relative energy error.                 *
*   MAP       FLAG(1)   Print total transfer map.                      *
*   ORBIT     FLAG(2)   Print transfer map w.r.t. closed orbit.        *
*   FIXED     FLAG(3)   Print map about fixed point.                   *
*   T         FLAG(4)   Print transformation to fixed point script T.  *
*   A         FLAG(5)   Print normalizing map script A.                *
*   N         FLAG(6)   Print normal form map script N.                *
*   RESONANCE FLAG(7)   Print resonance excitation terms.              *
*   EXPONENT  FLAG(8)   Print normal form exponent.                    *
*   HAMILTON  FLAG(9)   Print pseudo-Hamiltonian.                      *
*   BETATRON  FLAG(10)  Print betatron factor.                         *
*   NONLINEAR FLAG(11)  Print nonlinear factor of the map.             *
*   CONJUGATE FLAG(12)  Print conjugate to betatron factor.            *
*   INVARIANT FLAG(13)  Print linear invariants.                       *
*   LONG      FLAG(14)  Long output.                                   *
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
      integer i,j,mord,nline
      double precision alfx,alfy,betx,bety,dddpx,dddpy,dddx,dddy,ddpx,
     +ddpy,ddx,ddy,deltap,dpx,dpy,dx,dy,epxh,epxv,epyh,epyv,exh,exv,eyh,
     +eyv,gamx,gamy,tenp6,twopi,wt,wx,wy,zero
      logical           flag(*)
      integer ietflg,ipnflg, liftbeam,currbeam
      double precision alfa,amass,arad,beta,betas,bunch,bxmax,bymax,
     +charge,cosmux,cosmuy,currnt,deltas,deltat,dtbyds,dxmax,dymax,
     +et,ex,exn,ey,eyn,freq0,gamma,gammas,gamtr,parnum,pc,pdamp,
     +qs,qx,qy,sigdx,sigdy,sige,sigt,sigx,sigxco,sigy,sigyco,sinmux,
     +sinmuy,u0,xcomax,xix,xiy,ycomax,en0,beambv,elkfact,elmfact
 
*---- Particles, emittances and sigmas.
      integer mfact, mbmult
      parameter (mfact = 50, mbmult = 20)
      common /beanam/   prtnam, bsequnam,
     +                  prtnames(mttact), bseqnames(mttact)
      common /beaflt/   amass, charge, en0, pc, gamma,
     +                  ex, exn, ey, eyn, et, sigt, sige,
     +                  bunch, parnum, currnt
      common /beaaux/   sigx, qx, xix, cosmux, sinmux, bxmax, dxmax,
     +                  xcomax, sigxco, sigdx,
     +                  sigy, qy, xiy, cosmuy, sinmuy, bymax, dymax,
     +                  ycomax, sigyco, sigdy,
     +                  qs, alfa, gamtr, deltas, dtbyds, deltat,
     +                  freq0, beta, u0, arad, beambv, pdamp(3),
     +                  gammas, betas,
     +                  elkfact(mfact), elmfact(0:mbmult)
      common /beaint/   ietflg, ipnflg, liftbeam, currbeam
      save   /beaint/
      common /bealog/   fbch, frad
      save              /beanam/, /beaflt/, /beaaux/, /bealog/
      logical           fbch, frad
      character*(mcnam) prtnam, bsequnam, prtnames, bseqnames
      double precision a1m,a2m,bm,bp,fm,fp,gm,gp,hm,hp,pm,pp,tm,tp
 
*---- Working store for STATIC and DYNAMIC commands.
      common /lamaps/   bp(209), bm(6,6), fp(209), fm(6,6),
     +                  gp(209), gm(6,6), hp(209), hm(6,6),
     +                  pp(209), pm(6,6), tp(209), tm(6,6),
     +                  a1m(6,6), a2m(6,6)
      double precision dq1de1,dq1de2,dq2de2,q1,q2,q3,xi1,xi2,xi3,xin1,
     +xin2,xin3
 
*---- Global quantities computed by STATIC and DYNAMIC.
      common /largo/    q1, q2, q3, xi1, xi2, xi3, xin1, xin2, xin3,
     +                  dq1de1, dq1de2, dq2de2
      save              /largo/
 
      double precision alfx0,alfy0,amux0,amuy0,betx0,bety0,circ,ddisp0,
     +disp0,dmux0,dmuy0,orbit0,phix0,phiy0,r0mat,wx0,wy0, ener0
 
*---- Initial conditions for optical functions.
      common /optic0/   betx0, alfx0, amux0, bety0, alfy0, amuy0,
     +                  orbit0(6), disp0(6),
     +                  wx0, phix0, dmux0, wy0, phiy0, dmuy0,
     +                  ddisp0(6), circ, r0mat(2,2), ener0
      save              /optic0/
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      character*(*)     title
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0 * pi, zero  = 0.0, mord = 4)
      parameter         (tenp6 = 1.0d6)
      parameter         (title = 'Static analysis.')
 
*---- Check line definition.
      call lnchck('LASTAT', error)
      if (error) return
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
*---- SYMM is not allowed.
      if (symm) then
        call aafail('LASTAT', 1,
     +    '"SYMM" cannot be set for "STATIC" command.')
        return
      endif
 
*---- Initialize polynomial package.
      call painit(6)
 
*---- Print page header.
      if (flag(15)) then
        call prhead('STATIC', title, deltap, 4, nline, 1)
      endif
 
*---- Fix up environment.
      call aapdrp
      call enfix
      call enfreq(deltap)
 
*---- Compute one-turn map to order 4.
      call laturn(lcseq, deltas, flag(14), mord, fp, fm)
 
*---- Output for "MAP" or "ORBIT".
      if (flag(1) .or. flag(2)) then
        write (iqpr2, 910) 'Transfer map about closed orbit'
        call lmprnt(iqpr2, mord, fp, fm)
      endif
 
*---- Transform top fixed point and print its location.
      call lmfixp(fp, fm, gp, gm, tp, tm)
      dx    = tm(1,6)
      dpx   = tm(2,6)
      dy    = tm(3,6)
      dpy   = tm(4,6)
      ddx   = - tp(63)
      ddpx  =   tp(48)
      ddy   = - tp(79)
      ddpy  =   tp(73)
      dddx  = - tp(174)
      dddpx =   tp(139)
      dddy  = - tp(204)
      dddpy =   tp(194)
 
*---- Output for "FIXED".
      if (flag(3)) then
        write (iqpr2, 910) 'Transfer map about fixed point'
        call lmprnt(iqpr2, mord, gp, gm)
      endif
 
*---- Output for "T".
      if (flag(4)) then
        write (iqpr2, 910) 'Transformation script T to fixed point'
        call lmprnt(iqpr2, mord, tp, tm)
      endif
 
*---- Remove other linear terms.
      call laspu2(mord, gp, gm, gp, gm, pp, pm)
      if (error) return
      call lmcat(mord, pp, pm, tp, tm, tp, tm)
 
*---- Tunes and momentum compaction.
      wx = atan2(gm(1,2),gm(1,1))
      q1 = nsup * wx / twopi
      if (q1 .lt. 0.0) q1 = q1 + nsup
      wy = atan2(gm(3,4),gm(3,3))
      q2 = nsup * wy / twopi
      if (q2 .lt. 0.0) q2 = q2 + nsup
      wt = gm(5,6)
 
*---- Remove offending chromatic terms in F3 part of map.
      call laspuc(mord, gp, gm, gp, gm, hp, hm)
      call lmcat(mord, hp, hm, tp, tm, tp, tm)
 
*---- Compute chromaticities.
      call lasc2r(mord, gp, hp)
      xi1 = - nsup * hp(28) / pi
      xi2 = - nsup * hp(29) / pi
      xin1 = - (2.0 * nsup / pi) * hp(84)
      xin2 = - (2.0 * nsup / pi) * hp(85)
 
*---- Remove offending geometric terms in F3 part of map.
      call laspug(mord, gp, gm, gp, gm, hp, hm)
      call lmcat(mord, hp, hm, tp, tm, tp, tm)
 
*---- Dependence of tune on betatron amplitude.
      call lasc2r(mord, gp, hp)
      dq1de1 = - (2.0 * (nsup / pi)) * hp(87)
      dq2de2 = - (2.0 * (nsup / pi)) * hp(88)
      dq1de2 = - (nsup / pi) * hp(89)
 
*---- Short output ends here.
      if (.not. flag(14)) go to 800
 
*---- Remove offending terms in F4 part of map.
      call laspu4(mord, gp, gm, gp, gm, hp, hm)
      call lmcat(mord, hp, hm, tp, tm, tp, tm)
 
*---- Output for "A".
      if (flag(5)) then
        write (iqpr2, 910) 'Normalizing map script A'
        call lmprnt(iqpr2, mord, tp, tm)
      endif
 
*---- Output for "N".
      if (flag(6)) then
        write (iqpr2, 910) 'Normal form map script N'
        call lmprnt(iqpr2, mord, gp, gm)
      endif
 
*---- Output for "RESONANCE".
      call lasc2r(mord, gp, hp)
      if (flag(7)) then
        write (iqpr2, 910)
     +    'Generating coefficients in static resonance base'
        call lmsprt(iqpr2, mord, hp)
      endif
 
*---- Output for "EXPONENT".
      call pa6clr(gp, -2)
      gp(7)  = - wx / 2.0
      gp(13) = - wx / 2.0
      gp(18) = - wy / 2.0
      gp(22) = - wy / 2.0
      gp(27) = - wt / 2.0
      if (flag(8)) then
        write (iqpr2, 910) 'Exponent for normal form'
        call pa6prt(gp, mord, iqpr2)
      endif
 
*---- Output for "HAMILTON"; Pseudo Hamiltonian for normal form.
      if (flag(9)) then
        call lminv(mord, tp, tm, tp, tm)
        call lafxfm(mord, tp, tm, gp, hp)
        write (iqpr2, 910) 'Pseudo Hamiltonian'
        call pa6prt(hp, mord, iqpr2)
      endif
 
*==== Second pass --- Twiss parameters and envelopes.
      call lmfixp(fp, fm, gp, gm, tp, tm)
 
*---- Output for "BETATRON"; extract the betatron factor of the map.
      call labeta(mord, gp, gm, bp, bm)
      if (flag(10)) then
        write (iqpr2, 910) 'Betatron factor of transfer map'
        call lmprnt(iqpr2, mord, bp, bm)
      endif
 
*---- Compute B for specific value of delta.
      call lachrm(mord, bp, bm, a1m, a2m)
      write (iqpr2, 910) 'On-energy Twiss matrix'
      call m66prt(bm, iqpr2)
      write (iqpr2, 910) 'First derivative w.r.t. energy error'
      write (iqpr2, '(1X,6E16.8)') ((a1m(i,j), j = 1, 6), i = 1, 6)
      write (iqpr2, 910) 'Second derivative w.r.t. energy error'
      write (iqpr2, '(1X,6E16.8)') ((a2m(i,j), j = 1, 6), i = 1, 6)
 
*---- Output for "NONLINEAR"; non-linear factor about fixed point.
      if (flag(11)) then
        call lminv(mord, bp, bm, hp, hm)
        call lmcat(mord, hp, hm, gp, gm, hp, hm)
        write (iqpr2, 910) 'Non-linear factor about fixed point'
        call lmprnt(iqpr2, mord, hp, hm)
      endif
 
*---- Transforming (conjugating) map AB for the betatron factor.
      call lmsand(mord, pp, pm, bp, bm, gp, gm)
 
*---- Remove offending chromatic terms in F3 part of map.
      call laspuc(mord, gp, gm, gp, gm, hp, hm)
      call lmcat(mord, hp, hm, pp, pm, tp, tm)
 
*---- Remove offending terms in F4 part of map.
      call laspu4(mord, gp, gm, gp, gm, hp, hm)
      call lmcat(mord, hp, hm, tp, tm, tp, tm)
 
*---- output for "CONJUGATE".
      if (flag(12)) then
        write (iqpr2, 910) 'Conjugate factor for betatron map'
        call lmprnt(iqpr2, mord, tp, tm)
      endif
 
*---- Eigenvectors and their dependence on DELTA.
      call lachrm(mord, tp, tm, a1m, a2m)
      write (iqpr2, 910) 'On energy eigenvectors'
      write (iqpr2, '(1X,6E16.8)') ((tm(i,j), j = 1, 6), i = 1, 6)
      write (iqpr2, 910) 'First derivatives'
      write (iqpr2, '(1X,6E16.8)') ((a1m(i,j), j = 1, 6), i = 1, 6)
      write (iqpr2, 910) 'Second derivatives'
      write (iqpr2, '(1X,6E16.8)') ((a2m(i,j), j = 1, 6), i = 1, 6)
 
*---- Compute envelopes.
      exh  = sqrt(ex * (tm(1,1)**2 + tm(1,2)**2))
      exv  = sqrt(ex * (tm(1,3)**2 + tm(1,4)**2))
      epxh = sqrt(ex * (tm(2,1)**2 + tm(2,2)**2))
      epxv = sqrt(ex * (tm(2,3)**2 + tm(2,4)**2))
      eyh  = sqrt(ey * (tm(3,1)**2 + tm(3,2)**2))
      eyv  = sqrt(ey * (tm(3,3)**2 + tm(3,4)**2))
      epyh = sqrt(ey * (tm(4,1)**2 + tm(4,2)**2))
      epyv = sqrt(ey * (tm(4,3)**2 + tm(4,4)**2))
 
*---- Invariant for mode 1.
      call lminv(mord, tp, tm, tp, tm)
      call pa6clr(gp, 2)
      gp(7) = 1.0
      gp(13) = 1.0
      call pa6xfm(gp, 2, tm, hp)
      alfx = hp(8) / 2.0
      betx = hp(13)
      gamx = hp(7)
 
*---- Output for "INVARIANT".
      if (flag(13)) then
        write (iqpr2, 910) 'Invariant polynomial for mode 1'
        call pa6prt(hp, 2, iqpr2)
      endif
 
*---- Invariant for mode 2.
      call pa6clr(gp, 2)
      gp(18) = 1.0
      gp(22) = 1.0
      call pa6xfm(gp, 2, tm, hp)
      alfy = hp(19) / 2.0
      bety = hp(22)
      gamy = hp(18)
 
*---- Output for "INVARIANT".
      if (flag(13)) then
        write (iqpr2, 910) 'Invariant polynomial for mode 2'
        call pa6prt(hp, 2, iqpr2)
      endif
 
*---- Print closed orbit position and its derivatives.
      write (iqpr2, 920) (orbit0(j), j = 1, 4), dx, dpx, dy, dpy,
     +                   ddx, ddpx, ddy, ddpy, dddx, dddpx, dddy, dddpy
 
*---- Print tunes.
  800 continue
      if (flag(15)) then
        write (iqpr2, 930) deltas, q1, q2, xi1, xi2, xin1, xin2
 
*---- Print Twiss parameters and envelopes (long output only).
        if (flag(14)) then
          write (iqpr2, 940) betx, bety, alfx, alfy, gamx, gamy,
     +                       tenp6 * ex, tenp6 * ey,
     +                       exh, exv, epxh, epxv, eyh, eyv, epyh, epyv
        endif
 
*---- Print normalized anharmonicites.
        write (iqpr2, 950) dq1de1, dq1de2, dq2de2
      endif
 
  910 format(' '/' ',a,':'/' ')
  920 format(' '/' Fixed point position:'/
     +       ' Closed orbit position',t51,4f16.6/
     +       ' First derivative w.r.t. energy',t51,4e16.6/
     +       ' Second derivative w.r.t. energy',t51,4e16.6/
     +       ' Third derivative w.r.t. energy',t51,4e16.6)
  930 format(' '/' Delta(p)/p:',f16.8,
     +       t45,'M o d e   1',t75,'M o d e   2'/
     +       ' Fractional tunes:',t31,'Q1      =',f16.8,
     +       t61,'Q2      =',  f16.8/
     +       ' First order chromaticity:',t31,'Q1''     =',f16.8,
     +       t61,'Q2''     =', f16.8/
     +       ' Second order chromaticity:',t31,'Q1''''    =',f16.8,
     +       t61,'Q2''''    =',f16.8)
  940 format(t31,'Beta_1  =',e16.8,' m',t61,'Beta_2  =',e16.8,' m'/
     +       t31,'Alpha_1 =',e16.8,' m',t61,'Alpha_2 =',e16.8,' m'/
     +       t31,'Gamma_1 =',e16.8,' m',t61,'Gamma_2 =',e16.8,' m'/
     +  ' Emittances:',t40,e16.8,' pi*mmm*mrad',t70,e16.8,'pi*mm*mrad'/
     +  ' Horizontal extent:',    t40,e16.8,' m  ',t70,e16.8,' m'/
     +  ' Horizontal divergence:',t40,e16.8,' rad',t70,e16.8,' rad'/
     +  ' Vertical extent:',      t40,e16.8,' m  ',t70,e16.8,' m'/
     +  ' Vertical divergence:',  t40,e16.8,' rad',t70,e16.8,' rad')
  950 format(' Normalized anharmonicities:',t31,'dQ1/dE1 =',e16.8/
     +       t31,'dQ1/dE2 =',e16.8,t61,'dQ2/dE2 =',e16.8)
 
      end
