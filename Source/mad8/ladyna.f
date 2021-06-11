      subroutine ladyna(deltap, flag)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Analysis of dynamic maps; DYNAMIC command.                         *
* Attributes:                                                          *
*   DELTAP    (real)    Average relative momentum error.               *
*   MAP       FLAG(1)   Print total transfer map.                      *
*   ORBIT     FLAG(2)   Print transfer map w.r.t. closed orbit.        *
*   A         FLAG(3)   Print normalizing map script A.                *
*   N         FLAG(4)   Print normal form map script N.                *
*   RESONANCE FLAG(5)   Print resonance excitation terms.              *
*   EXPONENT  FLAG(6)   Print normal form exponent.                    *
*   HAMILTON  FLAG(7)   Print pseudo-Hamiltonian.                      *
*   INVARIANT FLAG(8)   Print linear invariants.                       *
*   LONG      FLAG(9)   Long output.                                   *
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
      double precision alft,alfx,alfy,bett,betx,bety,deltap,dq1de3,
     +dq2de3,dq3de3,epth,eptt,eptv,epxh,epxt,epxv,epyh,epyt,epyv,eth,
     +ett,etv,exh,ext,exv,eyh,eyt,eyv,gamt,gamx,gamy,tenp6,twopi,ws,wx,
     +wy
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
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi, mord = 4, tenp6 = 1.0d6)
 
      character*(*)     title
      parameter         (title = 'Dynamic analysis.')
 
*---- Check line definition.
      call lnchck('LADYNA', error)
      if (error) return
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
 
*---- SYMM is not allowed.
      if (symm) then
        call aafail('LADYNA', 1,
     +    '"SYMM" cannot be set for "DYNAMIC" command.')
        return
      endif
 
*---- Initialize polynomial package.
      call painit(6)
 
*---- Fix up environment.
      call aapdrp
      call enfix
      if (error) go to 800
      call enfreq(deltap)
 
*---- Print page header.
      if (flag(10)) then
        call prhead('DYNAMIC', title, deltap, 4, nline, 1)
        call enprgl
        call enprem
        call enprrf
      endif
 
*---- Compute one-turn map to order 4.
      call laturn(lcseq, deltas, flag(9), mord, fp, fm)
 
*---- Output for "MAP" and "ORBIT".
      if (flag(1) .or. flag(2)) then
        write (iqpr2, 910) 'Transfer map about closed orbit'
        call lmprnt(iqpr2, mord, fp, fm)
      endif
 
*---- Beginning of calculation.
*     Remove offensive terms from matrix part of map.
      call ladpu2(mord, fp, fm, gp, gm, tp, tm)
      if (error) return
 
*---- Remove offending terms in F3 part of map.
      call ladpu3(mord, gp, gm, gp, gm, hp, hm)
      call lmcat(mord, hp, hm, tp, tm, tp, tm)
 
*---- Remove offending terms in F4 part of map.
      call ladpu4(mord, gp, gm, gp, gm, hp, hm)
      call lmcat(mord, hp, hm, tp, tm, tp, tm)
 
*---- Output for "A"; print map script A (normalizing map).
      if (flag(3)) then
        write (iqpr2, 910) 'Normalizing map script A'
        call lmprnt(iqpr2, mord, tp, tm)
      endif
 
*---- Output for "N"; print map script N (normal form map).
      if (flag(4)) then
        write (iqpr2, 910) 'Normal form map script N'
        call lmprnt(iqpr2, mord, gp, gm)
      endif
 
*---- Compute tunes.
      wx = atan2(gm(1,2),gm(1,1))
      qx = nsup * wx / twopi
      if (qx .lt. 0.0) qx = qx + nsup
      wy = atan2(gm(3,4),gm(3,3))
      qy = nsup * wy / twopi
      if (qy .lt. 0.0) qy = qy + nsup
      ws = atan2(gm(5,6),gm(5,5))
      qs = nsup * abs(ws) / twopi
 
*---- Compute envelopes.
      exh  = sqrt(ex * (tm(1,1)**2 + tm(1,2)**2))
      exv  = sqrt(ex * (tm(1,3)**2 + tm(1,4)**2))
      ext  = sqrt(ex * (tm(1,5)**2 + tm(1,6)**2))
      epxh = sqrt(ex * (tm(2,1)**2 + tm(2,2)**2))
      epxv = sqrt(ex * (tm(2,3)**2 + tm(2,4)**2))
      epxt = sqrt(ex * (tm(2,5)**2 + tm(2,6)**2))
      eyh  = sqrt(ey * (tm(3,1)**2 + tm(3,2)**2))
      eyv  = sqrt(ey * (tm(3,3)**2 + tm(3,4)**2))
      eyt  = sqrt(ey * (tm(3,5)**2 + tm(3,6)**2))
      epyh = sqrt(ey * (tm(4,1)**2 + tm(4,2)**2))
      epyv = sqrt(ey * (tm(4,3)**2 + tm(4,4)**2))
      epyt = sqrt(ey * (tm(4,5)**2 + tm(4,6)**2))
      eth  = sqrt(et * (tm(5,1)**2 + tm(5,2)**2))
      etv  = sqrt(et * (tm(5,3)**2 + tm(5,4)**2))
      ett  = sqrt(et * (tm(5,5)**2 + tm(5,6)**2))
      epth = sqrt(et * (tm(6,1)**2 + tm(6,2)**2))
      eptv = sqrt(et * (tm(6,3)**2 + tm(6,4)**2))
      eptt = sqrt(et * (tm(6,5)**2 + tm(6,6)**2))
 
*---- Output for "RESONANCE"; resonance decomposition of purified map.
      call ladc2r(mord, gp, hp)
      if (flag(5)) then
        write (iqpr2, 910)
     +    'Generating coefficients in dynamic resonance base'
        call lmdprt(iqpr2, mord, hp)
      endif
 
*---- Dependence of tune on betatron amplitude.
      dq1de1 = - (2.0 * (nsup / pi)) * hp(84)
      dq2de2 = - (2.0 * (nsup / pi)) * hp(85)
      dq3de3 = - (2.0 * (nsup / pi)) * hp(86)
      dq1de2 = - hp(87) * (nsup / pi)
      dq1de3 = - hp(88) * (nsup / pi)
      dq2de3 = - hp(89) * (nsup / pi)
 
*---- Short output ends here.
      if (.not. flag(9)) go to 800
 
*---- Compute Twiss parameters.
      call lminv(mord, tp, tm, tp, tm)
 
*---- Exponent for normal form.
      call pa6clr(gp, 2)
      gp(7)  = - wx / 2.0
      gp(13) = - wx / 2.0
      gp(18) = - wy / 2.0
      gp(22) = - wy / 2.0
      gp(25) = - ws / 2.0
      gp(27) = - ws / 2.0
 
*---- Output for "EXPONENT".
      if (flag(6)) then
        write (iqpr2, 910) 'Exponent for normal form'
        call pa6prt(gp, mord, iqpr2)
      endif
 
*---- Output for "HAMILTONIAN"; pseudo Hamiltonian for normal form.
      if (flag(7)) then
        call lafxfm(mord, tp, tm, gp, hp)
        write (iqpr2, 910) 'Pseudo Hamiltonian'
        call pa6prt(hp, mord, iqpr2)
      endif
 
*---- Print eigenvectors.
      write (iqpr2, 910) 'Eigenvectors'
      write (iqpr2, '(1X,6E16.8)') ((tm(i,j), j = 1, 6), i = 1, 6)
 
*---- Invariant for mode 1.
      call pa6clr(gp, 2)
      gp(7) = 1.0
      gp(13) = 1.0
      call pa6xfm(gp, 2, tm, hp)
      gamx = hp(7)
      alfx = hp(8) / 2.0
      betx = hp(13)
 
*---- Invariant for mode 2.
      call pa6clr(gp, 2)
      gp(18) = 1.0
      gp(22) = 1.0
      call pa6xfm(gp, 2, tm, hp)
      gamy = hp(18)
      alfy = hp(19) / 2.0
      bety = hp(22)
 
*---- Invariant for mode 3.
      call pa6clr(gp, 2)
      gp(25) = 1.0
      gp(27) = 1.0
      call pa6xfm(gp, 2, tm, hp)
      gamt = hp(25)
      alft = hp(26) / 2.0
      bett = hp(27)
 
*---- Output for "INVARIANT".
      if (flag(8)) then
        write (iqpr2, 910) 'Invariant polynomial for mode 1'
        call pa6prt(hp, 2, iqpr2)
        write (iqpr2, 910) 'Invariant polynomial for mode 2'
        call pa6prt(hp, 2, iqpr2)
        write (iqpr2, 910) 'Invariant polynomial for mode 3'
        call pa6prt(hp, 2, iqpr2)
      endif
 
*---- Print tunes.
  800 continue
      if (flag(10)) then
        write (iqpr2, 920) deltas, qx, qy, qs
 
*---- Print Twiss parameters and envelopes (long output only).
        if (flag(9)) then
          write (iqpr2, 930)
     +      betx, bety, bett, alfx, alfy, alft, gamx, gamy, gamt,
     +      tenp6 * ex, tenp6 * ey, tenp6 * et,
     +      exh, exv, ext, epxh, epxv, epxt,
     +      eyh, eyv, eyt, epyh, epyv, epyt,
     +      eth, etv, ett, epth, eptv, eptt
        endif
 
*---- Print anharmonicities.
        write (iqpr2, 940)
     +    dq1de1, dq1de2, dq2de2, dq1de3, dq2de3, dq3de3
      endif
 
  910 format(' '/' ',a,':'/' ')
  920 format(' '/' Delta(p)/p:',f16.8,
     +       t45,'M o d e   1',t75,'M o d e   2',t105,'M o d e   3'/
     +       ' Fractional tunes:',t31,'Q1      =',f16.8,
     +       t61,'Q2      =',f16.8,t91,'Q3      =',f16.8)
  930 format(t31,'Beta_1  =',e16.8,' m',t61,'Beta_2  =',e16.8,' m',
     +       t91,'Beta_3  =',e16.8,' m'/
     +       t31,'Alpha_1 =',e16.8,' m',t61,'Alpha_2 =',e16.8,' m',
     +       t91,'Alpha_2 =',e16.8,' m'/
     +       t31,'Gamma_1 =',e16.8,' m',t61,'Gamma_2 =',e16.8,' m',
     +       t91,'Gamma_2 =',e16.8,' m'/
     +       ' Emittances:',t40,e16.8,'pi*mm*mrad',
     +       t70,e16.8,'pi*mm*mrad',t100,e16.8,'pi*mm*mrad'/
     +       ' Horizontal extent:',
     +       t40,e16.8,' m  ',t70,e16.8,' m  ',t100,e16.8,' m  '/
     +       ' Horizontal divergence:',
     +       t40,e16.8,' rad',t70,e16.8,' rad',t100,e16.8,' rad'/
     +       ' Vertical extent:',
     +       t40,e16.8,' m  ',t70,e16.8,' m  ',t100,e16.8,' m  '/
     +       ' Vertical divergence:',
     +       t40,e16.8,' rad',t70,e16.8,' rad',t100,e16.8,' rad'/
     +       ' Longitudinal extent:',
     +       t40,e16.8,' m  ',t70,e16.8,' m  ',t100,e16.8,' m  '/
     +       ' Longitudinal divergence:',
     +       t40,e16.8,' rad',t70,e16.8,' rad',t100,e16.8,' rad')
  940 format(' Normalized anharmonicities:',t31,'dQ1/dE1 =',e16.8/
     +       t31,'dQ1/dE2 =',e16.8,t61,'dQ2/dE2 =',e16.8/
     +       t31,'dQ1/dE3 =',e16.8,t61,'dQ2/dE3 =',e16.8,
     +       t91,'dQ3/dE3 =',e16.8)
 
      end
