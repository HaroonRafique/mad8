      subroutine mtufun(fprt, iflag, icon, fvec, fsum)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Deal with user constraints.                                        *
* Input:                                                               *
*   FPRT    (logical)   Print flag.                                    *
*   IFLAG   (integer)   Case flag:                                     *
*                       1: Initialize user constraints,                *
*                       2: Handle current position in machine,         *
*                       3: Return user constraint values.              *
* Input/output (updated for IFLAG = 3):                                *
*   ICON    (integer)   Counter for constraints.                       *
*   FVEC(*) (real)      Constraint values.                             *
*   FSUM    (real)      Sum of constraint squares.                     *
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
      integer i,icon,iflag,imodulold,iqlogold,iqpr2old,iqprntold,
     +lccmdold
      double precision formula,fsum,fvec,one,trturns,zero
      logical           fprt
      dimension         fvec(*)
      double precision dq1de1,dq1de2,dq2de2,q1,q2,q3,xi1,xi2,xi3,xin1,
     +xin2,xin3
 
*---- Global quantities computed by STATIC and DYNAMIC.
      common /largo/    q1, q2, q3, xi1, xi2, xi3, xin1, xin2, xin3,
     +                  dq1de1, dq1de2, dq2de2
      save              /largo/
 
      integer icall,icovar,ifirst,ilevel,imode,istrat,ncon,nfcn,nfcnmx,
     +nvar
      double precision edm,fmin,tol,up
 
*---- Communication area for routines derived from MINUIT.
      common /minchr/   crout, cstat
      common /mindbl/   edm, fmin, tol, up
      common /minint/   icall, icovar, ifirst, imode, ilevel, istrat,
     +                  ncon, nvar, nfcn, nfcnmx
      common /minflt/   time1, time2
      save              /minchr/, /mindbl/, /minint/, /minflt/
      character         crout*8, cstat*16
      real              time1, time2
 
*---- Flags for matching.
      common /mtcflg/   flbeta, florb, flrmat, fltmat, flchrm
      save              /mtcflg/
      logical           flbeta, florb, flrmat, fltmat, flchrm
      character *(mcnam)  sequd, betnm
      common / dmatchc / sequd(2), betnm(2)
      integer mtdbfl, imsequ
      common / dmatchi / mtdbfl, imsequ
      logical bdtflg
      common / dmatchl / bdtflg(2)
      integer iformula,igflag,nuglob,nuloc
      double precision gpesi,gtarget
 
*---- Communication area for GLOBAL constraints.
      common /mtfrgo/   gtarget(29,2), gpesi(29)
      common /mtirgo/   nuglob, nuloc, igflag(32,2), iformula
      common /mtlrgo/   dynapflag, fixpointfl, statflag
      logical           dynapflag, fixpointfl, statflag
      integer lcon,lmcon,lmtbet,lmtlin,lmtseq,lmtsub,lmvar,lptr,lref,
     +lsmat,lvar,lbeta0
 
*---- Link area for matching.
      common /mtlink/   lsmat, lmcon, lmvar,
     +                  lmtlin, lmtseq, lmtbet, lbeta0(2), lmtsub,
     +                  lcon, lref, lvar, lptr
      save              /mtlink/
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
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
      integer irg1,irg2,nsup
 
*---- Communication area for current beam line.
      common /rngchr/   linnam, rngnam
      common /rngint/   irg1, irg2, nsup
      common /rnglog/   symm
      save              /rngchr/, /rngint/, /rnglog/
      character         linnam*(mcnam), rngnam*(mcrng)
      logical           symm
      integer imodul,iplflg,nfail,nwarn
 
*---- Status flags (flags which are not under user control).
      common /status/   error,  scan,   nwarn,  nfail, imodul, iplflg,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      save              /status/
      logical           error,  scan,
     +                  inval,  maycpl, stabx,  staby,  stabt,
     +                  newcor, newmap, prompt
      integer itrturns,ktrturns
      double precision chkbelow,deltax,dtune,dynapfrac,fracmin,smear,
     +trstep,tunx,tuny,wxmax,wxmin,wxstart,wxymax,wxymin,wxystart,wymax,
     +wymin,wystart,yapunov,zendyn,zstart
 
*---- Communication area for DYNAP command.
      common /trcdyn/   trknam(2)
      character*(mcnam) trknam
      common /trfdyn/   chkbelow, deltax, dtune, dynapfrac, fracmin,
     +                  smear, trstep, tunx, tuny,
     +                  wxmax, wxmin, wymax, wymin, wxymax, wxymin,
     +                  wxstart, wystart, wxystart, yapunov,
     +                  zstart(6), zendyn(6)
      common /tridyn/   itrturns, ktrturns
      common /trldyn/   fastune, lyapflag, orbflag
      logical           fastune, lyapflag, orbflag
      save              /trcdyn/, /trfdyn/, /tridyn/, /trldyn/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      save i
      parameter         (zero = 0.0, one = 1.0)
      logical           flag(20)
      data flag         / 20 * .false. /
 
*---- IFLAG = 1: Initialize user constraints.
      if (nuglob .gt. 0) then
        if (iflag .eq. 1) then
          icon = nuglob
          fvec(1) = 0.0
          i = 1
 
*---- IFLAG = 2: Handle current position (sum up squares of DY).
*     Nothing to be done here.
        else if (iflag .eq. 2) then
 
*---- IFLAG = 3: Return (and print) global constraint functions.
        else if (iflag .eq. 3) then
 
*---- Constraints on STATIC results.
          if (statflag) then
            call lastat(zero, flag)
            if (igflag(1,imsequ) .ne. 0) then
              fvec(i) = gpesi(1) * (q1 - gtarget(1,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(2,imsequ) .ne. 0) then
              fvec(i) = gpesi(2) * (q2 - gtarget(2,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(3,imsequ) .ne. 0) then
              fvec(i) = gpesi(3) * (xi1 - gtarget(3,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(4,imsequ) .ne. 0) then
              fvec(i) = gpesi(4) * (xi2 - gtarget(4,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(5,imsequ) .ne. 0) then
              fvec(i) = gpesi(5) * (xin1 - gtarget(5,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(6,imsequ) .ne. 0) then
              fvec(i) = gpesi(6) * (xin2 - gtarget(6,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(7,imsequ) .ne. 0) then
              fvec(i) = gpesi(7) * (dq1de1 - gtarget(7,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(8,imsequ) .ne. 0) then
              fvec(i) = gpesi(8) * (dq1de2 - gtarget(8,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(9,imsequ) .ne. 0) then
              fvec(i) = gpesi(9) * (dq2de2 - gtarget(9,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
          endif
 
*---- Dynamic aperture constraints.
          if (dynapflag) then
            call trdynrun
            if (igflag(10,imsequ) .ne. 0) then
              fvec(i) = gpesi(10) * (dtune - gtarget(10,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(11,imsequ) .ne. 0) then
              fvec(i) = gpesi(11) * (tunx - gtarget(11,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(12,imsequ) .ne. 0) then
              fvec(i) = gpesi(12) * (tuny - gtarget(12,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(13,imsequ) .ne. 0) then
              fvec(i) = gpesi(13) * (dynapfrac - gtarget(13,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(14,imsequ) .ne. 0) then
              fvec(i) = gpesi(14) * (smear - gtarget(14,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(15,imsequ) .ne. 0) then
              trturns = ktrturns
              fvec(i) = gpesi(15) * (trturns - gtarget(15,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(16,imsequ) .ne. 0) then
              fvec(i) = gpesi(16) * (yapunov - gtarget(16,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
 
*---- Fixpoint search.
            if (fixpointfl) then
              if (igflag(17,imsequ) .ne. 0) then
                fvec(i) = gpesi(17) * (zendyn(1) - zstart(1))
                fsum = fsum + fvec(i)**2
                i = i + 1
              endif
              if (igflag(18,imsequ) .ne. 0) then
                fvec(i) = gpesi(18) * (zendyn(2) - zstart(2))
                fsum = fsum + fvec(i)**2
                i = i + 1
              endif
              if (igflag(19,imsequ) .ne. 0) then
                fvec(i) = gpesi(19) * (zendyn(3) - zstart(3))
                fsum = fsum + fvec(i)**2
                i = i + 1
              endif
              if (igflag(20,imsequ) .ne. 0) then
                fvec(i) = gpesi(20) * (zendyn(4) - zstart(4))
                fsum = fsum + fvec(i)**2
                i = i + 1
              endif
              if (igflag(21,imsequ) .ne. 0) then
                fvec(i) = gpesi(21) * (zendyn(5) - zstart(5))
                fsum = fsum + fvec(i)**2
                i = i + 1
              endif
              if (igflag(22,imsequ) .ne. 0) then
                fvec(i) = gpesi(22) * (zendyn(6) - zstart(6))
                fsum = fsum + fvec(i)**2
                i = i + 1
              endif
 
*---- End point constraint.
            else
              if (igflag(17,imsequ) .ne. 0) then
                fvec(i) = gpesi(17) * (zendyn(1) - gtarget(17,imsequ))
                fsum = fsum + fvec(i)**2
                i = i + 1
              endif
              if (igflag(18,imsequ) .ne. 0) then
                fvec(i) = gpesi(18) * (zendyn(2) - gtarget(18,imsequ))
                fsum = fsum + fvec(i)**2
                i = i + 1
              endif
              if (igflag(19,imsequ) .ne. 0) then
                fvec(i) = gpesi(19) * (zendyn(3) - gtarget(19,imsequ))
                fsum = fsum + fvec(i)**2
                i = i + 1
              endif
              if (igflag(20,imsequ) .ne. 0) then
                fvec(i) = gpesi(20) * (zendyn(4) - gtarget(20,imsequ))
                fsum = fsum + fvec(i)**2
                i = i + 1
              endif
              if (igflag(21,imsequ) .ne. 0) then
                fvec(i) = gpesi(21) * (zendyn(5) - gtarget(21,imsequ))
                fsum = fsum + fvec(i)**2
                i = i + 1
              endif
              if (igflag(22,imsequ) .ne. 0) then
                fvec(i) = gpesi(22) * (zendyn(6) - gtarget(22,imsequ))
                fsum = fsum + fvec(i)**2
                i = i + 1
              endif
            endif
 
*---- Invariant constraints.
            if (igflag(23,imsequ) .ne. 0) then
              fvec(i) = gpesi(23) * (wxmin - gtarget(23,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(24,imsequ) .ne. 0) then
              fvec(i) = gpesi(24) * (wxmax - gtarget(24,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(25,imsequ) .ne. 0) then
              fvec(i) = gpesi(25) * (wymin - gtarget(25,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(26,imsequ) .ne. 0) then
              fvec(i) = gpesi(26) * (wymax - gtarget(26,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(27,imsequ) .ne. 0) then
              fvec(i) = gpesi(27) * (wxymin - gtarget(27,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
            if (igflag(28,imsequ) .ne. 0) then
              fvec(i) = gpesi(28) * (wxymax - gtarget(28,imsequ))
              fsum = fsum + fvec(i)**2
              i = i + 1
            endif
          endif
 
*---- FORMULA and/or subroutine CALL.
          if (igflag(29,imsequ) .ne. 0) then
            if (lmtsub .ne. 0) then
*---- Variables for matching module which must be saved.
              lccmdold  = lccmd
              imodulold = imodul
              iqlogold  = iqlog
              iqprntold = iqprnt
              iqpr2old  = iqpr2
 
*---- Set up environment for subroutine and reroute output.
              imodul = 0
              iqlog  = iformula
              iqprnt = iformula
              iqpr2  = iformula
 
*---- Execute the subroutine.
              lccmd = lmtsub
              call aaruns
 
*---- Restore saved variables.
              iqlog  = iqlogold
              iqprnt = iqprntold
              iqpr2  = iqpr2old
              lccmd  = lccmdold
              imodul = imodulold
            endif
 
*---- Update dependent variables.
            call exupdt
            call aagetp('FORMULA', formula)
            fvec(i) = gpesi(29) * (formula - gtarget(29,imsequ))
            fsum = fsum + fvec(i)**2
            i = i + 1
          endif
 
*---- Print user constraints.
          if (fprt) then
            i = 1
            if (igflag(1,imsequ) .ne. 0) then
              write (iqpr2, 910) 'Q1', gpesi(1), q1,
     +        gtarget(1,imsequ), gtarget(1,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(2,imsequ) .ne. 0) then
              write (iqpr2, 910) 'Q2', gpesi(2), q2,
     +        gtarget(2,imsequ), gtarget(2,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(3,imsequ) .ne. 0) then
              write (iqpr2, 910) 'Q1''', gpesi(3), xi1,
     +        gtarget(3,imsequ), gtarget(3,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(4,imsequ) .ne. 0) then
              write (iqpr2, 910) 'Q2''', gpesi(4), xi2,
     +        gtarget(4,imsequ), gtarget(4,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(5,imsequ) .ne. 0) then
              write (iqpr2, 910) 'Q1''''', gpesi(5), xin1,
     +        gtarget(5,imsequ), gtarget(5,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(6,imsequ) .ne. 0) then
              write (iqpr2, 910) 'Q2''''', gpesi(6), xin2,
     +        gtarget(6,imsequ), gtarget(6,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(7,imsequ) .ne. 0) then
              write (iqpr2, 910) 'DQ1DE1', gpesi(7), dq1de1,
     +        gtarget(7,imsequ), gtarget(7,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(8,imsequ) .ne. 0) then
              write (iqpr2, 910) 'DQ1DE2', gpesi(8), dq1de2,
     +        gtarget(8,imsequ), gtarget(8,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(9,imsequ) .ne. 0) then
              write (iqpr2, 910) 'DQ2DE2', gpesi(9), dq2de2,
     +        gtarget(9,imsequ), gtarget(9,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(10,imsequ) .ne. 0) then
              write (iqpr2, 910) 'DTUNE', gpesi(10), dtune,
     +        gtarget(10,imsequ), gtarget(10,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(11,imsequ) .ne. 0) then
              write (iqpr2, 910) 'TUNX', gpesi(11), tunx,
     +        gtarget(11,imsequ), gtarget(11,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(12,imsequ) .ne. 0) then
              write (iqpr2, 910) 'TUNY', gpesi(12), tuny,
     +        gtarget(12,imsequ), gtarget(12,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(13,imsequ) .ne. 0) then
              write (iqpr2, 910) 'DYNAPFRAC', gpesi(13), dynapfrac,
     +        gtarget(13,imsequ), gtarget(13,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(14,imsequ) .ne. 0) then
              write (iqpr2, 910) 'SMEAR', gpesi(14), smear,
     +        gtarget(14,imsequ), gtarget(14,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(15,imsequ) .ne. 0) then
              write (iqpr2, 910) 'TRTURNS', gpesi(15), trturns,
     +        gtarget(15,imsequ), gtarget(15,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(16,imsequ) .ne. 0) then
              write (iqpr2, 910) 'LYAPUNOV', gpesi(16), yapunov,
     +        gtarget(16,imsequ), gtarget(16,imsequ), fvec(i)**2
              i = i + 1
            endif
 
*---- Fixpoint search.
            if (fixpointfl) then
              if (igflag(17,imsequ) .ne. 0) then
                write (iqpr2, 910) 'XEND', gpesi(17), zendyn(1),
     +          zstart(1), zstart(1), fvec(i)**2
                i = i + 1
              endif
              if (igflag(18,imsequ) .ne. 0) then
                write (iqpr2, 910) 'PXEND', gpesi(18), zendyn(2),
     +          zstart(2), zstart(2), fvec(i)**2
                i = i + 1
              endif
              if (igflag(19,imsequ) .ne. 0) then
                write (iqpr2, 910) 'YEND', gpesi(19), zendyn(3),
     +          zstart(3), zstart(3), fvec(i)**2
                i = i + 1
              endif
              if (igflag(20,imsequ) .ne. 0) then
                write (iqpr2, 910) 'PYEND', gpesi(20), zendyn(4),
     +          zstart(4), zstart(4), fvec(i)**2
                i = i + 1
              endif
              if (igflag(21,imsequ) .ne. 0) then
                write (iqpr2, 910) 'TEND', gpesi(21), zendyn(5),
     +          zstart(5), zstart(5), fvec(i)**2
                i = i + 1
              endif
              if (igflag(22,imsequ) .ne. 0) then
                write (iqpr2, 910) 'PTEND', gpesi(22), zendyn(6),
     +          zstart(6), zstart(6), fvec(i)**2
                i = i + 1
              endif
*---- End point constraints.
            else
              if (igflag(17,imsequ) .ne. 0) then
                write (iqpr2, 910) 'XEND', gpesi(17), zendyn(1),
     +          gtarget(17,imsequ), gtarget(17,imsequ), fvec(i)**2
                i = i + 1
              endif
              if (igflag(18,imsequ) .ne. 0) then
                write (iqpr2, 910) 'PXEND', gpesi(18), zendyn(2),
     +          gtarget(18,imsequ), gtarget(18,imsequ), fvec(i)**2
                i = i + 1
              endif
              if (igflag(19,imsequ) .ne. 0) then
                write (iqpr2, 910) 'YEND', gpesi(19), zendyn(3),
     +          gtarget(19,imsequ), gtarget(19,imsequ), fvec(i)**2
                i = i + 1
              endif
              if (igflag(20,imsequ) .ne. 0) then
                write (iqpr2, 910) 'PYEND', gpesi(20), zendyn(4),
     +          gtarget(20,imsequ), gtarget(20,imsequ), fvec(i)**2
                i = i + 1
              endif
              if (igflag(21,imsequ) .ne. 0) then
                write (iqpr2, 910) 'TEND', gpesi(21), zendyn(5),
     +          gtarget(21,imsequ), gtarget(21,imsequ), fvec(i)**2
                i = i + 1
              endif
              if (igflag(22,imsequ) .ne. 0) then
                write (iqpr2, 910) 'PTEND', gpesi(22), zendyn(6),
     +          gtarget(22,imsequ), gtarget(22,imsequ), fvec(i)**2
                i = i + 1
              endif
            endif
 
*---- Invariant constraints.
            if (igflag(23,imsequ) .ne. 0) then
              write (iqpr2, 910) 'WXMIN', gpesi(23), wxmin,
     +        gtarget(23,imsequ), gtarget(23,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(24,imsequ) .ne. 0) then
              write (iqpr2, 910) 'WXMAX', gpesi(24), wxmax,
     +        gtarget(24,imsequ), gtarget(24,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(25,imsequ) .ne. 0) then
              write (iqpr2, 910) 'WYMIN', gpesi(25), wymin,
     +        gtarget(25,imsequ), gtarget(25,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(26,imsequ) .ne. 0) then
              write (iqpr2, 910) 'WYMAX', gpesi(26), wymax,
     +        gtarget(26,imsequ), gtarget(26,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(27,imsequ) .ne. 0) then
              write (iqpr2, 910) 'WXYMIN', gpesi(27), wxymin,
     +        gtarget(27,imsequ), gtarget(27,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(28,imsequ) .ne. 0) then
              write (iqpr2, 910) 'WXYMAX', gpesi(28), wxymax,
     +        gtarget(28,imsequ), gtarget(28,imsequ), fvec(i)**2
              i = i + 1
            endif
            if (igflag(29,imsequ) .ne. 0) then
              write (iqpr2, 910) 'FORMULA', gpesi(29), formula,
     +        gtarget(29,imsequ), gtarget(29,imsequ), fvec(i)**2
              if (iformula .ne. 0) call flclos(iformula, error)
              i = i + 1
            endif
          endif
        endif
      endif
 
  910 format(t32,'GLOBAL'/t45,a,t56,1p,e12.6,4e16.6)
 
      end
