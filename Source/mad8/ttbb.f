      subroutine ttbb(parvec, track, ktrack)
      implicit none
*----------------------------------------------------------------------*
* purpose:
*   track a set of particle through a beam-beam interaction region.
*   see mad physicist's manual for the formulas used.
*input:
*   parvec    (double)  BB element parameter vector:
*                       1:  sigma_x [m]
*                       2:  sigma_y [m]
*                       3:  x_offset [m]
*                       4:  y_offset [m]
*                       5:  classical particle radius [m]
*                       6:  total bunch charge [electron charges] of
*                           opposite beam
*                       7:  gamma = E / (m c^2)
* input/output:
*   track(6,*)(double)  track coordinates: (x, px, y, py, t, pt).
*   ktrack    (integer) number of tracks.
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
      integer bbd_max, bbd_cnt, bbd_pos, bbd_loc, bbd_flag
      parameter (bbd_max = 200)
      common / bbcommi / bbd_cnt, bbd_pos, bbd_flag, bbd_loc(bbd_max)
      double precision bb_kick
      common / bbcommr / bb_kick(2, bbd_max)
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
      integer ktrack
      double precision parvec(*), track(6,*)
      double precision zero, one, two, three, explim, pi
      parameter (zero = 0.d0, one = 1.0d0, two = 2.0d0, three = 3.0d0)
*     if x > explim, exp(-x) is outside machine limits.
      parameter         (explim = 150.0d0)
      integer itrack, ipos
      double precision sx, sy, xm, ym, sx2, sy2, xs, ys, rho2, fk, tk,
     +phix, phiy, rk, xb, yb, crx, cry, xr, yr, r, r2, cbx, cby
 
*---- initialize.
      pi = 4 * atan(one)
      sx = parvec(1)
      sy = parvec(2)
      xm = parvec(3)
      ym = parvec(4)
      fk = two * parvec(5) * parvec(6) / parvec(7)
      if (fk .eq. zero)  return
      ipos = 0
      if (.not. bborbit)  then
*--- find position of closed orbit bb_kick
        do ipos = 1, bbd_cnt
          if (bbd_loc(ipos) .eq. bbd_pos)  goto 1
        enddo
        ipos = 0
    1   continue
      endif
      sx2 = sx*sx
      sy2 = sy*sy
*---- limit formulae for sigma(x) = sigma(y).
      if (abs(sx2 - sy2) .le. 1.0d-3 * (sx2 + sy2)) then
        do 10 itrack = 1, ktrack
          xs = track(1,itrack) - xm
          ys = track(3,itrack) - ym
          rho2 = xs * xs + ys * ys
          tk = rho2 / (two * sx2)
          if (tk .gt. explim) then
            phix = xs * fk / rho2
            phiy = ys * fk / rho2
          else if (rho2 .ne. zero) then
            phix = xs * fk / rho2 * (one - exp(-tk) )
            phiy = ys * fk / rho2 * (one - exp(-tk) )
          else
            phix = zero
            phiy = zero
          endif
          if (ipos .ne. 0)  then
*--- subtract closed orbit kick
            phix = phix - bb_kick(1,ipos)
            phiy = phiy - bb_kick(2,ipos)
          endif
          track(2,itrack) = track(2,itrack) + phix
          track(4,itrack) = track(4,itrack) + phiy
   10   continue
 
*---- case sigma(x) > sigma(y).
      else if (sx2 .gt. sy2) then
        r2 = two * (sx2 - sy2)
        r  = sqrt(r2)
        rk = fk * sqrt(pi) / r
        do 20 itrack = 1, ktrack
          xs = track(1,itrack) - xm
          ys = track(3,itrack) - ym
          xr = abs(xs) / r
          yr = abs(ys) / r
          call cperrf(xr, yr, crx, cry)
          tk = (xs * xs / sx2 + ys * ys / sy2) / two
          if (tk .gt. explim) then
            phix = rk * cry
            phiy = rk * crx
          else
            xb = (sy / sx) * xr
            yb = (sx / sy) * yr
            call cperrf(xb, yb, cbx, cby)
            phix = rk * (cry - exp(-tk) * cby)
            phiy = rk * (crx - exp(-tk) * cbx)
          endif
          track(2,itrack) = track(2,itrack) + phix * sign(one,xs)
          track(4,itrack) = track(4,itrack) + phiy * sign(one,ys)
          if (ipos .ne. 0)  then
*--- subtract closed orbit kick
            track(2,itrack) = track(2,itrack) - bb_kick(1,ipos)
            track(4,itrack) = track(4,itrack) - bb_kick(2,ipos)
          endif
   20   continue
 
*---- case sigma(x) < sigma(y).
      else
        r2 = two * (sy2 - sx2)
        r  = sqrt(r2)
        rk = fk * sqrt(pi) / r
        do 30 itrack = 1, ktrack
          xs = track(1,itrack) - xm
          ys = track(3,itrack) - ym
          xr = abs(xs) / r
          yr = abs(ys) / r
          call cperrf(yr, xr, cry, crx)
          tk = (xs * xs / sx2 + ys * ys / sy2) / two
          if (tk .gt. explim) then
            phix = rk * cry
            phiy = rk * crx
          else
            xb  = (sy / sx) * xr
            yb  = (sx / sy) * yr
            call cperrf(yb, xb, cby, cbx)
            phix = rk * (cry - exp(-tk) * cby)
            phiy = rk * (crx - exp(-tk) * cbx)
          endif
          track(2,itrack) = track(2,itrack) + phix * sign(one,xs)
          track(4,itrack) = track(4,itrack) + phiy * sign(one,ys)
          if (ipos .ne. 0)  then
*--- subtract closed orbit kick
            track(2,itrack) = track(2,itrack) - bb_kick(1,ipos)
            track(4,itrack) = track(4,itrack) - bb_kick(2,ipos)
          endif
   30   continue
      endif
 
      end
