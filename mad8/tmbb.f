      subroutine tmbb(fsec, ftrk, parvec, orbit, fmap, ek, re, te)
      implicit none
*----------------------------------------------------------------------*
* purpose:
*   transport map for beam-beam element.
* input:
*   fsec      (logical) must be .true. for this purpose
*   ftrk      (logical) must be true for this purpose
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
*   orbit(6)  (double)  closed orbit (only kick is added since BB thin)
* output:
*   fmap      (logical) true if map calculated
*   ek(6)     (double)  kick due to element.
*   re(6,6)   (double)  transfer matrix.
*   te(6,6,6) (double)  second-order terms.
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
      logical fsec, ftrk, fmap
      double precision parvec(*), orbit(*), ek(*), re(6,*), te(6,6,*)
      double precision zero, one, two, three, explim, pi
      parameter (zero = 0.d0, one = 1.0d0, two = 2.0d0, three = 3.0d0)
*     if x > explim, exp(-x) is outside machine limits.
      parameter         (explim = 150.0d0)
      integer i, j, k
      double precision sx, sy, xm, ym, sx2, sy2, xs, ys, rho2, fk, tk,
     +exk,phix, phiy, rho4, phixx, phixy, phiyy, rho6, rk, exkc, xb, yb,
     +phixxx, phixxy, phixyy, phiyyy, crx, cry, xr, yr, r, r2, cbx, cby
*---- initialize.
      if (bbd_flag .ne. 0 .and. .not. bborbit)  then
        if (bbd_cnt .eq. bbd_max)  then
          call aawarn('TMBB', 1, 'maximum bb number reached')
        else
          bbd_cnt = bbd_cnt + 1
          bbd_loc(bbd_cnt) = bbd_pos
          bb_kick(1,bbd_cnt) = zero
          bb_kick(2,bbd_cnt) = zero
        endif
      endif
      fmap = .true.
      pi = 3.141592653589793d0
      do i = 1, 6
        ek(i) = zero
        do j = 1, 6
          re(j,i) = zero
          if (fsec)  then
            do k = 1, 6
              te(k,j,i) = zero
            enddo
          endif
        enddo
        re(i,i) = one
      enddo
      sx = parvec(1)
      sy = parvec(2)
      xm = parvec(3)
      ym = parvec(4)
      fk = two * parvec(5) * parvec(6) / parvec(7)
      if (fk .ne. zero)  then
*---- if tracking is desired ...
        if (ftrk) then
          sx2 = sx * sx
          sy2 = sy * sy
          xs  = orbit(1) - xm
          ys  = orbit(3) - ym
 
*---- limit formulas for sigma(x) = sigma(y).
          if (abs(sx2 - sy2) .le. 1.0d-3 * (sx2 + sy2)) then
            rho2 = xs * xs + ys * ys
 
*---- limit case for xs = ys = 0.
            if (rho2 .eq. zero) then
              re(2,1) = fk / (two * sx2)
              re(4,3) = fk / (two * sx2)
 
*---- general case.
            else
              tk = rho2 / (two * sx2)
              if (tk .gt. explim) then
                exk  = zero
                exkc = one
                phix = xs * fk / rho2
                phiy = ys * fk / rho2
              else
                exk  = exp(-tk)
                exkc = one - exk
                phix = xs * fk / rho2 * exkc
                phiy = ys * fk / rho2 * exkc
              endif
 
*---- orbit kick - only applied if option bborbit (HG 5/12/01),
*     else stored
              if (bborbit) then
                orbit(2) = orbit(2) + phix
                orbit(4) = orbit(4) + phiy
              elseif (bbd_flag .ne. 0)  then
                bb_kick(1,bbd_cnt) = phix
                bb_kick(2,bbd_cnt) = phiy
              endif
*---- first-order effects.
              rho4 = rho2 * rho2
              phixx = fk * (- exkc * (xs*xs - ys*ys) / rho4
     +                      + exk * xs*xs / (rho2 * sx2))
              phixy = fk * (- exkc * two * xs * ys / rho4
     +                      + exk * xs*ys / (rho2 * sx2))
              phiyy = fk * (+ exkc * (xs*xs - ys*ys) / rho4
     +                      + exk * ys*ys / (rho2 * sx2))
              re(2,1) = phixx
              re(2,3) = phixy
              re(4,1) = phixy
              re(4,3) = phiyy
 
*---- second-order effects.
              if (fsec) then
                rho6 = rho4 * rho2
                phixxx = fk*xs * (+ exkc * (xs*xs - three*ys*ys) / rho6
     +            - exk * (xs*xs - three*ys*ys) / (two * rho4 * sx2)
     +            - exk * xs*xs / (two * rho2 * sx2**2))
                phixxy = fk*ys * (+ exkc * (three*xs*xs - ys*ys) / rho6
     +            - exk * (three*xs*xs - ys*ys) / (two * rho4 * sx2)
     +            - exk * xs*xs / (two * rho2 * sx2**2))
                phixyy = fk*xs * (- exkc * (xs*xs - three*ys*ys) / rho6
     +            + exk * (xs*xs - three*ys*ys) / (two * rho4 * sx2)
     +            - exk * ys*ys / (two * rho2 * sx2**2))
                phiyyy = fk*ys * (- exkc * (three*xs*xs - ys*ys) / rho6
     +            + exk * (three*xs*xs - ys*ys) / (two * rho4 * sx2)
     +            - exk * ys*ys / (two * rho2 * sx2**2))
                te(2,1,1) = phixxx
                te(2,1,3) = phixxy
                te(2,3,1) = phixxy
                te(4,1,1) = phixxy
                te(2,3,3) = phixyy
                te(4,1,3) = phixyy
                te(4,3,1) = phixyy
                te(4,3,3) = phiyyy
              endif
            endif
 
*---- case sigma(x) > sigma(y).
          else
            r2 = two * (sx2 - sy2)
            if (sx2 .gt. sy2) then
              r  = sqrt(r2)
              rk = fk * sqrt(pi) / r
              xr = abs(xs) / r
              yr = abs(ys) / r
              call cperrf(xr, yr, crx, cry)
              tk = (xs * xs / sx2 + ys * ys / sy2) / two
              if (tk .gt. explim) then
                exk = 0.0
                cbx = 0.0
                cby = 0.0
              else
                exk = exp(-tk)
                xb  = (sy / sx) * xr
                yb  = (sx / sy) * yr
                call cperrf(xb, yb, cbx, cby)
              endif
 
*---- case sigma(x) < sigma(y).
            else
              r  = sqrt(-r2)
              rk = fk * sqrt(pi) / r
              xr = abs(xs) / r
              yr = abs(ys) / r
              call cperrf(yr, xr, cry, crx)
              tk = (xs * xs / sx2 + ys * ys / sy2) / two
              if (tk .gt. explim) then
                exk = 0.0
                cbx = 0.0
                cby = 0.0
              else
                exk = exp(-tk)
                xb  = (sy / sx) * xr
                yb  = (sx / sy) * yr
                call cperrf(yb, xb, cby, cbx)
              endif
            endif
 
            phix = rk * (cry - exk * cby) * sign(one, xs)
            phiy = rk * (crx - exk * cbx) * sign(one, ys)
*---- orbit kick - only applied if option bborbit (HG 5/12/01),
*     else stored
            if (bborbit) then
              orbit(2) = orbit(2) + phix
              orbit(4) = orbit(4) + phiy
            elseif (bbd_flag .ne. 0)  then
              bb_kick(1,bbd_cnt) = phix
              bb_kick(2,bbd_cnt) = phiy
            endif
 
*---- first-order effects.
            phixx = (two / r2) * (- (xs * phix + ys * phiy)
     +                            + fk * (one - (sy / sx) * exk))
            phixy = (two / r2) * (- (xs * phiy - ys * phix))
            phiyy = (two / r2) * (+ (xs * phix + ys * phiy)
     +                            - fk * (one - (sx / sy) * exk))
            re(2,1) = phixx
            re(2,3) = phixy
            re(4,1) = phixy
            re(4,3) = phiyy
 
*---- second-order effects.
            if (fsec) then
              phixxx = (- phix - (xs * phixx + ys * phixy)
     +                  + fk * xs * sy * exk / sx**3) / r2
              phixxy = (- phiy - (xs * phixy - ys * phixx)) / r2
              phixyy = (+ phix - (xs * phiyy - ys * phixy)) / r2
              phiyyy = (+ phiy + (xs * phixy + ys * phiyy)
     +                  - fk * ys * sx * exk / sy**3) / r2
              te(2,1,1) = phixxx
              te(2,1,3) = phixxy
              te(2,3,1) = phixxy
              te(4,1,1) = phixxy
              te(2,3,3) = phixyy
              te(4,1,3) = phixyy
              te(4,3,1) = phixyy
              te(4,3,3) = phiyyy
            endif
          endif
 
*---- no tracking desired.
        else
          re(2,1) = fk / (sx * (sx + sy))
          re(4,3) = fk / (sy * (sx + sy))
        endif
      endif
      end
