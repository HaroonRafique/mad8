      subroutine hacfit(show, xix, xiy, prc)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Adjust chromaticities to desired values.                           *
* Input:                                                               *
*   SHOW      (logical) Print switch.                                  *
*   XIX       (real)    Desired horizontal chromaticity.               *
*   XIY       (real)    Desired vertical chromaticity.                 *
*   PRC       (real)    Tolerance for fit.                             *
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
      integer i,ijacx,ijacy,iter,itvec,ixvec,level,maxit,nfcnsv
      double precision alamx,alamy,cxx,cxy,cyy,det,edmsv,eps,errx,erry,
     +fminsv,h,prc,temp,xix,xix0,xixt,xiy,xiy0,xiyt
      logical           show
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer intmax
      double precision epsmch,fltmax,fltmin
 
*---- Parameters suitable for most computer systems.
      parameter         (epsmch = 1.0d-16)
      parameter         (fltmin = 1.0d-35)
      parameter         (fltmax = 1.0d+35)
      parameter         (intmax = 1000000000)
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
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
 
      parameter         (maxit = 10)
      character         crtsv*8, cstsv*16
 
*---- Assign working space.
      ixvec = iwork
      itvec = ixvec + nvar
      ijacx = itvec
      ijacy = ijacx + nvar
      iwork = ijacy + nvar
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Fetch initial values of variables.
      call mtgeti(nvar, dq(ixvec+1), dq(itvec+1))
 
*---- Save variables for minimization.
      crtsv = crout
      cstsv = cstat
      nfcnsv = nfcn
      fminsv = fmin
      edmsv = edm
 
*---- Begin iteration.
      crout = 'HTUNE'
      cstat = 'start'
      nfcn = 0
      eps = max(prc, sqrt(epsmch))
      do 90 iter = 1, maxit
 
*---- New point.
        call mtputi(nvar, dq(ixvec+1))
        call hachcl(xix0, xiy0)
        nfcn = nfcn + 1
        errx = xix0 - xix
        erry = xiy0 - xiy
        fmin = errx**2 + erry**2
        edm = fmin
        if (fmin .le. prc) then
          cstat = 'converged'
          go to 100
        endif
 
*---- Print iteration status.
        if (show) then
          level = 2
          if (iter .eq. 1) level = 1
          if (ilevel .ge. level) call mtprnt(nvar, dq(ixvec+1))
          cstat = 'progress'
          nfcn = nfcn + nvar
        endif
 
*---- Find Jacobian matrix.
        do 20 i = 1, nvar
          temp = dq(ixvec+i)
          h = eps * abs(temp)
          if (h .eq. 0.0) h = eps
          dq(ixvec+i) = temp + h
          call mtputi(nvar, dq(ixvec+1))
          call hachcl(xixt, xiyt)
          dq(ijacx+i) = (xixt - xix0) / h
          dq(ijacy+i) = (xiyt - xiy0) / h
          dq(ixvec+i) = temp
   20   continue
 
*---- Calculate Lagrange multipliers.
        cxx = 0.0
        cxy = 0.0
        cyy = 0.0
        do 70 i = 1, nvar
          cxx = cxx + dq(ijacx+i) * dq(ijacx+i)
          cxy = cxy + dq(ijacx+i) * dq(ijacy+i)
          cyy = cyy + dq(ijacy+i) * dq(ijacy+i)
   70   continue
        det = cxx * cyy - cxy * cxy
        if (abs(det) .lt. 1.0e-8) then
          call aawarn('HACFIT', 1,
     +    'Unable to tune chromaticity, variables have no effect.')
          cstat = 'failed'
          go to 100
        endif
        alamx = (cyy * errx - cxy * erry) / det
        alamy = (cxx * erry - cxy * errx) / det
 
*---- Corrections such as to minimize norm of correction vector.
        do 80 i = 1, nvar
          dq(ixvec+i) = dq(ixvec+i) -
     +      (alamx * dq(ijacx+i) + alamy * dq(ijacy+i))
   80   continue
   90 continue
 
*---- Final output.
  100 continue
      if (show) then
        if (ilevel .ge. 1) call mtprnt(nvar, dq(ixvec+1))
        write (iqpr2, 910) xix0, xiy0
        if (iqlog .ne. iqpr2) write (iqlog, 910) xix, xiy
      endif
 
*---- Restore variables for minimization.
      crout = crtsv
      cstat = cstsv
      nfcn = nfcnsv
      fmin = fminsv
      edm = edmsv
 
*---- Release working space.
      iwork = ixvec
 
  910 format(' '/' Chromaticity: Horizontal = ',f12.6,
     +       ',  vertical = ',f12.6/' ')
 
      end
