      subroutine beamint(parvec, track, np)
*----------------------------------------------------------------------*
* Purpose:
*   Hirata 6D beam-beam element (thin lens, but sliced)
*----------------------------------------------------------------------*
*   parvec    (double)  BB element parameter vector:
*                       1:  sigma_x [m]
*                       2:  sigma_y [m]
*                       3:  x_offset [m]
*                       4:  y_offset [m]
*                       5:  classical particle radius [m]
*                       6:  total bunch charge [electron charges] of
*                           opposite beam
*                       7:  gamma = E / (m c^2)
*                       8:  ex = hor. emittance
*                       9:  ey = vert. emittance
*                      10:  phi = crossing angle
*                      11:  cop = closed orbit px
*                      12:  coq = closed orbit py
*                      13:  alpha_x
*                      14:  alpha_y
*                      15:  disp_x
*                      16:  disp_y
*                      17:  ddisp_x/ds
*                      18:  ddisp_y/ds
*                      19:  sigzs = sigma_t of synch. movement
*                      20:  siges = sigma_e of synch. movement
*                      21:  coz   = closed orbit s off-set
*                      22:  coe   = closed orbit e off-set
*                      23:  no. of slices
*                      24:  iopt: 10 (if vertical crossing) + flag
*                           flag = 0: use table, 1: recalculate errf
*                      25:  cox = closed orbit off-set in x
*                      26:  coy = closed orbit off-set in y
      implicit none
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer i,iopt,ivert,np,nsli
      double precision alphaxs,alphays,alphxs,alphys,
     +betaxs,betays,coe,cop,coq,cox,coy,coz,cphi,d,dinv,emitxs,
     +emitys,etapxs,etapys,etaxs,etays,f,odlum,phi,pi,ptemp,
     +qtemp,siges,sigpps,sigqqs,sigxxn,sigxxs,sigyyn,sigyys,sigzs,
     +sigzzs,sphi,tphi,track,xiyn,xstar,xtemp,ystar,ytemp,
     +zstar, betas, ex, ey, exn, eyn, parvec(*)
      dimension track(6,np)
      dimension xstar(15),ystar(15),zstar(15)
      dimension sigxxs(15),sigpps(15),sigyys(15),sigqqs(15)
      dimension d(6,6),dinv(6,6)
      integer err_cnt
      external bbf, bbf1
      data err_cnt / 0 /
      data pi /3.141592653589793d0/
      phi=parvec(10)
      nsli=parvec(23)
      if (nsli .le. 0)  nsli = 1
      iopt=parvec(24)
      if (parvec(6) .eq. 0.)  goto 999
      betas = sqrt(1.d0 - 1.d0 / parvec(7)**2)
      ex = parvec(8)
      ey = parvec(9)
      exn = 4.d0 * betas * parvec(7) * ex
      eyn = 4.d0 * betas * parvec(7) * ey
      sigzs=parvec(19)
      siges=parvec(20)
      coz=parvec(21)
      coe=parvec(22)
*     in case of iopt=10 or 11 vertical crossing!!!!!
      ivert=0
      if(iopt.gt.2) then
        iopt=iopt-10
        ivert=1
        xiyn = -parvec(5) * parvec(6) / (pi * eyn)
        betaxs=parvec(2)**2 / ey
        betays=parvec(1)**2 / ex
        alphxs=parvec(14)
        alphys=parvec(13)
        etaxs=parvec(16)
        etays=parvec(15)
        etapxs=parvec(18)
        etapys=parvec(17)
        emitxs=ey
        emitys=ex
        cox=parvec(4) + parvec(26)
        cop=parvec(12)
        coy=-(parvec(3) + parvec(25))
        coq=-parvec(11)
*     rotation of 90 degrees!
        do i=1,np
          xtemp=track(1,i)
          ptemp=track(2,i)
          ytemp=track(3,i)
          qtemp=track(4,i)
          track(1,i)=+ytemp
          track(2,i)=+qtemp
          track(3,i)=-xtemp
          track(4,i)=-ptemp
        enddo
      else
        xiyn = -parvec(5) * parvec(6) / (pi * exn)
        betaxs=parvec(1)**2 / ex
        betays=parvec(2)**2 / ey
        alphxs=parvec(13)
        alphys=parvec(14)
        etaxs=parvec(15)
        etays=parvec(16)
        etapxs=parvec(17)
        etapys=parvec(18)
        emitxs=ex
        emitys=ey
        cox=parvec(3) + parvec(25)
        cop=parvec(11)
        coy=parvec(4) + parvec(26)
        coq=parvec(12)
      endif
      if (abs(emitxs) .lt. 1.d-16 .or. abs(emitys) .lt. 1.d-16) then
        err_cnt = err_cnt + 1
        if (err_cnt .le. 10)  then
          call aawarn('beamint', 1, 'emitxs or emitys = 0.')
        endif
        goto 999
      endif
      if (abs((emitxs - emitys)/(emitxs + emitys)) .lt. 1.d-3)
     +emitys = 0.4995 * (emitxs + emitys)
      sphi=sin(phi)
      cphi=cos(phi)
      tphi=tan(phi)
 
*     define slices
        call stsld(xstar,ystar,zstar,phi,
     &         sigzs,siges,emitxs,betaxs,alphxs,
     &         emitys,betays,alphys,
     &         etaxs,etapxs,etays,etapys,
     &         sigzzs,sigxxs,sigpps,sigyys,sigqqs,nsli)
 
*     define f
          sigxxn=emitxs*betaxs
          sigyyn=emitys*betays
      if (abs(betays) .lt. 1.d-16) then
        err_cnt = err_cnt + 1
        if (err_cnt .le. 10)  then
          call aawarn('beamint', 1, 'betays = 0.')
        endif
        goto 999
      endif
          f=xiyn/betays*2*pi*sqrt(sigyyn)*
     $         (sqrt(sigyyn)+sqrt(sigxxn))/nsli
 
        call revmat(etaxs,etapxs,etays,etapys,betaxs,betays,alphaxs,
     &     alphays, emitxs,emitys,sigzs,siges,d,dinv)
 
*       not necessary because mad has physical coordinates
*       call promvv(dinv,np,track)
 
           do i=1,np
           track(1,i)=track(1,i)-cox
           track(2,i)=track(2,i)-cop
           track(3,i)=track(3,i)-coy
           track(4,i)=track(4,i)-coq
           track(5,i)=track(5,i)-coz
           track(6,i)=track(6,i)-coe
           enddo
 
        call boost(sphi,cphi,tphi,np,track)
            if(iopt.eq.0) call sbc(xstar,ystar,zstar,bbf,
     &           odlum,sigxxs,sigpps,sigyys,sigqqs,f,np,nsli,track)
            if(iopt.eq.1) call sbc(xstar,ystar,zstar,bbf1,
     &           odlum,sigxxs,sigpps,sigyys,sigqqs,f,np,nsli,track)
        call boosti(sphi,cphi,np,track)
 
           do i=1,np
           track(1,i)=track(1,i)+cox
           track(2,i)=track(2,i)+cop
           track(3,i)=track(3,i)+coy
           track(4,i)=track(4,i)+coq
           track(5,i)=track(5,i)+coz
           track(6,i)=track(6,i)+coe
           enddo
 
*     in case of iopt=10 or 11 vertical crossing!!!!!
*     rotation of 90 degrees!
      if(ivert.eq.1) then
      do i=1,np
        xtemp=track(1,i)
        ptemp=track(2,i)
        ytemp=track(3,i)
        qtemp=track(4,i)
        track(1,i)=-ytemp
        track(2,i)=-qtemp
        track(3,i)=+xtemp
        track(4,i)=+ptemp
      enddo
      endif
 
  999 end
