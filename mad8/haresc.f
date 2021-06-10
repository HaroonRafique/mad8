      subroutine haresc
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   This subroutine does the resonance calculations.                   *
*   HRESONANCE command.                                                *
* Attributes:                                                          *
*   ORDER     (integer) Order of resonances to be calculated.          *
*   DISP      (logical) True: Include dispersion in resonance coeffs.  *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      double precision ensige,ensigx,ensigy
 
*---- Communication area for HARMON module.
      common /harchr/   lngnam, shtnam
      common /harflt/   ensigx, ensigy, ensige
      save              /harchr/, /harflt/
      character*(mcnam) lngnam, shtnam
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
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      integer i,ipass,n1,n2,norder,np
      double precision beat,der,des,dqr,dqr20,dqs,dqs20,ex0,ey0,fact,
     +fact1,fact2,fact3,factor,fn1,fn2,rsum,sum,summ,sup
 
      logical           acode
      dimension         beat(5), fact(5)
      dimension         sum(2)
 
      data beat
     +  / 1.0d0,      1.0d0,      2.0d0, 1.553774d0, 1.384812d0 /
      data fact
     +  / 1.0d0, 5.545455d0, 2.066667d0, 1.446735d0, 1.194130d0 /
 
*---- Retrieve command attributes.
      norder = 3
      call utgint(lccmd, 1, 1, norder)
      acode = .false.
      call utglog(lccmd, 2, 2, acode)
 
*---- Emittances.
      ex0 = ex * ensigx**2
      ey0 = ey * ensigy**2
      if (ex0 .le. 0.0  .or.  ey0 .le. 0.0) then
        write (msg, 910) ex0, ey0
        call aawarn('HARESC', 2, msg)
        go to 9999
      endif
 
      sup = nsup
      do 100 ipass = 1, 2
        write (iqpr2, 920) norder, ex0, ey0
        if (acode) then
          write (iqpr2, 930) 'in'
        else
          write (iqpr2, 930) 'ex'
        endif
 
*---- Sum resonances.
        do 40 n2 = 0, norder, 2
          n1 = norder - n2
          fact1 = 1.0 / (2.0**(norder - 1) * factor(n1) * factor(n2))
          fn1 = n1
          fn2 = n2
          np = nsup * int((fn1 * qx + fn2 * qy) / sup)
          if (ipass .eq. 2) np = np + nsup
          write (iqpr2, 940) n1, n2, np
          call harsig(acode, n1, n2, np, sum, rsum)
          sum(1) = sum(1) * fact1
          sum(2) = sum(2) * fact1
          rsum = rsum * fact1
          summ = sqrt(sum(1)**2 + sum(2)**2)
 
          fact2 = (fn1**2 / ex0 + fn2**2 / ey0) *
     +            (sqrt(ex0)**n1 * sqrt(ey0)**n2)
          fact3 = 1.0
          if (norder .gt. 2  .and.  norder .lt. 6) then
            fact2 = fact2 * beat(norder)**(norder-2)
            fact3 = fact(norder)
          endif
          des = summ * fact2
          der = rsum * fact2
          dqs = des / sqrt(fn1**2+fn2**2)
          dqr = der / sqrt(fn1**2+fn2**2)
          dqs20 = dqs * fact3
          dqr20 = dqr * fact3
          write (iqpr2, 950) sum, summ, rsum,
     +                       des, der, dqs, dqr, dqs20, dqr20
   40   continue
 
*---- Difference resonances.
        do 90 i = 2, norder - 1, 2
          n2 = i
          n1 = norder - n2
          fact1 = 1.0 / (2.0**(norder - 1) * factor(n1) * factor(n2))
          if (n2 .gt. n1) n1 = - n1
          if (n2 .le. n1) n2 = - n2
          fn1 = n1
          fn2 = n2
          np = nsup * int((fn1 * qx + fn2 * qy) / sup)
          if (ipass .eq. 2) np = np + nsup
          write (iqpr2, 960) n1, n2, np
          call harsig(acode, n1, n2, np, sum, rsum)
          sum(1) = sum(1) * fact1
          sum(2) = sum(2) * fact1
          rsum = rsum * fact1
          summ = sqrt(sum(1)**2 + sum(2)**2)
          write (iqpr2, 970) sum, summ, rsum
   90   continue
  100 continue
 
*---- Fourth order effects of sextupoles.
      if (norder .eq. 3) call ha4ana
 
  910 format('Emittances should be greater than zero.'/
     +       'Actual values = ',1p,2e12.6)
  920 format(' '/' Fourier analysis, order of resonance',19x,
     +       'Ex0',9x,'Ey0'/32x,i5,10x,1p,2e12.4)
  930 format(' '/' Dispersion effects are ',a,'cluded.')
  940 format(' '/' n1 = ',i5,'  n2 = ',i5,'  p = ',i5/
     +       7x,'cosine',8x,'sine',5x,'modulus',6x,'random',
     +       7x,'dE(s)',7x,'dE(r)',7x,'dQ(s)',7x,'dQ(r)',5x,
     +       'dQ20(s)',5x,'dQ20(r)')
  950 format(' ',1p,10e12.4)
  960 format(' '/' n1 = ',i5,'  n2 = ',i5,'  p = ',i5/
     +       7x,'cosine',8x,'sine',5x,'modulus',6x,'random')
  970 format(' ',1p,4e12.4)
 
 9999 end
