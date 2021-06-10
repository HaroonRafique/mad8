      subroutine twibs
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   INTRABEAM SCATTERING, IBS Command                                  *
*   These routines are a much reduced version of IBS as taken          *
*   from the program ZAP, written by M. Zisman.                        *
*   One should refer to the ZAP USERS MANUAL LBL-21270 UC-28.          *
* Attribute:                                                           *
*   TABLE     (name)    Name of Twiss table.                           *
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
      integer mcode,mfrst,mlump,mocc1,mocc2,moptc,mprnt,mrefe,msbet,
     +mscnd,mserr,mtrck
 
*---- Status flags for sequence group.
      parameter         (mcode = 3, mocc1 = 13, mocc2 = 20,
     +                   mfrst = mcode + 1, mlump = mcode + 2,
     +                   mrefe = mcode + 3, mscnd = mcode + 4,
     +                   moptc = mcode + 5, mprnt = mcode + 6,
     +                   mtrck = mcode + 7, msbet = mcode + 8,
     +                   mserr = mcode + 9)
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
      double precision alfx,alfy,amux,amuy,betx,bety,ddisp,disp,dmux,
     +dmuy,orbit,phix,phiy,rmat,suml,wx,wy,ener1
 
*---- Current conditions for optical functions.
      common /optic1/   betx, alfx, amux, bety, alfy, amuy,
     +                  orbit(6), disp(6),
     +                  wx, phix, dmux, wy, phiy, dmuy,
     +                  ddisp(6), suml, rmat(2,2), ener1
      save              /optic1/
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
      integer ltwbet,ltwbuf,ltwfun,ltwlin,ltwopt,ltwsum
 
*---- Reference links for lattice function tables.
      common /twlink/   ltwlin, ltwbet, ltwbuf, ltwfun, ltwopt, ltwsum
      save              /twlink/
      integer iqlog,iqpnch,iqpr2,iqprnt,iqread,iqttin,iqtype
 
*---- Logical unit numbers for ZEBRA system.
      common /zunit/    iqread, iqprnt, iqpr2,  iqlog,  iqpnch,
     +                  iqttin, iqtype
      save              /zunit/
      double precision amu0,asube,asubp,clight,elamda,emass,eps0,erad,
     +falfa,hbar,plamda,pmass,qelect,mumass
 
*---- Universal physical constants.
*     Velocity of light [m/s]:
      parameter         (clight = 2.997 924 58 d+08)
*     Permeability of vacuum [V*s/A*m]:
      parameter         (amu0   = 1.256 637 061d-06)
*     Permittivity of vaccum [A*S/V*m]:
      parameter         (eps0   = 8.854 187 817d-12)
*     Reduced Plack's constant [GeV*s]:
      parameter         (hbar   = 6.58211889d-25)
 
*---- Electromagnetic constants.
*     Elementary charge [A*s]:
      parameter         (qelect = 1.602176462d-19)
*     Fine structure constant [1]:
      parameter         (falfa  = 7.297 353 08 d-03)
 
*---- Electron.
*     Rest mass [GeV]:
      parameter         (emass  = 0.510998902d-3)
*     Classical radius [m]:
      parameter         (erad   = 2.817940285d-15)
*     Reduced Compton wavelength [m]:
      parameter         (elamda = 3.861 593 23 d-13)
*     Magnetic moment anomaly [1]:
      parameter         (asube  = 1.159 652 193d-03)
 
*---- Proton.
*     Rest mass [GeV]:
      parameter         (pmass  = 0.938271998d+00)
*     Reduced Compton wavelength [m]:
      parameter         (plamda = 2.103 089 37 d-16)
*     Magnetic moment anomaly [1]:
      parameter         (asubp  = 1.792 847 386d+00)
 
*---- Muon.
*     Rest mass [GeV]:
      parameter         (mumass  = 0.1056583568d+00)
      integer ienum,iflag,iocc,ipos,jbit,jbyt,nline
      double precision alx,alxbar,alxwtd,aly,alybar,ax1,ax2,ay1,ay2,
     +betax,betay,beteff,bx1,bx2,bxbar,bxinv,by1,by2,bybar,byinv,bywtd,
     +const,dels,dpx,dpx1,dpx2,dpxbr,dpxwtd,dx,dx1,dx2,dxbar,dxwtd,half,
     +hscrpt,hscwtd,s1,s2,salxb,salyb,sbxb,sbxinv,sbyb,sbyinv,sdpxb,
     +sdxb,taul,taux,tauy,tavl,tavlc,tavx,tavxc,tavy,tavyc,tlbar,tlidc,
     +tlwtd,txbar,txidc,txwtd,tybar,tyidc,tywtd,wnorm,zero
 
      character*(mcnam) elmnam*(mcnam), tabnam*(mcnam), title*(*)
      logical           head
 
      parameter         (half = 0.5d0, zero = 0.0d0)
      parameter         (title = 'Intrabeam scattering.')
 
*---- Retrieve table name to be used.
      tabnam = 'TWISS'
      call utgnam(lccmd, 1, 1, tabnam)
      call enfix
 
*---- Initialize variables to accumulate weighted average lifetimes.
      tavlc  = 0.0
      tavxc  = 0.0
      tavyc  = 0.0
      dxwtd  = 0.0
      dpxwtd = 0.0
      bywtd  = 0.0
      alxwtd = 0.0
      hscwtd = 0.0
      wnorm  = 0.0
      sbxb   = 0.0
      sbyb   = 0.0
      salxb  = 0.0
      salyb  = 0.0
      sdxb   = 0.0
      sdpxb  = 0.0
      sbxinv = 0.0
      sbyinv = 0.0
 
*---- Open file for lattice parameters.
      call tbopen(tabnam, 1, ltwfun)
 
*---- Pick up the beam line data.
      call utbeam(lcseq, irg1, irg2, symm, nsup, linnam, rngnam)
 
*---- Print header.
      call prhead('IBS', title, zero, 0, nline, 1)
      if (fbch) then
        write (iqpr2, 910) prtnam, '  '
      else
        write (iqpr2, 910) prtnam, 'un'
      endif
      call enprgl
      call enprem
      call enprrf
 
*---- Loop over lattice points to calculate lifetimes.
      head = .false.
      do 90 ipos = irg1, irg2
 
*---- Pickup various pointers for elements.
        call utelem(lcseq, ipos, iflag, elmnam, iocc, ienum)
 
*---- Advance through an element ?
        if (jbyt(iflag,1,mcode) .eq. 1) then
 
*---- Pick up optical functions for entrance of element.
          call twbtsv(3, ipos - 1)
          s1     = suml
          bx1    = betx
          by1    = bety
          ax1    = alfx
          ay1    = alfy
          dx1    = disp(1)
          dpx1   = disp(2)
 
*---- Pick up optical functions for exit of element.
          call twbtsv(3, ipos)
          s2     = suml
          bx2    = betx
          by2    = bety
          ax2    = alfx
          ay2    = alfy
          dx2    = disp(1)
          dpx2   = disp(2)
          dels   = s2 - s1
 
          betax  = half * (bx2 + bx1)
          sbxb   = sbxb + betax * dels
          sbxinv = sbxinv + dels / betax
 
          betay  = half * (by2 + by1)
          sbyb   = sbyb + betay * dels
          sbyinv = sbyinv + dels / betay
 
          alx    = half * (ax2 + ax1)
          salxb  = salxb + alx * dels
 
          aly    = half * (ay2 + ay1)
          salyb  = salyb + aly * dels
 
          dx     = half * (dx2 + dx1)
          sdxb   = sdxb + dx * dels
 
          dpx    = half * (dpx2 + dpx1)
          sdpxb  = sdpxb + dpx * dels
 
*---- Calculate weighted average in region of non-zero DX's.
*     These values are used to calculate "average" ring lifetimes
*     in TWSINT.
          if (dx .gt. 0.0) then
            wnorm  = wnorm + dels
            dxwtd  = dxwtd + dels * dx
            dpxwtd = dpxwtd + dels * dpx
            bywtd  = bywtd + dels / sqrt(betay)
            alxwtd = alxwtd + dels * alx
            hscrpt = betax * dpx**2 + 2.0 * alx * dx * dpx +
     +               (1.0 + alx**2) * dx**2 / betax
            hscwtd = hscwtd + dels * sqrt(hscrpt)
          endif
 
*---- TWSINT calculates the Bjorken/Mtingwa integral.
          call twsint(betax, betay, alx, dx, dpx, txidc, tyidc, tlidc)
 
*---- Accumulate contributions.
          tavlc = tavlc + tlidc * dels
          tavxc = tavxc + txidc * dels
          tavyc = tavyc + tyidc * dels
 
*---- See if an element has been flagged by the PRINT command.
          if(jbit(iflag,mprnt) .ne. 0) then
            if (.not. head) then
              write (iqpr2, 920)
              head = .true.
            endif
            write (iqpr2, 930) ienum, elmnam, tlidc, txidc, tyidc
          endif
        endif
   90 continue
 
*---- Close optical function table.
      call tbclos(ltwfun)
 
*---- We have finished reading the lattice from MAD
      bxbar  = sbxb / s2
      bybar  = sbyb / s2
      alxbar = salxb / s2
      alybar = salyb / s2
      dxbar  = sdxb / s2
      dpxbr  = sdpxb / s2
      bxinv  = sbxinv / s2
      byinv  = sbyinv / s2
 
      dxwtd  = dxwtd / wnorm
      dpxwtd = dpxwtd / wnorm
      bywtd  = bywtd / wnorm
      bywtd  = 1.0 / bywtd**2
      alxwtd = alxwtd / wnorm
      hscwtd = (hscwtd/wnorm)**2
      beteff = dxwtd**2 / hscwtd
 
*---- Integral for averaged quantities.
      call twsint(bxbar,bybar,alxbar,dxbar,dpxbr, txbar,tybar,tlbar)
 
*---- Integral for effective quantities.
      call twsint(beteff,bywtd,alxwtd,dxwtd,dpxwtd, txwtd,tywtd,tlwtd)
 
*---- Calculate the Coulomb logarithm.
      call twclog(bxbar, bybar, const)
 
*---- Output (weighted) average values.
      write (iqpr2, 940) bxbar, bybar, dxbar, alxbar, alybar, dpxbr,
     +                   bxinv, byinv
 
*---- Output averaged values.
      tavl   = tavlc * const / s2
      tavx   = tavxc * const / s2
      tavy   = tavyc * const / s2
 
      taul   = 1.0 / tavl
      taux   = 1.0 / tavx
      tauy   = 1.0 / tavy
 
      write (iqpr2, 950) tavl, tavx, tavy, taul, taux, tauy
 
  910 format(' '/' Particle beam: ',a,10x,a,'bunched.')
  920 format(' '/' Individual lattice point lifetimes'/' '/
     +       26x,'TLI/const',10x,'TXI/const',10x,'TYI/const'/
     +       27x,'(1/sec)',12x,'(1/sec)',12x,'(1/sec)'/' ')
  930 format(1x,i8,2x,a8,3x,3(1pe15.6,3x))
  940 format(' '/' Ring average values (m)'/' '/ 5x,'betx   = ',
     +       1pe13.5,4x, 'bety   = ',1pe13.5,4x,'Dx  = ',1pe12.5/
     +       5x,'alfx   = ',1pe13.5,4x,'alfy   = ',1pe13.5,4x,'Dpx = ',
     +       1pe12.5/5x,'1/betx = ',1pe13.5,4x,'1/bety = ',1pe13.5)
  950 format(' '/5x,'(Weighted) average rates (1/sec):'/
     +       5x,'Longitudinal= ',1p,e15.6/
     +       5x,'Horizontal  = ',   e15.6/
     +       5x,'Vertical    = ',   e15.6/
     +       ' '/5x,'(Weighted) average lifetimes (sec):'/
     +       5x,'Longitudinal= ',1p,e15.6/
     +       5x,'Horizontal  = ',   e15.6/
     +       5x,'Vertical    = ',   e15.6/' ')
 
      end
