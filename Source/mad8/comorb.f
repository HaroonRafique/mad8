      subroutine comorb(symm, jpl, a, nm, nc, betm, amum, betc, amuc)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Set up influence matrix for closed orbit correction.               *
* Input:                                                               *
*   SYMM      (logical) Symmetry flag.                                 *
*   JPL       (integer) 1: horizontal, 2: vertical.                    *
*   NM        (integer) Number of monitors.                            *
*   NC        (integer) Number of correctors.                          *
*   BETM(*)   (real)    Beta functions at monitors.                    *
*   AMUM(*)   (real)    Monitor phases.                                *
*   BETC(*)   (real)    Beta functions at correctors.                  *
*   AMUC(*)   (real)    Corrector phases.                              *
* Output:                                                              *
*   A(NDIM,*) (real)    Influence matrix.                              *
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
      integer ncor,nmon
      double precision akl,amuxcm,amuycm,betxcm,betycm,dxcm,dycm,halfqx,
     +halfqy,qual,scm,weight,xcm,ycm
 
*---- Data for current corrector or monitor.
*     Order of variables is important for UCOPY calls.
      common /codata/   xcm, ycm, dxcm, dycm, scm, betxcm, betycm,
     +                  amuxcm, amuycm, akl, halfqx, halfqy,
     +                  qual, weight(2), ncor(2), nmon(2)
      save              /codata/
      integer jc,jm,jpl,nc,nm
      double precision a,amuc,amum,betc,betm,cosecq,faccm,half,halfq
      logical           symm
      dimension         a(nm,nc), betm(*), amum(*), betc(*), amuc(*)
 
      parameter         (half = 0.5d0)
 
      if (jpl .eq. 1) then
        halfq = halfqx
      else
        halfq = halfqy
      endif
      cosecq = half / sin(halfq)
 
      do 10 jc = 1, nc
      do 10 jm = 1, nm
        faccm = cos(abs(amum(jm)-amuc(jc))-halfq)
        if (symm) then
          faccm = faccm + cos(amum(jm)+amuc(jc)-halfq)
        endif
        a(jm,jc) = faccm * sqrt(betm(jm)*betc(jc)) * cosecq
   10 continue
 
      end
