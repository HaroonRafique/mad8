      subroutine tmfrng(fsec, h, sk1, edge, he, sig, corr, ek, re, te)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   TRANSPORT map for fringe field of a dipole.                        *
* Input:                                                               *
*   FSEC      (logical) If true, return second order terms.            *
*   H         (real)    Curvature of magnet body.                      *
*   SK1       (real)    Quadrupole strength in magnet body.            *
*   EDGE      (real)    Edge focussing angle.                          *
*   HE        (real)    Curvature of pole face.                        *
*   SIG       (real)    Sign: +1.0 for entry, -1.0 for exit.           *
*   CORR      (real)    Correction factor according to SLAC 75.        *
* Output:                                                              *
*   EK(6)     (real)    Kick due to fringe field.                      *
*   RE(6,6)   (real)    Transfer matrix.                               *
*   TE(6,6,6) (real)    Second order terms.                            *
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
      double precision corr,edge,ek,h,he,hh,psip,re,secedg,sig,sk1,
     +tanedg,te
      logical           fsec
      dimension         ek(6), re(6,6), te(6,6,6)
 
*---- Initialize.
      call uzero(ek, 1, 6*mwflt)
      call m66one(re)
 
*---- Linear terms.
      tanedg = tan(edge)
      secedg = 1.0 / cos(edge)
      psip = edge - corr * secedg * (1.0 + sin(edge)**2)
      re(2,1) = + h * tanedg
      re(4,3) = - h * tan(psip)
 
*---- Second-order terms.
      if (fsec) then
        call uzero(te, 1, 216*mwflt)
        hh = sig * (h/2)
        te(1,1,1) = - hh * tanedg**2
        te(1,3,3) = + hh * secedg**2
        te(2,1,1) = (h/2) * he * secedg**3 + sk1 * tanedg
        te(2,1,2) = - te(1,1,1)
        te(2,3,3) = hh * h * tanedg**3 - te(2,1,1)
        te(2,3,4) = + te(1,1,1)
        te(3,1,3) = - te(1,1,1)
        te(4,1,3) = - te(2,1,1)
        te(4,1,4) = + te(1,1,1)
        te(4,2,3) = - te(1,3,3)
        if (sig .gt. 0.0) then
          te(2,3,3) = te(2,3,3) + (h*secedg)**2 * tanedg/2
        else
          te(2,1,1) = te(2,1,1) - (h*tanedg)**2 * tanedg/2
          te(4,1,3) = te(4,1,3) + (h*secedg)**2 * tanedg/2
        endif
        call tmsymm(te)
      endif
 
      end
