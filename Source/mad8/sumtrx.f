      subroutine sumtrx(the, phi, psi, w)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Given three survey angles, compute rotation matrix.                *
* Input:                                                               *
*   THE       (real)    Azimuthal angle.                               *
*   PHI       (real)    Elevation angle.                               *
*   PSI       (real)    Roll angle.                                    *
* Output:                                                              *
*   W(3,3)    (real)    Rotation matrix.                               *
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
      double precision cosphi,cospsi,costhe,phi,psi,sinphi,sinpsi,
     +sinthe,the,w
      dimension         w(3,3)
 
      costhe = cos(the)
      sinthe = sin(the)
      cosphi = cos(phi)
      sinphi = sin(phi)
      cospsi = cos(psi)
      sinpsi = sin(psi)
      w(1,1) = + costhe * cospsi - sinthe * sinphi * sinpsi
      w(1,2) = - costhe * sinpsi - sinthe * sinphi * cospsi
      w(1,3) =                     sinthe * cosphi
      w(2,1) =                              cosphi * sinpsi
      w(2,2) =                              cosphi * cospsi
      w(2,3) =                              sinphi
      w(3,1) = - sinthe * cospsi - costhe * sinphi * sinpsi
      w(3,2) = + sinthe * sinpsi - costhe * sinphi * cospsi
      w(3,3) =                     costhe * cosphi
 
      end
