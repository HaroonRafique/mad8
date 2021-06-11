      subroutine suangl(w, theta, phi, psi)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Given a rotation matrix, compute the survey angles.                *
* Input:                                                               *
*   W(3,3)    (real)    Rotation matrix.                               *
* Output:                                                              *
*   THETA     (real)    Azimuthal angle.                               *
*   PHI       (real)    Elevation angle.                               *
*   PSI       (real)    Roll angle.                                    *
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
      double precision a,arg,b,phi,proxim,psi,theta,twopi,utwopi,w
      dimension         w(3,3)
 
      double precision pi
      parameter         (pi = 3.141592653589793d0)
      parameter         (twopi = 2.0d0 * pi, utwopi = 1.0d0 / twopi)
      proxim(a,b) = a + twopi * anint((b - a) * utwopi)
 
      arg = sqrt(w(2,1)**2 + w(2,2)**2)
      phi = atan2(w(2,3), arg)
      if (arg .gt. 1.0e-20) then
        theta = proxim(atan2(w(1,3), w(3,3)), theta)
        psi = proxim(atan2(w(2,1), w(2,2)), psi)
      else
        psi = proxim(atan2(-w(1,2), w(1,1))-theta, psi)
      endif
 
      end
