      function gauinv(p0)
      implicit none
*gauinv***********************************************
*  inverse of (integrated) normal distribution function
*              1         x= y
*     p(y)=-----------* integral exp(-x**2/2) dx
*          sqrt(2*pi)    x= -inf
*     if p(y)=p0, then gauinv(p0)=y.
*        0 < p0 < 1 ,   -inf < y < +inf
*  if this routine is used to convert uniform random numbers to
*  gaussian, maximum relative error in the distribution function
*  dp/dx=exp(-x**2/2)/sqrt(2*pi) is less than 0.640e-3 everywhere
*  in the range  2**(-31) < p0 < 1-2**31.  (minimax approximation)
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
c------------------------
      double precision a0,a1,a2,a3,b0,b1,b2,b3,b4,c0,c1,c2,c3,c4,d0,d1,
     +d2,d3,d4,e0,e1,e2,e3,e4,f0,f1,f2,gauinv,p,p0,p1,p2,pp1,q,qq2,qq3,
     +qq4,qq5,t
      data pp1/0.334624883253d0/, qq2/0.090230446775d0/,
     1     qq3/0.049905685242d0/, qq4/0.027852994157d0/,
     2     qq5/0.015645650215d0/
      data a3/ 4.5585614d+01/, a2/ 2.1635544d+00/, a1/ 2.7724523d+00/,
     1     a0/ 2.5050240d+00/,
     2     b4/ 4.0314354d+02/, b3/-2.7713713d+02/, b2/ 7.9731883d+01/,
     3     b1/-1.4946512d+01/, b0/ 2.2157257d+00/,
     4     c4/ 4.1394487d+03/, c3/-1.5585873d+03/, c2/ 2.4648581d+02/,
     5     c1/-2.4719139d+01/, c0/ 2.4335936d+00/,
     6     d4/ 4.0895693d+04/, d3/-8.5400893d+03/, d2/ 7.4942805d+02/,
     7     d1/-4.1028898d+01/, d0/ 2.6346872d+00/,
     8     e4/ 3.9399134d+05/, e3/-4.6004775d+04/, e2/ 2.2566998d+03/,
     9     e1/-6.8317697d+01/, e0/ 2.8224654d+00/
      data f0/-8.1807613d-02/, f1/-2.8358733d+00/, f2/ 1.4902469d+00/
c------------------------
      gauinv = 0
      p=p0-0.5d0
      p1=abs(p)
      if(p1.ge.pp1) goto 120
      p2=p**2
      gauinv=(((a3*p2+a2)*p2+a1)*p2+a0)*p
      return
  120 q=0.5d0-p1
      if(q.le.qq2) goto 140
      gauinv=(((b4*q+b3)*q+b2)*q+b1)*q+b0
      goto 200
  140 if(q.le.qq3) goto 150
      gauinv=(((c4*q+c3)*q+c2)*q+c1)*q+c0
      goto 200
  150 if(q.le.qq4) goto 160
      gauinv=(((d4*q+d3)*q+d2)*q+d1)*q+d0
      goto 200
  160 if(q.le.qq5) goto 170
      gauinv=(((e4*q+e3)*q+e2)*q+e1)*q+e0
      goto 200
  170 if(q.le.0d0) goto 900
      t=sqrt(-2d0*log(q))
      gauinv=t+f0+f1/(f2+t)
  200 if(p.lt.0d0) gauinv=-gauinv
      return
  900 write(6,910) p0
      gauinv = 0
  910 format(' (func.gauinv) invalid input argument ',1pd20.13)
      return
      end
