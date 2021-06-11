      subroutine lmdif(fcn, m, n, x, fvec, epsfcn, diag, factor,
     +                 fjac, ldfjac, ipvt, qtf, wa1, wa2, wa3, wa4)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   The purpose of LMDIF is to minimize the sum of the squares of      *
*   M nonlinear functions in N variables by a modification of          *
*   the Levenberg-Marquardt algorithm. The user must provide a         *
*   subroutine which calculates the functions. The Jacobian is         *
*   then calculated by a forward-difference approximation.             *
*                                                                      *
*       FCN is the name of the user-supplied subroutine which          *
*         calculates the functions. FCN must be declared               *
*         in an external statement in the user calling                 *
*         program, and should be written as follows:                   *
*                                                                      *
*         SUBROUTINE FCN(M,N,X,FVEC,IFLAG)                             *
*         DIMENSION X(N),FVEC(M)                                       *
*         CALCULATE THE FUNCTIONS AT X AND                             *
*         RETURN THIS VECTOR IN FVEC.                                  *
*         RETURN                                                       *
*         END                                                          *
*                                                                      *
*         The value of IFLAG should be set to zero, unless there       *
*         is an error in evaluation of the function.                   *
*                                                                      *
*       M is a positive integer input variable set to the number       *
*         of functions.                                                *
*                                                                      *
*       N is a positive integer input variable set to the number       *
*         of variables. N must not exceed M.                           *
*                                                                      *
*       X is an array of length N. On input X must contain             *
*         an initial estimate of the solution vector. On output X      *
*         contains the final estimate of the solution vector.          *
*                                                                      *
*       FVEC is an output array of length M which contains             *
*         the functions evaluated at the output X.                     *
*                                                                      *
*       EPSFCN is an input variable used in determining a suitable     *
*         step length for the forward-difference approximation. This   *
*         approximation assumes that the relative errors in the        *
*         functions are of the order of EPSFCN. If EPSFCN is less      *
*         than the machine precision, it is assumed that the relative  *
*         errors in the functions are of the order of the machine      *
*         precision.                                                   *
*                                                                      *
*       DIAG is an array of length N. If MODE = 1 (see                 *
*         below), DIAG is internally set. If MODE = 2, DIAG            *
*         must contain positive entries that serve as                  *
*         multiplicative scale factors for the variables.              *
*                                                                      *
*       FACTOR is a positive input variable used in determining the    *
*         initial step bound. This bound is set to the product of      *
*         FACTOR and the Euclidean norm of DIAG*X if nonzero, or else  *
*         to FACTOR itself. In most cases FACTOR should lie in the     *
*         interval (.1,100.). 100. Is a generally recommended value.   *
*                                                                      *
*       FJAC is an output M by N array. The upper N by N submatrix     *
*         of FJAC contains an upper triangular matrix R with           *
*         diagonal elements of nonincreasing magnitude such that       *
*                                                                      *
*                T     T           T                                   *
*               P *(JAC *JAC)*P = R *R,                                *
*                                                                      *
*         where P is a permutation matrix and JAC is the final         *
*         calculated Jacobian. column J of P is column IPVT(J)         *
*         (see below) of the identity matrix. The lower trapezoidal    *
*         part of FJAC contains information generated during           *
*         the computation of R.                                        *
*                                                                      *
*       LDFJAC is a positive integer input variable not less than M    *
*         which specifies the leading dimension of the array FJAC.     *
*                                                                      *
*       IPVT is an integer output array of length N. IPVT              *
*         defines a permutation matrix P such that JAC*P = Q*r,        *
*         where JAC is the final calculated Jacobian, Q is             *
*         orthogonal (not stored), and R is upper triangular           *
*         with diagonal elements of nonincreasing magnitude.           *
*         column J of P is column IPVT(J) of the identity matrix.      *
*                                                                      *
*       QTF is an output array of length N which contains              *
*         the first N elements of the vector (Q transpose)*FVEC.       *
*                                                                      *
*       WA1, WA2, and WA3 are work arrays of length N.                 *
*                                                                      *
*       WA4 is a work array of length M.                               *
* Source:                                                              *
*   Argonne National Laboratory. MINPACK Project. March 1980.          *
*   Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More.             *
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
      integer i,iflag,info,iter,j,l,ldfjac,level,m,n
      double precision actred,delta,diag,dirder,epsfcn,epsil,factor,
     +fjac,fnorm,fnorm1,ftol,fvec,gnorm,gtol,one,p0001,p1,p25,p5,p75,
     +par,pnorm,prered,qtf,ratio,sum,temp,temp1,temp2,two,vmod,wa1,wa2,
     +wa3,wa4,x,xnorm,xtol,zero
      external          fcn
      dimension         x(n), fvec(m), diag(n), fjac(ldfjac,n), qtf(n),
     +                  wa1(n), wa2(n), wa3(n), wa4(m)
      integer           ipvt(n)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
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
      integer intmax
      double precision epsmch,fltmax,fltmin
 
*---- Parameters suitable for most computer systems.
      parameter         (epsmch = 1.0d-16)
      parameter         (fltmin = 1.0d-35)
      parameter         (fltmax = 1.0d+35)
      parameter         (intmax = 1000000000)
 
      parameter         (one    = 1.0d0)
      parameter         (two    = 2.0d0)
      parameter         (p1     = 0.1d0)
      parameter         (p5     = 0.5d0)
      parameter         (p25    = 0.25d0)
      parameter         (p75    = 0.75d0)
      parameter         (p0001  = 0.0001d0)
      parameter         (epsil  = 1.0d-8)
      parameter         (zero   = 0.0d0)
 
      info = 0
      ftol = epsfcn
      gtol = epsil
      xtol = epsil
      crout = 'LMDIF'
      cstat = 'start'
 
*---- Check the input parameters for errors.
      if (n .le. 0 .or. m .lt. n .or. ldfjac .lt. m
     +    .or. ftol .lt. zero .or. xtol .lt. zero .or. gtol .lt. zero
     +    .or. nfcnmx .le. 0 .or. factor .le. zero) go to 300
 
*---- Evaluate the function at the starting point and find its norm.
      call fcn(m,n,x,fvec,iflag)
      nfcn = nfcn + 1
      if (iflag .ne. 0) then
        msg(1) = 'Matching stopped, start point seems to be unstable,'
        msg(2) = '(Maybe a "LINE = ..." condition is unstable).'
        call aawarn('LMDIF', 2, msg)
        info = - 1
        go to 300
      endif
      fnorm = vmod(m, fvec)
      fmin = fnorm**2
      edm = fmin
 
*---- Quit, when initial value is already OK.
      if (fmin .le. ftol) then
        info = 4
        go to 300
      endif
      if (ilevel .ge. 1) call mtprnt(n, x)
 
*---- Initialize Levenberg-Marquardt parameter and iteration count
      par = zero
      iter = 1
 
*---- Beginning of the outer loop.
   30 continue
 
*---- Calculate the Jacobian matrix.
         call fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,xtol,wa4)
         nfcn = nfcn + n
         if (iflag .ne. 0) then
           info = - 1
           go to 300
         endif
 
*---- Compute the QR factorization of the Jacobian.
         call qrfac(m,n,fjac,ldfjac,.true.,ipvt,n,wa1,wa2,wa3)
 
*---- On the first iteration scale according to the norms
*     of the columns of the initial Jacobian.
*     Calculate the norm of the scaled X
*     and initialize the step bound delta.
         if (iter .eq. 1) then
            do 50 j = 1, n
               diag(j) = wa2(j)
               if (wa2(j) .eq. zero) diag(j) = one
               wa3(j) = diag(j)*x(j)
   50       continue
            xnorm = vmod(n, wa3)
            delta = factor*xnorm
            if (delta .eq. zero) delta = factor
         endif
 
*---- Form (Q transpose)*FVEC and store the first N components in QTF.
         do 90 i = 1, m
            wa4(i) = fvec(i)
   90    continue
         do 130 j = 1, n
            if (fjac(j,j) .ne. zero) then
               sum = zero
               do 100 i = j, m
                  sum = sum + fjac(i,j)*wa4(i)
  100          continue
               temp = -sum/fjac(j,j)
               do 110 i = j, m
                  wa4(i) = wa4(i) + fjac(i,j)*temp
  110          continue
            endif
            fjac(j,j) = wa1(j)
            qtf(j) = wa4(j)
  130    continue
 
*---- Compute the norm of the scaled gradient.
         gnorm = zero
         if (fnorm .ne. zero) then
            do 160 j = 1, n
               l = ipvt(j)
               if (wa2(l) .ne. zero) then
                  sum = zero
                  do 140 i = 1, j
                     sum = sum + fjac(i,j)*(qtf(i)/fnorm)
  140             continue
                  gnorm = max(gnorm,abs(sum/wa2(l)))
               endif
  160       continue
         endif
 
*---- Test for convergence of the gradient norm.
         if (gnorm .le. gtol) info = 4
         if (info .ne. 0) go to 300
 
*---- Rescale if necessary.
         do 180 j = 1, n
            diag(j) = max(diag(j),wa2(j))
  180    continue
 
*---- Beginning of the inner loop.
  200    continue
 
*---- Determine the Levenberg-Marquardt parameter.
            call lmpar(n,fjac,ldfjac,ipvt,diag,qtf,delta,par,wa1,wa2,
     +                 wa3,wa4)
 
*---- Store the direction P and X + P. Calculate the norm of P.
            do 210 j = 1, n
               wa1(j) = -wa1(j)
               wa2(j) = x(j) + wa1(j)
               wa3(j) = diag(j)*wa1(j)
  210       continue
            pnorm = vmod(n, wa3)
 
*---- On the first iteration, adjust the initial step bound.
            if (iter .eq. 1) delta = min(delta,pnorm)
 
*---- Evaluate the function at X + P and calculate its norm.
            call fcn(m,n,wa2,wa4,iflag)
            nfcn = nfcn + 1
            if (iflag .ne. 0) then
              fnorm1 = two * fnorm
            else
              fnorm1 = vmod(m, wa4)
            endif
 
*---- Compute the scaled actual reduction.
            actred = -one
            if (p1*fnorm1 .lt. fnorm) actred = one - (fnorm1/fnorm)**2
 
*---- Compute the scaled predicted reduction and
*     the scaled directional derivative.
            do 230 j = 1, n
               wa3(j) = zero
               l = ipvt(j)
               temp = wa1(l)
               do 220 i = 1, j
                  wa3(i) = wa3(i) + fjac(i,j)*temp
  220          continue
  230       continue
            temp1 = vmod(n, wa3)/fnorm
            temp2 = (sqrt(par)*pnorm)/fnorm
            prered = temp1**2 + temp2**2/p5
            dirder = -(temp1**2 + temp2**2)
 
*---- Compute the ratio of the actual to the predicted reduction.
            ratio = zero
            if (prered .ne. zero) ratio = actred/prered
 
*---- Update the step bound.
            if (ratio .le. p25) then
               if (actred .ge. zero) temp = p5
               if (actred .lt. zero)
     +            temp = p5*dirder/(dirder + p5*actred)
               if (p1*fnorm1 .ge. fnorm .or. temp .lt. p1) temp = p1
               delta = temp*min(delta,pnorm/p1)
               par = par/temp
            else if (par .eq. zero .or. ratio .ge. p75) then
               delta = pnorm/p5
               par = p5*par
            endif
 
*---- Test for successful iteration.
            if (ratio .ge. p0001) then
 
*---- Successful iteration. Update X, FVEC, and their norms.
               do 270 j = 1, n
                  x(j) = wa2(j)
                  wa2(j) = diag(j)*x(j)
  270          continue
               do 280 i = 1, m
                  fvec(i) = wa4(i)
  280          continue
               xnorm = vmod(n, wa2)
               fnorm = fnorm1
               iter = iter + 1
 
*---- If requested, print iterates.
               fmin = fnorm**2
               edm = gnorm * fmin
               cstat = 'progress'
               level = 3
               if (mod(iter,10) .eq. 0) level = 2
               if (ilevel .ge. level) call mtprnt(n, x)
            endif
 
*---- Tests for convergence.
            if (abs(actred) .le. ftol .and. prered .le. ftol
     +          .and. p5*ratio .le. one) info = 1
            if (delta .le. xtol*xnorm) info = 2
            if (abs(actred) .le. ftol .and. prered .le. ftol
     +          .and. p5*ratio .le. one .and. info .eq. 2) info = 3
            if (fmin .le. ftol) info = 4
            if (info .ne. 0) go to 300
 
*---- Tests for termination and stringent tolerances.
            if (nfcn .ge. nfcnmx) info = 5
            if (abs(actred) .le. epsmch .and. prered .le. epsmch
     +          .and. p5*ratio .le. one) info = 6
            if (delta .le. epsmch*xnorm) info = 7
            if (gnorm .le. epsmch) info = 8
            if (info .ne. 0) go to 300
 
*---- End of the inner loop. Repeat if iteration unsuccessful.
          if (ratio .lt. p0001) go to 200
 
*---- End of the outer loop.
       go to 30
 
*---- Termination, either normal or user imposed.
  300 continue
      call mtputi(n, x)
      if (info .lt. 0) then
        cstat = 'unstable'
      else if (info .eq. 0) then
        cstat = 'error'
      else if (info .lt. 5) then
        cstat = 'converged'
      else if (info .eq. 5) then
        cstat = 'call limit'
      else
        cstat = 'accuracy limit'
      endif
      if (ilevel .ge. 1) call mtprnt(n, x)
 
      end
