      subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   This subroutine computes a forward-difference approximation        *
*   to the M by N Jacobian matrix associated with a specified          *
*   problem of M functions in N variables.                             *
* Input:                                                               *
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
*       X is an input array of length N.                               *
*                                                                      *
*       FVEC is an input array of length M which must contain the      *
*         functions evaluated at X.                                    *
*                                                                      *
*       FJAC is an output M by N array which contains the              *
*         approximation to the Jacobian matrix evaluated at X.         *
*                                                                      *
*       LDFJAC is a positive integer input variable not less than M    *
*         which specifies the leading dimension of the array FJAC.     *
*                                                                      *
*       IFLAG is an integer variable which tells the calling program   *
*         wether the approximation is valid.                           *
*                                                                      *
*       EPSFCN is an input variable used in determining a suitable     *
*         step length for the forward-difference approximation. This   *
*         approximation assumes that the relative errors in the        *
*         functions are of the order of EPSFCN. If EPSFCN is less      *
*         than the machine precision, it is assumed that the relative  *
*         errors in the functions are of the order of the machine      *
*         precision.                                                   *
*                                                                      *
*       WA is a work array of length M.                                *
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
      integer i,iflag,j,ldfjac,m,n
      double precision eps,epsfcn,fjac,fvec,h,temp,wa,x,zero
      external          fcn
      dimension         x(n), fvec(m), fjac(ldfjac,n), wa(m)
      integer intmax
      double precision epsmch,fltmax,fltmin
 
*---- Parameters suitable for most computer systems.
      parameter         (epsmch = 1.0d-16)
      parameter         (fltmin = 1.0d-35)
      parameter         (fltmax = 1.0d+35)
      parameter         (intmax = 1000000000)
      parameter         (zero   = 0.0d0)
 
      eps = sqrt(max(epsfcn,epsmch))
      iflag = 0
 
      do 20 j = 1, n
         temp = x(j)
         h = eps*abs(temp)
         if (h .eq. zero) h = eps
         x(j) = temp + h
         call fcn(m,n,x,wa,iflag)
         x(j) = temp
         if (iflag .ne. 0) go to 30
         do 10 i = 1, m
            fjac(i,j) = (wa(i) - fvec(i))/h
   10    continue
   20 continue
   30 continue
 
      end
