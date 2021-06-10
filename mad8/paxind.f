      subroutine paxind(jexp, nvar, index)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Find Giorgelli index for a given monomial from known exponents.    *
*   Valid for polynomials in up to 6 variables, and orders up to 12.   *
* Algorithm by: David Douglas and Liam Healy.                          *
* Input:                                                               *
*   JEXP(NVAR)(integer) Set of exponents for monomial.                 *
*   NVAR      (integer) Number of variables in polynomials.            *
* Output:                                                              *
*   INDEX     (integer) Monomial index.                                *
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
      integer index,isum,jvar,nvar
      integer           jexp(nvar)
 
*---- Table of binomial coefficients.
      integer           ibin(0:12,5)
      data              ibin
     + / 0, 1, 3,  6, 10,  15,  21,  28,   36,   45,   55,   66,    78,
     +   0, 1, 4, 10, 20,  35,  56,  84,  120,  165,  220,  286,   364,
     +   0, 1, 5, 15, 35,  70, 126, 210,  330,  495,  715, 1001,  1365,
     +   0, 1, 6, 21, 56, 126, 252, 462,  792, 1287, 2002, 3003,  4368,
     +   0, 1, 7, 28, 84, 210, 462, 924, 1716, 3003, 5005, 8008, 12376 /
 
      isum  = jexp(nvar)
      index = isum
      do 10 jvar = nvar-1, 1, -1
        isum  = isum  + jexp(jvar)
        index = index + ibin(isum,nvar-jvar)
   10 continue
 
      end
