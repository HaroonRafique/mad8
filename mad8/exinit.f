      subroutine exinit
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Initialize expression handler.                                     *
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
      integer lexbnk,lexexp,lexpar,lexsub,lexvar
 
*---- Local links for expression handler.
      common /exlink/   lexbnk, lexexp, lexpar, lexsub, lexvar
      save              /exlink/
      integer ifun,ipre,narg,nfun
 
*---- Function definitions for expressions.
      parameter         (nfun = 26)
      common /funnam/   funnam(nfun)
      common /fundat/   ipre(-8:nfun), ifun(nfun), narg(nfun)
      save              /funnam/, /fundat/
      character*(mcnam) funnam
      integer iexflg
 
      data iexflg       / 0 /
 
*---- Local links.
      if (iexflg .eq. 0) then
        call mzlink(0, '/EXLINK/', lexbnk, lexbnk, lexvar)
        iexflg = 1
      endif
 
*---- Precedences for load and store operations.
      ipre  (-8) = 4
      ipre  (-7) = 4
      ipre  (-6) = 4
      ipre  (-5) = 4
      ipre  (-4) = 4
      ipre  (-3) = 4
      ipre  (-2) = 4
      ipre  (-1) = 4
      ipre  ( 0) = 0
 
*---- Binary operators.
      funnam( 1) = '+'
      ipre  ( 1) = 1
      ifun  ( 1) = 1
      narg  ( 1) = 2
      funnam( 2) = '-'
      ipre  ( 2) = 1
      ifun  ( 2) = 1
      narg  ( 2) = 2
      funnam( 3) = '*'
      ipre  ( 3) = 2
      ifun  ( 3) = 1
      narg  ( 3) = 2
      funnam( 4) = '/'
      ipre  ( 4) = 2
      ifun  ( 4) = 1
      narg  ( 4) = 2
      funnam( 5) = '^'
      ipre  ( 5) = 3
      ifun  ( 5) = 1
      narg  ( 5) = 2
 
*---- Unary operators.
      funnam( 6) = '+'
      ipre  ( 6) = 2
      ifun  ( 6) = 1
      narg  ( 6) = 1
      funnam( 7) = '-'
      ipre  ( 7) = 2
      ifun  ( 7) = 1
      narg  ( 7) = 1
 
*---- Mathematical functions.
      funnam( 8) = 'SQRT'
      ipre  ( 8) = 4
      ifun  ( 8) = 2
      narg  ( 8) = 1
      funnam( 9) = 'LOG'
      ipre  ( 9) = 4
      ifun  ( 9) = 2
      narg  ( 9) = 1
      funnam(10) = 'EXP'
      ipre  (10) = 4
      ifun  (10) = 2
      narg  (10) = 1
      funnam(11) = 'SIN'
      ipre  (11) = 4
      ifun  (11) = 2
      narg  (11) = 1
      funnam(12) = 'COS'
      ipre  (12) = 4
      ifun  (12) = 2
      narg  (12) = 1
      funnam(13) = 'ABS'
      ipre  (13) = 4
      ifun  (13) = 2
      narg  (13) = 1
      funnam(14) = 'TAN'
      ipre  (14) = 4
      ifun  (14) = 2
      narg  (14) = 1
      funnam(15) = 'ASIN'
      ipre  (15) = 4
      ifun  (15) = 2
      narg  (15) = 1
      funnam(16) = 'ACOS'
      ipre  (16) = 4
      ifun  (16) = 2
      narg  (16) = 1
      funnam(17) = 'ATAN'
      ipre  (17) = 4
      ifun  (17) = 2
      narg  (17) = 1
      funnam(18) = 'ATAN2'
      ipre  (18) = 4
      ifun  (18) = 2
      narg  (18) = 2
      funnam(19) = 'MAX'
      ipre  (19) = 4
      ifun  (19) = 2
      narg  (19) = 2
      funnam(20) = 'MIN'
      ipre  (20) = 4
      ifun  (20) = 2
      narg  (20) = 2
 
*---- Random generators.
      funnam(21) = 'RANF'
      ipre  (21) = 4
      ifun  (21) = 3
      narg  (21) = 0
      funnam(22) = 'GAUSS'
      ipre  (22) = 4
      ifun  (22) = 3
      narg  (22) = 0
      funnam(23) = 'USER0'
      ipre  (23) = 4
      ifun  (23) = 3
      narg  (23) = 0
      funnam(24) = 'TGAUSS'
      ipre  (24) = 4
      ifun  (24) = 3
      narg  (24) = 1
      funnam(25) = 'USER1'
      ipre  (25) = 4
      ifun  (25) = 3
      narg  (25) = 1
      funnam(26) = 'USER2'
      ipre  (26) = 4
      ifun  (26) = 3
      narg  (26) = 2
 
      end
