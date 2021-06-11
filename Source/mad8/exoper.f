      subroutine exoper(iopr)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Perform one operation in parameter evaluation.                     *
* Input:                                                               *
*   IOPR     (integer)  Operation type code.                           *
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
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
      integer isopr,isval,level,maxstk
      double precision rsval
 
*---- Stack for expression decoding and evaluation.
      parameter         (maxstk = 100)
      common /exstki/   level, isopr(maxstk), isval(maxstk)
      common /exstkr/   rsval(maxstk)
      save              /exstki/, /exstkr/
      integer iopr,iy,lbadtp,nbadtp,nint
      double precision dum1,dum2,frndm,user0,user1,user2,x,y
 
      data   lbadtp, nbadtp      / 50, 0 /
 
*---- Select operation.
      go to ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     +       110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     +       210, 220, 230, 240, 250, 260), iopr
 
*---- Invalid operation.
      if (nbadtp .le. lbadtp) then
        call aafail('EXOPER', 1, 'Unknown operation code.')
        nbadtp = nbadtp + 1
        go to 9999
      else
        call zfatam(' Too many bad operations in EXOPER.')
      endif
 
*==== Operators.
*---- Addition.
   10 continue
        level = level - 1
        rsval(level) = rsval(level) + rsval(level+1)
      go to 9999
 
*---- Subtraction.
   20 continue
        level = level - 1
        rsval(level) = rsval(level) - rsval(level+1)
      go to 9999
 
*---- Multiplication.
   30 continue
        level = level - 1
        rsval(level) = rsval(level) * rsval(level+1)
      go to 9999
 
*---- Division.
   40 continue
        level = level - 1
        if (rsval(level+1) .eq. 0.) then
          write (msg, 920) 'Division by zero'
          call aawarn('EXOPER', 1, msg)
          rsval(level) = 0.
        else
          rsval(level) = rsval(level) / rsval(level+1)
        endif
      go to 9999
 
*---- Power.
   50 continue
        level = level - 1
        x = rsval(level)
        y = rsval(level+1)
        if (y .eq. 0.) then
          rsval(level) = 1.
        else if (x .gt. 0.) then
          rsval(level) = exp(log(x) * y)
        else if (x .eq. 0.) then
          if (y .ge. 0.) then
            rsval(level) = 0.
          else
            write (msg, 920) 'Negative power of zero'
            call aawarn('EXOPER', 1, msg)
            rsval(level) = 0.
          endif
        else
          iy = nint(y)
          if (abs(y-iy) .lt. 1.e-15) then
            rsval(level) = x**iy
          else
            write (msg, 920) 'Fractional power of negative number'
            call aawarn('EXOPER', 1, msg)
            rsval(level) = 0.
          endif
        endif
      go to 9999
 
*---- Unary plus.
   60 continue
      go to 9999
 
*---- Unary minus.
   70 continue
        rsval(level) = - rsval(level)
      go to 9999
 
*==== Mathematical functions.
*---- Square root.
   80 continue
        if (rsval(level) .lt. 0.) then
          write (msg, 920) 'Square root of negative number'
          call aawarn('EXOPER', 1, msg)
          rsval(level) = 0.
        else
          rsval(level) = sqrt(rsval(level))
        endif
      go to 9999
 
*---- Logarithm.
   90 continue
        if (rsval(level) .le. 0.) then
          write (msg, 920) 'Logarithm of non-positive number'
          call aawarn('EXOPER', 1, msg)
          rsval(level) = 0.
        else
          rsval(level) = log(rsval(level))
        endif
      go to 9999
 
*---- Exponential.
  100 continue
        rsval(level) = exp(rsval(level))
      go to 9999
 
*---- Sine.
  110 continue
        rsval(level) = sin(rsval(level))
      go to 9999
 
*---- Cosine.
  120 continue
        rsval(level) = cos(rsval(level))
      go to 9999
 
*---- Absolute value.
  130 continue
        rsval(level) = abs(rsval(level))
      go to 9999
 
*---- Tangent.
  140 continue
        rsval(level) = tan(rsval(level))
      go to 9999
 
*---- Arc sine.
  150 continue
        if (abs(rsval(level)) .gt. 1.0) then
          write (msg, 920) 'Arcsine(x) with |x| > 1'
          call aawarn('EXOPER', 1, msg)
          rsval(level) = 0.
        else
          rsval(level) = asin(rsval(level))
        endif
      go to 9999
 
*---- Arc cosine.
  160 continue
        if (abs(rsval(level)) .gt. 1.0) then
          write (msg, 920) 'Arccosine(x) with |X| > 1'
          call aawarn('EXOPER', 1, msg)
          rsval(level) = 0.
        else
          rsval(level) = acos(rsval(level))
        endif
      go to 9999
 
*---- Arc tangent (one argument).
  170 continue
        rsval(level) = atan(rsval(level))
      go to 9999
 
*---- Arc tangent (2 arguments).
  180 continue
        level = level - 1
        if (rsval(level) .eq. 0.  .and.  rsval(level+1) .eq. 0.) then
          write (msg, 920) 'Arctangent(0/0)'
          call aawarn('EXOPER', 1, msg)
          rsval(level) = 0.
        else
          rsval(level) = atan2(rsval(level), rsval(level+1))
        endif
      go to 9999
 
*---- Maximum.
  190 continue
        level = level - 1
        rsval(level) = max(rsval(level), rsval(level+1))
      go to 9999
 
*---- Minimum.
  200 continue
        level = level - 1
        rsval(level) = min(rsval(level), rsval(level+1))
      go to 9999
 
*==== Random generators.
*---- Random number, uniformly distributed in 0..1.
  210 continue
        level = level + 1
        rsval(level) = frndm()
      go to 9999
 
*---- Random number, Gaussian distribution with unit sigma.
  220 continue
        level = level + 1
        call grndm(dum1, dum2)
        rsval(level) = dum1
      go to 9999
 
*---- Random number, user defined, no arguments.
  230 continue
        level = level + 1
        rsval(level) = user0()
      go to 9999
 
*---- Random number, Gaussian with unit sigma,
*     Truncated at 'argument'.
  240 continue
  245   call grndm(dum1, dum2)
        if (abs(dum1) .le. abs(rsval(level))) then
          rsval(level) = dum1
        else
          go to 245
        endif
      go to 9999
 
*---- Random number, user defined, one argument.
  250 continue
        rsval(level) = user1(rsval(level))
      go to 9999
 
*---- Random number, user defined, two arguments.
  260 continue
        level = level - 1
        rsval(level) = user2(rsval(level), rsval(level+1))
      go to 9999
 
  920 format(a,' encountered --- Result set to zero.')
 
 9999 end
