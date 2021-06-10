      subroutine lmexpo(op, iopord, arg, iarord, out, imxord)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Apply the exponential of a Lie operator to an argument.            *
*   The series is truncated at a given order.                          *
*   A second-order operator is not allowed.                            *
* Source:     MARYLIE, version 5.1 (routine EXPOP).                    *
* Algorithm:  Liam Healy.                                              *
* Input:                                                               *
*   OP(*)     (poly)    The homogeneous polynomial for the operator.   *
*   IOPORD    (integer) The order of OP.                               *
*   ARG(*)    (poly)    The polynomial to be operated upon.            *
*   IARORD    (integer) The order of ARG:                              *
*                       IARORD > 0: Only order IARORD.                 *
*                       IARORD < 0: Orders from 1 to - IARORD.         *
*   IMXORD    (integer) The order at which to truncate the series.     *
* Output:                                                              *
*   OUT(*)    (poly)    The result of OUT = exp(:OP:) ARG.             *
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
      integer iacc,iarmax,iarmin,iarord,imxord,iopord,iornew,iorold,
     +isave,jord,jpower
      double precision arg,factor,op,out
      dimension         out(*), op(*), arg(*)
      integer memlen,memmin
      parameter         (memmin =  1600 000)
      parameter         (memlen = 16000 000)
      integer llump,lq,lroot
      double precision dq
 
*---- Memory pool definition.
      common //         fence, lq(mwflt*memlen)
      integer           iq(mwflt*memlen)
      real              fence(2), q(mwflt*memlen)
      dimension         dq(memlen)
      equivalence       (iq(1), q(1), dq(1), lq(9))
      equivalence       (lroot, lq(1)), (llump, lq(2))
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
      integer iwork,nwork
 
*---- Working space stack pointers (all in double words).
      common /wstack/   iwork, nwork
      save              /wstack/
 
*---- Allocate working space.
      isave = iwork
      iacc  = iwork
      iwork = iacc + itop6(imxord)
      if (iwork .gt. nwork) then
        call mzwork(0, dq(1), dq(iwork+1), 2)
        nwork = iwork
      endif
 
*---- Set limits for argument orders.
      if (iarord .lt. 0) then
        iarmin = 1
        iarmax = - iarord
      else
        iarmin = iarord
        iarmax = iarord
      endif
 
*---- Clear result array.
      call pa6clr(out, -imxord)
 
*---- Identity operator for all orders in ARG.
      call pa6cpy(arg, iarord, out)
 
*---- For all orders in ARG do...
      do 90 jord = iarmin, iarmax
 
*---- Copy current order of ARG to working array.
        call pa6cpy(arg, jord, dq(iacc+1))
        iorold = jord
        factor = 1.0
 
*---- Accumulate powers of Lie operators.
        do 80 jpower = 1, imxord
          iornew = iopord + iorold - 2
          if (iornew .le. 0  .or.  iornew .gt. imxord) go to 90
          call pa6brk(op, iopord, dq(iacc+1), iorold, dq(iacc+1))
          factor = factor / jpower
          call pa6sum(factor, dq(iacc+1), iornew, out)
          iorold = iornew
   80   continue
   90 continue
 
*---- Drop working storage.
      iwork = isave
 
      end
