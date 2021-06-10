      subroutine tt_wake(val, irec, nrec, array)
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Binary search for wakefield routine TTWAKE                         *
* Input:                                                               *
*   VAL           (real)                                               *
*   NREC          (integer)                                            *
*   ARRAY(0:NREC) (real)                                               *
* Output:                                                              *
*   IREC          (integer)                                            *
*----------------------------------------------------------------------*
* Created:  ??-???-????, T. Raubenheimer (SLAC)                        *
*----------------------------------------------------------------------*
 
      implicit none
      integer irec, nrec
      double precision val, array(0:nrec)
      integer i, j, k
 
*---- Search.
      i = 0
      j = nrec
   10 if (j - i .le. 4) goto 100
      k = int((j + i + 1)/2)
      if (array(k) .gt. val) then
        j = k
      else
        i = k
      endif
      goto 10
 
  100 continue
      if (array(i) .gt. val) then
        irec = i
      elseif (array(i+1) .gt. val) then
        irec = i+1
      elseif (array(i+2) .gt. val) then
        irec = i+2
      elseif (array(i+3) .gt. val) then
        irec = i+3
      else
        irec = i+4
      endif
 
      end
