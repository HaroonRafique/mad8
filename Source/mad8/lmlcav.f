      subroutine lmlcav(nord, el, rfv, rfl, rff, fp, fm)
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Lie-algebraic map for a travelling wave RF cavity.                 *
* Input:                                                               *
*   NORD      (integer) Order desired.                                 *
*   EL        (real)    Cavity length.                                 *
*   RFV       (real)    Cavity voltage.                                *
*   RFL       (real)    Cavity phase lag.                              *
*   RFF       (real)    Cavity frequency.                              *
* Output:                                                              *
*   FP, FM    (map)     Cavity map.                                    *
*----------------------------------------------------------------------*
* Created:  28-DEC-1998, T. Raubenheimer (SLAC)                        *
*   Routine copied and modified from LMRF ... non-operational          *
*----------------------------------------------------------------------*
      implicit none
      integer nord
      double precision el, rfv, rfl, rff
      double precision fp(*), fm(6,6)
 
*---- Buffer for error and warning messages.
      common /message/  msg(8)
      save   /message/
      character*120     msg
 
*---- Issue an error message
      msg(1) = 'Cannot use LM routines (LIE) with LCAVITY'
      call aafail ('lmlcav', 1, msg)
 
      end
