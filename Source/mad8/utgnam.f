      subroutine utgnam(lcmd, icat1, icat2, data)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Fetch command attributes of character type.                        *
* Input:                                                               *
*   LCMD(1)   (pointer) Command to be used.                            *
*   ICAT1     (integer) First attribute number.                        *
*   ICAT2     (integer) Last attribute number.                         *
* Output:                                                              *
*   DATA(*)   (char)    Vector to be filled.                           *
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
      integer ibias,icat,icat1,icat2,idest
      integer           lcmd(*)
      character*(mcnam) data(*)
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
      integer mbat,mbecls,mbelie,mbemap,mbfrm,mbln,mbnam,mbpr,mbsp
 
*---- Bias for bank descriptor words.
      parameter         (mbfrm  = 1, mbnam  = 2, mbln   = 3,
     +                   mbpr   = 4, mbsp   = 5, mbat   = 6)
      parameter         (mbemap = 1, mbelie = 2, mbecls = 3)
      integer mcf1,mcf2,mcsiz,mctyp,mcval
 
*---- Bias for command attribute groups.
      parameter         (mcf1 = 1, mctyp = 2, mcf2 = 3, mcval = 4,
     +                   mcsiz = mwnam + 3)
      integer mtcon,mtdef,mtflt,mtint,mtlin,mtlog,mtnam,mtrng,mtstr,
     +mtvar
 
*---- Attribute type codes.
      parameter         (mtnam =  1, mtint =  2, mtflt =  3,
     +                   mtdef =  4, mtlog =  5, mtstr =  6,
     +                   mtlin =  7, mtrng =  8, mtcon =  9,
     +                   mtvar = 10)
 
      ibias = mbat + (icat1 - 1) * mcsiz
      idest = 1
      do 90 icat = icat1, icat2
        if (iq(lcmd(1)+ibias+mctyp) .eq. 10 * mtnam + 1) then
          call uhtoc(q(lcmd(1)+ibias+mcval), mcwrd, data(idest), mcnam)
        endif
        ibias = ibias + mcsiz
        idest = idest + 1
   90 continue
 
      end
