      subroutine plgetv(string, i1s, i2, ii, rr, itype, klch)
      implicit none
*-----------------------------------------------------------------------
*
*--- returns a numeric constant, and its type. Constant must start on
*    STRING(I1:)
*--- input
*    STRING         string
*    I1S            start of scan
*    I2             end of scan
*--- output
*    II             integer value if ITYPE = 1
*    RR             double precision value if ITYPE = 2
*    ITYPE          1 integer, 2 floating, 3 exp. floating
*    KLCH           last ch. of var.
*
*-----------------------------------------------------------------------
      integer mcfil,mcnam,mcrng,mcstr,mcwrd,mreal,mwflt,mwnam,
     +mbbparam
*---- Double precision version.
      parameter         (mwflt = 2, mcwrd = 4, mreal = 4)
      parameter         (mcnam = 16, mwnam = mcnam / mcwrd)
      parameter         (mcfil = 80, mcrng = 40, mcstr = 512)
      parameter         (mbbparam = 26)
      integer mttact
      parameter (mttact = 100)
      integer i,i1,i1s,i2,ig,ii,index,itype,iv,ivf,k,klch
      double precision rr
      character *(*)  string
      character*1 stemp, slast
      character * 10       numeri
      integer ip(3), ic(3)
 
      data numeri / '0123456789' /
 
*--- get sign
      ig = index('-+', string(i1s:i1s))
      i1 = i1s + min(ig, 1)
      ig = (-1)**ig
      do 10 i = 1, 3
        ip(i) = 0
        ic(i) = 0
   10 continue
      ivf = 1
      itype = 1
      klch = i1
      stemp = string(i1:i1)
      iv = index(numeri, stemp)
      if (stemp .eq. '.')  then
        itype    = 2
      elseif (iv .eq. 0)  then
        klch = 0
        goto 999
      endif
      ip(1) = max(0, iv - 1)
      slast = stemp
      do 20 i = i1 + 1, i2
        stemp = string(i:i)
        k = index(numeri, stemp)
        if (k .gt. 0) then
          ip(itype) = 10 * ip(itype) + k - 1
          ic(itype) = ic(itype) + 1
        elseif(stemp .eq. '.') then
          itype = 2
        elseif(stemp .eq. 'D' .or. stemp .eq. 'd'
     +  .or. stemp .eq. 'E' .or. stemp .eq. 'e') then
          itype = 3
        elseif(slast .eq. 'D' .or. slast .eq. 'd'
     +  .or. slast .eq. 'E' .or. slast .eq. 'e') then
          if(stemp .eq. '+') then
            continue
          elseif (stemp .eq. '-') then
            ivf = -1
          else
            goto 30
          endif
        else
          goto 30
        endif
        klch=i
        slast=stemp
   20 continue
   30 continue
      if (itype .eq. 1) then
        ii = ig * ip(1)
      else
        rr = ig * (ip(1) + ip(2) / 10.d0**ic(2)) * 10.d0**(ivf * ip(3))
      endif
  999 end
