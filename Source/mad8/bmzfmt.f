      subroutine bmzfmt(sfin, ii1, ii2, vec, sfout)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   make format (1P,E... or 0P,G...) depending on values               *
* Input:                                                               *
*   SFIN        (char)  format with "$" for missing descriptors        *
*   II1         (int)   n1 in En1.n2                                   *
*   II2         (int)   n2 in En1.n2                                   *
*   VEC         (real)  vector of values to be printed, as many as $'s *
* Output:                                                              *
*   SFOUT       (char)  final format                                   *
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
      integer i1,i2,i3,ii1,ii2,index,ip1,ip2,k,l,len,n
      double precision vec
      dimension        vec(*)
      character*(*)     sfin, sfout
      character         sl*8, slc*8, temp*40
 
      l   = len(sfin)
      n   = 0
      ip1 = 0
      ip2 = 0
      i1  = ii1
      i2  = min(ii2, i1 - 6)
      if (i2 .eq. i1 - 6)  then
        i3 = i2
      else
         i3 = i2 + 1
      endif
      sl  = '(G  .  )'
      write (sl(3:4),' (I2)')  i1
      write (sl(6:7), '(I2)')  i3
 
   10 continue
      k = index(sfin(ip1+1:),'$')
      if (k .gt. 0)  then
        sfout(ip2+1:) = sfin(ip1+1:)
        ip2 = ip2 + k -1
        n   = n + 1
        temp = ' '
        slc = sl
        write (temp, sl)  vec(n)
        if (index(temp, 'E') .eq. 0)  then
          sfout(ip2+1:) = '0P,' // slc(2:7)
        else
          write (slc(6:7), '(I2)')  i2
          sfout(ip2+1:) = '1P,E' // slc(3:7)
        endif
        ip2 = ip2 + 9
        ip1 = ip1 + k
        if (ip1 .lt. l)  goto 10
      endif
      sfout(ip2+1:) = sfin(ip1+1:)
 
      end
