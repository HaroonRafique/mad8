      subroutine tbform(iform, ileng)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Decode a single table descriptor format.                           *
* Output:                                                              *
*   IFORM     (integer) Zebra format code, with extensions:            *
*                       11 = character, 12 = octal, 13 = hexadecimal.  *
*   ILENG     (integer) Field length expected.                         *
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
      integer jtok,lentok,lintok,ntok
 
*---- Statement input buffer.
      parameter         (lentok = 150000)
      common /stbufc/   token(lentok)
      common /stbufi/   lintok, jtok, ntok
      save              /stbufc/, /stbufi/
      character*1       token
      integer idigit,iform,ileng,index,j
 
*---- "C" like format control codes, and their MAD equivalent.
      character*1       code(12)
      integer           ifcod(12)
      logical           long
 
      data code         / 'c', 'd', 'e', 'E', 'f', 'g',
     +                    'i', 'o', 's', 'u', 'x', 'X' /
      data ifcod        / 11,   2,   3,   3,   3,   3,
     +                     2,  12,   5,   1,  13,  13  /
 
      iform = 0
      ileng = 0
      long = .false.
 
*---- Skip leading blanks.
   10 if (jtok .le. ntok) then
        if (token(jtok) .eq. ' ') then
          jtok = jtok + 1
          go to 10
        endif
 
*---- Check leading percent sign.
        if (token(jtok) .ne. '%') then
          call rdfail('TBFORM', 1, 'Format does not begin with "%".')
        else
          jtok = jtok + 1
 
*---- Decode field length.
   20     continue
          idigit = index('0123456789',token(jtok)) - 1
          if (idigit .ge. 0) then
            ileng = 10 * ileng + idigit
            jtok = jtok + 1
            go to 20
          endif
          if (token(jtok) .eq. 'l'  .or.  token(jtok) .eq. 'h') then
            long = token(jtok) .eq. 'l'
            jtok = jtok + 1
          endif
 
*---- Decode format character.
          do 80 j = 1, 12
            if (token(jtok) .eq. code(j)) then
              iform = ifcod(j)
              if (long  .and.  iform .eq. 3) iform = 4
              go to 90
            endif
   80     continue
          call rdfail('TBFORM', 1, 'Unknown format code.')
   90     continue
          jtok = jtok + 1
        endif
      endif
 
 9999 end
