      subroutine lmsprt(iunit, nord, fp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print the polynomials of a map in the static resonance basis.      *
* Source:     MARYLIE, version 3.0 (routine PSRMAP).                   *
* Input:                                                               *
*   IUNIT     (integer) Logical output unit.                           *
*   NORD      (integer) Order of the map.                              *
*   FP(*)     (poly)    Static resonance basis coefficients.           *
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
      integer i,iunit,j,jl,jord,nl,nord
      double precision fp,tol
      dimension         fp(*)
      integer ibot6,itop6,larrp,larrq,lexp6,lind61,lind62,lprd6
 
*---- Bookkeeping tables for polynomials of six variables.
      common /pa6lnk/   ibot6(-6:6), itop6(-6:6), lexp6(6),
     +                  lind61, lind62, larrq, larrp, lprd6
      save              /pa6lnk/
 
      parameter         (tol = 1.0d-8)
      integer           il(209)
      character*6       sln(209)
 
      data (sln(i), i = 1, 25)
     +   / 'R00001', 'R10000', 'I10000', 'R00100', 'I00100',
     +     '000010', 'R11000', 'R00110', 'R00002', 'R10001',
     +     'I10001', 'R00101', 'I00101', 'R20000', 'I20000',
     +     'R00200', 'I00200', 'R10100', 'I10100', 'R10010',
     +     'I10010', '100010', '010010', '001010', '000110' /
      data (sln(i), i = 26, 50)
     +   / '000020', '000011', 'R11001', 'R00111', 'R00003',
     +     'R10002', 'I10002', 'R00102', 'I00102', 'R20001',
     +     'I20001', 'R00201', 'I00201', 'R10101', 'I10101',
     +     'R10011', 'I10011', 'R21000', 'I21000', 'R00210',
     +     'I00210', 'R10110', 'I10110', 'R11100', 'I11100' /
      data (sln(i), i = 51, 75)
     +   / 'R30000', 'I30000', 'R00300', 'I00300', 'R20100',
     +     'I20100', 'R10200', 'I10200', 'R20010', 'I20010',
     +     'R01200', 'I01200', '200010', '110010', '101010',
     +     '100110', '100020', '100011', '020010', '011010',
     +     '010110', '010020', '010011', '002010', '001110' /
      data (sln(i), i = 76, 100)
     +   / '001020', '001011', '000210', '000120', '000111',
     +     '000030', '000021', '000012', 'R11002', 'R00112',
     +     'R00004', 'R22000', 'R00220', 'R11110', 'R10003',
     +     'I10003', 'R00103', 'I00103', 'R20002', 'I20002',
     +     'R00202', 'I00202', 'R10102', 'I10102', 'R10012' /
      data (sln(i), i = 101, 125)
     +   / 'I10012', 'R21001', 'I21001', 'R00211', 'I00211',
     +     'R10111', 'I10111', 'R11101', 'I11101', 'R30001',
     +     'I30001', 'R00301', 'I00301', 'R20101', 'I20101',
     +     'R10201', 'I10201', 'R20011', 'I20011', 'R01201',
     +     'I01201', 'R31000', 'I31000', 'R00310', 'I00310' /
      data (sln(i), i = 126, 150)
     +   / 'R20110', 'I20110', 'R11200', 'I11200', 'R21100',
     +     'I21100', 'R10210', 'I10210', 'R21010', 'I21010',
     +     'R01210', 'I01210', 'R40000', 'I40000', 'R00400',
     +     'I00400', 'R30100', 'I30100', 'R10300', 'I10300',
     +     'R30010', 'I30010', 'R01300', 'I01300', 'R20200' /
      data (sln(i), i = 151, 175)
     +   / 'I20200', 'R20020', 'I20020', '300010', '210010',
     +     '201010', '200110', '200020', '200011', '120010',
     +     '111010', '110110', '110020', '110011', '102010',
     +     '101110', '101020', '101011', '100210', '100120',
     +     '100111', '100030', '100021', '100012', '030010' /
      data (sln(i), i = 176, 200)
     +   / '021010', '020110', '020020', '020011', '012010',
     +     '011110', '011020', '011011', '010210', '010120',
     +     '010111', '010030', '010021', '010012', '003010',
     +     '002110', '002020', '002011', '001210', '001120',
     +     '001111', '001030', '001021', '001012', '000310' /
      data (sln(i), i = 201, 209)
     +   / '000220', '000211', '000130', '000121', '000112',
     +     '000040', '000031', '000022', '000013' /
 
      do 90 jord = 1, nord
        nl = 0
        do 70 j = ibot6(jord), itop6(jord)
          if (abs(fp(j)) .gt. tol) then
            nl = nl + 1
            il(nl) = j
          endif
   70   continue
        if (nl .gt. 0) then
          write (iunit, 910) jord
          write (iunit, 920) (sln(il(jl)), fp(il(jl)), jl = 1, nl)
        endif
   90 continue
 
  910 format(' Terms of order ',i1,':')
  920 format(4(' ',a6,'  = ',1pe14.6:,5x))
 
      end
