      subroutine lmdprt(iunit, nord, fp)
      implicit none
*----------------------------------------------------------------------*
* Purpose:                                                             *
*   Print the polynomials of a dynamic resonance basis.                *
* Source:     MARYLIE, version 3.0 (routine PDRMAP).                   *
* Input:                                                               *
*   IUNIT     (integer) Logical output unit.                           *
*   NORD      (integer) Order of the map.                              *
*   FP(*)     (poly)    Coefficients in resonance basis.               *
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
      character*8       dln(209)
 
      data (dln(i), i = 1, 25)
     +   / 'R100000', 'I100000', 'R001000', 'I001000', 'R000010',
     +     'I000010', 'R110000', 'R001100', 'R000011', 'R000020',
     +     'I000020', 'R100010', 'I100010', 'R100001', 'I100001',
     +     'R001010', 'I001010', 'R001001', 'I001001', 'R200000',
     +     'I200000', 'R002000', 'I002000', 'R101000', 'I101000' /
      data (dln(i), i = 26, 50)
     +   / 'R100100', 'I100100', 'R110010', 'I110010', 'R001110',
     +     'I001110', 'R000030', 'I000030', 'R000021', 'I000021',
     +     'R100011', 'I100011', 'R001011', 'I001011', 'R100020',
     +     'I100020', 'R100002', 'I100002', 'R001020', 'I001020',
     +     'R001002', 'I001002', 'R200010', 'I200010', 'R200001' /
      data (dln(i), i = 51, 75)
     +   / 'I200001', 'R002010', 'I002010', 'R002001', 'I002001',
     +     'R101010', 'I101010', 'R101001', 'I101001', 'R100110',
     +     'I100110', 'R100101', 'I100101', 'R210000', 'I210000',
     +     'R002100', 'I002100', 'R101100', 'I101100', 'R111000',
     +     'I111000', 'R300000', 'I300000', 'R003000', 'I003000' /
      data (dln(i), i = 76, 100)
     +   / 'R201000', 'I201000', 'R102000', 'I102000', 'R200100',
     +     'I200100', 'R100200', 'I100200', 'R220000', 'R002200',
     +     'R000022', 'R111100', 'R110011', 'R001111', 'R110020',
     +     'I110020', 'R001120', 'I001120', 'R000040', 'I000040',
     +     'R000031', 'I000031', 'R100030', 'I100030', 'R100003' /
      data (dln(i), i = 101, 125)
     +   / 'I100003', 'R001030', 'I001030', 'R001003', 'I001003',
     +     'R100021', 'I100021', 'R100012', 'I100012', 'R001021',
     +     'I001021', 'R001012', 'I001012', 'R200011', 'I200011',
     +     'R002011', 'I002011', 'R200020', 'I200020', 'R200002',
     +     'I200002', 'R002020', 'I002020', 'R002002', 'I002002' /
      data (dln(i), i = 126, 150)
     +   / 'R101011', 'I101011', 'R100111', 'I100111', 'R101020',
     +     'I101020', 'R101002', 'I101002', 'R100120', 'I100120',
     +     'R100102', 'I100102', 'R210010', 'I210010', 'R210001',
     +     'I210001', 'R002110', 'I002110', 'R002101', 'I002101',
     +     'R101110', 'I101110', 'R101101', 'I101101', 'R111010' /
      data (dln(i), i = 151, 175)
     +   / 'I111010', 'R111001', 'I111001', 'R300010', 'I300010',
     +     'R300001', 'I300001', 'R003010', 'I003010', 'R003001',
     +     'I003001', 'R201010', 'I201010', 'R201001', 'I201001',
     +     'R102010', 'I102010', 'R102001', 'I102001', 'R200110',
     +     'I200110', 'R200101', 'I200101', 'R100201', 'I100201' /
      data (dln(i), i = 176, 200)
     +   / 'R100210', 'I100210', 'R310000', 'I310000', 'R003100',
     +     'I003100', 'R201100', 'I201100', 'R112000', 'I112000',
     +     'R211000', 'I211000', 'R102100', 'I102100', 'R210100',
     +     'I210100', 'R101200', 'I101200', 'R400000', 'I400000',
     +     'R004000', 'I004000', 'R301000', 'I301000', 'R103000' /
      data (dln(i), i = 201, 209)
     +   / 'I103000', 'R300100', 'I300100', 'R100300', 'I100300',
     +     'R202000', 'I202000', 'R200200', 'I200200' /
 
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
          write (iunit, 920) (dln(il(jl)), fp(il(jl)), jl = 1, nl)
        endif
   90 continue
 
  910 format(' Terms of order ',i1,':')
  920 format(4(' ',a7,' = ',1pe14.6:,5x))
 
      end
