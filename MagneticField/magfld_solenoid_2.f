*$ CREATE MAGFLD.FOR
*COPY MAGFLD
*
*===magfld=============================================================*
*
      SUBROUTINE MAGFLD ( X, Y, Z, BTX, BTY, BTZ, B, NREG, IDISC )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'

*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1988-2009      by Alberto Fasso` & Alfredo Ferrari *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     Created  in 1988    by     Alberto Fasso`, CERN - TIS            *
*                                                                      *
*     Last change on 15-oct-09     by    Alfredo Ferrari               *
*                                                                      *
*     Input variables:                                                 *
*            x,y,z = current position                                  *
*            nreg  = current region                                    *
*     Output variables:                                                *
*            btx,bty,btz = cosines of the magn. field vector           *
*            B = magnetic field intensity (Tesla)                      *
*            idisc = set to 1 if the particle has to be discarded      *
*                                                                      *
*----------------------------------------------------------------------*
*
*     Last change on 5-oct-10     by    Advanced FLUKA course teacher  *
*
      INCLUDE '(RTDFCM)'
      INCLUDE '(LTCLCM)'
*         
      LOGICAL LFIRST
      SAVE LFIRST
      SAVE  GRADIENT
*
      DATA LFIRST / .TRUE. /
*
      INTEGER NR, NZ, NPOINT,LENT
      REAL RMIN,RMAX,ZMIN,ZMAX
      REAL ZMIN2,ZMAX2
      REAL PHIOFF,TOTSEC
      PARAMETER (NR=601)
      PARAMETER (NZ=1201)
      PARAMETER (NPOINT=NR*NZ)
      PARAMETER (LENT=NR+NZ)
      REAL BR(NPOINT)
      REAL BZ(NPOINT)
      REAL RB(NR), ZB(NZ)
      REAL DX, DY, DZ, DR
      REAL BRV, BZV
      INTEGER I, J, K, INDEX
      PARAMETER (RMIN = 0.0)
      PARAMETER (RMAX = 3.0)
      PARAMETER (ZMIN = -3.0)
      PARAMETER (ZMAX = 3.0)
      PARAMETER (ZC = -133.5)
      
  
      INTEGER NSEC
      INTEGER BINR,BINZ
      REAL BXV,BYV
      REAL BR111,BZ111
      REAL    ARG(2),FX,FZ
      INTEGER NENT(2)
      REAL ENT(LENT) 
      common/magtable/init,NENT,ENT,BR,BZ
c
c
      IDISC = 0
      IF (LFIRST) THEN
*     gradient in tesla per cm
         GRADIENT = 0.08D+00  
         INDEX = 1
         OPEN(UNIT=17, FILE ='/home/zana/clas12_solenoid.dat')
*         LUNRD = NINT(WHASOU(1))
         DO I = 1, NR
            DO  K = 1, NZ
               READ(17,*) DX,DZ,BRV,BZV
               RB(I) = DX*100
               ZB(K) = DZ*100
               BR(INDEX) = BRV
               BZ(INDEX) = BZV
               INDEX = INDEX + 1
            END DO
         END DO
         INDEX = 1
         NENT(1) = NZ
         DO  I = 1, NZ
            ENT(INDEX) = ZB(I)
            INDEX = INDEX + 1
         END DO
         NENT(2) = NR
         DO  K = 1, NR
            ENT(INDEX) = RB(K)
            INDEX = INDEX + 1
         END DO
         LFIRST  = .FALSE.
      END IF
* Phi offset is added to the limits as from the Geant4 code (L245 remollMagneticField.cc)
      R = SQRT(X*X + Y*Y)/100
      PHI = ATAN2(Y,X) * 180. / 3.1416
* Setting PHI  between 0-360 deg
      IF ( PHI .LT. 0.0) THEN
         PHI = PHI + 360.
      END IF
      IF ( R .LT. RMAX .AND. R .GT. RMIN ) THEN 
         IF ( ((Z-ZC)/100) .LT. ZMAX .AND. ((Z-ZC)/100) .GT. ZMIN )THEN
            ARG(1) = Z
            ARG(2) = R
            DR = (RMAX -RMIN)/NR
            DZ = (ZMAX -ZMIN)/NZ
            BINR = INT((R-RMIN)/DR) + 1
            BINZ = INT(((Z-ZC)/100-ZMIN)/DZ) +1
            
            CALL MAGFINT3(ARG,FX,FZ)
*     Getting the value of Field and Angle (in radiants) at the reticolate rotated to the correct sector 
*     Here is used 111 as corner in lowR,lowphi,lowZ
            BR111 = BR((BINR-1)*NZ+BINZ)
            BZ111 = BZ((BINR-1)*NZ+BINZ)
            P111 = PHI/180*3.1416
*     Here should happen the interpolation, before getting the XYZ coordinates, since the grid is a section of a tube
               BXV = BR111*COS(P111) 
               BYV = BR111*SIN(P111)
               BZV = BZ111
               B=SQRT(BXV**2+BYV**2+BZV**2)
               IF (B.GT.1D-12) THEN
                  BTX = BXV / B
                  BTY = BYV / B
                  BTZ = BZV / B
               ELSE
                  B = 0.0
                  BTX = 1.0
                  BTZ= 0.0
                  BTY = 0.0
               END IF 
            ELSE
               BTX = 0.0
               BTY = 1.0
               BTZ = 0.0
               B   = 0.0 
            END IF
         ELSE
            BTX = 0.0
            BTY = 1.0
            BTZ = 0.0
            B   = 0.0 
         END IF
      RETURN
*===  End of subroutine magfld =========================================*
      END
*
      subroutine MAGFINT3(ARG,FX,FZ)
C
C   P. Degtiarenko, JLAB, 2019
C   Based on the INTERPOLATION ROUTINE 'FINT' by C. LETERTRE
C   and MODIFIED BY B. SCHORR, 1.07.1982, from the CERNLIB
C
      implicit none
      REAL      FX,FZ
      INTEGER   N,LMAX,ISTEP,KNOTS,NDIM,LOCA,LMIN,LOCB
      INTEGER   LOCC,ISHIFT,I,K
      REAL      X,H,ETA
      INTEGER   INDEX(32)
      REAL      WEIGHT(32)
c
c     -- common block description (makes sense to make it an include file) --
c     -- or make sure it is exactly the same in the calling routine        --
      integer NARG,NR,NPHI,NZ,LENT,LENBT
      parameter (NARG=2) ! =3 for the 3-dimensional space x,y,z
      parameter (NR=601)   ! Number of table nodes in x
      parameter (NZ=1201)   ! Number of table nodes in z
      parameter (LENT=NR+NZ)   ! Length of the ENT array
      parameter (LENBT=NR*NZ)  ! Length of the BX, BY, BZ component tables
      logical init
      integer NENT(NARG)
      real ENT(LENT),BR(LENBT),BZ(LENBT)
      common/magtable/init,NENT,ENT,BR,BZ
c
      real    ARG(NARG)
c
      FX  =  0.
      FZ  =  0.
      IF(NARG .LT. 1  .OR.  NARG .GT. 5)  RETURN
      LMAX      =  0
      ISTEP     =  1
      KNOTS     =  1
      INDEX(1)  =  1
      WEIGHT(1) =  1.
      DO 100    N  =  1, NARG
         X     =  ARG(N)
         NDIM  =  NENT(N)
         LOCA  =  LMAX
         LMIN  =  LMAX + 1
         LMAX  =  LMAX + NDIM
         IF(NDIM .GT. 2)  GOTO 10
         IF(NDIM .EQ. 1)  GOTO 100
         H  =  X - ENT(LMIN)
         IF(H .EQ. 0.)  GOTO 90
         ISHIFT  =  ISTEP
         IF(X-ENT(LMIN+1) .EQ. 0.)  GOTO 21
         ISHIFT  =  0
         ETA     =  H / (ENT(LMIN+1) - ENT(LMIN))
         GOTO 30
 10      LOCB  =  LMAX + 1
 11      LOCC  =  (LOCA+LOCB) / 2
         IF(X-ENT(LOCC))  12, 20, 13
 12      LOCB  =  LOCC
         GOTO 14
 13      LOCA  =  LOCC
 14      IF(LOCB-LOCA .GT. 1)  GOTO 11
         LOCA    =  MIN( MAX(LOCA,LMIN), LMAX-1 )
         ISHIFT  =  (LOCA - LMIN) * ISTEP
         ETA     =  (X - ENT(LOCA)) / (ENT(LOCA+1) - ENT(LOCA))
         GOTO 30
 20      ISHIFT  =  (LOCC - LMIN) * ISTEP
 21      DO 22  K  =  1, KNOTS
            INDEX(K)  =  INDEX(K) + ISHIFT
 22      CONTINUE
         GOTO 90
 30      DO 31  K  =  1, KNOTS
            INDEX(K)         =  INDEX(K) + ISHIFT
            INDEX(K+KNOTS)   =  INDEX(K) + ISTEP
            WEIGHT(K+KNOTS)  =  WEIGHT(K) * ETA
            WEIGHT(K)        =  WEIGHT(K) - WEIGHT(K+KNOTS)
 31      CONTINUE
         KNOTS  =  2*KNOTS
 90      ISTEP  =  ISTEP * NDIM
 100  CONTINUE
      DO 200    K  =  1, KNOTS
         I  =  INDEX(K)
         FX  =  FX + WEIGHT(K) * BR(I)
         FZ  =  FZ + WEIGHT(K) * BZ(I)
 200  CONTINUE
      RETURN
      END
*
*
*
