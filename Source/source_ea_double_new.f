*$ CREATE SOURCE.FOR
*COPY SOURCE
*
*=== source ===========================================================*
*
      SUBROUTINE SOURCE ( NOMORE )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2010      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     New source for FLUKA9x-FLUKA20xy:                                *
*                                                                      *
*     Created on 07 January 1990   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on  17-Oct-10    by    Alfredo Ferrari               *
*                                                                      *
*  This is just an example of a possible user written source routine.  *
*  note that the beam card still has some meaning - in the scoring the *
*  maximum momentum used in deciding the binning is taken from the     *
*  beam momentum.  Other beam card parameters are obsolete.            *
*                                                                      *
*       Output variables:                                              *
*                                                                      *
*              Nomore = if > 0 the run will be terminated              *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(BEAMCM)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(FLKSTK)'
      INCLUDE '(IOIOCM)'
      INCLUDE '(LTCLCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(SOURCM)'
      INCLUDE '(SUMCOU)'
C ++<PD>
      DOUBLE PRECISION E_ELEC,E_EQPHOT,EqPhThr,F_GEFLUX
      DOUBLE PRECISION D_Beam,RAN01,RAN02
      DATA EqPhThr / 7.0D-3 / ! -- energy thresholld for eq.photon generator
      DATA D_Beam  / 0.1D+0 / ! -- Diameter of the beam spot (uniform circle)
      SAVE EqPhThr
      SAVE D_Beam
C --<PD>
*
      LOGICAL LFIRST
*
      SAVE LFIRST
      DATA LFIRST / .TRUE. /
*======================================================================*
*                                                                      *
*                 BASIC VERSION                                        *
*                                                                      *
*======================================================================*
      NOMORE = 0
*  +-------------------------------------------------------------------*
*  |  First call initializations:
      IF ( LFIRST ) THEN
*  |  *** The following 3 cards are mandatory ***
         TKESUM = ZERZER
         LFIRST = .FALSE.
         LUSSRC = .TRUE.
*  |  *** User initialization ***
      END IF
*  |
*  +-------------------------------------------------------------------*
*  Push one source particle to the stack. Note that you could as well
*  push many but this way we reserve a maximum amount of space in the
*  stack for the secondaries to be generated
C ++<PD>
C   The virtual photon energy is generated once per a source particle
C   in accordance with its energy distribution, and average multiplicity
C   as long as the multiplicity is below 1, which should be the case 
C   (if not, error message is printed)
C   If E_EQPHOT=0, no photon is generated, and the event should have
C   only the beam electron with full energy
C   The balance between the events with photon and without it will give
C   the average multiplicity of the photons, calculated internally,
C   function of full energy of beam electron and the photon threshold
c
      E_ELEC   = SQRT(PBEAM**2+AM(IJBEAM)**2)
      E_EQPHOT = F_GEFLUX(EqPhThr,E_ELEC)
      if(E_ELEC-E_EQPHOT.le.AM(IJBEAM)) E_EQPHOT = ZERZER ! ***Non-physical
      if(E_EQPHOT.gt.ZERZER) then
         E_ELEC = E_ELEC - E_EQPHOT
      endif
c      print *, ' %%%SOURCE: E_ELEC,IJBEAM,E_EQPHOT =',
c     &     E_ELEC,IJBEAM,E_EQPHOT
C --<PD>
*  Npflka is the stack counter: of course any time source is called it
*  must be =0
      NPFLKA = NPFLKA + 1
*  Wt is the weight of the particle
      WTFLK  (NPFLKA) = ONEONE
      WEIPRI = WEIPRI + WTFLK (NPFLKA)
*  Particle type (1=proton.....). Ijbeam is the type set by the BEAM
*  card
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope:
      IF ( IJBEAM .EQ. -2 .AND. LRDBEA ) THEN
         IARES  = IPROA
         IZRES  = IPROZ
         IISRES = IPROM
         CALL STISBM ( IARES, IZRES, IISRES )
         IJHION = IPROZ  * 1000 + IPROA
         IJHION = IJHION * 100 + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
*  |
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      ELSE IF ( IJBEAM .EQ. -2 ) THEN
         IJHION = IPROZ  * 1000 + IPROA
         IJHION = IJHION * 100 + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         ILOFLK (NPFLKA) = IJHION
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
*  |
*  +-------------------------------------------------------------------*
*  |  Normal hadron:
      ELSE
         IONID = IJBEAM
         ILOFLK (NPFLKA) = IJBEAM
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
      END IF
*  |
*  +-------------------------------------------------------------------*
*  From this point .....
*  Particle generation (1 for primaries)
      LOFLK  (NPFLKA) = 1
*  User dependent flag:
      LOUSE  (NPFLKA) = 0
*  No channeling:
      LCHFLK (NPFLKA) = .FALSE.
      DCHFLK (NPFLKA) = ZERZER
*  User dependent spare variables:
      DO 100 ISPR = 1, MKBMX1
         SPAREK (ISPR,NPFLKA) = ZERZER
 100  CONTINUE
*  User dependent spare flags:
      DO 200 ISPR = 1, MKBMX2
         ISPARK (ISPR,NPFLKA) = 0
 200  CONTINUE
*  Save the track number of the stack particle:
      ISPARK (MKBMX2,NPFLKA) = NPFLKA
      NPARMA = NPARMA + 1
      NUMPAR (NPFLKA) = NPARMA
      NEVENT (NPFLKA) = 0
      DFNEAR (NPFLKA) = +ZERZER
*  ... to this point: don't change anything
*  Particle age (s)
      AGESTK (NPFLKA) = +ZERZER
      AKNSHR (NPFLKA) = -TWOTWO
*  Kinetic energy of the particle (GeV)
      TKEFLK (NPFLKA) = E_ELEC - AM (IONID)
*  Particle momentum
      PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
     &                       + TWOTWO * AM (IONID) ) )
*  Cosines (tx,ty,tz)
      TXFLK  (NPFLKA) = UBEAM
      TYFLK  (NPFLKA) = VBEAM
      TZFLK  (NPFLKA) = WBEAM
*     TZFLK  (NPFLKA) = SQRT ( ONEONE - TXFLK (NPFLKA)**2
*    &                       - TYFLK (NPFLKA)**2 )
*  Polarization cosines:
      TXPOL  (NPFLKA) = -TWOTWO
      TYPOL  (NPFLKA) = +ZERZER
      TZPOL  (NPFLKA) = +ZERZER
*  Particle coordinates
C ++<PD>
 25   continue
      RAN01 = D_Beam*(FLRNDM(DUMMY)-5.0D-1)
      RAN02 = D_Beam*(FLRNDM(DUMMY)-5.0D-1)
      if(RAN01**2+RAN02**2.gt.2.5D-1*D_Beam**2) goto 25
      XFLK   (NPFLKA) = XBEAM + RAN01
      YFLK   (NPFLKA) = YBEAM + RAN02
C --<PD>
      ZFLK   (NPFLKA) = ZBEAM
*  Calculate the total kinetic energy of the primaries: don't change
      IF ( ILOFLK (NPFLKA) .EQ. -2 .OR. ILOFLK (NPFLKA) .GT. 100000 )
     &   THEN
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      ELSE IF ( ILOFLK (NPFLKA) .NE. 0 ) THEN
         TKESUM = TKESUM + ( TKEFLK (NPFLKA) + AMDISC (ILOFLK(NPFLKA)) )
     &          * WTFLK (NPFLKA)
      ELSE
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      END IF
      RADDLY (NPFLKA) = ZERZER
*  Here we ask for the region number of the hitting point.
*     NREG (NPFLKA) = ...
*  The following line makes the starting region search much more
*  robust if particles are starting very close to a boundary:
      CALL GEOCRS ( TXFLK (NPFLKA), TYFLK (NPFLKA), TZFLK (NPFLKA) )
      CALL GEOREG ( XFLK  (NPFLKA), YFLK  (NPFLKA), ZFLK  (NPFLKA),
     &              NRGFLK(NPFLKA), IDISC )
*  Do not change these cards:
      CALL GEOHSM ( NHSPNT (NPFLKA), 1, -11, MLATTC )
      NLATTC (NPFLKA) = MLATTC
      CMPATH (NPFLKA) = ZERZER
C ++<PD>
C   Add the equivalent photon to the stack
      if(E_EQPHOT.gt.ZERZER) then
c      if(E_EQPHOT.lt.0.0) then ! *** Turn this OFF for the first debugging ***
*       Increment the stack counter
         NPFLKA = NPFLKA + 1
*       Weight of the photon (and it shouldn't be added to the WEIPRI, I guess...)
         WTFLK  (NPFLKA) = ONEONE
*       Real photon particle ID - Check, please -
         ILOFLK (NPFLKA) = 7
*       Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*       Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
*  From this point .....
*  Particle generation (1 for primaries)
         LOFLK  (NPFLKA) = 1
*  User dependent flag:
         LOUSE  (NPFLKA) = 0
*  No channeling:
         LCHFLK (NPFLKA) = .FALSE.
         DCHFLK (NPFLKA) = ZERZER
*  User dependent spare variables:
         DO 110 ISPR = 1, MKBMX1
            SPAREK (ISPR,NPFLKA) = ZERZER
 110     CONTINUE
*  User dependent spare flags:
         DO 210 ISPR = 1, MKBMX2
            ISPARK (ISPR,NPFLKA) = 0
 210     CONTINUE
*  Save the track number of the stack particle:
         ISPARK (MKBMX2,NPFLKA) = NPFLKA
         NPARMA = NPARMA + 1
         NUMPAR (NPFLKA) = NPARMA
         NEVENT (NPFLKA) = 0
         DFNEAR (NPFLKA) = +ZERZER
*  ... to this point: don't change anything
*  Particle age (s)
         AGESTK (NPFLKA) = +ZERZER
         AKNSHR (NPFLKA) = -TWOTWO
*  Kinetic energy of the particle - eq. photon (GeV)
         TKEFLK (NPFLKA) = E_EQPHOT
*  Particle - eq. photon  momentum
         PMOFLK (NPFLKA) = E_EQPHOT
*  Cosines (tx,ty,tz)
         TXFLK  (NPFLKA) = UBEAM
         TYFLK  (NPFLKA) = VBEAM
         TZFLK  (NPFLKA) = WBEAM
*  Polarization cosines:
         TXPOL  (NPFLKA) = -TWOTWO
         TYPOL  (NPFLKA) = +ZERZER
         TZPOL  (NPFLKA) = +ZERZER
*  Particle coordinates (take it from the electron)
         XFLK   (NPFLKA) = XFLK(NPFLKA-1)
         YFLK   (NPFLKA) = YFLK(NPFLKA-1)
         ZFLK   (NPFLKA) = ZBEAM
*  Calculate the total kinetic energy of the primaries: don't change
         IF ( ILOFLK (NPFLKA) .EQ. -2 .OR. ILOFLK (NPFLKA) .GT. 100000 )
     &        THEN
            TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
         ELSE IF ( ILOFLK (NPFLKA) .NE. 0 ) THEN
            TKESUM = TKESUM + (TKEFLK(NPFLKA) + AMDISC(ILOFLK(NPFLKA)))
     &           * WTFLK (NPFLKA)
         ELSE
            TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
         END IF
         RADDLY (NPFLKA) = ZERZER
*  Here we ask for the region number of the hitting point.
*     NREG (NPFLKA) = ...
*  The following line makes the starting region search much more
*  robust if particles are starting very close to a boundary:
         CALL GEOCRS ( TXFLK (NPFLKA), TYFLK (NPFLKA), TZFLK (NPFLKA) )
         CALL GEOREG ( XFLK  (NPFLKA), YFLK  (NPFLKA), ZFLK  (NPFLKA),
     &        NRGFLK(NPFLKA), IDISC )
*  Do not change these cards:
         CALL GEOHSM ( NHSPNT (NPFLKA), 1, -11, MLATTC )
         NLATTC (NPFLKA) = MLATTC
         CMPATH (NPFLKA) = ZERZER
      endif    
C --<PD>
      CALL SOEVSV
      RETURN
*=== End of subroutine Source =========================================*
      END
*
      DOUBLE PRECISION FUNCTION F_GEFLUX(E0i,Ei)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generates equivalent photons
C-                         in accordance with the flux formula
C-
C-   Input:  Ei  = initial energy of electron (GeV)
C-           E0i = lower energy boundary for the photon spectrum (GeV)
C-
C-   Output: F_GEFLUX = generated energy of a photon (GeV)
C-
C-  The total integrated flux (TIF) of equivalent photons for realistic (~ MeV)  
C-  thresholds is generally less than 1. Thus the program would generate either
C-  F_GEFLUX > 0, and that would be the generated energy of the photon, or 
C-  F_GEFLUX = 0, meaning that the photon shouldn't be generated
C-  The average number of the photons generated per one electron will correspond to the 
C-  TIF. If for some reason the TIF value is above 1, the program will print a message. 
C-
C-   Created  23-Jan-2002  Pavel Degtyarenko
C-   Modified for the FLUKA source card Oct-09-2013  Pavel Degtiarenko
C-   Converted to all-around DOUBLE PRECISION Mar-03-2015  Pavel Degtiarenko
C-
C----------------------------------------------------------------------
      IMPLICIT  NONE
      DOUBLE PRECISION E0g,E0i,Ei,E0,E,r,om,RNDM0,F_EFLUX,F_EFLUXI
      DOUBLE PRECISION FLRNDM,DUMMY
      integer i,ngam,F_INTRNDM
      common/comfeflux/E0,E
      DOUBLE PRECISION alpha, em, pi, tif
      PARAMETER (alpha = 0.007297352D+0)
      PARAMETER (em    = 0.000511D+0)
      PARAMETER (pi    = 3.14159265D+0)
      SAVE
C----------------------------------------------------------------------
      tif  = F_EFLUXI(E0i,Ei)
      ngam = F_INTRNDM(tif)
      if(ngam.eq.0) then
         om = 0.
         goto 90
      elseif(ngam.gt.1) then
         print *, ' %%%F_GEFLUX -- ERROR: ngam =',ngam
      endif
      E0 = E0i
      E  = Ei
      E0g = E0*1.0001D+0
      i = 0
 10   continue
      i = i + 1
      RNDM0 = FLRNDM(DUMMY)
      om = E0g*exp(RNDM0*(log(E-em)-log(E0g)))
      r  = F_EFLUX(om)*om/(2.*alpha/pi*(log(E-em)-log(em)))
      if(r.gt.1.0.or.i.gt.555) then
         print *, ' %%%F_GEFLUX -- ERROR: i,r,om,E0,E=',i,r,om,E0,E
      else
         RNDM0 = FLRNDM(DUMMY)
         if(RNDM0.gt.r) goto 10
      endif
 90   continue
      F_GEFLUX = om
*                                                             END F_GEFLUX
      END
C
      DOUBLE PRECISION function F_EFLUX(om)
C -- Formula 6.17b from Budnev et al. Input: E0, E (in common), om
      implicit none
      DOUBLE PRECISION E0,E,om,alQmax,alQmin,rQmnmx,f
      DOUBLE PRECISION Q2max,Q2min,aloQmx,aloQmn,term1,term2,term3
      DOUBLE PRECISION alpha, em, pi
      common/comfeflux/E0,E
      PARAMETER (alpha = 0.007297352D+0)
      PARAMETER (em    = 0.000511D+0)
      PARAMETER (pi    = 3.14159265D+0)
      SAVE
C
      f = 0.
      if(om.lt.E.and.om.gt.E0) then
         Q2max = min(0.77**2,4.*E*(E-om))
         Q2min = em**2*om**2/E/(E-om)
         if(Q2max.gt.Q2min) then
            alQmax = log(Q2max)
            alQmin = 2.*(log(em)+log(om)) - log(E) - log(E-om)
            aloQmx = log(om**2+Q2max)
            aloQmn = log(om**2+Q2min)
            rQmnmx = Q2min/Q2max
            term1 = (1.-om/E+0.5*om**2/E**2)*(alQmax-alQmin)
            term2 = (1.-0.5*om/E)**2*(aloQmx-aloQmn)
            term3 = E*(E-om)/E**2*(1.-rQmnmx)
            f = alpha/pi*(term1-term2-term3)/om
         endif
      endif
C#      print *, ' %%%F_EFLUX: om,f =',om,f
      F_EFLUX=f
*                                                             END F_EFLUX
      end
C
      DOUBLE PRECISION function F_EFLUXI(E0i,Ei)
C -- Calculates total flux of equivalent photons with omega > E0i 
C -- in an electron with energy Ei
      implicit none
      DOUBLE PRECISION E0,E
      common/comfeflux/E0,E
      DOUBLE PRECISION E0oldL,EoldL,E0i,Ei,EiL,EaL,EbL,Sa,Sb
      DOUBLE PRECISION    E0old/0./
      DOUBLE PRECISION    Eold/0./
      DOUBLE PRECISION    EiPrev/0./
      DOUBLE PRECISION    S/0./
      logical FirstTime/.true./
      DOUBLE PRECISION F_GAUSS,F_EFLUX,eps,EStart
      integer npts,i
      PARAMETER (npts = 500)
      PARAMETER (eps  = 0.1e-6)
      PARAMETER (EStart = 10.0)
      DOUBLE PRECISION StoreI(npts),StoreE(npts)
      external F_EFLUX
      SAVE
C
      if(E0i.eq.E0old.and.Ei.eq.EiPrev) goto 900 ! -- Keep previous value 
      if(E0i.ne.E0old.or.Ei.gt.Eold.or.FirstTime) then
C -- Make new approximation
C#         print *, ' %%%F_EFLUXI: First Call'
         if(FirstTime) then
            FirstTime = .false.
            E0old = E0i
            Eold  = EStart
         else
            if(E0i.ne.E0old) E0old = E0i
            if(Ei .gt. Eold) Eold  = Ei
         endif
         E0 = E0old
         EoldL  = log(Eold)
         E0oldL = log(E0old)
         do i = 1 , npts
            EiL = float(i)*(EoldL-E0oldL)/float(npts)+E0oldL
            E  = exp(EiL)
            StoreE(i) = EiL
            StoreI(i) = F_GAUSS(F_EFLUX,E0,E,eps)
         enddo
C#         print *, ' %%%F_EFLUXI: debug print of StoreE - StoreI '
C#         print 999, (exp(StoreE(i)),StoreI(i),i=1,npts)
C# 999     format(1x,2E14.7)
      endif
C -- Use calculated approximation, linear interpolation
C#      print *, ' %%%F_EFLUXI: Continue'
      EiL = log(Ei)
      i = int(float(npts)*(EiL-E0oldL)/(EoldL-E0oldL))+1
      if(i.gt.npts) i=npts
      if(i.le.1) then
         i = 1
         EaL = E0oldL
         Sa  = 0.
      else
         EaL = StoreE(i-1)
         Sa  = StoreI(i-1)
      endif
      EbL = StoreE(i)
      Sb  = StoreI(i)
      S = Sa + (EiL-EaL)*(Sb-Sa)/(EbL-EaL)
      EiPrev = Ei
C#      print *, ' %%%F_EFLUXI: E0i,Ei,i,E0oldL,EoldL:',
C#     &                      E0i,Ei,i,E0oldL,EoldL
C#      print *, ' %%%F_EFLUXI: S,Sa,Sb,EiL,EaL,Ebl',
C#     &                      S,Sa,Sb,EiL,EaL,Ebl
 900  continue
      F_EFLUXI = S
*                                                             END F_EFLUXI
      end
C
      INTEGER FUNCTION F_INTRNDM(x)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns random integer "i" equal int(x) or
C-                         int(x)+1 such that mean value of "i" equals x
C-
C-   Inputs  : X - DOUBLE PRECISION
C-   Outputs : INTRNDM - integer
C-   Controls: none
C-
C-   Created  27-JUN-1993   Pavel V. Degtyarenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER i
      DOUBLE PRECISION FLRNDM,DUMMY
      DOUBLE PRECISION rndm0,x,xi
C----------------------------------------------------------------------
      i  = x
      rndm0 = FLRNDM(DUMMY)
      xi = i + rndm0
      IF(xi .GT. x) THEN
        F_INTRNDM = i
      ELSE
        F_INTRNDM = i + 1
      ENDIF
      END
C
*
* $Id: gauss.F,v 1.1.1.1 1996/04/01 15:02:13 mclareni Exp $
*
* $Log: gauss.F,v $
* Revision 1.1.1.1  1996/04/01 15:02:13  mclareni
* Mathlib gen
*
*
C#include "gen/pilot.h"
C This will be GAUSS,IF=DOUBLE and GAUSS64,IF=-DOUBLE.
      DOUBLE PRECISION FUNCTION F_GAUSS(F,A,B,EPS)

c      CHARACTER NAME*(*)
c      PARAMETER (NAME = 'GAUSS')
C#include "gausscod.inc"
*
* $Id: gausscod.inc,v 1.1.1.1 1996/04/01 15:02:13 mclareni Exp $
*
* $Log: gausscod.inc,v $
* Revision 1.1.1.1  1996/04/01 15:02:13  mclareni
* Mathlib gen
*
*
*
* gausscod.inc
*
      IMPLICIT NONE
      DOUBLE PRECISION W(12),X(12)
      INTEGER I
      DOUBLE PRECISION F,A,B,EPS,H,CONST,BB,AA,C1,C2,S8,U,S16

      DOUBLE PRECISION Z1,HF,CST
      PARAMETER (Z1 = 1.0D0, HF = Z1/2, CST = 5*Z1/1000)

      DATA X( 1) /9.6028985649753623D-1/, W( 1) /1.0122853629037626D-1/
      DATA X( 2) /7.9666647741362674D-1/, W( 2) /2.2238103445337447D-1/
      DATA X( 3) /5.2553240991632899D-1/, W( 3) /3.1370664587788729D-1/
      DATA X( 4) /1.8343464249564980D-1/, W( 4) /3.6268378337836198D-1/
      DATA X( 5) /9.8940093499164993D-1/, W( 5) /2.7152459411754095D-2/
      DATA X( 6) /9.4457502307323258D-1/, W( 6) /6.2253523938647893D-2/
      DATA X( 7) /8.6563120238783174D-1/, W( 7) /9.5158511682492785D-2/
      DATA X( 8) /7.5540440835500303D-1/, W( 8) /1.2462897125553387D-1/
      DATA X( 9) /6.1787624440264375D-1/, W( 9) /1.4959598881657673D-1/
      DATA X(10) /4.5801677765722739D-1/, W(10) /1.6915651939500254D-1/
      DATA X(11) /2.8160355077925891D-1/, W(11) /1.8260341504492359D-1/
      DATA X(12) /9.5012509837637440D-2/, W(12) /1.8945061045506850D-1/

      SAVE X
      SAVE W

      H=0
      IF(B .EQ. A) GO TO 99
      CONST=CST/ABS(B-A)
      BB=A
    1 AA=BB
      BB=B
    2 C1=HF*(BB+AA)
      C2=HF*(BB-AA)
      S8=0
      DO 3 I = 1,4
      U=C2*X(I)
    3 S8=S8+W(I)*(F(C1+U)+F(C1-U))
      S16=0
      DO 4 I = 5,12
      U=C2*X(I)
    4 S16=S16+W(I)*(F(C1+U)+F(C1-U))
      S16=C2*S16
      IF(ABS(S16-C2*S8) .LE. EPS*(1+ABS(S16))) THEN
       H=H+S16
       IF(BB .NE. B) GO TO 1
      ELSE
       BB=C1
       IF(1.0D0+CONST*ABS(C2) .NE. 1.0D0) GO TO 2
       H=0
C       CALL MTLPRT(NAME,'D103.1','TOO HIGH ACCURACY REQUIRED')
       print *, ' %%%F_GAUSS: TOO HIGH ACCURACY REQUIRED'
       GO TO 99
      END IF
   99 F_GAUSS=H
      RETURN
      END
