*
*=== source ===========================================================*
*
      SUBROUTINE SOURCE ( NOMORE )

      INCLUDE 'dblprc.inc'
      INCLUDE 'dimpar.inc'
      INCLUDE 'iounit.inc'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 2003-2019:  CERN & INFN                            *
*     All Rights Reserved.                                             *
*                                                                      *
*     New source for FLUKA9x-FLUKA20xy:                                *
*                                                                      *
*     Created on 07 January 1990   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
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
      INCLUDE 'beamcm.inc'
      INCLUDE 'fheavy.inc'
      INCLUDE 'flkstk.inc'
      INCLUDE 'ioiocm.inc'
      INCLUDE 'ltclcm.inc'
      INCLUDE 'paprop.inc'
      INCLUDE 'sourcm.inc'
      INCLUDE 'sumcou.inc'
*
      LOGICAL LFIRST, LISNUT
      PARAMETER (NMAX=10000000)

*
      SAVE LFIRST
      DATA LFIRST / .TRUE. /

      CHARACTER*250 LINE

      INTEGER    NUM, NNN
      REAL ENE_MAX, ENE_MIN
      INTEGER,DIMENSION(NMAX) :: IP
      REAL,DIMENSION(NMAX) :: XXX, YYY, ZZZ
      REAL,DIMENSION(NMAX) :: UUU, VVV, WWW
      REAL,DIMENSION(NMAX) :: ERG,PLA
      SAVE XXX, YYY, ZZZ
      SAVE UUU, VVV, WWW
      SAVE IP, ERG, PLA

*  Statement function:
*      LISNUT (IJ) = INDEX ( PRNAME (IJ), 'NEUTRI' ) .GT. 0
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
         LUNRD = NINT(WHASOU(1))
         NNN = 0
*  |  *** Energy region ***

         ENE_MAX = 1000
         ENE_MIN = 0.000000000001
 
 10      CONTINUE
            OPEN(UNIT=88,FILE="../source.dat", STATUS="OLD", form='formatted')
            READ( 88, '(A)', END=20 ) LINE
*            READ( LUNRD, *, END=20 ) LINE
            READ (LINE,*,ERR=10) I, P, E, U, V, W, X, Y, Z
*            IF ( E.GT.ENE_MIN .AND. E.LT.ENE_MAX ) THEN
                NNN = NNN + 1
             
                IP(NNN) = I
                
                PLA(NNN) = P
                ERG(NNN) = E
                UUU(NNN) = U
                VVV(NNN) = V
                WWW(NNN) = W
                XXX(NNN) = X
                YYY(NNN) = Y
                ZZZ(NNN) = Z
*            END IF
         GOTO 10
 20      CONTINUE
         IF (NNN.EQ.0) CALL FLABRT('SOURCE','Error reading file')
         WRITE (LUNOUT,*)
         WRITE (LUNOUT,*) '*** rdsource: ',NNN,' particles loaded',
     &' - total weight: ',WTOT
         WRITE (LUNOUT,*)
      END IF


*      DO NUM =1, NNN
*  |
*  +-------------------------------------------------------------------*
*  Push one source particle to the stack. Note that you could as well
*  push many but this way we reserve a maximum amount of space in the
*  stack for the secondaries to be generated
*  Npflka is the stack counter: of course any time source is called it
*  must be =0
      NPFLKA = NPFLKA + 1

*  | Choose a random particle
      RNDSIG = FLRNDM (RNDSIG)
      NUM = INT(NNN*RNDSIG)+1

*  Wt is the weight of the particle
      WTFLK  (NPFLKA) = ONEONE
      WEIPRI = WEIPRI + WTFLK (NPFLKA)
*  Particle type (1=proton.....). Ijbeam is the type set by the BEAM
*  card
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope:
      IF ( IP(NUM) .EQ. -2 .AND. LRDBEA ) THEN
         IARES  = IPROA
         IZRES  = IPROZ
         IISRES = IPROM
         CALL STISBM ( IARES, IZRES, IISRES )
         IJHION = IPROM  * 100000 + MOD ( IPROZ, 100 ) * 1000 + IPROA
         IJHION = IJHION * 100    + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         LFRPHN (NPFLKA) = .FALSE.
*  |
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      ELSE IF ( IP(NUM) .EQ. -2 ) THEN
         IJHION = IPROM  * 100000 + MOD ( IPROZ, 100 ) * 1000 + IPROA
         IJHION = IJHION * 100    + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         ILOFLK (NPFLKA) = IJHION
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
*  |  Parent radioactive isotope:
         IRDAZM (NPFLKA) = 0
*  |  Kinetic energy of the particle (GeV)
         TKEFLK (NPFLKA) = SQRT ( PBEAM**2 + AM (IONID)**2 )
     &                   - AM (IONID)
*  |  Particle momentum
         PMOFLK (NPFLKA) = PBEAM
*        PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
*    &                          + TWOTWO * AM (IONID) ) )
         LFRPHN (NPFLKA) = .FALSE.
*  |
*  +-------------------------------------------------------------------*
*  |  Normal hadron:
      ELSE
         IONID = IP(NUM)
         ILOFLK (NPFLKA) = IP(NUM)
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
*  |  Parent radioactive isotope:
         IRDAZM (NPFLKA) = 0
*  |  Kinetic energy of the particle (GeV)
*         TKEFLK (NPFLKA) = SQRT ( PBEAM**2 + AM (IONID)**2 )
*     &                   - AM (IONID)
         TKEFLK (NPFLKA) = ERG(NUM)
*  |  Particle momentum
*      PMOFLK (NPFLKA) = PLA(NUM)
*      PMOFLK (NPFLKA) = PBEAM
         PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
     &                          + TWOTWO * AM (IONID) ) )
     
*  |  +----------------------------------------------------------------*
*  |  |  Check if it is a neutrino, if so force the interaction
*  |  |  (unless the relevant flag has been disabled)
*         IF ( LISNUT (IP(NUM)) .AND. LNUFIN ) THEN
*            LFRPHN (NPFLKA) = .TRUE.
*  |  |
*  |  +----------------------------------------------------------------*
*  |  |  Not a neutrino
*         ELSE
*            LFRPHN (NPFLKA) = .FALSE.
*         END IF

*  |  |
*  |  +----------------------------------------------------------------*
      END IF
*  |
*  +-------------------------------------------------------------------*
*  From this point .....
*  Particle generation (1 for primaries)
      LOFLK  (NPFLKA) = 1
*  User dependent flag:
      LOUSE  (NPFLKA) = 0
*  No channeling:
      KCHFLK (NPFLKA) = 0
      ECRFLK (NPFLKA) = ZERZER
*  Extra infos:
      INFSTK (NPFLKA) = 0
      LNFSTK (NPFLKA) = 0
      ANFSTK (NPFLKA) = ZERZER
*  Parent variables:
      IPRSTK (NPFLKA) = 0
      EKPSTK (NPFLKA) = ZERZER
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
      AKNSHR (NPFLKA) = -TWOTWO
*  Particle age (s)
      AGESTK (NPFLKA) = +ZERZER
*  Cosines (tx,ty,tz)
      TXFLK  (NPFLKA) = UUU(NUM)
      TYFLK  (NPFLKA) = VVV(NUM)
      TZFLK  (NPFLKA) = WWW(NUM)
*     TZFLK  (NPFLKA) = SQRT ( ONEONE - TXFLK (NPFLKA)**2
*    &                       - TYFLK (NPFLKA)**2 )
*  Polarization cosines:
      TXPOL  (NPFLKA) = -TWOTWO
      TYPOL  (NPFLKA) = +ZERZER
      TZPOL  (NPFLKA) = +ZERZER
*  Particle coordinates
      XFLK   (NPFLKA) = -10.0+20.0*FLRNDM (XDUMMY)
      YFLK   (NPFLKA) = -10.0+20.0*FLRNDM (YDUMMY)
*      XPOS = FLRNDM (XPOS)
*      YPOS = FLRNDM (YPOS)
*      XFLK   (NPFLKA) = -10.0+20.0*XPOS
*      YFLK   (NPFLKA) = -10+20*YPOS
      ZFLK   (NPFLKA) = 3.5

      PRINT 101,XFLK(NPFLKA),YFLK(NPFLKA),ZFLK(NPFLKA),TXFLK(NPFLKA),TYFLK(NPFLKA),TZFLK(NPFLKA),TKEFLK (NPFLKA)
 101   format(F30.16, F30.16, F30.16, F30.16, F30.16, F30.16, F30.16) 


*      PRINT 101, XFLK(NPFLKA),YFLK(NPFLKA),ZFLK(NPFLKA)
* 101  format(F6.2, F6.2, F6.2) 

*  Calculate the total kinetic energy of the primaries: don't change
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope:
      IF ( IP(NUM) .EQ. -2 .AND. LRDBEA ) THEN
*  |
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      ELSE IF ( ILOFLK (NPFLKA) .EQ. -2 .OR.
     &          ILOFLK (NPFLKA) .GT. 100000 ) THEN
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
*  |
*  +-------------------------------------------------------------------*
*  |  Standard particle:
      ELSE IF ( ILOFLK (NPFLKA) .NE. 0 ) THEN
         TKESUM = TKESUM + ( TKEFLK (NPFLKA) + AMDISC (ILOFLK(NPFLKA)) )
     &          * WTFLK (NPFLKA)
*  |
*  +-------------------------------------------------------------------*
*  |
      ELSE
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      END IF
*  |

*  +-------------------------------------------------------------------*
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
      CALL SOEVSV
*      END DO
      RETURN
*      PRINT *,"Hello world 8"
* 9999 CONTINUE
*      CALL FLABRT('SOURCE','Error reading source file')
*=== End of subroutine Source =========================================*
      END
