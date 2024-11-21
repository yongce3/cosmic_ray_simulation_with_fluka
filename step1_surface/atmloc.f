*
*=== Atmloc ===========================================================*
*
      PROGRAM ATMLOC

      INCLUDE 'dblprc.inc'
      INCLUDE 'dimpar.inc'
      INCLUDE 'iounit.inc'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 2003-2019:  CERN & INFN                            *
*     All Rights Reserved.                                             *
*                                                                      *
*  ATMosphere LOCal geometry:                                          *
*                                                                      *
*  Created  on 04 November 1996    by         Alfredo Ferrari          *
*                                               INFN - Milan           *
*                                                                      *
*----------------------------------------------------------------------*
*
      PARAMETER ( NATMSH = 101 )
      PARAMETER ( NAIRSH = 100 )
*
      LOGICAL   LPLANE, LCYL
      DIMENSION ANGLE (4), RATLA(-1:NATMSH), RGEO(3), RGMG(3),
     &          IASIGN(4), IBTRC(4)
      CHARACTER CFIELD*10, CFIELD1*10, CBODY2*10, CBODY3*10, CBSPC*10,
     &          CNAME*8, CBLACK*8, CVACUUM*8
      DATA ( RATLA (I), I = -1, 51 ) /
     &                 1.00000000D+10, 6.60000000D+08,
     & 6.44969163D+08, 6.44817107D+08, 6.44667740D+08,
     & 6.44521014D+08, 6.44376882D+08, 6.44235299D+08,
     & 6.44096219D+08, 6.43959598D+08, 6.43825393D+08,
     & 6.43693561D+08, 6.43564060D+08, 6.43436848D+08,
     & 6.43311886D+08, 6.43189134D+08, 6.43068552D+08,
     & 6.42950102D+08, 6.42833747D+08, 6.42719449D+08,
     & 6.42607172D+08, 6.42496880D+08, 6.42388538D+08,
     & 6.42282112D+08, 6.42177568D+08, 6.42074872D+08,
     & 6.41973992D+08, 6.41874896D+08, 6.41777552D+08,
     & 6.41681929D+08, 6.41587998D+08, 6.41495726D+08,
     & 6.41405087D+08, 6.41316050D+08, 6.41228587D+08,
     & 6.41142671D+08, 6.41058274D+08, 6.40975370D+08,
     & 6.40893931D+08, 6.40813932D+08, 6.40735348D+08,
     & 6.40658153D+08, 6.40582323D+08, 6.40507834D+08,
     & 6.40434662D+08, 6.40362784D+08, 6.40292177D+08,
     & 6.40222818D+08, 6.40154686D+08, 6.40087758D+08,
     & 6.40022014D+08, 6.39957432D+08, 6.39893992D+08/
      DATA ( RATLA (I), I = 52, 101 ) /
     & 6.39831674D+08, 6.39770458D+08, 6.39710324D+08,
     & 6.39651254D+08, 6.39593228D+08, 6.39536228D+08,
     & 6.39480235D+08, 6.39425233D+08, 6.39371203D+08,
     & 6.39318129D+08, 6.39265993D+08, 6.39214779D+08,
     & 6.39164471D+08, 6.39115052D+08, 6.39066507D+08,
     & 6.39018820D+08, 6.38971976D+08, 6.38925961D+08,
     & 6.38880759D+08, 6.38836357D+08, 6.38792740D+08,
     & 6.38749894D+08, 6.38707805D+08, 6.38666461D+08,
     & 6.38625848D+08, 6.38585952D+08, 6.38546763D+08,
     & 6.38508266D+08, 6.38470450D+08, 6.38433302D+08,
     & 6.38396812D+08, 6.38360966D+08, 6.38325755D+08,
     & 6.38291166D+08, 6.38257188D+08, 6.38223812D+08,
     & 6.38191025D+08, 6.38158819D+08, 6.38127181D+08,
     & 6.38096103D+08, 6.38065575D+08, 6.38035587D+08,
     & 6.38006128D+08, 6.37977191D+08, 6.37948765D+08,
     & 6.37920842D+08, 6.37893412D+08, 6.37866468D+08,
     & 6.37840000D+08, 6.37814000D+08/
*
      RATLA (NATMSH) = REARTH
*
      LPLANE = .FALSE.
      LCYL   = .FALSE.
*
      WRITE (*,*)' Geographic (1) or geomagnetic (2) coordinates?'
      READ  (*,*) ICOOR
      IF ( ICOOR .EQ. 1 ) THEN
         CALL GEOGMG (RGEO,RGMG,1)
         WRITE (*,*)' Geographic latitude  (deg.):'
         READ  (*,*) AGEO
         WRITE (*,*)' Geographic longitude (deg.):'
         READ  (*,*) PHGE
         WRITE (*,*)' Altitude (km):'
         READ  (*,*) ALT
         RGEO (1) = (ALT*1.D+05+REARTH)*COS(DEGRAD*PHGE)
     &              *SIN(DEGRAD*(90.D+00-AGEO))
         RGEO (2) = (ALT*1.D+05+REARTH)*SIN(DEGRAD*PHGE)
     &              *SIN(DEGRAD*(90.D+00-AGEO))
         RGEO (3) = (ALT*1.D+05+REARTH)*COS(DEGRAD*(90.D+00-AGEO))
         CALL GEOGMG (RGEO,RGMG,4)
         HELP = SQRT (RGMG(1)**2+RGMG(2)**2)
         TGMG = RADDEG * ATAN2 (HELP,RGMG(3))
         ALAM = 90.D+00 - TGMG
         PHIM = RADDEG * ATAN2 (RGMG(2),RGMG(1))
      ELSE IF ( ICOOR .EQ. 2 ) THEN
         WRITE (*,*)' Geomagnetic latitude  (deg.):'
         READ  (*,*) ALAM
         WRITE (*,*)' Geomagnetic longitude (deg.):'
         READ  (*,*) PHIM
      END IF
      WRITE (*,*)' Geomagnetic cut-off acceptance  (GeV)'
      READ  (*,*) DPCO
*
      PCOV  = 59.6D+00/4.0D+00 * COS (DEGRAD*ALAM)**4
      DO I = 1,100
         ALA1  = ALAM+I
         ALA0  = ALAM-I
         PCOV1 = 59.6D+00/4.0D+00 * COS (DEGRAD*ALA1)**4
         PCOV0 = 59.6D+00/4.0D+00 * COS (DEGRAD*ALA0)**4
         WRITE (*,*) ' ALA0, ALAM, ALA1, PCOV0, PCOV, PCOV1',
     &                 ALA0, ALAM, ALA1, PCOV0, PCOV, PCOV1
         IF(ABS(PCOV1-PCOV).GT.DPCO .OR. ABS(PCOV0-PCOV) .GT.DPCO)THEN
            IOK = I
            GO TO 100
         END IF
      END DO
      STOP
100   CONTINUE
      DO J = 1,100
         ALA1  = ALAM+IOK-1+J*0.01D+00
         ALA0  = ALAM-IOK+1-J*0.01D+00
         PCOV1 = 59.6D+00/4.0D+00 * COS (DEGRAD*ALA1)**4
         PCOV0 = 59.6D+00/4.0D+00 * COS (DEGRAD*ALA0)**4
         WRITE (*,*) ' ALA0, ALAM, ALA1, PCOV0, PCOV, PCOV1',
     &                 ALA0, ALAM, ALA1, PCOV0, PCOV, PCOV1
         IF(ABS(PCOV1-PCOV).GT.DPCO .OR. ABS(PCOV0-PCOV) .GT.DPCO)THEN
            JOK = J
            GO TO 200
         END IF
      END DO
      STOP
200   CONTINUE
      WRITE(*,*)' Pcov0, Pcov, Pcov1, Dpco', PCOV0, PCOV, PCOV1, DPCO
      ALPHA = IOK - 1 + JOK * 0.01D+00
      WRITE(*,*)' Alpha (deg.):',ALPHA
      WRITE(*,*)' Enter topmost altitude for particle tracking (R_atmo)'
      WRITE(*,*)' Must be tailored on and consistent with your set-up '
      WRITE (*,'(A,E13.6)')
     &           ' Must be larger than top of atmosphere:', RATLA(1)
      READ (*,*)  RATMO
      RATLA(0) = RATMO
      BETA   = RADDEG * ACOS (REARTH/RATMO) + ALPHA
*  Be a bit careful:
      BETA   = BETA + 0.00001D+00
*     BETA   = BETA + 0.5D+00
      WRITE(*,*)' Beta (deg.):',BETA
      PHIMIN = -180.D+00
      PHIMAX = +180.D+00
      DELPHI = PHIMAX-PHIMIN
      PHSMMN = -180.D+00
      PHSMMX = +180.D+00
      DEPHSM = PHSMMX-PHSMMN
*
*      PI=ATAN2(0.D+00,-1.D+00)
      LOUT=7
*     OPEN (UNIT=LOUT,FILE='atmloc.geo',STATUS='UNKNOWN')
      CALL OAUXFI('atmloc.geo',LOUT,'UNKNOWN',IERR)
      IBODY=0
      IREG=0
      WRITE(LOUT,'(I5,I5,20X,''ATMOSPHERE, LAM='',F5.1,'' OME='',F6.1)')
     &      0,0,ALAM,PHIM
      IBODY   = IBODY+1
      CBLACK  = 'black   '
      CVACUUM = 'void    '
      WRITE (LOUT, 901) '* 1 black hole',
     &          'SPH', CBLACK, 0.D0, 0.D0, 0.D0, RATLA(-1)
      WRITE (LOUT, 901) '* 2 vacuum',
     &                'SPH',CVACUUM, 0.D0, 0.D0, 0.D0, RATLA(0)
      WRITE (LOUT, '(A)') '* atmosphere shells '
      DO J=1,NAIRSH
         IBODY=IBODY+1
         CNAME = 'lay000  '
         WRITE (CNAME(4:6), '(I3.3)') J
         WRITE (LOUT, 1001) 'SPH', CNAME, 0.D0, 0.D0, 0.D0, RATLA(J)
      END DO
      IBODY = IBODY+1
      CNAME = 'lay000  '
      WRITE (CNAME(4:6), '(I3.3)') NATMSH
      WRITE (LOUT, '(A)') '*  earth'
      WRITE (LOUT, 1001) 'SPH',CNAME, 0.D0, 0.D0, 0.D0, RATLA(NATMSH)
*
      IF ( ALAM .GE. 0.D+00 ) THEN
         IALAM    = +1
         ANGLE(1) = ALAM-BETA
         ANGLE(2) = ALAM-ALPHA
         ANGLE(3) = ALAM+ALPHA
         ANGLE(4) = ALAM+BETA
         ANGMAX   = 90.D+00-ALAM+ALPHA
         IF ( ALAM+ALPHA.GE. 88.D+00 ) THEN
            ANGMIN = 0.D+00
         ELSE
            ANGMIN = 90.D+00-ALAM-ALPHA
         END IF
      ELSE
         IALAM    = -1
         ANGLE(4) = ALAM-BETA
         ANGLE(3) = ALAM-ALPHA
         ANGLE(2) = ALAM+ALPHA
         ANGLE(1) = ALAM+BETA
         ANGMIN=90.D+00-ALAM-ALPHA
         IF ( ALAM-ALPHA.LE.-88.D+00 ) THEN
            ANGMAX = 180.D+00
         ELSE
            ANGMAX = 90.D+00-ALAM+ALPHA
         END IF
      END IF
*  Old (wrong for Lamda<30 deg)
*     ZATRC = REARTH/2.D+00
      ZATRC = RATMO * MIN(ABS(SIN(DEGRAD*ANGLE(2))),
     &                    ABS(SIN(DEGRAD*ANGLE(3)))) / 2.D+00
      ZBTRC = RATMO * 1.5D+00
*      OPEN(UNIT=33,FILE='atmloc.sur',STATUS='UNKNOWN')
      CALL OAUXFI('atmloc.sur', 33, 'UNKNOWN', IERR)

* BNV: To be consistent with gcrspe.f
* Despite being numerically the same as the above line this is
* how it is done in gcrspe.f and results in a different precisison
      ALMGMG = DEGRAD * ALAM
      ALPSCR = DEGRAD * ALPHA
      DPHSCR = DEGRAD * DELPHI
      DOMEGA = DPHSCR * ( COS (MAX(PIHALF-ALMGMG-ALPSCR,ZERZER))
     &                  - COS (MIN(PIHALF-ALMGMG+ALPSCR,PIPIPI)) )

      WRITE(33,'(A9,1P,G27.17)')' ALAM  : ',ALAM
      WRITE(33,'(A9,1P,G27.17)')' PHIM  : ',PHIM
      WRITE(33,'(A9,1P,G27.17)')' DOMEGA: ',DOMEGA
      WRITE(33,'(A9,1P,G27.17)')' ALPHA : ',ALPHA
      WRITE(33,'(A9,1P,G27.17)')' DELPHI: ',DELPHI
      SMPMIN=90.D+00-ALAM
      SMPMIN=MAX(0.D+00,SMPMIN-BETA)
      SMPMAX=90.D+00-ALAM
      SMPMAX=MIN(180.D+00,SMPMAX+BETA)
      DOMSMP=DEPHSM * DEGRAD *(COS(DEGRAD*SMPMIN)-COS(DEGRAD*SMPMAX))
      WRITE(33,'(A9,1P,G27.17)')' BETA  : ',BETA
      WRITE(33,'(A9,1P,G27.17)')' DOMSMP: ',DOMSMP
      WRITE(33,'(A9,1P,G27.17)')' SMPMIN: ',SMPMIN
      WRITE(33,'(A9,1P,G27.17)')' SMPMAX: ',SMPMAX
      WRITE(33,'(A9,1P,G27.17)')' PHSMMN: ',PHSMMN
      WRITE(33,'(A9,1P,G27.17)')' PHSMMX: ',PHSMMX
      DO J=1,NATMSH
         AREA = DOMEGA*RATLA(J)**2
         WRITE(33,'(A9,I5,1P,G25.15)')' AREA    ',J,AREA
      END DO
*  Set up the cone which should cut the "extra" atmosphere
*  zones:
      DIFRAD = MAX(1.0000001D+00*REARTH*(ONEONE-COS(DEGRAD*ALPHA)),
     &             REARTH-RATMO*COS(DEGRAD*BETA))
*  This the distance from the earth centre to the cone surface
      RADCON = MIN (RATMO*COS(DEGRAD*BETA),REARTH-DIFRAD)
      BETEFF = RADDEG * ACOS(RADCON/RATMO)
      IF ( RADCON .GE. REARTH ) STOP ' RADCON'
      IF ( ABS (ANGMIN) .GT. 0.D+00 .AND. ABS(ANGMAX-180.D+00) .GT.
     &     0.D+00 ) THEN
         LPLANE = .FALSE.
         IF ( ABS (ALAM) .GT. 0.1D+00 ) THEN
            LCYL  = .FALSE.
            THETA = ABS (ALAM)
*  This is the cone radius at Z=0:
            RADZ0 = RADCON / COS (DEGRAD*THETA)
            ZVRTX = RADZ0  / TAN (DEGRAD*THETA)
            IF ( ZVRTX .GT. 1.2D+00*RATMO ) THEN
               ZSMALL = MIN(0.92D+00*ZVRTX,2.D+00*RATMO)
            ELSE IF ( ZVRTX .GT. 1.05D+00*RATMO ) THEN
               ZSMALL = 0.98D+00*ZVRTX
            ELSE
               ZSMALL = MIN ( 0.99D+00*ZVRTX, 0.99D+00*REARTH )
            END IF
            RSMALL = (ZVRTX-ZSMALL) * TAN(DEGRAD*THETA)
*  Set the cone height such that it will be 2 Ratmo large:
            HEICON = (2.D+00*RATMO-RSMALL)/TAN(DEGRAD*THETA)
            RLARGE = 2.D+00*RATMO
            HEICON = -IALAM*HEICON
            ZSMALL = +IALAM*ZSMALL
         ELSE
            LCYL = .TRUE.
            RCYL = RADCON
         END IF
      ELSE
         LPLANE = .TRUE.
         IF ( ALAM .GT. 0.D+00 ) THEN
            ZPLANE = +RADCON
         ELSE
            ZPLANE = -RADCON
         END IF
      END IF
      IAOK = 0
      DO IA = 1,4
         IF ( ANGLE(IA) .GT. 0.D+00 ) THEN
            IF ( ANGLE(IA) .GE. 88.D+00 ) THEN
               IASIGN (IA) = 0
               ANGLE  (IA) =-100000.D+00
            ELSE
               IAOK        = IAOK + 1
               IASIGN (IA) = +1
               ANGLE  (IA) = 90.D+00 - ANGLE(IA)
            END IF
         ELSE
            IF ( ANGLE(IA) .LE. -88.D+00 ) THEN
               IASIGN (IA) = 0
               ANGLE  (IA) =-10000.0D+00
            ELSE
               IAOK        = IAOK + 1
               IASIGN (IA) = -1
               ANGLE  (IA) = 90.D+00 + ANGLE(IA)
            END IF
         END IF
         IF ( IASIGN (IA) .NE. 0 ) THEN
            IF ( IA.NE.1 .AND. IA.NE.4 ) THEN
               TANTHE = TAN (DEGRAD*ANGLE(IA))
               IBODY  = IBODY+1
               WRITE(LOUT,'(A,I4)') '*  cutting cone:',IA
               CNAME = 'cutcone '
               WRITE(CNAME(8:8), '(I1)') IA
               WRITE(LOUT,1002)'TRC', CNAME, ZERZER , ZERZER,
     &                          IASIGN(IA)*ZATRC, ZERZER, ZERZER,
     &                          IASIGN(IA)*(ZBTRC-ZATRC), TANTHE*ZATRC,
     &                          TANTHE*ZBTRC
               IBTRC(IA) = IBODY*IALAM*IASIGN(IA)
            END IF
         ELSE
            IBTRC(IA) = 5000
         END IF
      END DO
*  Cutting special cone:
      IBODY = IBODY+1
      CNAME = 'scutcone'
      WRITE(LOUT,'(A2,I4,A23)') '* ',IBODY,') special cutting body:'
      IF ( LCYL ) THEN
         WRITE(LOUT,1501)'ZCC', CNAME, 0.D+00, 0.D+00, RCYL
         IBSPC = IBODY
      ELSE IF ( LPLANE ) THEN
         WRITE(LOUT,1501)'XYP', CNAME, ZPLANE
         IBSPC = IBODY
      ELSE
         WRITE(LOUT,1002)'TRC', CNAME, 0.D+00, 0.D+00, ZSMALL,
     &                                 0.D+00, 0.D+00, HEICON,
     &                                 RSMALL, RLARGE
         IBSPC=IBODY
      END IF
      WRITE(LOUT,'(A)') '  END'
*  Now regions:
      WRITE(LOUT,'(A)') '*  Regions:'
      DO J=1,NAIRSH
         CNAME ='Air000  '
         WRITE ( CNAME(4:6), '(I3.3)') J
** keep track of signs: we have to "multiply" bodys by IALAM and
** IASIGN
         CFIELD1 = '+lay000   '
         CFIELD  = '-lay000   '
         WRITE ( CFIELD1 (5:7), '(I3.3)') J
         WRITE ( CFIELD  (5:7), '(I3.3)') J+1
         CBODY2  = '+cutcone2 '
         CBODY3  = '-cutcone3 '
         IF ( IALAM * IASIGN (2) .LT. 0 ) CBODY2 = '-cutcone2 '
         IF ( IALAM * IASIGN (3) .LT. 0 ) CBODY3 = '+cutcone3 '
         IREG    = IREG + 1
         WRITE(LOUT,'(A,I4)')'*  Atmo. scoring layer:',J
         IF ( IAOK .GE. 3 ) THEN
            WRITE(LOUT,8301) CNAME , 5, CFIELD, CFIELD1,
     &                       CBODY2, CBODY3
*            WRITE(LOUT,8300)IREG,'    5','  ',J+2,'  ',-(J+3),'  ',
*     &                     +IBTRC(2),'  ',-IBTRC(3)
         ELSE
*            WRITE(LOUT,8300)IREG,'    5','  ',J+2,'  ',-(J+3),'  ',
*     &                     +IBTRC(2)
            WRITE(LOUT,8301) CNAME, 5, CFIELD, CFIELD1, CBODY2
         END IF
      END DO
      IREG = IREG + 1
      WRITE(LOUT,'(A)')'*  Earth scoring layer:'
      CFIELD1 ='+lay000   '
      WRITE ( CFIELD1 (5:7), '(I3.3)') NAIRSH+1
      IF ( IAOK .GE. 3 ) THEN
         WRITE(LOUT,8301) 'Earth   ', 5, CFIELD1, CBODY2, CBODY3
      ELSE
         WRITE(LOUT,8301) 'Earth   ', 5, CFIELD1, CBODY2
      END IF
      IREG = IREG + 1
      WRITE(LOUT,'(A)')'*  External vacuum (outside the atmo.):'
*      WRITE(LOUT,8300)IREG,'    5','  ',+2,'  ',-3,'  ',
*     &                -IBSPC
      WRITE (LOUT,8301) 'Vacuum  ', 5, '+void     ', '-lay001   ',
     &  '-scutcone '
      CBODY2 = '+cutcone2 '
      CBODY3 = '-cutcone3 '
      IF ( IALAM * IASIGN (2) .GT. 0 ) CBODY2 = '-cutcone2 '
      IF ( IALAM * IASIGN (3) .GT. 0 ) CBODY3 = '+cutcone3 '
      CBSPC  = '-scutcone '
      WRITE(LOUT,'(A)')'*  Atmo. extra (side) layers:'
      DO J=1,NAIRSH
         IREG = IREG + 1
         CNAME ='Side000 '
         CFIELD1 ='+lay000   '
         CFIELD = '-lay000   '
         WRITE ( CFIELD1 (5:7), '(I3.3)') J
         WRITE ( CFIELD (5:7), '(I3.3)') J+1
         WRITE (CNAME(5:7),'(I3.3)' ) J
         WRITE(LOUT,'(A,I4)')'*  Atmo. extra (side) layer n. ',J
         IF ( IAOK .GE. 4 ) THEN
            WRITE(LOUT,8302) CNAME, -5,'|', CFIELD, CFIELD1,
     &                     CBSPC, CBODY2
            WRITE(LOUT,8401)            '|', CFIELD, CFIELD1,
     &                     CBODY3, CBSPC
         ELSE IF ( IAOK .GE. 3 ) THEN
            WRITE(LOUT,8302) CNAME, -5,'|', CFIELD, CFIELD1,
     &                     CBSPC, CBODY2
            WRITE(LOUT,8401)            '|', CFIELD, CFIELD1,
     &                     CBODY3, CBSPC
         ELSE
            WRITE(LOUT,8301) CNAME, 5, IREG, CFIELD, CFIELD1,
     &                      CBSPC, CBODY2
         END IF
      END DO
      IREG = IREG + 1
      WRITE(LOUT,'(A)')'*  Earth black hole:'
      CNAME =  'EarBLK  '
      CFIELD ='+lay000   '
      WRITE ( CFIELD (5:7), '(I3.3)') NAIRSH+1

      IF ( IAOK .GE. 4 ) THEN
         WRITE(LOUT,8302) CNAME, -5,'|', CFIELD, CBODY2
         WRITE(LOUT,8401) '|', CFIELD,  CBODY3
      ELSE
         WRITE(LOUT,8302)CNAME, -5, CFIELD, CBODY2
      END IF
      IREG    = IREG + 1
      CNAME   ='AtmBLK  '
      CFIELD  ='-lay000   '
      CFIELD1 ='+lay001   '
      CBSPC   = '+scutcone '
      WRITE ( CFIELD (5:7), '(I3.3)') NAIRSH+1

      WRITE(LOUT,'(A)')'*  Atmo. (side) black hole:'
      WRITE(LOUT,8301)CNAME, 5, CFIELD, CFIELD1, CBSPC
      IREG = IREG + 1
      CNAME =  'VacBLK  '
      WRITE(LOUT,'(A)')'*  Vacuum (side) black hole:'
      WRITE(LOUT,8301)CNAME, 5, '+void     ', '-lay001   ', CBSPC
      IREG = IREG + 1
      WRITE(LOUT,'(A)')'*  External black hole:'
      WRITE (LOUT,8301) 'BlackH  ', 5, '+black    ', '-void     '
      WRITE(LOUT,'(A)') '  END'
*
*      OPEN (UNIT=31,FILE='atmlocmat.cards',STATUS='UNKNOWN')
      CALL OAUXFI('atmlocmat.cards', 31, 'UNKNOWN', IERR)
      WRITE (31,'(A)') '* Assignmat cards:'
      DO N = 1, NAIRSH
         CFIELD=' AIR000   '
         CFIELD1=' Side000  '
         WRITE ( CFIELD(5:7),'(I3.3)') N
         WRITE ( CFIELD1(6:8),'(I3.3)') N
         WRITE (31,801) CFIELD, CFIELD1, ZERZER
      END DO
      CFIELD  = ' BLCKHOLE '
      CFIELD1 = ' EarBLK   '
      WRITE (31,801) CFIELD, CFIELD1, ZERZER
      CFIELD1 = ' VacBLK   '
      WRITE (31,801) CFIELD, CFIELD1, ZERZER
      CFIELD1 = ' AtmBLK   '
      WRITE (31,801) CFIELD, CFIELD1, ZERZER
*
 801  FORMAT('ASSIGNMAT ',A10,A10,20X,F6.1)
 901  FORMAT(A,/,2X,A3,1X,A8,1P,4(1x,E11.4) )
C1000 FORMAT(2X,A3,1X,I4,3(1P,D22.15),2(1(/,1P,D32.15),2(1P,D22.15)))
 1001 FORMAT( 2X,A3,1X,A8, 1P, 3(1x,E11.4), 1X, D22.15)
 1002 FORMAT( 2X,A3,1X,A8, 1P, 2(1x,E11.4), 1X, D22.15,
     &        2( /,1P,D32.15 ,2(1P,1X,D22.15) ))
C1500 FORMAT(2X,A3,1X,I4,3(1P,D22.15))
 1501 FORMAT(2X,A3,1X,A8,3(1P,D22.15))
C8300 FORMAT(2X,I3,A5,9(A2,I5))
 8301 FORMAT(2X,A8,I5,2X,5A10)
 8302 FORMAT(2X,A8,I5,2X,A1,1X,5A10)
C8400 FORMAT(2X,A3,A5,9(A2,I5))
 8401 FORMAT(2X,15X,A1,1X,5A10)
      STOP
      END
