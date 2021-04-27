C-----------------------------------------------------------------------
C     1�����̏����M�ʂ��v�Z����
C     LATEST REVISION   2012.08.03
C-----------------------------------------------------------------------
      SUBROUTINE EXTRC2(NHR,IPEAK,ISEAS,NAZ,IOPTG,NZ,IOPVG,SMRT1,SMRT2,
     -   VOAG,LCG,CLDG,NWD,WD,REFWD,P0,NSTP,VFLOW,EXCAP,SPCAP,RMMX,RMMN,
     -   NITER,NUOT1,ID,MDW,MODE,IOUT,LSZSPC,IBECS,NUOB)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

      PARAMETER ( NSL=2 )              !     ���M�Ɛ��M
C     ����
      INTEGER     NHR                  ! I   1���̃X�e�b�v��
      INTEGER     IPEAK                ! I   1:�s�[�N�v�Z���[�h�A0:�V�~�����[�V�������[�h
      INTEGER     ISEAS                ! I   1:�Ċ��A2:�~���A3:���Ԋ�
      INTEGER     NAZ                  ! I   �]�[������\�킷�������@
      INTEGER     IOPTG(NAZ,NHR)       ! I   �󒲉^�]��ԃt���O�A=0:��~���A
                                       !     =1:�^�]���A=2:�N���A=3:��~
      INTEGER     NZ                   ! I   ���݂̃O���[�v�̃]�[����
      INTEGER     IOPVG(NAZ,NHR)       ! I   �O�C������ԃt���O�A=0:�J�b�g��
                                       !     =1:�������A=2:�����J�n�A=3:������~
      REAL        SMRT1(NAZ,NHR)       ! I   �ʐς������Ȃ����ʂ���̗�[����
      REAL        SMRT2(NAZ,NHR)       ! I   INFL�̋z�M�����W��
      REAL        VOAG(NZ)             ! I   �������̊O�C��
      INTEGER     LCG(NZ)              ! I   XMQ�z���SPAC�f�[�^�ւ̃|�C���^�iL�j
      REAL        CLDG(NAZ,NHR,NSL)    ! I   ��[����
      INTEGER     NWD                  ! I   WD�̐������@�i=7�j
      REAL        WD(NWD,NHR)          ! I   �O�E�C�ہi������x����̕΍��ł͂Ȃ��j
      REAL        REFWD(NSL)           ! I   ������x
      REAL        P0(NAZ,0:1,NHR,NSL)  ! I   �u���~�M�����W���i�z�M����鑤�����j
                                       !     ��2�Y��=0:�񓙕ӎO�p
                                       !     ��2�Y��=1:�E�����p�񓙕ӎO�p
      INTEGER     NSTP                 ! I   �\�M���ԁi�X�e�b�v�j
      REAL        VFLOW(NAZ,NAZ,NHR)   ! I   ��1�Y���ڂ̃]�[�������2�Y���ڂ̃]�[���ւ̗���
                                       !     ���ʁi�̐ϗ��ʁA0�ȏ�A�Ίp����0�Ƃ���j
      REAL        EXCAP(NAZ,NSL)       ! I   �e�X�y�[�X�̑��u�e�ʁi��p�A0�ȏ�j
      REAL        SPCAP(NAZ,NSL)       ! I   �e�X�y�[�X�̑��u�e�ʁi���M�A0�ȏ�j
      REAL        RMMX(NAZ,NSL)        ! I   �e�X�y�[�X�̐ݒ艷���x���
      REAL        RMMN(NAZ,NSL)        ! I   �e�X�y�[�X�̐ݒ艷���x����
                                       !     �iRMMX,RMMN�͊�����x����̕΍��ł͂Ȃ��j
      INTEGER     NITER                ! I   �����v�Z�ɂ����鋖�e�J��Ԃ��v�Z��
      INTEGER     NUOT1                ! I   �e�L�X�g�o�̓t�@�C���̑��u�ԍ��i�ŏ��̑��u�ԍ��j
      INTEGER     ID(3)                ! I   �N�E���E���i�o�͏��j
      INTEGER     MDW                  ! I   �j���i�o�͏��j
      INTEGER     MODE                 ! I   =1:�����A=2:�{�v�Z�A=3:�ŏI��
      INTEGER     IOUT                 ! I   =0:�ȈՏo��(1h1�s)�A=1:�ڍ׏o��(1h2�s)
      INTEGER     LSZSPC(0:4)          ! I   XMQ�z��̂����A(0):SPAC, (1):OWAL, (2):IWAL,
                                       !     (3):WNDW, (4):INFL �̕ϐ��̐�
      INTEGER     IBECS                ! I   BECS�ւ̎󂯓n���f�[�^���A=1:�o�͂���, 0:�o�͂��Ȃ�
      INTEGER     NUOB                 ! I   BECS�ւ̎󂯓n���t�@�C���p���u�ԍ�

C     ���[�J���ϐ�
      REAL        CRHO(NSL)            !     ��C�̗e�ϔ�M
                                       !     �i���M�̏ꍇ�͖��x�ɏ������M���|�������́j
      INTEGER     NTRM(NSL)            !     �~�M�����W���̍���
      PARAMETER ( MTRM=2 )             !     �~�M�����W���̍���
      INTEGER     LSTP(MTRM,NSL)       !     �~�M�����W���i�u�����������j�ւ̃|�C���^
      INTEGER     LSTQ(MTRM,NSL)       !     �~�M���ׁi�u�����������j�ւ̃|�C���^
      INTEGER     ISL                  !     =1:���M�A=2:���M
      INTEGER     KSTP                 !     �\�M�J�n��̌o�߃X�e�b�v
      INTEGER     JHR                  !     ����
      INTEGER     IWARM                !     ���̃X�e�b�v���\�M��(=1)������ȊO(=0)��
      INTEGER     JHR0                 !     �\�M���J�n��������
      INTEGER     ICHNG                !     ���̃X�e�b�v�ł����ꂩ�̃]�[���ŊK�i��̕ω�
                                       !     �i�󒲔���A�O�C�ʂ̕ω��j�����������ǂ���
      INTEGER     ISTOP                !     ���̃X�e�b�v�őS�Ẵ]�[���ŋ󒲒�~�ƂȂ邩
      PARAMETER ( MSTP=24 )            !     ���e�ő�\�M�X�e�b�v��
      INTEGER     IDLT(MSTP)           !     �\�M�J�n��A�e�X�e�b�v�ɂ����Ē��㎺�����x��
                                       !     =0�F���m���Ƃ��Ȃ��A=1�F���m���Ƃ���
      INTEGER     IZ                   !     ���݂̃]�[�������]�[���ڂ��i1<=IZ<=NZ�j
      INTEGER     IREP                 !     ���݂̎����X�e�b�v�̒��O�����ォ
                                       !     =0�F���O���邢�͓񓙕ӎO�p�A=1�F����
      PARAMETER ( MZ=20 )              !     1�O���[�v������̍ő�]�[����
      PARAMETER ( MHR=24 )             !     �������@�i=NHR�j
      REAL        RMSET(MZ)            !     �e�]�[���̈�菜���M�ʌv�Z���ݒ艷���x
      REAL        VOAWK(MZ,0:1,MHR)    !     �e�]�[���̊O�C�ʁi�̐ϗ��ʁA0�ȏ�j
      REAL        OATX(MZ,MHR)         !     �����O�C�����x�i�O���@�l���A������x�����
                                       !     �΍��ł͂Ȃ��j
      INTEGER     IOPTWK(MZ)           !     �e�]�[���E�e�i�̋󒲉^�]��ԁi=1:�^�]�A=0:��~�j
      INTEGER     IWARM1               !     ���݂̒i�i���O��������ʂ���j���\�M���ԑт�
      REAL        FIXEDL(MZ)           !     �e�]�[���́A���m�ϐ��Ɉˑ����Ȃ��Œ藬���M��
      INTEGER     LSTPWK
      INTEGER     LSTQWK
      REAL        PV(0:MSTP-1,MZ)      !     �\�M�J�n��e�X�e�b�v�ɂ�����~�M�����W��
                                       !     �i�񓙕ӎO�p�A�z�M�������j
      REAL        PR(0:MSTP,MZ)        !     �\�M�J�n��e�X�e�b�v�ɂ�����~�M�����W��
                                       !     �i�E�����p�O�p�A�z�M�������j
                                       !     PV(0),PR(0)�͌��ݎ����̂���
      INTEGER     I
      INTEGER     J
      INTEGER     NSIZE                !     �������̐��i���m���̐��j
      INTEGER     IPS(0:MSTP)          !     �\�M�J�n��A�e�����X�e�b�v�̎������x���A���m�x�N
                                       !     �g���̉������ڂ���n�܂邩�i=IPS+1�����ڂ���j
      INTEGER     LMODE(MZ,0:1,MHR,NSL)!     �e�]�[���̕��׏�ԃ��[�h�i-2�F�g�[�ߕ���,
                                       !     -1�F�g�[�y����,0�F������,1�F��[�y����,
                                       !     2�F��[�ߕ���,9�F��~�j
      REAL        AN(MZ,0:1,MHR,NSL)   !     �e�]�[���̑��u�����M��
      REAL        RM(MZ,0:1,MHR,NSL)   !     �e�]�[���̎������x�i������x����̕΍��j
      REAL        EOA                  !     �e�]�[���̊O�C���ׁi���������x��j
      REAL        RN(MZ,0:1,MHR,NSL)   !     �e�]�[���̎������M��
      REAL        AMRT(MZ,0:1,MHR)     !     MRT
      PARAMETER ( NA=100 )             !     ���e�ő���������i���m���̐��j
      REAL*8      AA(NA,NA)            !     Work Array �i�A�����������ӌW���s��j
      REAL*8      BB(NA)               !     Work Array �i�A���������E�ӌW���x�N�g���j
      INTEGER     IP(NA)               !     Work Array
      INTEGER     INFO
      REAL*8      WK(MTRM)             !     Work Array
      REAL        FF
      REAL        EOUT(4,NSL)
      REAL        EMRT
      INTEGER     LMODEB(NSL)
      INTEGER     LC
      INTEGER     NSTP1
      INTEGER     L
      INTEGER     IWL
      INTEGER     JZ
      INTEGER     ISTAT

      DATA CRHO   /0.288, 0.720/       !     �P�ʈˑ�
      DATA NTRM   /2, 1/
      DATA LSTP   /17,20, 27,0/        !     ���M��2���ڂ̓_�~�[�iXMQ�z��ύX�����Ӂj
      DATA LSTQ   /23,24, 30,0/        !     ���M��2���ڂ̓_�~�[�iXMQ�z��ύX�����Ӂj

      IF(NZ.GT.NAZ) CALL ERROR2(139,2)
      IF(NZ.GT.MZ) CALL ERROR2(138,2)
      IF(NSTP.GT.MSTP) CALL ERROR2(137,2)
      IF((IPEAK.EQ.1).AND.(NSTP.LT.0)) CALL ERROR2(136,2)
      IF(NHR.NE.MHR) CALL ERROR2(135,2)

      DO ISL = 1, NSL   ! ���M�E���M loop

         DO IZ = 1, NZ
            IF ( ISEAS.EQ.2 ) THEN
               RMSET(IZ) = RMMN(IZ,ISL)
            ELSE
               RMSET(IZ) = RMMX(IZ,ISL)
            END IF
         END DO
         KSTP = NSTP
         NSTP1 = NSTP

         DO JHR = 1, NHR   ! �������[�v

C           �����O�C�����x�i�O���@�l���j (OUT: OATX)
            DO IZ = 1, NZ
               L = M(LCG(IZ)+202)   ! OAHU�f�[�^�ւ̃|�C���^(L)
               IF ( L.EQ.0 ) THEN   ! OAHU�f�[�^���w�肳��Ă��Ȃ��ꍇ
                  OATX(IZ,JHR) = WD(ISL,JHR)
               ELSE
                  OATX(IZ,JHR) = X(L+80+(ISL-1)*25+JHR)
               END IF
            END DO

C           �\�M�X�e�b�v���i���̏ꍇ�̗\�M��X�e�b�v���j�iOUT: IWARM, KSTP�j
            IF ( (IPEAK.EQ.1).AND.(ISEAS.LE.2).AND.   ! �s�[�N�v�Z���[�h�ŉĊ��E�~����
     -           (IOPTG(1,JHR).NE.0) ) THEN   ! ���A�󒲉ғ��X�e�b�v�ł���

               IF ( (IOPTG(1,JHR).EQ.2).AND.(NSTP.GE.1) ) THEN
               ! ��菜���M�ʂ̌v�Z�X�e�b�v�ł���i�\�M�J�n�����j
               ! ���̃O���[�v�̑S�Ẵ]�[���ŗ\�M���Ԃ������łȂ��ƃ_��
                  IF ( JHR.EQ.NHR ) THEN   ! 24��(0��)�̋N���ɑ΂���s�[�N�v�Z�͖��Ή�
                     CALL ERROR2(120,1)
                  ELSE
                     KSTP = 0
                     IWARM = 1
                     JHR0 = JHR   ! �\�M�J�n�����̋L�^
                     NSTP1 = NSTP
                     DO J = 1, NSTP-1
                        IF ( JHR+J.EQ.NHR ) THEN   ! �\�M�I���O��24���ɒB����
                           CALL ERROR2(121,0)
                           NSTP1 = J
                           GO TO 100
                        ELSE
                           DO IZ = 1, NZ
                              IF ( IOPTG(IZ,JHR+J).EQ.3 ) THEN
                              ! �\�M�I���O�ɋ󒲒�~����
                                 CALL ERROR2(122,0)
                                 NSTP1 = J
                                 GO TO 100
                              END IF
                           END DO
                        END IF
                     END DO
  100                CONTINUE
                  END IF
               ELSE IF ( (KSTP.GE.0).AND.(KSTP.LT.NSTP1) ) THEN
               ! ��菜���M�ʂ̌v�Z�X�e�b�v�ł���i�\�M�J�n�����ȊO�j
                  KSTP = KSTP + 1
                  IWARM = 1
               ELSE
               ! �V�~�����[�V�����v�Z���s���X�e�b�v�ł���
                  IWARM = 0
               END IF

            ELSE
            ! �V�~�����[�V�����v�Z���s���X�e�b�v�ł���
               IWARM = 0
            END IF

C           �����ꂩ�̃]�[���ŊK�i��ω��������邩(OUT: ICHNG, ISTOP, IDLT)
            ICHNG = 0
            ISTOP = 1
            DO IZ = 1, NZ
               IF ( (IOPTG(IZ,JHR).GE.2).OR.   ! �󒲋N�����邢�͒�~�����A
     -              (IOPVG(IZ,JHR).GE.2) ) THEN   ! �O�C�����J�n�E�I������
                  ICHNG = 1
               END IF

               IF ( IOPTG(IZ,JHR).NE.3 ) THEN
                  ISTOP = 0   ! �S�Ẵ]�[���ŋ󒲒�~�ƂȂ�킯�ł͂Ȃ�
               END IF
            END DO

            IF ( (IWARM.EQ.1).AND.(KSTP.GE.1) ) THEN
               IF ( (ICHNG.EQ.1).AND.(ISTOP.EQ.0) ) THEN
                  IDLT(KSTP) = 1
               ELSE
                  IDLT(KSTP) = 0
               END IF
            END IF

            DO IREP = 0, 1   ! ���O�E���� loop

               DO IZ = 1, NZ
C                 �O�C�����ʁiVOAWK(NZ,IREP,JHR)�j
                  IF ( (IOPVG(IZ,JHR).EQ.2).AND.(IREP.EQ.0) ) THEN
                     VOAWK(IZ,IREP,JHR) = 0.0
                  ELSE IF ( (IOPVG(IZ,JHR).EQ.3).AND.(IREP.EQ.1) ) THEN
                     VOAWK(IZ,IREP,JHR) = 0.0
                  ELSE IF (IOPVG(IZ,JHR).GE.1) THEN
                     VOAWK(IZ,IREP,JHR) = VOAG(IZ)
                  ELSE
                     VOAWK(IZ,IREP,JHR) = 0.0
                  END IF

C                 �󒲉^�]��ԁiIOPTWK(NZ))
                  IF ( (IOPTG(IZ,JHR).EQ.2).AND.(IREP.EQ.0) ) THEN
                     IOPTWK(IZ) = 0
                  ELSE IF ( (IOPTG(IZ,JHR).EQ.3).AND.(IREP.EQ.1) ) THEN
                     IOPTWK(IZ) = 0
                  ELSE IF ( IOPTG(IZ,JHR).GE.1 ) THEN
                     IOPTWK(IZ) = 1
                  ELSE
                     IOPTWK(IZ) = 0
                  END IF
               END DO

C              ��菜���M�ʌv�Z�����iIWARM1�j
               IF ( IWARM.EQ.1 ) THEN
                  IF  ( (KSTP.EQ.0).AND.(IREP.EQ.0) ) THEN
                     IWARM1 = 0
                  ELSE IF ( (KSTP.EQ.NSTP1).AND.
     -               (IDLT(KSTP).EQ.0).AND.(IREP.EQ.1) ) THEN
                     IWARM1 = 0
                  ELSE
                     IWARM1 = 1
                  END IF
               ELSE
                  IWARM1 = 0
               END IF

C              FIXEDL(NZ)�̃Z�b�g
               DO IZ = 1, NZ
                  FIXEDL(IZ) = CLDG(IZ,JHR,ISL)
     -            + CRHO(ISL)*VOAWK(IZ,IREP,JHR)
     -               *( OATX(IZ,JHR) - REFWD(ISL) )
                  DO I = 1, NTRM(ISL)
                     FIXEDL(IZ) = FIXEDL(IZ) + X(LCG(IZ)+LSTQ(I,ISL))
                  END DO
                  IF ( ISL.EQ.1 ) THEN
                     L = LCG(IZ) + LSZSPC(0)
                     DO IWL = 1, 9999
                        CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                        IF ( ISTAT.NE.1 ) THEN
                           GO TO 200
                        ELSE
                           FIXEDL(IZ) = FIXEDL(IZ) + X(L+11) + X(L+12)
                           L = L + LSZSPC(M(L))
                        END IF
                     END DO
                     CALL ERROR2(133,2)
                  END IF
  200             CONTINUE

                  ! ���O�̎������x�ϓ��ɂ�钼��̗����M�����Z����
                  IF ( ( (IWARM1.EQ.0).AND.(IREP.EQ.1) ).OR.
     -                 ( (IWARM1.EQ.1).AND.(KSTP.EQ.0) ) ) THEN
                     IF(IREP.EQ.0) CALL ERROR2(134,2)
                     FIXEDL(IZ) = FIXEDL(IZ)
     -               - ( P0(IZ,0,JHR,ISL) - P0(IZ,1,JHR,ISL) )
     -                  *RM(IZ,0,JHR,ISL)

                     IF ( ISL.EQ.1 ) THEN
                        L = LCG(IZ) + LSZSPC(0)
                        DO IWL = 1, 9999
                           CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                           IF ( ISTAT.NE.1 ) THEN
                              GO TO 300
                           ELSE
                              FIXEDL(IZ) = FIXEDL(IZ) + ( X(L+3) -
     -                           X(L+4) )*RM(JZ,0,JHR,ISL)
                              L = L + LSZSPC(M(L))
                           END IF
                        END DO
                     END IF
  300                CONTINUE

                  END IF
               END DO

C              ��菜���M�ʂ��v�Z����i�ł���
               IF ( IWARM1.EQ.1 ) THEN

C                 �O�����iOUT: PV(0:NSTP1-1,NZ), PR(0:NSTP1,NZ), �Y��0�������j
                  IF ( KSTP.EQ.0 ) THEN
                     DO IZ = 1, NZ
                        DO J = 1, NSTP1
                           PR(J,IZ) = 0.0
                           IF ( J.LE.NSTP1-1 ) THEN
                              PV(J,IZ) = 0.0
                           END IF
                           DO I = 1, NTRM(ISL)
                              LSTPWK = LCG(IZ) + LSTP(I,ISL)
                              PR(J,IZ) = PR(J,IZ)
     -                           + X(LSTPWK+1)*X(LSTPWK+2)**(J-1)
                              IF ( J.LE.NSTP1-1 ) THEN
                                 PV(J,IZ) = PV(J,IZ) + X(LSTPWK)
     -                                 *X(LSTPWK+2)**(J-1)
                              END IF
                           END DO
                        END DO
                     END DO

                     ! �W���s���0�N���A
                     DO I = 1, NA
                        DO J = 1, NA
                           AA(I,J) = 0.0D0
                        END DO
                        BB(I) = 0.0D0
                     END DO
                     IPS(KSTP) = NZ
                     NSIZE = 0
                  END IF   ! �O����

C                 �������ɌW�������Z����
                  IF ( (IREP.EQ.0).OR.(ICHNG.EQ.1) ) THEN
                     DO IZ = 1, NZ
                        PR(0,IZ) = P0(IZ,1,JHR,ISL)
                        PV(0,IZ) = P0(IZ,0,JHR,ISL)
                        CALL COEFFP(IZ,KSTP,NZ,IREP,NSTP1,IDLT,
     -                  VFLOW(1,IZ,JHR),PV(0,IZ),PR(0,IZ),CRHO(ISL),
     -                  VOAWK(IZ,IREP,JHR),FIXEDL(IZ),RMSET,REFWD(ISL),
     -                  ISL,LCG(IZ)+LSZSPC(0),LSZSPC,NA,NSIZE,AA,BB,IPS)
                     END DO
                  END IF

C                 �\�M�J�n���O�܂ł̎������x�ϓ��ɂ��~�M���ׂ��X�V����
                  IF ( IREP.EQ.1 ) THEN
                     DO IZ = 1, NZ
                        DO J = 1, NTRM(ISL)
                           LSTPWK = LCG(IZ) + LSTP(J,ISL)
                           LSTQWK = LCG(IZ) + LSTQ(J,ISL)
                           IF ( KSTP.EQ.0 ) THEN
                              X(LSTQWK) = X(LSTPWK+2)*X(LSTQWK)
     -                        - ( X(LSTPWK) - X(LSTPWK+1) )
     -                           *RM(IZ,0,JHR,ISL)
                           ELSE
                              X(LSTQWK) = X(LSTPWK+2)*X(LSTQWK)
                           END IF
                        END DO

                        IF ( ISL.EQ.1 ) THEN
                           L = LCG(IZ) + LSZSPC(0)
                           DO IWL = 1, 9999
                              CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                              IF ( ISTAT.NE.1 ) THEN
                                 GO TO 400
                              ELSE
                                 IF ( KSTP.EQ.0 ) THEN
                                    X(L+11) = X(L+7)*X(L+11)
     -                                + (X(L+5)-X(L+6))*RM(JZ,0,JHR,ISL)
                                    X(L+12) = X(L+10)*X(L+12)
     -                                + (X(L+8)-X(L+9))*RM(JZ,0,JHR,ISL)
                                 ELSE
                                    X(L+11) = X(L+7)*X(L+11)
                                    X(L+12) = X(L+10)*X(L+12)
                                 END IF
                                 L = L + LSZSPC(M(L))
                              END IF
                           END DO
                        END IF
  400                   CONTINUE

                     END DO
                  END IF

C                 �\�M�I���i�ł��������������
                  IF ( (KSTP.EQ.NSTP1).AND.
     -               ( (IDLT(KSTP).EQ.0).OR.(IREP.EQ.1) ) ) THEN
                     IF(NSIZE.GT.NA) CALL ERROR2(124,1)

C                    ������������
                     CALL DGESV(NSIZE,1,AA,NA,IP,BB,NA,INFO)
                     IF(INFO.NE.0) CALL ERROR2(132,2)

C                    �\�M�I�����̌㏈���i���ד��̒��o�ƒ~�M���ׂ̍X�V�j
                     CALL POSTP(MZ,NZ,LCG,NSTP1,IDLT,JHR0,NHR,NSIZE,BB,
     -               IPS,CRHO(ISL),VOAWK,OATX,REFWD(ISL),ISL,
     -               RMSET,NTRM(ISL),LSTP(1,ISL),LSTQ(1,ISL),
     -               NAZ,VFLOW,SMRT1,SMRT2,LSZSPC,
     -               LMODE(1,0,1,ISL),AN(1,0,1,ISL),RM(1,0,1,ISL),
     -               RN(1,0,1,ISL),AMRT,WK)

                  END IF

C              �V�~�����[�V�������s���i�ł���
               ELSE

C                 ! �iOUT: LMODE,AN,RM,RN�j
                  IF ( ( ICHNG.EQ.0 ).AND.( IREP.EQ.1 ) ) THEN
                  ! �K�i��ω����Ȃ�����̌v�Z���s�v�ł���

                     DO IZ = 1, NZ
                        LMODE(IZ,IREP,JHR,ISL) = LMODE(IZ,0,JHR,ISL)
                        AN(IZ,IREP,JHR,ISL) = AN(IZ,0,JHR,ISL)
                        RM(IZ,IREP,JHR,ISL) = RM(IZ,0,JHR,ISL)
                        RN(IZ,IREP,JHR,ISL) = RN(IZ,0,JHR,ISL)
                     END DO

                  ELSE

                     ! �M���t��������
                     CALL SLVSM(NZ,IOPTWK,EXCAP(1,ISL),SPCAP(1,ISL),NAZ,
     -               VFLOW(1,1,JHR),P0(1,IREP,JHR,ISL),CRHO(ISL),
     -               VOAWK(1,IREP,JHR),RMMX(1,ISL),RMMN(1,ISL),
     -               REFWD(ISL),IPEAK,ISEAS,NITER,FIXEDL,
     -               ISL,IREP,LCG,LSZSPC,
     -               LMODE(1,IREP,JHR,ISL),AN(1,IREP,JHR,ISL),
     -               RM(1,IREP,JHR,ISL),NA,AA,BB,IP)

                     ! �O�C���ׁE�����ׂ����߂�
                     DO IZ = 1, NZ
                        EOA = CRHO(ISL)
     -                       *VOAWK(IZ,IREP,JHR)*( OATX(IZ,JHR)
     -                             - (RM(IZ,IREP,JHR,ISL)+REFWD(ISL)) )
                        RN(IZ,IREP,JHR,ISL) = AN(IZ,IREP,JHR,ISL) - EOA
                     END DO
                  END IF

C                 MRT�̌v�Z
                  IF ( ISL.EQ.1 ) THEN
                     DO IZ = 1, NZ
                        CALL CLCMRT(NZ,VFLOW(1,IZ,JHR),MZ,
     -                    RM(1,0,JHR,ISL),
     -                    IZ,IREP,CRHO(ISL),SMRT1(IZ,JHR),SMRT2(IZ,JHR),
     -                    LCG(IZ),RN(IZ,IREP,JHR,ISL),AMRT(IZ,IREP,JHR))
                     END DO
                  END IF

C                 �~�M���ׂ̍X�V
                  IF ( IREP.EQ.1 ) THEN
                     DO IZ = 1, NZ
                        DO J = 1, NTRM(ISL)
                           LSTPWK = LCG(IZ) + LSTP(J,ISL)
                           LSTQWK = LCG(IZ) + LSTQ(J,ISL)
                           X(LSTQWK) = X(LSTPWK+2)*X(LSTQWK)
     -                     - ( X(LSTPWK) - X(LSTPWK+1) )
     -                        *RM(IZ,0,JHR,ISL)
     -                     - X(LSTPWK+1)*RM(IZ,1,JHR,ISL)
                        END DO

                        IF ( ISL.EQ.1 ) THEN
                           L = LCG(IZ) + LSZSPC(0)
                           DO IWL = 1, 9999
                              CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                              IF ( ISTAT.NE.1 ) THEN
                                 GO TO 500
                              ELSE
                                 X(L+11)=X(L+7)*X(L+11)
     -                              +(X(L+5)-X(L+6))*RM(JZ,0,JHR,ISL)
     -                              +X(L+6)*RM(JZ,1,JHR,ISL)
                                 X(L+12)=X(L+10)*X(L+12)
     -                              +(X(L+8)-X(L+9))*RM(JZ,0,JHR,ISL)
     -                              +X(L+9)*RM(JZ,1,JHR,ISL)
                                 L = L + LSZSPC(M(L))
                              END IF
                           END DO
                        END IF
  500                   CONTINUE

                     END DO
                  END IF

               END IF

            END DO   ! ���O�E���� loop
         END DO   ! ���� loop
      END DO   ! ���M�E���M loop


C     �v�Z���ʂ̏o��
      DO IZ = 1, NZ   ! �]�[�����[�v
         LC = LCG(IZ)
         FF = 1.163/X(LC+2)   ! Kcal/h ���� W/m2 �ւ̊��Z�W��
         DO JHR = 1, NHR   ! �������[�v
            DO IREP = 0, 1   ! ���O�E���ニ�[�v
               IF ( IOUT.EQ.1 ) THEN
                  IF ( MODE.GE.2 ) THEN
                     WRITE(NUOT1-1+IZ,'(I4,'','',2(I2,'',''),I1,'','',
     -               I2,'','',I1,'','',2(F6.2,'','',3(F6.1,'',''),I2,
     -               '',''),F6.2)') ( ID(J), J = 1, 3 ),
     -               MDW, JHR, IREP, ( RM(IZ,IREP,JHR,ISL)+REFWD(ISL),
     -               CLDG(IZ,JHR,ISL)*FF, RN(IZ,IREP,JHR,ISL)*FF,
     -               AN(IZ,IREP,JHR,ISL)*FF,
     -               LMODE(IZ,IREP,JHR,ISL), ISL = 1, NSL ),
     -               AMRT(IZ,IREP,JHR)+REFWD(1)
                  END IF
               ELSE   ! �ȈՏo�́i1���ԕ��̕��ς̏o�́j
                  DO ISL = 1, NSL
                     EOUT(1,ISL) = RM(IZ,IREP,JHR,ISL)+REFWD(ISL)
                     EOUT(2,ISL) = CLDG(IZ,JHR,ISL)*FF
                     EOUT(3,ISL) = RN(IZ,IREP,JHR,ISL)*FF
                     EOUT(4,ISL) = AN(IZ,IREP,JHR,ISL)*FF
                     LMODEB(ISL) = LMODE(IZ,IREP,JHR,ISL)
                  END DO
                  EMRT = AMRT(IZ,IREP,JHR)+REFWD(1)
                  IF ( (IREP.EQ.0).AND.(MODE.GE.2) ) THEN   ! ���ς�����ďo��
                     DO ISL = 1, NSL
                        DO I = 1, 4
                           EOUT(I,ISL) = 0.5*( X(LC+86+(ISL-1)*7+I)
     -                                         + EOUT(I,ISL) )
                        END DO
                        IF ( (M(LC+93+(ISL-1)*7).EQ.9).AND.
     -                       (LMODEB(ISL).EQ.9) ) THEN
                           LMODEB(ISL) = 9
                        ELSE IF ( (M(LC+93+(ISL-1)*7).NE.9).AND.
     -                            (LMODEB(ISL).NE.9) ) THEN
                           LMODEB(ISL) = 10
                        ELSE
                           CALL ERROR2(131,2)
                        END IF
                     END DO
                     WRITE(NUOT1-1+IZ,'(I4,'','',2(I2,'',''),I1,'','',
     -               I2,'','',I1,'','',2(F6.2,'','',3(F6.1,'',''),I2,
     -               '',''),F6.2)') ( ID(J), J = 1, 3 ),
     -               MDW, JHR, IREP, ( ( EOUT(I,ISL), I = 1, 4 ),
     -               LMODEB(ISL), ISL = 1, NSL ), 0.5*(X(LC+92)+EMRT)
                  ELSE IF ( IREP.EQ.1 ) THEN   ! ���X�e�b�v�̂��߂ɋL��
                     DO ISL = 1, NSL
                        DO I = 1, 4
                           X(LC+86+(ISL-1)*7+I) = EOUT(I,ISL)
                        END DO
                        M(LC+93+(ISL-1)*7) = LMODEB(ISL)
                     END DO
                     X(LC+92) = EMRT
                  END IF
               END IF
            END DO
         END DO
      END DO

C     BECS�ւ̏o��
      IF(IBECS.EQ.1) THEN
         CALL OUTBECS(NUOB,NHR,NZ,LCG,NAZ,IOPTG,MZ,MHR,RN)
      END IF

C     �����O�C�ʂ̊O���@�ʉ��Z
      DO IZ = 1, NZ   ! �]�[�����[�v
         L = M(LCG(IZ)+202)   ! OAHU�f�[�^�ւ̃|�C���^(L)
         IF ( L.NE.0 ) THEN   ! OAHU�f�[�^���w�肳��Ă���ꍇ
            DO JHR = 1, NHR   ! �������[�v
               DO IREP = 0, 1   ! ���O�E���ニ�[�v
                  X(L+131+(JHR-1)*2+IREP) = X(L+131+(JHR-1)*2+IREP)
     -               + VOAWK(IZ,IREP,JHR)
               END DO
            END DO
         END IF
      END DO

      RETURN
      END
C-----------------------------------------------------------------------
C     BECS�p��1�����̏o�́i���t����ъO�C�����x�������j
C     1���̍ő�X�e�b�v����24
C     LATEST REVISION   2011.09.19
C                       2011.10.15 �R�����g�̏C���̂�
C-----------------------------------------------------------------------
      SUBROUTINE OUTBECS(NUOB,NHR,NZ,LCG,NAZ,IOPTG,MZ,MHR,RN)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

      PARAMETER ( NSL=2 )              !     ���M�Ɛ��M

C     ����
      INTEGER     NUOB                 ! I   BECS�ւ̎󂯓n���t�@�C���p���u�ԍ�
      INTEGER     NHR                  ! I   1���̃X�e�b�v��
      INTEGER     NZ                   ! I   ���݂̃O���[�v�̃]�[����
      INTEGER     LCG(NZ)              ! I   XMQ�z���SPAC�f�[�^�ւ̃|�C���^�iL�j
      INTEGER     NAZ                  ! I   �O���[�v������̍ő�]�[������\�킷�������@
      INTEGER     IOPTG(NAZ,NHR)       ! I   �󒲉^�]��ԃt���O�A=0:��~���A
                                       !     =1:�^�]���A=2:�N���A=3:��~
      INTEGER     MZ                   ! I   �O���[�v������̍ő�]�[������\�킷�������@
      INTEGER     MHR                  ! I   1���̍ő�X�e�b�v����\�킷�������@
      REAL        RN(MZ,0:1,MHR,NSL)   ! I   �e�]�[���̎������M��
C     ���[�J���ϐ�
      INTEGER     JSTART(2)            !     �N������
      INTEGER     JSTOP(2)             !     ��~����
      CHARACTER   QSP*4                !     �X�y�[�X��
      PARAMETER(  MHR2=24 )            !     1���̍ő�X�e�b�v��
      REAL        QLOAD(MHR2)          !     �����M�i���M�j����[kcal/h]

      IF(NHR.GT.MHR2) CALL ERROR2(180,2)

C     �]�[�����[�v
      DO IZ = 1, NZ
         LC = LCG(IZ)
         FF = 1.0/X(LC+2)   ! kcal/h ���� kcal/m2h �ւ̊��Z�W��

         ! �N���E��~�����A�X�y�[�X���̒��o�Əo��
         ISTART = 0   ! �����ڂ̋N����
         ISTOP  = 0   ! �����ڂ̒�~��
         DO I = 1, 2
            JSTART(I) = 0
            JSTOP(I) = 0
         END DO
         DO JHR = 1, NHR   ! �������[�v
            IF(IOPTG(IZ,JHR).EQ.2) THEN   ! �N��
               ISTART = ISTART + 1
               IF(ISTART.LE.2) THEN
                  JSTART(ISTART) = JHR
               END IF
            ELSE IF(IOPTG(IZ,JHR).EQ.3) THEN   ! ��~
               ISTOP = ISTOP + 1
               IF(ISTOP.LE.2) THEN
                  JSTOP(ISTOP) = JHR
               END IF
            END IF
         END DO
         CALL NAME(QSP,M(LC+1))
         WRITE(NUOB,'(2(I3,A,I3,A),1X,A)')
     -          (JSTART(I), '00', JSTOP(I), '00', I = 1, 2), QSP

         ! �����M�E���M�̃f�[�^�̐����i�����j�Əo��
         DO ISL = 1, NSL   ! ���M�E���M���[�v
            DO JHR = 1, NHR
               QLOAD(JHR) = 0.0
               IF(IOPTG(IZ,JHR).EQ.2) THEN        ! �N��
                  QLOAD(JHR) = RN(IZ,1,JHR,ISL)      ! ��������̒l
               ELSE IF(IOPTG(IZ,JHR).EQ.3) THEN   ! ��~
                  QLOAD(JHR) = RN(IZ,0,JHR,ISL)      ! ��~���O�̒l
               ELSE
                  QLOAD(JHR) = 0.5*(RN(IZ,0,JHR,ISL) + RN(IZ,1,JHR,ISL)) ! ���O�ƒ���̕���
               END IF
            END DO
            WRITE(NUOB,'(24E15.7)') (QLOAD(JHR)*FF, JHR = 1, NHR)
         END DO

      END DO

      RETURN
      END
