C-----------------------------------------------------------------------
C     �\�M�I�����̌㏈���i���ד��̒��o�ƒ~�M���ׂ̍X�V�j
C     LATEST REVISION   2012.03.05
C-----------------------------------------------------------------------
      SUBROUTINE POSTP(MZ,NZ,LCG,NSTP,IDLT,JHR0,NHR,NSIZE,BB,IPS,CRHO,
     -     VOA,OATX,REFWD,ISL,RMSET,NTRM,LSTP,LSTQ,NAZ,VFLOW,
     -     SMRT1,SMRT2,LSZSPC,LMODE,AN,RM,RN,AMRT,WK)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

C     ����
      INTEGER     MZ                   ! I   �������@�i1�O���[�v������̍ő�]�[�����j
      INTEGER     NZ                   ! I   ���݂̃O���[�v�̃]�[����
      INTEGER     LCG(NZ)              ! I   XMQ�z���SPAC�f�[�^�ւ̃|�C���^�iL�j
      INTEGER     NSTP                 ! I   �\�M���ԁi�X�e�b�v�j
      INTEGER     IDLT(NSTP)           ! I   �\�M�J�n��A�e�X�e�b�v�ɂ����Ē��㎺�����x��
                                       !     =0�F���m���Ƃ��Ȃ��A=1�F���m���Ƃ���
      INTEGER     JHR0                 ! I   �\�M���J�n��������
      INTEGER     NHR                  ! I   1���̃X�e�b�v��
      INTEGER     NSIZE                ! I   �������̐��i���m���̐��j
      REAL*8      BB(NSIZE)            ! I   �A���������̉��i�E�Ӄx�N�g���j
      INTEGER     IPS(0:NSTP)          ! I   �\�M�J�n��A�e�����X�e�b�v�̎������x���A���m�x�N
                                       !     �g���̉������ڂ���n�܂邩�i=IPS+1�����ڂ���
      REAL        CRHO                 ! I   ��C�̗e�ϔ�M
                                       !     �i���M�̏ꍇ�͖��x�ɏ������M���|�������́j
      REAL        VOA(MZ,0:1,NHR)      ! I   �e�]�[���̊O�C�ʁi�̐ϗ��ʁA0�ȏ�j
      REAL        OATX(MZ,NHR)         ! I   �����O�E�����x�i�O���@�l���A������x�����
                                       !     �΍��ł͂Ȃ��j
      REAL        REFWD                ! I   ������x
      INTEGER     ISL                  ! I   =1:���M�A=2:���M
      REAL        RMSET(NZ)            ! I   �ݒ艷���x�i������x����̕΍��ł͂Ȃ��j
      INTEGER     NTRM                 ! I   �~�M�����W���̍���
      INTEGER     LSTP(NTRM)           ! I   �~�M�����W���i�u�����������j�ւ̃|�C���^
      INTEGER     LSTQ(NTRM)           ! I   �~�M���ׁi�u�����������j�ւ̃|�C���^
      INTEGER     NAZ                  ! I   �]�[������\�킷�������@
      REAL        VFLOW(NAZ,NAZ,NHR)   ! I   ��1�Y���ڂ̃]�[�������2�Y���ڂ̃]�[���ւ̗���
                                       !     ���ʁi�̐ϗ��ʁA0�ȏ�A�Ίp����0�Ƃ���j
      REAL        SMRT1(NAZ,NHR)       ! I   �ʐς������Ȃ����ʂ���̗�[����
      REAL        SMRT2(NAZ,NHR)       ! I   INFL�̋z�M�����W��
      INTEGER     LSZSPC(0:4)          ! I   XMQ�z��̂����A(0):SPAC, (1):OWAL, (2):IWAL,
                                       !     (3):WNDW, (4):INFL �̕ϐ��̐�
      INTEGER     LMODE(MZ,0:1,NHR)    ! O   �e�]�[���̕��׏�ԃ��[�h�i-2�F�g�[�ߕ���,
                                       !     -1�F�g�[�y����,0�F������,1�F��[�y����,
                                       !     2�F��[�ߕ���,9�F��~�j
      REAL        AN(MZ,0:1,NHR)       ! O   �e�]�[���̑��u�����M��
      REAL        RM(MZ,0:1,NHR)       ! I/O �e�]�[���̎������x�i������x����̕΍��j
                                       !     (���p�����̂͗\�M�J�n���O�̃f�[�^�̂�)
      REAL        RN(MZ,0:1,NHR)       ! O   �e�]�[���̎������M��
      REAL        AMRT(MZ,0:1,NHR)     ! O   MRT
      REAL*8      WK(NTRM)
C     ���[�J���ϐ�
      INTEGER     IZ                   !     ���݂̃]�[�������]�[���ڂ��i1<=IZ<=NZ�j
      INTEGER     LC
      INTEGER     KSTP2                !     �\�M�J�n��̌o�߃X�e�b�v
      INTEGER     IREPS
      INTEGER     IREPE
      INTEGER     IREP2                !     ���݂̎����X�e�b�v�̒��O�����ォ
                                       !     =0�F���O���邢�͓񓙕ӎO�p�A=1�F����
      INTEGER     JHR2                 !     ����
      REAL        RMWK
      INTEGER     J
      INTEGER     LSTPWK
      INTEGER     L
      INTEGER     IWL
      INTEGER     JZ
      INTEGER     ISTAT
      REAL        EOA                  !     �O�C���ׁi���������x��j


      DO IZ = 1, NZ   ! �]�[�� loop
         LC = LCG(IZ)
         DO KSTP2 = 0, NSTP   ! �\�M�X�e�b�v loop
            JHR2 = JHR0 + KSTP2
            IF(JHR2.GT.NHR) CALL ERROR2(159,2)

            IF ( KSTP2.EQ.0 ) THEN
               IREPS = 1
            ELSE
               IREPS = 0
            END IF

            IF ( (KSTP2.EQ.NSTP).AND.(IDLT(NSTP).EQ.0) ) THEN
               IREPE = 0
            ELSE
               IREPE = 1
            END IF

            DO IREP2 = IREPS, IREPE   ! ���O�E���� loop

C              �\�M���ԑт̎������x�E�����M�ʂ𔲂��o��
               LMODE(IZ,IREP2,JHR2) = SIGN(1.0D0,BB(IZ))
               AN(IZ,IREP2,JHR2) = BB(IZ)
               IF ( KSTP2.EQ.0 ) THEN
                  RMWK = BB(IPS(KSTP2)+IZ)
               ELSE IF ( (KSTP2.EQ.NSTP).AND.(IREP2.EQ.IREPE) ) THEN
                  RMWK = RMSET(IZ) - REFWD
               ELSE
                  RMWK = BB(IPS(KSTP2)+IDLT(KSTP2)*IREP2*NZ+IZ)
               END IF
               RM(IZ,IREP2,JHR2) = RMWK

C              �\�M���ԑт̊O�C���ׁE�����ׁAMRT�����߂�
               EOA = CRHO*VOA(IZ,IREP2,JHR2)
     -                   *( OATX(IZ,JHR2) - (RMWK+REFWD) )
               RN(IZ,IREP2,JHR2) = AN(IZ,IREP2,JHR2) - EOA
               IF ( ISL.EQ.1 ) THEN
                  CALL CLCMRT(NZ,VFLOW(1,IZ,JHR2),MZ,RM(1,0,JHR2),IZ,
     -                        IREP2,CRHO,SMRT1(IZ,JHR2),SMRT2(IZ,JHR2),
     -                        LC,RN(IZ,IREP2,JHR2),AMRT(IZ,IREP2,JHR2))
               END IF

C              �\�M���ԑт̎������x�ϓ��ɂ��~�M���ׂ��X�V����
               IF ( (KSTP2.EQ.NSTP).AND.(IDLT(NSTP).EQ.0) ) THEN
                  ! �\�M�I�����㑤�i�V�~�����[�V�������[�h�j�ōX�V����
               ELSE
                  DO J = 1, NTRM
                     LSTPWK = LC + LSTP(J)
                     IF ( KSTP2.EQ.0 ) THEN
                        WK(J) = -X(LSTPWK+1)*RMWK
                     ELSE IF ( IREP2.EQ.0 ) THEN
                        WK(J) = X(LSTPWK+2)*WK(J)
     -                  - ( X(LSTPWK) - X(LSTPWK+1) )*RMWK
                     ELSE
                        WK(J) = WK(J) - X(LSTPWK+1)*RMWK
                     END IF
                  END DO

                  IF ( ISL.EQ.1 ) THEN
                     L = LCG(IZ) + LSZSPC(0)
                     DO IWL = 1, 9999
                        CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                        IF ( ISTAT.NE.1 ) THEN
                           GO TO 100
                        ELSE
                           IF ( KSTP2.EQ.0 ) THEN
                              X(L+13) = X(L+6)*RMWK
                              X(L+14) = X(L+9)*RMWK
                           ELSE IF ( IREP2.EQ.0 ) THEN
                              X(L+13) = X(L+7)*X(L+13)
     -                           + ( X(L+5) - X(L+6) )*RMWK
                              X(L+14) = X(L+10)*X(L+14)
     -                           + ( X(L+8) - X(L+9) )*RMWK
                           ELSE
                              X(L+13) = X(L+13) + X(L+6)*RMWK
                              X(L+14) = X(L+14) + X(L+9)*RMWK
                           END IF
                           L = L + LSZSPC(M(L))
                        END IF
                     END DO
                     CALL ERROR2(158,2)
                  END IF
  100             CONTINUE

               END IF
            END DO   ! ���O�E���� loop
         END DO   ! �\�M�X�e�b�v loop

         DO J = 1, NTRM
            X(LC+LSTQ(J)) = X(LC+LSTQ(J)) + WK(J)
         END DO
         ! �\�M�I�����ɒ���̌v�Z�����Ȃ��ꍇ��NSTP-1�܂ł�
         ! �ϓ��ɂ��NSTP�̂��߂̒~�M���ׁB�v�Z����ꍇ��NSTP
         ! �܂ł̕ϓ��ɂ��NSTP+1�̂��߂̒~�M���ׁB

         IF ( ISL.EQ.1 ) THEN
            L = LCG(IZ) + LSZSPC(0)
            DO IWL = 1, 9999
               CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
               IF ( ISTAT.NE.1 ) THEN
                  GO TO 200
               ELSE
                  X(L+11) = X(L+11) + X(L+13)
                  X(L+12) = X(L+12) + X(L+14)
                  L = L + LSZSPC(M(L))
               END IF
            END DO
         END IF
  200    CONTINUE

      END DO   ! �]�[�� loop

      RETURN
      END
