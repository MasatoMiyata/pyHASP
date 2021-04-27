C-----------------------------------------------------------------------
C     �V�~�����[�V�����v�Z
C     LATEST REVISION   2012.03.30
C-----------------------------------------------------------------------
      SUBROUTINE SLVSM(NZ,IOPT,EXCAP,SPCAP,NAZ,VFLOW,P0,CRHO,VOA,
     -   RMMX,RMMN,REFWD,IPEAK,ISEAS,NITER,FIXEDL,ISL,IREP,LCG,LSZSPC,
     -   LMODE,AN,RM,NA,AA,BB,IP)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

C     ����
      INTEGER     NZ             ! I   ���݂̃O���[�v�̃]�[����
      INTEGER     IOPT(NZ)       ! I   ���i�̋󒲉^�]��ԁi0:��~,���̑�:�ғ��j
      REAL        EXCAP(NZ)      ! I   �e�X�y�[�X�̑��u�e�ʁi��p�A0�ȏ�j
      REAL        SPCAP(NZ)      ! I   �e�X�y�[�X�̑��u�e�ʁi���M�A0�ȏ�j
      INTEGER     NAZ            ! I   �z��VFLOW�̐������@
      REAL        VFLOW(NAZ,NZ)  ! I   ��1�Y���ڂ̃]�[�������2�Y���ڂ̃]�[���ւ̗�������
                                 !     �i�̐ϗ��ʁA0�ȏ�A�Ίp����0�ƂȂ��Ă��Ȃ���΂Ȃ�Ȃ��j
      REAL        P0(NZ)         ! I   �e�X�y�[�X�̏u���~�M�����W���i�z�M�����ꍇ�����j
      REAL        CRHO           ! I   ��C�̗e�ϔ�M�i���M�̏ꍇ�͖��x�ɏ������M���|�������́j
      REAL        VOA(NZ)        ! I   �e�]�[���̊O�C�ʁi�̐ϗ��ʁA0�ȏ�j
      REAL        RMMX(NZ)       ! I   �e�X�y�[�X�̐ݒ艷���x���
      REAL        RMMN(NZ)       ! I   �e�X�y�[�X�̐ݒ艷���x����
                                 !     �iRMMX,RMMN�͊�����x����̕΍��ł͂Ȃ��j
      REAL        REFWD          ! I   ������x
      INTEGER     IPEAK          ! I   1:�s�[�N�v�Z���[�h�A0:�V�~�����[�V�������[�h
      INTEGER     ISEAS          ! I   1:�Ċ��A2:�~���A3:���Ԋ�
      INTEGER     NITER          ! I   �ő�����v�Z��
      REAL        FIXEDL(NZ)     ! I   �e�]�[���́A�����x�⏜���M�ʂɈˑ����Ȃ��Œ藬���M��
      INTEGER     ISL            ! I   =1:���M�A=2:���M
      INTEGER     IREP           ! I   ���݂̎����X�e�b�v�̒��O�����ォ
                                 !     =0�F���O���邢�͓񓙕ӎO�p�A=1�F����
      INTEGER     LCG(NZ)        ! I   XMQ�z���SPAC�f�[�^�ւ̃|�C���^�iL�j
      INTEGER     LSZSPC(0:4)    ! I   XMQ�z��̂����A(0):SPAC, (1):OWAL, (2):IWAL,
                                 !     (3):WNDW, (4):INFL �̕ϐ��̐�
      INTEGER     LMODE(NZ)      ! O   �e�]�[���̕��׏�ԃ��[�h�i-2�F�g�[�ߕ���,-1�F�g�[�y����,
                                 !     0�F������,1�F��[�y����,2�F��[�ߕ���,9�F��~�j
      REAL        AN(NZ)         ! O   �e�]�[���̑��u�����M��
      REAL        RM(NZ)         ! O   �e�]�[���̎������x�i������x����̕΍��j
      INTEGER     NA             ! I   AA�̐������@
      REAL*8      AA(NA,NZ)      !     Work Array �i�A�����������ӌW���s��j
      REAL*8      BB(NZ)         !     Work Array �i�A���������E�ӌW���x�N�g���j
      INTEGER     IP(NZ)         !     Work Array

C     ���[�J���ϐ�
      INTEGER     IZ
      PARAMETER ( MZ=20 )        !     1�O���[�v������ő�]�[����
      REAL        EXCAP1(MZ)     !     �e�]�[���̑��u�e�ʁi��p�A0�ȏ�j
      REAL        SPCAP1(MZ)     !     �e�]�[���̑��u�e�ʁi���M�A0�ȏ�j
      REAL*8      WK
      INTEGER     J
      REAL        GRADL(MZ)      !     �e�]�[���̒P�ʉ����x�ω��ɑ΂��鏜���M�ʂ�
                                 !     �ω��i�������t�ɂ������́B���l�j
      REAL        CNST(4,MZ)     !     ��2�Y���ڂ̃]�[���̕��׏�ԁi�ߕ��ד��j�̕���_��
                                 !     �^���镉�ׂ̐���
                                 !     ��1�Y���ڂ̓Y��1:�g�[�ߕ��ׂƒg�[�y���ׂ̋��E
                                 !           �Y��2:�g�[�y���ׂƖ����ׂ̋��E
                                 !           �Y��3:�����ׂƗ�[�y���ׂ̋��E
                                 !           �Y��4:��[�y���ׂƗ�[�ߕ��ׂ̋��E
      INTEGER     ITER
      INTEGER     ICONV          !     ��������t���O�i1:�����A0:������j
      INTEGER     INFO
      PARAMETER ( EPS2=0.01 )    !     ��������̂��߂̋��e�͈́ikcal/h�j
      INTEGER     L
      INTEGER     IWL
      INTEGER     JZ
      INTEGER     ISTAT

      IF(NZ.GT.MZ) CALL ERROR2(169,2)
      DO IZ = 1, NZ
         IF(VOA(IZ).LT.0) CALL ERROR2(168,2)
         DO J = 1, NZ
            IF ( J.EQ.IZ ) THEN
               IF(ABS(VFLOW(IZ,J)).GT.0.01) CALL ERROR2(167,2)
            ELSE
               IF(VFLOW(IZ,J).LT.0) CALL ERROR2(166,2)
            END IF
         END DO
         IF(RMMN(IZ).GT.RMMX(IZ)) CALL ERROR2(165,2)
         IF(EXCAP(IZ).LT.0) CALL ERROR2(164,2)
         IF(SPCAP(IZ).LT.0) CALL ERROR2(163,2)
      END DO

      DO IZ = 1, NZ   ! �]�[�� loop

C        �������׏�ԃ��[�h�𖳕��ׁi��󒲎��͒�~�j�Ƃ���
         IF ( IOPT(IZ).EQ.0 ) THEN
            LMODE(IZ) = 9
         ELSE
            LMODE(IZ) = 0
         END IF

C        ���u�e�ʂ��Z�b�g����
         IF ( (IPEAK.EQ.1).AND.(ISEAS.LE.2) ) THEN
         ! �e�ʌv�Z�Ċ��E�~�����[�h�ł���
C           ���u�e�ʂ𖳌���ɃZ�b�g����
            EXCAP1(IZ) = 9.9E+10
            SPCAP1(IZ) = 9.9E+10
         ELSE  ! �V�~�����[�V�������[�h���邢�͗e�ʌv�Z���[�h���Ԋ�
C           ���u�e�ʂ͈����œn���ꂽ���̂����̂܂ܗp����
            EXCAP1(IZ) = EXCAP(IZ)
            SPCAP1(IZ) = SPCAP(IZ)
         END IF

C        �M���t���̌X�������߂�
         WK = 0.0D0
         DO J = 1, NZ
            WK = WK + VFLOW(J,IZ)
         END DO
         GRADL(IZ) = P0(IZ) + CRHO*( VOA(IZ) + WK )

C        ���׏�ԃ��[�h�̕���_�����߂�
         CNST(1,IZ) = -SPCAP1(IZ) + GRADL(IZ)*(RMMN(IZ)-REFWD)
         CNST(2,IZ) = GRADL(IZ)*(RMMN(IZ)-REFWD)
         CNST(3,IZ) = GRADL(IZ)*(RMMX(IZ)-REFWD)
         CNST(4,IZ) = EXCAP1(IZ) + GRADL(IZ)*(RMMX(IZ)-REFWD)
      END DO   ! �]�[�� loop

      DO ITER = 1, NITER   ! �����v�Z loop
         ICONV = 1

         DO IZ = 1, NZ   ! �]�[�� loop
C           �W���s��̊Y�������ɒl���Z�b�g����
            CALL COEFFS(LMODE(IZ),NZ,IZ,RMMX(IZ)-REFWD,RMMN(IZ)-REFWD,
     -      GRADL(IZ),CRHO,VFLOW(1,IZ),FIXEDL(IZ),EXCAP1(IZ),SPCAP1(IZ),
     -      ISL,IREP,LCG(IZ)+LSZSPC(0),LSZSPC,NA,AA,BB(IZ))
         END DO   ! End �]�[�� loop

C        ������������
         CALL DGESV(NZ,1,AA,NA,IP,BB,NA,INFO)
         IF(INFO.NE.0) CALL ERROR2(162,2)

         DO IZ = 1, NZ   ! �]�[�� loop

C           �M���t���̐ؕЂ����߂�
            WK = 0.0D0
            DO J = 1, NZ
               WK = WK + CRHO*VFLOW(J,IZ)*BB(J)
            END DO
            IF ( ISL.EQ.1 ) THEN
               L = LCG(IZ) + LSZSPC(0)
               DO IWL = 1, 9999
                  CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                  IF ( ISTAT.NE.1 ) THEN
                     GO TO 200
                  ELSE
                     WK = WK + X(L+3+IREP)*BB(JZ)
                     L = L + LSZSPC(M(L))
                  END IF
               END DO
            END IF
  200       CONTINUE
            WK = WK + FIXEDL(IZ)

C           ���׏�ԃ��[�h���ω��������ǂ������`�F�b�N����
            IF ( LMODE(IZ).NE.9 ) THEN  ! �󒲉ғ���ԂŁA
               IF ( (LMODE(IZ).GE.-1).AND.
     -              (WK.LT.CNST(LMODE(IZ)+2,IZ)-EPS2) ) THEN
                  LMODE(IZ) = LMODE(IZ) - 1      ! ���׏�ԃ��[�h���g�[���փV�t�g����
                  ICONV = 0
               ELSE IF ( (LMODE(IZ).LE.1).AND.
     -              (WK.GT.CNST(LMODE(IZ)+3,IZ)+EPS2) ) THEN
                  LMODE(IZ) = LMODE(IZ) + 1      ! ���׏�ԃ��[�h����[���փV�t�g����
                  ICONV = 0
               END IF
            END IF

C           �����M�ʂ����߂�
            AN(IZ) = -GRADL(IZ)*BB(IZ) + WK
         END DO   ! End �]�[�� loop

         IF ( ICONV.EQ.1 ) THEN   ! �S�Ẵ]�[���ŕ��׏�ԃ��[�h���ω����Ȃ�
            DO IZ = 1, NZ
               RM(IZ) = BB(IZ)
            END DO
            RETURN
         END IF

      END DO   ! End �����v�Z loop

      CALL ERROR2(161,2)

      RETURN
      END
