C-----------------------------------------------------------------------
C     �����M�ʌv�Z�̂��߂̑O�����i����2�j
C     LATEST REVISION   2020.03.24 (2005.01.30���)                               rev 20200324(T.Nagai)
C                       ���u�e�ʂ��G�ߕʂɐݒ肷��                                rev 20200324(T.Nagai)
C-----------------------------------------------------------------------
      SUBROUTINE EXTRC1(JHR,NHR,LOPC,LC,NAZ,ISEAS,KSCH,IOPTWK,IOPTG,
     -         IOPVG,SMRT1,SMRT2,LCG,VOAG,CLDG,P0,RMMN,RMMX,SPCAP,EXCAP,
     -         VFLOW)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

      PARAMETER ( NSL=2 )              !     ���M�Ɛ��M
C     ����
      INTEGER     JHR                  ! I   ����
      INTEGER     NHR                  ! I   1���̃X�e�b�v��
      INTEGER     LOPC                 ! I   OPCO�f�[�^�ւ̃|�C���^(L)
      INTEGER     LC                   ! I   SPAC�f�[�^�ւ̃|�C���^(L)
      INTEGER     NAZ                  ! I   �]�[������\�킷�������@
      INTEGER     ISEAS                ! I   (�{����)�����̋G�� �l=1:�Ċ��A2:�~���A3:���Ԋ�
      INTEGER     KSCH                 ! I   (�{����)�X�P�W���[���l(1�`3)
      INTEGER     IOPTWK               ! I   IOPTG�Ɠ����i���݂̃X�y�[�X�ɂ��Ă̒l�j
      INTEGER     IOPTG(NAZ,NHR)       ! 0   �󒲉^�]��ԃt���O�A=0:��~���A
                                       !     =1:�^�]���A=2:�N���A=3:��~
      INTEGER     IOPVG(NAZ,NHR)       ! O   �O�C������ԃt���O�A=0:�J�b�g��
                                       !     =1:�������A=2:�����J�n�A=3:������~
      REAL        SMRT1(NAZ,NHR)       ! O   �ʐς������Ȃ����ʂ���̗�[����
      REAL        SMRT2(NAZ,NHR)       ! O   INFL�̋z�M�����W��
      INTEGER     LCG(*)               ! O   XMQ�z���SPAC�f�[�^�ւ̃|�C���^�iL�j
                                       !     �Y���͌��݂̃]�[���̃O���[�v���ɂ����鏇��(=IZ)
      REAL        VOAG(*)              ! O   �������̊O�C��(�Y���̓O���[�v���̏���=IZ)
      REAL        CLDG(NAZ,NHR,NSL)    ! O   ��[����
      REAL        P0(NAZ,0:1,NHR,NSL)  ! O   �u���~�M�����W���i�z�M����鑤�����j
                                       !     ��2�Y��=0:�񓙕ӎO�p
                                       !     ��2�Y��=1:�E�����p�񓙕ӎO�p
      REAL        RMMN(NAZ,NSL)        ! O   �e�X�y�[�X�̐ݒ艷���x����
      REAL        RMMX(NAZ,NSL)        ! O   �e�X�y�[�X�̐ݒ艷���x���
      REAL        SPCAP(NAZ,NSL)       ! O   �e�X�y�[�X�̑��u�e�ʁi���M�A0�ȏ�j
      REAL        EXCAP(NAZ,NSL)       ! O   �e�X�y�[�X�̑��u�e�ʁi��p�A0�ȏ�j
      REAL        VFLOW(NAZ,NAZ,NHR)   ! O   ��1�Y���ڂ̃X�y�[�X�����2�Y���ڂ̃X�y�[�X�ւ̗���
                                       !     ���ʁi�̐ϗ��ʁA0�ȏ�A�Ίp����0�Ƃ���j
C     X(JHR)                             I   ��[���ׁi���M�j
C     X(24+JHR)                          !   �u���~�M�����W���␳���i���M�j
C     X(48+JHR)                          I   ��[���ׁi���M�j
C     X(72+JHR)                          !   �u���~�M�����W���␳���i���M�j
C     M(LOPC+164)                        I   �O�C�����J�n���� [��]
C     X(LOPC+165)                        I   �O�C������ [m3/m2h]
C     X(LOPC+4*ISEAS+i), i = -2, 1       I   ���݂̋G�߂̐ݒ艷���x�̏㉺��
C     X(LC+2)                            !   ���ʐ� [m2]
C     X(LC+i), i = 15, 16                !   �u���~�M�����W���Œ萬���i���M�A�񓙕ӁE���p�O�p�j
C     X(LC+i), i = 25, 26                !   �u���~�M�����W���Œ萬���i���M�A�񓙕ӁE���p�O�p�j
C     X(LC+i), i = 56, 59                I   ���u�e�ʁi���M�E��p�A���M�E���M�j
C     X(LC+74)                           I   �ʐς������Ȃ����ʂ̗�[���ׁi=INFL�Ƌ�������HEAT�j
C     X(LC+75)                           I   INFL�̋z�M�����W���i�u���j ��0.288V �i���ϐ��j
C     M(LC+61)                           I/O �O������̊O�C�����p�����(=1:�p��, =0:�r�؂ꂽ)
C     M(LC+101)                          I   ���݂̃]�[��������O���[�v�̉��]�[���ڂ�
C     XMQ(LC+102�`201)                   I   CFLW�֘A�f�[�^

C     ���[�J���ϐ�
      INTEGER     IZ                   !     ���݂̃]�[��������O���[�v�̉��]�[���ڂ�
      REAL        RFLW                 !     �������ʔ䗦
      INTEGER     IOPVWK
      INTEGER     I
      INTEGER     L1

      IF(JHR.LT.1) CALL ERROR2(119,2)
      IF(JHR.GT.NHR) CALL ERROR2(118,2)


C     �O�C�������[�h ( IOPVG )
      IF ( X(LOPC+165).LT.0.01 ) THEN
         IOPVWK = 0
      ELSE IF ( M(LC+61).EQ.1 ) THEN   ! �O�����O�C�����p����
         IOPVWK = IOPTWK
         IF(IOPTWK.EQ.0) CALL ERROR2(117,2)
         IF(IOPTWK.EQ.2) CALL ERROR2(116,2)
      ELSE IF ( JHR.LT.M(LOPC+164) ) THEN
         IOPVWK = 0
      ELSE IF ( JHR.GT.M(LOPC+164) ) THEN
         IOPVWK = IOPTWK
      ELSE IF ( (IOPTWK.EQ.0).OR.(IOPTWK.EQ.3) ) THEN
         IOPVWK = 0
      ELSE
         IOPVWK = 2
      END IF

      IF ( (IOPVWK.EQ.0).OR.(IOPVWK.EQ.3) ) THEN
         M(LC+61) = 0   ! ���Ƃ��O�X�e�b�v�܂ŊO�C�������p�����Ă�����
                        ! ���Ă����̃X�e�b�v�œr�₦��
      ELSE IF ( JHR.EQ.NHR ) THEN
         M(LC+61) = 1
      END IF


C     1�����̃f�[�^��ۑ����邽�߂̑��
      IZ = M(LC+101)
      IF(IZ.GT.NAZ) CALL ERROR2(115,2)
      IOPTG(IZ,JHR) = IOPTWK
      IOPVG(IZ,JHR) = IOPVWK
      SMRT1(IZ,JHR) = X(LC+74)
      SMRT2(IZ,JHR) = X(LC+75)
      IF ( JHR.EQ.1 ) THEN   ! 1����1��������Ώ\��
         LCG (IZ) = LC
         VOAG(IZ) = X(LOPC+165)*X(LC+2)
      END IF

      ! ���M
      CLDG(IZ,JHR,1) = X(JHR)
      P0(IZ,0,JHR,1) = X(LC+15) + X(24+JHR)   ! ���M�A�񓙕ӎO�p
      P0(IZ,1,JHR,1) = X(LC+16) + X(24+JHR)   ! ���M�A�E�����p�O�p
      IF ( JHR.EQ.1 ) THEN
         RMMX (IZ,1) = X(LOPC+4*ISEAS-2)
         RMMN (IZ,1) = X(LOPC+4*ISEAS-1)
         SPCAP(IZ,1) = X(LC+205+4*(ISEAS-1))*X(LC+2)                            ! rev 20200324(T.Nagai)
         EXCAP(IZ,1) = X(LC+203+4*(ISEAS-1))*X(LC+2)                            ! rev 20200324(T.Nagai)
      END IF
*TNRS
C     XLC74(    KSPAC,JHR) = X(LC+74)
C     XLC75(    KSPAC,JHR) = X(LC+75)
*TNRE

      ! ���M
      CLDG(IZ,JHR,2) = X(48+JHR)
      P0(IZ,0,JHR,2) = X(LC+25) + X(72+JHR)   ! ���M�A�񓙕ӎO�p
      P0(IZ,1,JHR,2) = X(LC+26) + X(72+JHR)   ! ���M�A�E�����p�O�p
      IF ( JHR.EQ.1 ) THEN
         RMMX (IZ,2) = X(LOPC+4*ISEAS)
         RMMN (IZ,2) = X(LOPC+4*ISEAS+1)
         SPCAP(IZ,2) = X(LC+206+4*(ISEAS-1))*X(LC+2)                            ! rev 20200324(T.Nagai)
         EXCAP(IZ,2) = X(LC+204+4*(ISEAS-1))*X(LC+2)                            ! rev 20200324(T.Nagai)
      END IF

      ! �X�y�[�X�Ԉړ�����
      DO I = 1, NAZ   ! �������X�y�[�X���[�v
         L1 = LC+101+(I-1)*5
         IF ( M(L1+2).EQ.2 ) THEN
            IF ( (IOPTWK.EQ.1).OR.(IOPTWK.EQ.3) ) THEN
               RFLW = X(L1+4)   ! ��on���̊������g�p
            ELSE
               RFLW = X(L1+5)   ! ��off���̊������g�p
            END IF
         ELSE IF ( M(L1+2).EQ.1 ) THEN  ! DSCH�g�p
            RFLW = X(M(L1+3)+(KSCH-1)*24+JHR)
         END IF
         IF ( (M(L1+2).GE.1).AND.(I.NE.IZ) ) THEN   ! M(L1+2)=0�͖���`�̏ꍇ
            VFLOW(I,IZ,JHR) = RFLW*X(L1+1)
         ELSE
            VFLOW(I,IZ,JHR) = 0.0
         END IF
      END DO

      RETURN
      END
