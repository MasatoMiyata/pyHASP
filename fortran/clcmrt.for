C-----------------------------------------------------------------------
C     �Ȉ�MRT�v�Z
C     LATEST REVISION   2004.02.06
C-----------------------------------------------------------------------
      SUBROUTINE CLCMRT(NZ,VFLOW,MZ,RM,IZ,IREP,CRHO,SMRT1,SMRT2,LC,RN,
     -                  AMRT)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

C     ����
      INTEGER     NZ                   ! I   ���݂̃O���[�v�̃]�[����
      REAL        VFLOW(NZ)            ! I   �e�]�[������̗������ʁi�̐ϗ��ʁA���l�j
      INTEGER     MZ                   ! I   �������@�i1�O���[�v������̍ő�]�[�����j
      REAL        RM(MZ,0:1)           ! I   �e�]�[���̎����i����x����̕΍��j
                                       !     ��2�Y����(0):���O�A(1):����
      INTEGER     IZ                   ! I   ���݂̃]�[�������]�[���ڂ��i1<=IZ<=NZ�j
      INTEGER     IREP                 ! I   ���݂̎����X�e�b�v�̒��O�����ォ
                                       !     =0�F���O�A=1�F����
      REAL        CRHO                 ! I   ��C�̗e�ϔ�M
      REAL        SMRT1                ! I   �ʐς������Ȃ����ʂ���̗�[����
      REAL        SMRT2                ! I   INFL�̋z�M�����W��
      INTEGER     LC                   ! I   XMQ�z���SPAC�f�[�^�ւ̃|�C���^�iL�j
      REAL        RN                   ! I   �������M��
      REAL        AMRT                 ! O   MRT
C     M(LC+63)                         ! I   �\�ʐύ��v
C     M(LC+64)                         ! I   FURN �~�M�����W���i���M�j PV0
C     M(LC+65)                         ! I     PR0
C     M(LC+66)                         ! I     PV1
C     M(LC+67)                         ! I     PR1
C     M(LC+68)                         ! I     R1
C     M(LC+69)                         ! I     PV2
C     M(LC+70)                         ! I     PR2
C     M(LC+71)                         ! I     R2
C     M(LC+72)                         ! I/O   Q1
C     M(LC+73)                         ! I/O   Q2

C     ���[�J���ϐ�
      REAL*8      WK
      INTEGER     J
      REAL        RNNA
      REAL        HC                   !     �Η��M�`�B��
      DATA HC     /3.5/                !     [kcal/m2hC](�P�ʈˑ�)


C     �אڃ]�[���Ƃ̑Η��ɂ��M�����x
      WK = 0.0D0
      DO J = 1, NZ
         WK = WK + VFLOW(J)*( RM(J,IREP) - RM(IZ,IREP) )
      END DO
      WK = WK*CRHO


C     �����ׂ̂����A�ʐς������Ȃ����ʂ���̂���
      RNNA = SMRT1 + X(LC+72) + X(LC+73) - SMRT2*RM(IZ,IREP) + WK
      IF ( IREP.EQ.0 ) THEN
         RNNA = RNNA - X(LC+64)*RM(IZ,IREP)
      ELSE
         RNNA = RNNA - ( X(LC+64) - X(LC+65) )*RM(IZ,0)
     -        - X(LC+65)*RM(IZ,IREP)
      END IF


C     �Ȉ�MRT
      AMRT = ( RN - RNNA )/(HC*X(LC+63)) + RM(IZ,IREP)


C     �Ƌ�M�e�ʂɂ��~�M���ׂ̍X�V
      IF ( IREP.EQ.1 ) THEN
         X(LC+72) = X(LC+68)*X(LC+72)
     -            - ( X(LC+66) - X(LC+67) )*RM(IZ,0)
     -            - X(LC+67)*RM(IZ,1)
         X(LC+73) = X(LC+71)*X(LC+73)
     -            - ( X(LC+69) - X(LC+70) )*RM(IZ,0)
     -            - X(LC+70)*RM(IZ,1)
      END IF

      RETURN
      END
