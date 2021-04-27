C-----------------------------------------------------------------------
C     �����M�ʌv�Z�̂��߂̑O�����i����1�j
C     LATEST REVISION   2005.01.14 (extrc1.for 2004.02.21���j
C-----------------------------------------------------------------------
      SUBROUTINE EXTRC0(JHR,LOPC,LC,ISEAS,KSCH,IOPTWK)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

C     ����
      INTEGER     JHR                  ! I   ����
      INTEGER     LOPC                 ! I   OPCO�f�[�^�ւ̃|�C���^(L)
      INTEGER     LC                   ! I   SPAC�f�[�^�ւ̃|�C���^(L)
      INTEGER     ISEAS(2)             ! I   �{���A�����̋G�� �l=1:�Ċ��A2:�~���A3:���Ԋ�
      INTEGER     KSCH(2)              ! I   �{���A�����̃X�P�W���[���l(1�`3�A3�͋󒲂͒�~)
      INTEGER     IOPTWK               ! 0   �󒲉^�]��ԃt���O�A=0:��~���A
                                       !     =1:�^�]���A=2:�N���A=3:��~
C     X(119,120,...,143)                 I   �󒲗p�_�~�[�X�P�W���[���i=-1,0,0,..,0�j
C     X(LOPC+14,15,...,163)              I   �󒲔��⑀��i=1:�N���A=-1:��~�A=0:��Ԍp���j
C     M(LC+60)                           I   1���ԑO���猻�����܂ł̉^�]��ԁi0 or 1)
C                                        O   ����������1���Ԍ�܂ł̉^�]��ԁi0 or 1)

C     ���[�J���ϐ�
      INTEGER     LPNT(2)              !     �����A�����̋󒲃X�P�W���[���|�C���^(0:00�̑���)
      INTEGER     ISTATB               !     1���ԑO���猻�����܂ł̉^�]��ԁi0 or 1)
      INTEGER     ISTATA               !     ����������1���Ԍ�܂ł̉^�]��ԁi0 or 1)
      INTEGER     ISW                  !     =1:�󒲒�~����on, -1:�󒲎���off, 0:��Ԍp��
      INTEGER     I


C     �����A�����̋󒲃X�P�W���[���|�C���^(0:00�̑���)
      DO I = 1, 2
         IF ( KSCH(I).EQ.3 ) THEN   ! 1�����x�~�̏ꍇ
            LPNT(I) = 119   ! �_�~�[�X�P�W���[�����Q��
         ELSE
            LPNT(I) = LOPC + 14 + (ISEAS(I)-1)*50 + (KSCH(I)-1)*25
         END IF
      END DO


C     �󒲉^�]���[�h ( IOPTG )

      ! ���O�̋󒲉^�]���
      ISTATB = M(LC+60)

      ! ��������on-off����
      IF ( ( JHR.EQ.24 ).AND.( NINT(X(LPNT(2))).NE.0 ) ) THEN
        ISW = NINT(X(LPNT(2)))   ! �����u0���v��on-off����Ƃ���
      ELSE
        ISW = NINT(X(LPNT(1)+JHR))
      END IF

      IF ( ( ISTATB.EQ.0 ).AND.( ISW.EQ.1 ) ) THEN
         IOPTWK = 2   ! �N��
         ISTATA = 1
      ELSE IF ( ( ISTATB.EQ.1 ).AND.( ISW.EQ.-1 ) ) THEN
         IOPTWK = 3   ! ��~
         ISTATA = 0
      ELSE
         IOPTWK = ISTATB   ! ��~�����邢�͉^�]��
         ISTATA = ISTATB
      END IF
      M(LC+60) = ISTATA   ! ����̋󒲉^�]���

      RETURN
      END
