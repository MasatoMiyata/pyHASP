C-----------------------------------------------------------------------
C     SPAC�f�[�^�̂����A����IWAL�f�[�^��T���Đ擪�|�C���^����Ԃ�
C     LATEST REVISION   2012.03.05
C-----------------------------------------------------------------------
      SUBROUTINE RTVADJ(LSZSPC,L,JZ,ISTAT)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

C     ����
      INTEGER     LSZSPC(0:4)          ! I   XMQ�z��̂����A(0):SPAC, (1):OWAL, (2):IWAL,
                                       !     (3):WNDW, (4):INFL �̕ϐ��̐�
      INTEGER     L                    ! I   SPAC�f�[�^�̂����AOWAL, IWAL���̐擪�|�C���^
                                       !     �i�����J�n�_�j
                                       ! O   �� �������ꂽIWAL(adjacent)�̐擪�|�C���^
                                       !     (ISTAT=1,-2�̂Ƃ��̂ݗL��)
      INTEGER     JZ                   ! O   �������ꂽIWAL(adjacent)�̗אڃX�y�[�X�́A���Y
                                       !     �O���[�v�̉��X�y�[�X�ڂ�(ISTAT=1�̂Ƃ��̂ݗL���j
      INTEGER     ISTAT                ! O   =1 : IWAL(adjacent)����������
                                       !     =0 : IWAL(adjacent)�͌����炸�ɐ���I��
                                       !     =-1: �ُ�I��
                                       !     =-2: adjacent wall�ɂ��ւ�炸�א�SPAC��������Ȃ�
C     M(L)                               I   =1:OWAL, =2:IWAL, =3:WNDW, 4:INFL, 5:SPAC�I��
C     M(L+1) (IWAL�f�[�^)                I   �׎����[�h(=3�̂Ƃ�adjacent wall)
C     Q(L+2) (IWAL�f�[�^)                I   �׎�SPAC��(M(L+1)=3�̂Ƃ��L��)

C     ���[�J���ϐ�
      INTEGER     II
      CHARACTER   QSP*4                !     �׎�SPAC��(M(L+1)=3�̂Ƃ��L��)
      INTEGER     NNAM
      INTEGER     LC
      INTEGER     LD

      DO II = 1, 9999
         IF( (M(L).LT.1).OR.(M(L).GT.5) ) THEN
            CALL ERROR2(191,2)
            ISTAT = -1   ! �ُ�I��
            RETURN
         ELSE IF( M(L).EQ.5 ) THEN   ! SPAC�f�[�^�I��
            ISTAT = 0   ! �����炸�ɐ���I��
            RETURN
         ELSE IF( M(L).NE.2 ) THEN   ! IWAL�ȊO
            L = L + LSZSPC(M(L))
         ELSE IF( M(L+1).NE.3 ) THEN ! IWAL����adjacent wall�ł͂Ȃ�
            L = L + LSZSPC(M(L))
         ELSE   ! IWAL��adjacent wall
            CALL NAME(QSP,M(L+2))
            CALL RETRIV(106,QSP,NNAM,LC,LD)
            IF(LD.NE.LC) THEN
               ISTAT = -2   ! adjacent wall�����א�SPAC�͌�����Ȃ�
                            ! �iERROR5�����B������L �����͐������Ԃ���j
            ELSE
               JZ = M(LC+101)   ! �אڃX�y�[�X�͓��Y�O���[�v�̂������Ԗڂɓo�^����Ă��邩
               ISTAT = 1   ! IWAL(adjacent)�Ƃ��̗אڃX�y�[�X����������
            END IF
            RETURN
         END IF
      END DO
      CALL ERROR2(192,2)
      ISTAT = -1   ! �ُ�I��

      RETURN
      END
