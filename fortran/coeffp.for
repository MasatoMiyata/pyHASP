C-----------------------------------------------------------------------
C     ��菜���M�ʌv�Z�̂��߂̕��������쐬����
C     LATEST REVISION   2012.03.05
C     �E ���̃��[�`���́A�\�M���Ԃ��I������܂ŁA���̃O���[�v�Ȃ�
C        ���瓯���ɌĂ΂�Ă͂Ȃ�Ȃ�
C     �E �����̂����o�́iII, AA, BB,IPS�j�ɂ��Ă͗\�M���ԏI������
C        �������l�ƂȂ�B����܂ŁA�e���[�`���ɂ����Ă�����
C        �ϐ������p�E��`���Ă͂Ȃ�Ȃ��B
C     �E ���ԃ��[�v�̒��́A���O�E���ニ�[�v�̒��́A
C        �]�[�����[�v�̒��ŌĂ΂�邱�Ƃ�z�肵�Ă���B
C-----------------------------------------------------------------------
      SUBROUTINE COEFFP(IZ,KSTP,NZ,IREP,NSTP,IDLT,VFLOW,PV,PR,CRHO,VOA,
     -   FIXEDL,RMSET,REFWD,ISL,LSTRT,LSZSPC,NA,II,AA,BB,IPS)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

C     ����
      INTEGER     IZ             ! I   ���݂̃]�[�������]�[���ڂ��i1<=IZ<=NZ�j
      INTEGER     KSTP           ! I   �\�M�J�n�㉽�X�e�b�v�ڂ�(0<=KSTP<=NSTP)
      INTEGER     NZ             ! I   ���݂̃O���[�v�̃]�[����
      INTEGER     IREP           ! I   ���݂̎����X�e�b�v�̒��O�����ォ
                                 !     =0�F���O���邢�͓񓙕ӎO�p�A=1�F����
      INTEGER     NSTP           ! I   �\�M���ԁiNSTP>=1�j
      INTEGER     IDLT(NSTP)     ! I   �\�M�J�n��A�e�X�e�b�v�ɂ����Ē��㎺�����x��
                                 !     =0�F���m���Ƃ��Ȃ��A=1�F���m���Ƃ���
                                 !     �iKSTP���܂�ł���ȑO�̒l�݈̂��p�����j
      REAL        VFLOW(NZ)      ! I   �e�]�[������̗������ʁi�̐ϗ��ʁA���l�j
      REAL        PV(0:NSTP-1)   ! I   �\�M�J�n��e�X�e�b�v�ɂ�����~�M�����W��
                                 !     �i�񓙕ӎO�p�A�z�M�������j
      REAL        PR(0:NSTP)     ! I   �\�M�J�n��e�X�e�b�v�ɂ�����~�M�����W��
                                 !     �i�E�����p�O�p�A�z�M�������j
                                 !     PV(0), PR(0)�͂��ꂼ�ꌻ�ݎ����̂��̂���͂���
      REAL        CRHO           ! I   ��C�̗e�ϔ�M�i���M�̏ꍇ�͖��x�ɏ������M���|�������́j
      REAL        VOA            ! I   ���݂̃]�[���ɑ΂���O�C�ʁi�̐ϗ��ʁA0�ȏ�j
      REAL        FIXEDL         ! I   ���݂̃]�[���́A���m�ϐ��Ɉˑ����Ȃ��Œ藬���M��
      REAL        RMSET(NZ)      ! I   �e�]�[���̐ݒ艷���x�i�\�M�I����ɕK���B�������j
                                 ! I   �i������x����̕΍��ł͂Ȃ��j
      REAL        REFWD          ! I   ������x
      INTEGER     ISL            ! I   =1:���M�A=2:���M
      INTEGER     LSTRT          ! I   SPAC�f�[�^�̂����AOWAL, IWAL���̐擪�|�C���^(�����J�n�_)
      INTEGER     LSZSPC(0:4)    ! I   XMQ�z��̂����A(0):SPAC, (1):OWAL, (2):IWAL,
                                 !     (3):WNDW, (4):INFL �̕ϐ��̐�
      INTEGER     NA             ! I   �s��AA�̐������@
      INTEGER     II             ! I/O ���m���̐��i�s��̃T�C�Y�j
      REAL*8      AA(NA,NA)      ! I/O �A�����������ӌW���s��
      REAL*8      BB(NA)         ! I/O �A���������E�ӌW���x�N�g��
      INTEGER     IPS(0:NSTP)    ! I/O �\�M�J�n��A�e�����X�e�b�v�̎������x���A���m�x�N�g����
                                 !     �������ڂ���n�܂邩�i=IPS+1�����ڂ���j

C     ���[�J���ϐ�
      INTEGER     I
      INTEGER     J
      REAL*8      VSUM        !     ���݂̃]�[���ɗ������镗�ʂ̍��v
      PARAMETER ( MZ=20 )     !     1�O���[�v������ő�]�[����
      REAL        COEF(MZ)    !     ���݂̃]�[���ɂ��Ă̕������̂����A�Y���ڂ̃]�[����
                              !     ���݂̎������x�ɑ΂���W��
      INTEGER     MSTP
      REAL*8      WK0, WK1
      INTEGER     L
      INTEGER     IWL
      INTEGER     JZ
      INTEGER     ISTAT

      IF((NZ.GT.MZ).OR.(IZ.GT.NZ)) CALL ERROR2(149,2)
      IF((KSTP.LT.0).OR.(KSTP.GT.NSTP)) CALL ERROR2(148,2)
      IF((IREP.NE.0).AND.(IREP.NE.1)) CALL ERROR2(147,2)
      IF(NSTP.LT.1) CALL ERROR2(146,2)
      IF((KSTP.EQ.0).AND.(IREP.EQ.0)) CALL ERROR2(145,2)

C     �s��v�f�̃C���f�b�N�X�̍X�V
      IF ( (IREP.EQ.0).AND.(IZ.EQ.1) ) THEN
         IF ( KSTP.EQ.1 ) THEN
            IPS(KSTP) = IPS(KSTP-1) + NZ   ! �\�M�J�n����̂ݖ��m
         ELSE
            IPS(KSTP) = IPS(KSTP-1) + ( IDLT(KSTP-1) + 1 )*NZ
         END IF
      END IF
      II = II + 1
      IF(II.GT.NA) CALL ERROR2(140,1)

C     �O����

C     ���݂̃]�[���ɗ������镗�ʂ̍��v
      VSUM = 0.0D0
      DO J = 1, NZ
         VSUM = VSUM + VFLOW(J)
      END DO

C     ���݂̎������x�i���m���j�ɑ΂���W���i�O�����j
      DO J = 1, NZ
         IF ( J.EQ.IZ ) THEN
            IF ( IREP.EQ.1 ) THEN  ! ���݂��K�i��ω�����ł���i�\�M�J�n�������܂ށj
               COEF(J) = PR(0) + CRHO*( VOA + VSUM )
            ELSE
               COEF(J) = PV(0) + CRHO*( VOA + VSUM )
            END IF
         ELSE
            COEF(J) = -CRHO*VFLOW(J)
         END IF
      END DO
      IF ( ISL.EQ.1 ) THEN
         L = LSTRT
         DO IWL = 1, 9999
            CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
            IF ( ISTAT.NE.1 ) THEN
               GO TO 100
            ELSE
               IF ( IREP.EQ.1 ) THEN
                  COEF(JZ) = COEF(JZ) - X(L+4)
               ELSE
                  COEF(JZ) = COEF(JZ) - X(L+3)
               END IF
               L = L + LSZSPC(M(L))
            END IF
         END DO
         CALL ERROR2(143,2)
      END IF
  100 CONTINUE

C     �E�Ӄx�N�g���̏����Z�b�g
      BB(II) = FIXEDL


C     �s��̊Y���v�f�ւ̑��

C     ��菜���M�ʁi���m���j�̌W��
      AA(II,IZ) = 1.0D0

C     ���݁E�ߋ��̎������x�i���m���j�ɑ΂���W��
      DO MSTP = 0, KSTP

C        ���݂̎����X�e�b�v�̎������x�ɑ΂���W��
         IF ( MSTP.EQ.KSTP ) THEN
            IF ( (KSTP.GE.1).AND.(IREP.EQ.1) ) THEN
            ! �K�i��ω�����̏ꍇ�i�\�M�J�n����������j
               AA(II,IPS(MSTP)+IZ) = PV(0) - PR(0)
               ! �K�i��ω����O�̎������x�ɑ΂���W��
               IF ( ISL.EQ.1 ) THEN
                  L = LSTRT
                  DO IWL = 1, 9999
                     CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                     IF ( ISTAT.NE.1 ) THEN
                        GO TO 200
                     ELSE
                        AA(II,IPS(MSTP)+JZ) = AA(II,IPS(MSTP)+JZ)
     -                     - ( X(L+3) - X(L+4) )
                        L = L + LSZSPC(M(L))
                     END IF
                  END DO
               END IF
  200          CONTINUE
            END IF

            IF ( (MSTP.EQ.NSTP).AND.   ! �\�M�I�������ŁA����
     -           ((IDLT(NSTP).EQ.0).OR.(IREP.EQ.1)) ) THEN
               ! �ݒ艷���x�ɒB���鑤�̃X�e�b�v

               ! �E�Ӄx�N�g���̏C��
               WK1 = 0.0D0
               DO J = 1, NZ
                  WK1 = WK1 + COEF(J)*( RMSET(J) - REFWD )
               END DO
               BB(II) = BB(II) - WK1
            ELSE
               DO J = 1, NZ
                  IF ( KSTP.EQ.0 ) THEN
                     AA(II,IPS(MSTP)+J) = COEF(J)
                  ELSE
                     AA(II,IPS(MSTP)+IREP*NZ+J) = COEF(J)
                  END IF
               END DO
            END IF

C        �\�M�J�n1�����ォ��A1�X�e�b�v�O�܂ł̎������x�ɑ΂���W���iKSTP>=2�̏ꍇ�̂݁j
         ELSE IF ( MSTP.GE.1 ) THEN
            AA(II,IPS(MSTP)+IZ) = PV(KSTP-MSTP)
     -         - PR(KSTP-MSTP)*IDLT(MSTP)
            IF ( IDLT(MSTP).EQ.1 ) THEN   ! �\�M�J�nMSTP��ɊK�i��ω���������
               AA(II,IPS(MSTP)+NZ+IZ) = PR(KSTP-MSTP)
            END IF
            IF ( ISL.EQ.1 ) THEN
               L = LSTRT
               DO IWL = 1, 9999
                  CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                  IF ( ISTAT.NE.1 ) THEN
                     GO TO 300
                  ELSE
                     WK0 = X(L+5)*X(L+ 7)**(KSTP-MSTP-1)
     -                   + X(L+8)*X(L+10)**(KSTP-MSTP-1)
                     WK1 = X(L+6)*X(L+ 7)**(KSTP-MSTP-1)
     -                   + X(L+9)*X(L+10)**(KSTP-MSTP-1)
                     AA(II,IPS(MSTP)+JZ) = AA(II,IPS(MSTP)+JZ)
     -                  - ( WK0 - WK1*IDLT(MSTP) )
                     IF ( IDLT(MSTP).EQ.1 ) THEN
                        AA(II,IPS(MSTP)+NZ+JZ)
     -                     = AA(II,IPS(MSTP)+NZ+JZ) - WK1
                     END IF
                     L = L + LSZSPC(M(L))
                  END IF
               END DO
            END IF
  300       CONTINUE

C        �\�M�J�n����̎������x�ɑ΂���W���iKSTP>=1�̏ꍇ�̂݁j
         ELSE
            AA(II,IPS(MSTP)+IZ) = PR(KSTP)
            IF ( ISL.EQ.1 ) THEN
               L = LSTRT
               DO IWL = 1, 9999
                  CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                  IF ( ISTAT.NE.1 ) THEN
                     GO TO 400
                  ELSE
                     WK1 = X(L+6)*X(L+ 7)**(KSTP-1)
     -                   + X(L+9)*X(L+10)**(KSTP-1)
                     AA(II,IPS(MSTP)+JZ)
     -                  = AA(II,IPS(MSTP)+JZ) - WK1
                     L = L + LSZSPC(M(L))
                  END IF
               END DO
            END IF
  400       CONTINUE
         END IF
      END DO   ! MSTP loop

      IF ( (KSTP.EQ.NSTP).AND.(IZ.EQ.NZ).AND.
     -     ((IDLT(NSTP).EQ.0).OR.(IREP.EQ.1)) ) THEN
         IF(II.NE.IPS(KSTP)+IDLT(KSTP)*NZ) CALL ERROR2(144,2)
      END IF

      RETURN
      END
