C-----------------------------------------------------------------------
C     �V�~�����[�V�����v�Z�̂��߂̕��������쐬����
C     LATEST REVISION   2005.02.02�i2004.02.21���j
C-----------------------------------------------------------------------
      SUBROUTINE COEFFS(LMODE,NZ,IZ,RMMX,RMMN,GRADL,CRHO,VFLOW,FIXEDL,
     -   EXCAP1,SPCAP1,ISL,IREP,LSTRT,LSZSPC,NA,AA,BB)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

C     ����
      INTEGER     LMODE          ! I   ���׏�ԃ��[�h�i-2�F�g�[�ߕ���,-1�F�g�[�y����,
                                 !     0�F������,1�F��[�y����,2�F��[�ߕ���,99�F��~�j
      INTEGER     NZ             ! I   ���݂̃O���[�v�̃]�[����
      INTEGER     IZ             ! I   ���݂̃]�[�������]�[���ڂ��i1<=IZ<=NZ�j
      REAL        RMMX           ! I   �ݒ艷���x����i������x����̕΍��j
      REAL        RMMN           ! I   �ݒ艷���x�����i������x����̕΍��j
                                 !     �iRMMX,RMMN�͊�����x����̕΍��ł͂Ȃ��j
      REAL        GRADL          ! I   ���݂̃]�[���̒P�ʉ����x�ω��ɑ΂��鏜���M�ʂ̕ω�
                                 !     �i�������t�ɂ������́B�������̂͂��j
      REAL        CRHO           ! I   ��C�̗e�ϔ�M�i���M�̏ꍇ�͖��x�ɏ������M���|�������́j
      REAL        VFLOW(NZ)      ! I   �e�]�[������̗������ʁi�̐ϗ��ʁA���l�j
      REAL        FIXEDL         ! I   ���݂̃]�[���́A�����x�⏜���M�ʂɈˑ����Ȃ��Œ藬���M��
      REAL        EXCAP1         ! I   ���u�e�ʁi��p�j�B0�ȏ�B
      REAL        SPCAP1         ! I   ���u�e�ʁi���M�j�B0�ȏ�B
      INTEGER     ISL            ! I   =1:���M�A=2:���M
      INTEGER     IREP           ! I   ���݂̎����X�e�b�v�̒��O�����ォ
                                 !     =0�F���O���邢�͓񓙕ӎO�p�A=1�F����
      INTEGER     LSTRT          ! I   SPAC�f�[�^�̂����AOWAL, IWAL���̐擪�|�C���^(�����J�n�_)
      INTEGER     LSZSPC(0:4)    ! I   XMQ�z��̂����A(0):SPAC, (1):OWAL, (2):IWAL,
                                 !     (3):WNDW, (4):INFL �̕ϐ��̐�
      INTEGER     NA             ! I   �s��AA�̐������@
      REAL*8      AA(NA,NZ)      ! O   �A�����������ӌW���s��
      REAL*8      BB             ! O   �A���������E�ӌW���x�N�g����IZ��ځiIZ�]�[���ځj
C     ���[�J���ϐ�
      INTEGER     J
      INTEGER     L
      INTEGER     IWL
      INTEGER     JZ
      INTEGER     ISTAT

      IF((NZ.GT.NA).OR.(IZ.GT.NZ)) CALL ERROR2(179,2)
      IF(GRADL.LT.0) CALL ERROR2(178,2)
      IF(EXCAP1.LT.0) CALL ERROR2(177,2)
      IF(SPCAP1.LT.0) CALL ERROR2(176,2)
      IF(RMMN.GT.RMMX) CALL ERROR2(175,2)

C     �y���׃��[�h�̂Ƃ��͎������x���ݒ�l�Ƃ������𗧂Ă�
      IF ( ( LMODE.EQ.1).OR.( LMODE.EQ.-1 ) ) THEN
         DO J = 1, NZ
            IF ( J.EQ.IZ ) THEN
               AA(IZ,J) = 1.0D0
            ELSE
               AA(IZ,J) = 0.0D0
            END IF
         END DO

         IF ( LMODE.EQ.1 ) THEN
            BB = RMMX
         ELSE
            BB = RMMN
         END IF

C     ���̑��̏ꍇ�͔M���t����p����
      ELSE
         DO J = 1, NZ
            IF ( J.EQ.IZ ) THEN
               AA(IZ,J) = GRADL
            ELSE
               IF(VFLOW(J).LT.0) CALL ERROR2(174,2)
               AA(IZ,J) = -CRHO*VFLOW(J)
            END IF
         END DO

         IF ( ISL.EQ.1 ) THEN
            L = LSTRT
            DO IWL = 1, 9999
               CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
               IF ( ISTAT.NE.1 ) THEN
                  GO TO 100
               ELSE
                  AA(IZ,JZ) = AA(IZ,JZ) - X(L+3+IREP)
                  L = L + LSZSPC(M(L))
               END IF
            END DO
         END IF
  100    CONTINUE

         IF ( LMODE.EQ.2 ) THEN
            BB = FIXEDL - EXCAP1
         ELSE IF ( LMODE.EQ.-2 ) THEN
            BB = FIXEDL + SPCAP1
         ELSE                    ! ��~�E���邢�͖�����
            BB = FIXEDL
         END IF
      END IF

      RETURN
      END
