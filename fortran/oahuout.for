C-----------------------------------------------------------------------
C     �O���@�̏o��
C     LATEST REVISION   2006.04.05
C-----------------------------------------------------------------------
      SUBROUTINE OAHUOUT(NHR,LL,ISEAS,IOUT,NUOT2,NWD,WD,ID,MDW,MODE)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

      INTEGER     NHR                  ! I   1���̃X�e�b�v��
      INTEGER     LL                   ! I   OAHU���|�C���^�i�擪�j
      INTEGER     ISEAS                ! I   1:�Ċ��A2:�~���A3:���Ԋ�
      INTEGER     IOUT                 ! I   =0:�ȈՏo��(1h1�s)�A=1:�ڍ׏o��(1h2�s)
      INTEGER     NUOT2                ! I   �O���@�o�̓t�@�C���̑��u�ԍ��i�ŏ��̑��u�ԍ��j
      INTEGER     NWD                  ! I   WD�̐������@�i=7�j
      REAL        WD(NWD,NHR)          ! I   �O�E�C�ہi������x����̕΍��ł͂Ȃ��j
      INTEGER     ID(3)                ! I   �N�E���E���i�o�͏��j
      INTEGER     MDW                  ! I   �j���i�o�͏��j
      INTEGER     MODE                 ! I   =1:�����A=2:�{�v�Z�A=3:�ŏI��
C     XMQ(L+179�`182)                    I/O OAHU����E�����M��(24����������)

*     ���[�J���ϐ�
      INTEGER     JHR                  !     ����
      INTEGER     IREP                 !     ���݂̎����X�e�b�v�̒��O�����ォ
                                       !     =0�F���O���邢�͓񓙕ӎO�p�A=1�F����
      INTEGER     LLWK                 !     ���݂�OAHU���|�C���^
      INTEGER     L1
      INTEGER     IOHU                 !     �����ڂ�OAHU��
      PARAMETER ( NSL=2 )              !     ���M�Ɛ��M
      REAL        VFLW(-1:0)           !     ���ʁi�Y����-1:1�i�O,0:���i�j
      REAL        TXHEX(NSL,-1:0)      !     HEX�o�������x�i��2�Y����-1:1�i�O,0:���i�j
      REAL        TXCIL(NSL,-1:0)      !     �R�C���o�������x
      REAL        QHEX(NSL,-1:0)       !     HEX����M�ʁi�����E���������j
      REAL        QCIL(NSL,-1:0)       !     �R�C�������M�ʁi�����E���������j
      INTEGER     ISL                  !     =1:���M�A=2:���M
      INTEGER     J

      LLWK = LL
      IOHU = 0
  101 IF(LLWK.EQ.0) GO TO 109   ! OAHU���[�v
         IOHU = IOHU + 1
         L1 = LLWK+2+(ISEAS-1)*9
         VFLW(-1) = X(LLWK+130)
         DO ISL = 1, 2
            TXHEX(ISL,-1) = X(LLWK+30+(ISL-1)*25)
            TXCIL(ISL,-1) = X(LLWK+80+(ISL-1)*25)
            QHEX(ISL,-1) = X(LLWK+178+ISL)   ! �v�Z������0
            QCIL(ISL,-1) = X(LLWK+180+ISL)   ! �v�Z������0
         END DO
         DO JHR = 1, NHR   ! �������[�v
            DO IREP = 0, 1   ! ���O�E���ニ�[�v
               DO ISL = 1, 2
                  TXHEX(ISL,0) = X(LLWK+30+(ISL-1)*25+JHR)
                  TXCIL(ISL,0) = X(LLWK+80+(ISL-1)*25+JHR)
               END DO
               VFLW(0) = X(LLWK+131+(JHR-1)*2+IREP)
               CALL QOAHU(WD(1,JHR),VFLW(0),
     -            TXHEX(1,0),TXCIL(1,0),QHEX(1,0),QCIL(1,0))
               IF( MODE.GE.2 ) THEN
                  IF( IOUT.EQ.1 ) THEN   ! �ڍ׏o��
                     WRITE(NUOT2-1+IOHU,'(I4,'','',2(I2,'',''),I1,'','',
     -               I2,'','',I1,'','',2(F6.2,'',''),2(F7.2,'',''),
     -               2(F6.2,'',''),2(F7.2,'',''),F8.1)')
     -               (ID(J), J = 1, 3), MDW, JHR, IREP,
     -               (TXHEX(ISL,0), TXCIL(ISL,0),
     -                QHEX(ISL,0)*1.163/1000.0,
     -                QCIL(ISL,0)*1.163/1000.0, ISL = 1, 2), VFLW(0)
                  ELSE IF( IREP.EQ.0 ) THEN  ! �ȈՏo��
                     WRITE(NUOT2-1+IOHU,'(I4,'','',2(I2,'',''),I1,'','',
     -               I2,'','',I1,'','',2(F6.2,'',''),2(F7.2,'',''),
     -               2(F6.2,'',''),2(F7.2,'',''),F8.1)')
     -               (ID(J), J = 1, 3), MDW, JHR, IREP,
     -               (0.5*(TXHEX(ISL,-1)+TXHEX(ISL,0)),
     -                0.5*(TXCIL(ISL,-1)+TXCIL(ISL,0)),
     -                0.5*(QHEX(ISL,-1)+QHEX(ISL,0))*1.163/1000.0,
     -                0.5*(QCIL(ISL,-1)+QCIL(ISL,0))*1.163/1000.0,
     -                ISL = 1, 2),
     -                0.5*(VFLW(-1)+VFLW(0))
                  ELSE   ! �ȈՏo�͂�IREP=1�i��i�j
                     VFLW(-1) = VFLW(0)
                     DO ISL = 1, 2
                        TXHEX(ISL,-1) = TXHEX(ISL,0)
                        TXCIL(ISL,-1) = TXCIL(ISL,0)
                        QHEX(ISL,-1) = QHEX(ISL,0)
                        QCIL(ISL,-1) = QCIL(ISL,0)
                     END DO
                  END IF
               END IF
            END DO
         END DO
         DO ISL = 1, 2
            X(LLWK+178+ISL) = QHEX(ISL,0)
            X(LLWK+180+ISL) = QCIL(ISL,0)
         END DO
      LLWK=M(LLWK)
      GO TO 101
  109 RETURN
      END
*-----------------------------------------------------------------------
      SUBROUTINE QOAHU(WD,VFLW,TXHEX,TXCIL,QHEX,QCIL)
      REAL        WD(2)                ! I   �O�E�C�ہi������x����̕΍��ł͂Ȃ��j
      REAL        VFLW                 ! I   �����O�C��[m3/h]
      REAL        TXHEX(2)             ! I/O HEX�o�������x�i�Y���͌��M�E���M�A�ȉ����j
      REAL        TXCIL(2)             ! I/O �R�C���o�������x
      REAL        QHEX(2)              ! O   HEX����M�ʁi�����E���������j
      REAL        QCIL(2)              ! O   �R�C�������M�ʁi�����E���������j
      PARAMETER ( NSL=2 )              !     ���M�Ɛ��M
      REAL        CRHO(NSL)            !     ��C�̗e�ϔ�M
                                       !     �i���M�̏ꍇ�͖��x�ɏ������M���|�������́j
      DATA CRHO   /0.288, 0.720/       !     �P�ʈˑ�

      DO ISL = 1, NSL
         IF( ABS(VFLW).LT.0.1 ) THEN   ! ��������0
            TXHEX(ISL) = WD(ISL)
            TXCIL(ISL) = WD(ISL)
            QHEX(ISL) = 0.0
            QCIL(ISL) = 0.0
         ELSE
            QHEX(ISL) = CRHO(ISL)*VFLW*(WD(ISL) - TXHEX(ISL))
            QCIL(ISL) = CRHO(ISL)*VFLW*(TXHEX(ISL) - TXCIL(ISL))
         END IF
      END DO

      RETURN
      END
