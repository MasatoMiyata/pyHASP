C-----------------------------------------------------------------------
C     �O�C�����x�̏o��
C     LATEST REVISION   2008.08.02
C-----------------------------------------------------------------------
      SUBROUTINE WEOUT(NHR,NWD,WD,ID,MDW,MODE,IOUT,NUOW)
      INTEGER     NHR                  ! I   1���̃X�e�b�v��
      INTEGER     NWD                  ! I   WD�̐������@�i=7�j
      REAL        WD(NWD,NHR)          ! I   �O�E�C�ہi������x����̕΍��ł͂Ȃ��j
      INTEGER     ID(3)                ! I   �N�E���E���i�o�͏��j
      INTEGER     MDW                  ! I   �j���i�o�͏��j
      INTEGER     MODE                 ! I   =1:�����A=2:�{�v�Z�A=3:�ŏI��
      INTEGER     IOUT                 ! I   =0:�ȈՏo��(1h1�s)�A=1:�ڍ׏o��(1h2�s)
      INTEGER     NUOW                 ! I   ���u�ԍ��i�O�C���E�O�C���x�t�@�C���o�́j

      PARAMETER ( NSL=2 )              !     ���M�Ɛ��M
      PARAMETER ( MHR=24 )             !     �������@�i=NHR�j

      REAL        WETH(0:1,MHR,NSL)    !     �O�C���E��Ύ��x�i������x����̕΍��ł͂Ȃ��j
                                       !     (���Y����IREP)
      REAL        WOUT(-1:0,NSL)       !     �O�C���E��Ύ��x�i������x����̕΍��ł͂Ȃ��j
                                       !     (���Y����-1:1�X�e�b�v�O, 0:���X�e�b�v)
      DATA WOUT   /4*0/
      SAVE WOUT


      DO JHR = 1, NHR   ! �������[�v
         DO IREP = 0, 1   ! ���O�E���ニ�[�v

C           �O�C�����x�̃Z�b�g
            DO ISL = 1, NSL   ! ���M�E���M loop
               WETH(IREP,JHR,ISL) = WD(ISL,JHR)
            END DO

C           �o��
            IF ( IOUT.EQ.1 ) THEN
               IF ( MODE.GE.2 ) THEN
                  WRITE(NUOW,'(I4,'','',2(I2,'',''),I1,'','',
     -            I2,'','',I1,'','',F5.1,'','',F5.1)')
     -            ( ID(J), J = 1, 3 ),
     -            MDW, JHR, IREP, (WETH(IREP,JHR,ISL), ISL = 1, NSL)
               END IF
            ELSE   ! �ȈՏo�́i1���ԕ��̕��ς̏o�́j
               DO ISL = 1, NSL
                  WOUT(0,ISL) = WETH(IREP,JHR,ISL)
               END DO
               IF ( (IREP.EQ.0).AND.(MODE.GE.2) ) THEN   ! ���ς�����ďo��
                  DO ISL = 1, NSL
                     WOUT(0,ISL) = 0.5*(WOUT(-1,ISL) + WOUT(0,ISL))
                  END DO
                  WRITE(NUOW,'(I4,'','',2(I2,'',''),I1,'','',
     -            I2,'','',I1,'','',F5.1,'','',F5.1)')
     -            ( ID(J), J = 1, 3 ),
     -            MDW, JHR, IREP, (WOUT(0,ISL), ISL = 1, NSL)
               ELSE IF ( IREP.EQ.1 ) THEN   ! ���X�e�b�v�̂��߂ɋL��
                  DO ISL = 1, NSL
                     WOUT(-1,ISL) = WOUT(0,ISL)
                  END DO
               END IF
            END IF
         END DO
      END DO

      RETURN
      END
