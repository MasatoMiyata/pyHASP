C-----------------------------------------------------------------------
C     外調機の出力
C     LATEST REVISION   2006.04.05
C-----------------------------------------------------------------------
      SUBROUTINE OAHUOUT(NHR,LL,ISEAS,IOUT,NUOT2,NWD,WD,ID,MDW,MODE)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

      INTEGER     NHR                  ! I   1日のステップ数
      INTEGER     LL                   ! I   OAHU名ポインタ（先頭）
      INTEGER     ISEAS                ! I   1:夏期、2:冬期、3:中間期
      INTEGER     IOUT                 ! I   =0:簡易出力(1h1行)、=1:詳細出力(1h2行)
      INTEGER     NUOT2                ! I   外調機出力ファイルの装置番号（最初の装置番号）
      INTEGER     NWD                  ! I   WDの整合寸法（=7）
      REAL        WD(NWD,NHR)          ! I   外界気象（基準温湿度からの偏差ではない）
      INTEGER     ID(3)                ! I   年・月・日（出力情報）
      INTEGER     MDW                  ! I   曜日（出力情報）
      INTEGER     MODE                 ! I   =1:助走、=2:本計算、=3:最終日
C     XMQ(L+179～182)                    I/O OAHU回収・処理熱量(24時正時直後)

*     ローカル変数
      INTEGER     JHR                  !     時刻
      INTEGER     IREP                 !     現在の時刻ステップの直前か直後か
                                       !     =0：直前あるいは二等辺三角、=1：直後
      INTEGER     LLWK                 !     現在のOAHU名ポインタ
      INTEGER     L1
      INTEGER     IOHU                 !     いくつ目のOAHUか
      PARAMETER ( NSL=2 )              !     顕熱と潜熱
      REAL        VFLW(-1:0)           !     風量（添字は-1:1段前,0:現段）
      REAL        TXHEX(NSL,-1:0)      !     HEX出口温湿度（第2添字は-1:1段前,0:現段）
      REAL        TXCIL(NSL,-1:0)      !     コイル出口温湿度
      REAL        QHEX(NSL,-1:0)       !     HEX回収熱量（除去・除湿が正）
      REAL        QCIL(NSL,-1:0)       !     コイル処理熱量（除去・除湿が正）
      INTEGER     ISL                  !     =1:顕熱、=2:潜熱
      INTEGER     J

      LLWK = LL
      IOHU = 0
  101 IF(LLWK.EQ.0) GO TO 109   ! OAHUループ
         IOHU = IOHU + 1
         L1 = LLWK+2+(ISEAS-1)*9
         VFLW(-1) = X(LLWK+130)
         DO ISL = 1, 2
            TXHEX(ISL,-1) = X(LLWK+30+(ISL-1)*25)
            TXCIL(ISL,-1) = X(LLWK+80+(ISL-1)*25)
            QHEX(ISL,-1) = X(LLWK+178+ISL)   ! 計算初日は0
            QCIL(ISL,-1) = X(LLWK+180+ISL)   ! 計算初日は0
         END DO
         DO JHR = 1, NHR   ! 時刻ループ
            DO IREP = 0, 1   ! 直前・直後ループ
               DO ISL = 1, 2
                  TXHEX(ISL,0) = X(LLWK+30+(ISL-1)*25+JHR)
                  TXCIL(ISL,0) = X(LLWK+80+(ISL-1)*25+JHR)
               END DO
               VFLW(0) = X(LLWK+131+(JHR-1)*2+IREP)
               CALL QOAHU(WD(1,JHR),VFLW(0),
     -            TXHEX(1,0),TXCIL(1,0),QHEX(1,0),QCIL(1,0))
               IF( MODE.GE.2 ) THEN
                  IF( IOUT.EQ.1 ) THEN   ! 詳細出力
                     WRITE(NUOT2-1+IOHU,'(I4,'','',2(I2,'',''),I1,'','',
     -               I2,'','',I1,'','',2(F6.2,'',''),2(F7.2,'',''),
     -               2(F6.2,'',''),2(F7.2,'',''),F8.1)')
     -               (ID(J), J = 1, 3), MDW, JHR, IREP,
     -               (TXHEX(ISL,0), TXCIL(ISL,0),
     -                QHEX(ISL,0)*1.163/1000.0,
     -                QCIL(ISL,0)*1.163/1000.0, ISL = 1, 2), VFLW(0)
                  ELSE IF( IREP.EQ.0 ) THEN  ! 簡易出力
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
                  ELSE   ! 簡易出力でIREP=1（後段）
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
      REAL        WD(2)                ! I   外界気象（基準温湿度からの偏差ではない）
      REAL        VFLW                 ! I   供給外気量[m3/h]
      REAL        TXHEX(2)             ! I/O HEX出口温湿度（添字は顕熱・潜熱、以下同）
      REAL        TXCIL(2)             ! I/O コイル出口温湿度
      REAL        QHEX(2)              ! O   HEX回収熱量（除去・除湿が正）
      REAL        QCIL(2)              ! O   コイル処理熱量（除去・除湿が正）
      PARAMETER ( NSL=2 )              !     顕熱と潜熱
      REAL        CRHO(NSL)            !     空気の容積比熱
                                       !     （潜熱の場合は密度に蒸発潜熱を掛けたもの）
      DATA CRHO   /0.288, 0.720/       !     単位依存

      DO ISL = 1, NSL
         IF( ABS(VFLW).LT.0.1 ) THEN   ! 供給風量0
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