C-----------------------------------------------------------------------
C     外気温湿度の出力
C     LATEST REVISION   2008.08.02
C-----------------------------------------------------------------------
      SUBROUTINE WEOUT(NHR,NWD,WD,ID,MDW,MODE,IOUT,NUOW)
      INTEGER     NHR                  ! I   1日のステップ数
      INTEGER     NWD                  ! I   WDの整合寸法（=7）
      REAL        WD(NWD,NHR)          ! I   外界気象（基準温湿度からの偏差ではない）
      INTEGER     ID(3)                ! I   年・月・日（出力情報）
      INTEGER     MDW                  ! I   曜日（出力情報）
      INTEGER     MODE                 ! I   =1:助走、=2:本計算、=3:最終日
      INTEGER     IOUT                 ! I   =0:簡易出力(1h1行)、=1:詳細出力(1h2行)
      INTEGER     NUOW                 ! I   装置番号（外気温・外気湿度ファイル出力）

      PARAMETER ( NSL=2 )              !     顕熱と潜熱
      PARAMETER ( MHR=24 )             !     整合寸法（=NHR）

      REAL        WETH(0:1,MHR,NSL)    !     外気温・絶対湿度（基準温湿度からの偏差ではない）
                                       !     (第一添字はIREP)
      REAL        WOUT(-1:0,NSL)       !     外気温・絶対湿度（基準温湿度からの偏差ではない）
                                       !     (第一添字は-1:1ステップ前, 0:現ステップ)
      DATA WOUT   /4*0/
      SAVE WOUT


      DO JHR = 1, NHR   ! 時刻ループ
         DO IREP = 0, 1   ! 直前・直後ループ

C           外気温湿度のセット
            DO ISL = 1, NSL   ! 顕熱・潜熱 loop
               WETH(IREP,JHR,ISL) = WD(ISL,JHR)
            END DO

C           出力
            IF ( IOUT.EQ.1 ) THEN
               IF ( MODE.GE.2 ) THEN
                  WRITE(NUOW,'(I4,'','',2(I2,'',''),I1,'','',
     -            I2,'','',I1,'','',F5.1,'','',F5.1)')
     -            ( ID(J), J = 1, 3 ),
     -            MDW, JHR, IREP, (WETH(IREP,JHR,ISL), ISL = 1, NSL)
               END IF
            ELSE   ! 簡易出力（1時間分の平均の出力）
               DO ISL = 1, NSL
                  WOUT(0,ISL) = WETH(IREP,JHR,ISL)
               END DO
               IF ( (IREP.EQ.0).AND.(MODE.GE.2) ) THEN   ! 平均を取って出力
                  DO ISL = 1, NSL
                     WOUT(0,ISL) = 0.5*(WOUT(-1,ISL) + WOUT(0,ISL))
                  END DO
                  WRITE(NUOW,'(I4,'','',2(I2,'',''),I1,'','',
     -            I2,'','',I1,'','',F5.1,'','',F5.1)')
     -            ( ID(J), J = 1, 3 ),
     -            MDW, JHR, IREP, (WOUT(0,ISL), ISL = 1, NSL)
               ELSE IF ( IREP.EQ.1 ) THEN   ! 次ステップのために記憶
                  DO ISL = 1, NSL
                     WOUT(-1,ISL) = WOUT(0,ISL)
                  END DO
               END IF
            END IF
         END DO
      END DO

      RETURN
      END
