C-----------------------------------------------------------------------
C     シミュレーション計算のための方程式を作成する
C     LATEST REVISION   2005.02.02（2004.02.21より）
C-----------------------------------------------------------------------
      SUBROUTINE COEFFS(LMODE,NZ,IZ,RMMX,RMMN,GRADL,CRHO,VFLOW,FIXEDL,
     -   EXCAP1,SPCAP1,ISL,IREP,LSTRT,LSZSPC,NA,AA,BB)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

C     引数
      INTEGER     LMODE          ! I   負荷状態モード（-2：暖房過負荷,-1：暖房軽負荷,
                                 !     0：無負荷,1：冷房軽負荷,2：冷房過負荷,99：停止）
      INTEGER     NZ             ! I   現在のグループのゾーン数
      INTEGER     IZ             ! I   現在のゾーンが何ゾーン目か（1<=IZ<=NZ）
      REAL        RMMX           ! I   設定温湿度上限（基準温湿度からの偏差）
      REAL        RMMN           ! I   設定温湿度下限（基準温湿度からの偏差）
                                 !     （RMMX,RMMNは基準温湿度からの偏差ではない）
      REAL        GRADL          ! I   現在のゾーンの単位温湿度変化に対する除去熱量の変化
                                 !     （符合を逆にしたもの。いつも正のはず）
      REAL        CRHO           ! I   空気の容積比熱（潜熱の場合は密度に蒸発潜熱を掛けたもの）
      REAL        VFLOW(NZ)      ! I   各ゾーンからの流入風量（体積流量、正値）
      REAL        FIXEDL         ! I   現在のゾーンの、温湿度や除去熱量に依存しない固定流入熱量
      REAL        EXCAP1         ! I   装置容量（冷却）。0以上。
      REAL        SPCAP1         ! I   装置容量（加熱）。0以上。
      INTEGER     ISL            ! I   =1:顕熱、=2:潜熱
      INTEGER     IREP           ! I   現在の時刻ステップの直前か直後か
                                 !     =0：直前あるいは二等辺三角、=1：直後
      INTEGER     LSTRT          ! I   SPACデータのうち、OWAL, IWAL等の先頭ポインタ(検索開始点)
      INTEGER     LSZSPC(0:4)    ! I   XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,
                                 !     (3):WNDW, (4):INFL の変数の数
      INTEGER     NA             ! I   行列AAの整合寸法
      REAL*8      AA(NA,NZ)      ! O   連立方程式左辺係数行列
      REAL*8      BB             ! O   連立方程式右辺係数ベクトルのIZ列目（IZゾーン目）
C     ローカル変数
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

C     軽負荷モードのときは室温湿度＝設定値という式を立てる
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

C     その他の場合は熱平衡式を用いる
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
         ELSE                    ! 停止・あるいは無負荷
            BB = FIXEDL
         END IF
      END IF

      RETURN
      END
