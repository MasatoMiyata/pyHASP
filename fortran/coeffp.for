C-----------------------------------------------------------------------
C     一定除去熱量計算のための方程式を作成する
C     LATEST REVISION   2012.03.05
C     ・ このルーチンは、予熱時間が終了するまで、他のグループなど
C        から同時に呼ばれてはならない
C     ・ 引数のうち出力（II, AA, BB,IPS）については予熱時間終了時に
C        正しい値となる。それまで、親ルーチンにおいてこれらの
C        変数を引用・定義してはならない。
C     ・ 時間ループの中の、直前・直後ループの中の、
C        ゾーンループの中で呼ばれることを想定している。
C-----------------------------------------------------------------------
      SUBROUTINE COEFFP(IZ,KSTP,NZ,IREP,NSTP,IDLT,VFLOW,PV,PR,CRHO,VOA,
     -   FIXEDL,RMSET,REFWD,ISL,LSTRT,LSZSPC,NA,II,AA,BB,IPS)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

C     引数
      INTEGER     IZ             ! I   現在のゾーンが何ゾーン目か（1<=IZ<=NZ）
      INTEGER     KSTP           ! I   予熱開始後何ステップ目か(0<=KSTP<=NSTP)
      INTEGER     NZ             ! I   現在のグループのゾーン数
      INTEGER     IREP           ! I   現在の時刻ステップの直前か直後か
                                 !     =0：直前あるいは二等辺三角、=1：直後
      INTEGER     NSTP           ! I   予熱時間（NSTP>=1）
      INTEGER     IDLT(NSTP)     ! I   予熱開始後、各ステップにおいて直後室温湿度を
                                 !     =0：未知数としない、=1：未知数とする
                                 !     （KSTPを含んでそれ以前の値のみ引用される）
      REAL        VFLOW(NZ)      ! I   各ゾーンからの流入風量（体積流量、正値）
      REAL        PV(0:NSTP-1)   ! I   予熱開始後各ステップにおける蓄熱応答係数
                                 !     （二等辺三角、吸熱側が正）
      REAL        PR(0:NSTP)     ! I   予熱開始後各ステップにおける蓄熱応答係数
                                 !     （右側直角三角、吸熱側が正）
                                 !     PV(0), PR(0)はそれぞれ現在時刻のものを入力する
      REAL        CRHO           ! I   空気の容積比熱（潜熱の場合は密度に蒸発潜熱を掛けたもの）
      REAL        VOA            ! I   現在のゾーンに対する外気量（体積流量、0以上）
      REAL        FIXEDL         ! I   現在のゾーンの、未知変数に依存しない固定流入熱量
      REAL        RMSET(NZ)      ! I   各ゾーンの設定温湿度（予熱終了後に必ず達成される）
                                 ! I   （基準温湿度からの偏差ではない）
      REAL        REFWD          ! I   基準温湿度
      INTEGER     ISL            ! I   =1:顕熱、=2:潜熱
      INTEGER     LSTRT          ! I   SPACデータのうち、OWAL, IWAL等の先頭ポインタ(検索開始点)
      INTEGER     LSZSPC(0:4)    ! I   XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,
                                 !     (3):WNDW, (4):INFL の変数の数
      INTEGER     NA             ! I   行列AAの整合寸法
      INTEGER     II             ! I/O 未知数の数（行列のサイズ）
      REAL*8      AA(NA,NA)      ! I/O 連立方程式左辺係数行列
      REAL*8      BB(NA)         ! I/O 連立方程式右辺係数ベクトル
      INTEGER     IPS(0:NSTP)    ! I/O 予熱開始後、各時刻ステップの室温湿度が、未知ベクトルの
                                 !     何次元目から始まるか（=IPS+1次元目から）

C     ローカル変数
      INTEGER     I
      INTEGER     J
      REAL*8      VSUM        !     現在のゾーンに流入する風量の合計
      PARAMETER ( MZ=20 )     !     1グループあたり最大ゾーン数
      REAL        COEF(MZ)    !     現在のゾーンについての方程式のうち、添字目のゾーンの
                              !     現在の室温湿度に対する係数
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

C     行列要素のインデックスの更新
      IF ( (IREP.EQ.0).AND.(IZ.EQ.1) ) THEN
         IF ( KSTP.EQ.1 ) THEN
            IPS(KSTP) = IPS(KSTP-1) + NZ   ! 予熱開始直後のみ未知
         ELSE
            IPS(KSTP) = IPS(KSTP-1) + ( IDLT(KSTP-1) + 1 )*NZ
         END IF
      END IF
      II = II + 1
      IF(II.GT.NA) CALL ERROR2(140,1)

C     前処理

C     現在のゾーンに流入する風量の合計
      VSUM = 0.0D0
      DO J = 1, NZ
         VSUM = VSUM + VFLOW(J)
      END DO

C     現在の室温湿度（未知数）に対する係数（前処理）
      DO J = 1, NZ
         IF ( J.EQ.IZ ) THEN
            IF ( IREP.EQ.1 ) THEN  ! 現在が階段状変化直後である（予熱開始時刻を含む）
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

C     右辺ベクトルの初期セット
      BB(II) = FIXEDL


C     行列の該当要素への代入

C     一定除去熱量（未知数）の係数
      AA(II,IZ) = 1.0D0

C     現在・過去の室温湿度（未知数）に対する係数
      DO MSTP = 0, KSTP

C        現在の時刻ステップの室温湿度に対する係数
         IF ( MSTP.EQ.KSTP ) THEN
            IF ( (KSTP.GE.1).AND.(IREP.EQ.1) ) THEN
            ! 階段状変化直後の場合（予熱開始直後を除く）
               AA(II,IPS(MSTP)+IZ) = PV(0) - PR(0)
               ! 階段状変化直前の室温湿度に対する係数
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

            IF ( (MSTP.EQ.NSTP).AND.   ! 予熱終了時刻で、かつ
     -           ((IDLT(NSTP).EQ.0).OR.(IREP.EQ.1)) ) THEN
               ! 設定温湿度に達する側のステップ

               ! 右辺ベクトルの修正
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

C        予熱開始1時刻後から、1ステップ前までの室温湿度に対する係数（KSTP>=2の場合のみ）
         ELSE IF ( MSTP.GE.1 ) THEN
            AA(II,IPS(MSTP)+IZ) = PV(KSTP-MSTP)
     -         - PR(KSTP-MSTP)*IDLT(MSTP)
            IF ( IDLT(MSTP).EQ.1 ) THEN   ! 予熱開始MSTP後に階段状変化が生じた
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

C        予熱開始直後の室温湿度に対する係数（KSTP>=1の場合のみ）
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