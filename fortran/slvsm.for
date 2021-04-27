C-----------------------------------------------------------------------
C     シミュレーション計算
C     LATEST REVISION   2012.03.30
C-----------------------------------------------------------------------
      SUBROUTINE SLVSM(NZ,IOPT,EXCAP,SPCAP,NAZ,VFLOW,P0,CRHO,VOA,
     -   RMMX,RMMN,REFWD,IPEAK,ISEAS,NITER,FIXEDL,ISL,IREP,LCG,LSZSPC,
     -   LMODE,AN,RM,NA,AA,BB,IP)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

C     引数
      INTEGER     NZ             ! I   現在のグループのゾーン数
      INTEGER     IOPT(NZ)       ! I   現段の空調運転状態（0:停止,その他:稼動）
      REAL        EXCAP(NZ)      ! I   各スペースの装置容量（冷却、0以上）
      REAL        SPCAP(NZ)      ! I   各スペースの装置容量（加熱、0以上）
      INTEGER     NAZ            ! I   配列VFLOWの整合寸法
      REAL        VFLOW(NAZ,NZ)  ! I   第1添字目のゾーンから第2添字目のゾーンへの流入風量
                                 !     （体積流量、0以上、対角項は0となっていなければならない）
      REAL        P0(NZ)         ! I   各スペースの瞬時蓄熱応答係数（吸熱される場合が正）
      REAL        CRHO           ! I   空気の容積比熱（潜熱の場合は密度に蒸発潜熱を掛けたもの）
      REAL        VOA(NZ)        ! I   各ゾーンの外気量（体積流量、0以上）
      REAL        RMMX(NZ)       ! I   各スペースの設定温湿度上限
      REAL        RMMN(NZ)       ! I   各スペースの設定温湿度下限
                                 !     （RMMX,RMMNは基準温湿度からの偏差ではない）
      REAL        REFWD          ! I   基準温湿度
      INTEGER     IPEAK          ! I   1:ピーク計算モード、0:シミュレーションモード
      INTEGER     ISEAS          ! I   1:夏期、2:冬期、3:中間期
      INTEGER     NITER          ! I   最大収束計算数
      REAL        FIXEDL(NZ)     ! I   各ゾーンの、温湿度や除去熱量に依存しない固定流入熱量
      INTEGER     ISL            ! I   =1:顕熱、=2:潜熱
      INTEGER     IREP           ! I   現在の時刻ステップの直前か直後か
                                 !     =0：直前あるいは二等辺三角、=1：直後
      INTEGER     LCG(NZ)        ! I   XMQ配列のSPACデータへのポインタ（L）
      INTEGER     LSZSPC(0:4)    ! I   XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,
                                 !     (3):WNDW, (4):INFL の変数の数
      INTEGER     LMODE(NZ)      ! O   各ゾーンの負荷状態モード（-2：暖房過負荷,-1：暖房軽負荷,
                                 !     0：無負荷,1：冷房軽負荷,2：冷房過負荷,9：停止）
      REAL        AN(NZ)         ! O   各ゾーンの装置除去熱量
      REAL        RM(NZ)         ! O   各ゾーンの室温湿度（基準温湿度からの偏差）
      INTEGER     NA             ! I   AAの整合寸法
      REAL*8      AA(NA,NZ)      !     Work Array （連立方程式左辺係数行列）
      REAL*8      BB(NZ)         !     Work Array （連立方程式右辺係数ベクトル）
      INTEGER     IP(NZ)         !     Work Array

C     ローカル変数
      INTEGER     IZ
      PARAMETER ( MZ=20 )        !     1グループあたり最大ゾーン数
      REAL        EXCAP1(MZ)     !     各ゾーンの装置容量（冷却、0以上）
      REAL        SPCAP1(MZ)     !     各ゾーンの装置容量（加熱、0以上）
      REAL*8      WK
      INTEGER     J
      REAL        GRADL(MZ)      !     各ゾーンの単位温湿度変化に対する除去熱量の
                                 !     変化（符合を逆にしたもの。正値）
      REAL        CNST(4,MZ)     !     第2添字目のゾーンの負荷状態（過負荷等）の分岐点を
                                 !     与える負荷の成分
                                 !     第1添字目の添字1:暖房過負荷と暖房軽負荷の境界
                                 !           添字2:暖房軽負荷と無負荷の境界
                                 !           添字3:無負荷と冷房軽負荷の境界
                                 !           添字4:冷房軽負荷と冷房過負荷の境界
      INTEGER     ITER
      INTEGER     ICONV          !     収束判定フラグ（1:収束、0:非収束）
      INTEGER     INFO
      PARAMETER ( EPS2=0.01 )    !     収束判定のための許容範囲（kcal/h）
      INTEGER     L
      INTEGER     IWL
      INTEGER     JZ
      INTEGER     ISTAT

      IF(NZ.GT.MZ) CALL ERROR2(169,2)
      DO IZ = 1, NZ
         IF(VOA(IZ).LT.0) CALL ERROR2(168,2)
         DO J = 1, NZ
            IF ( J.EQ.IZ ) THEN
               IF(ABS(VFLOW(IZ,J)).GT.0.01) CALL ERROR2(167,2)
            ELSE
               IF(VFLOW(IZ,J).LT.0) CALL ERROR2(166,2)
            END IF
         END DO
         IF(RMMN(IZ).GT.RMMX(IZ)) CALL ERROR2(165,2)
         IF(EXCAP(IZ).LT.0) CALL ERROR2(164,2)
         IF(SPCAP(IZ).LT.0) CALL ERROR2(163,2)
      END DO

      DO IZ = 1, NZ   ! ゾーン loop

C        初期負荷状態モードを無負荷（非空調時は停止）とする
         IF ( IOPT(IZ).EQ.0 ) THEN
            LMODE(IZ) = 9
         ELSE
            LMODE(IZ) = 0
         END IF

C        装置容量をセットする
         IF ( (IPEAK.EQ.1).AND.(ISEAS.LE.2) ) THEN
         ! 容量計算夏期・冬期モードである
C           装置容量を無限大にセットする
            EXCAP1(IZ) = 9.9E+10
            SPCAP1(IZ) = 9.9E+10
         ELSE  ! シミュレーションモードあるいは容量計算モード中間期
C           装置容量は引数で渡されたものをそのまま用いる
            EXCAP1(IZ) = EXCAP(IZ)
            SPCAP1(IZ) = SPCAP(IZ)
         END IF

C        熱平衡式の傾きを求める
         WK = 0.0D0
         DO J = 1, NZ
            WK = WK + VFLOW(J,IZ)
         END DO
         GRADL(IZ) = P0(IZ) + CRHO*( VOA(IZ) + WK )

C        負荷状態モードの分岐点を求める
         CNST(1,IZ) = -SPCAP1(IZ) + GRADL(IZ)*(RMMN(IZ)-REFWD)
         CNST(2,IZ) = GRADL(IZ)*(RMMN(IZ)-REFWD)
         CNST(3,IZ) = GRADL(IZ)*(RMMX(IZ)-REFWD)
         CNST(4,IZ) = EXCAP1(IZ) + GRADL(IZ)*(RMMX(IZ)-REFWD)
      END DO   ! ゾーン loop

      DO ITER = 1, NITER   ! 収束計算 loop
         ICONV = 1

         DO IZ = 1, NZ   ! ゾーン loop
C           係数行列の該当部分に値をセットする
            CALL COEFFS(LMODE(IZ),NZ,IZ,RMMX(IZ)-REFWD,RMMN(IZ)-REFWD,
     -      GRADL(IZ),CRHO,VFLOW(1,IZ),FIXEDL(IZ),EXCAP1(IZ),SPCAP1(IZ),
     -      ISL,IREP,LCG(IZ)+LSZSPC(0),LSZSPC,NA,AA,BB(IZ))
         END DO   ! End ゾーン loop

C        方程式を解く
         CALL DGESV(NZ,1,AA,NA,IP,BB,NA,INFO)
         IF(INFO.NE.0) CALL ERROR2(162,2)

         DO IZ = 1, NZ   ! ゾーン loop

C           熱平衡式の切片を求める
            WK = 0.0D0
            DO J = 1, NZ
               WK = WK + CRHO*VFLOW(J,IZ)*BB(J)
            END DO
            IF ( ISL.EQ.1 ) THEN
               L = LCG(IZ) + LSZSPC(0)
               DO IWL = 1, 9999
                  CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
                  IF ( ISTAT.NE.1 ) THEN
                     GO TO 200
                  ELSE
                     WK = WK + X(L+3+IREP)*BB(JZ)
                     L = L + LSZSPC(M(L))
                  END IF
               END DO
            END IF
  200       CONTINUE
            WK = WK + FIXEDL(IZ)

C           負荷状態モードが変化したかどうかをチェックする
            IF ( LMODE(IZ).NE.9 ) THEN  ! 空調稼動状態で、
               IF ( (LMODE(IZ).GE.-1).AND.
     -              (WK.LT.CNST(LMODE(IZ)+2,IZ)-EPS2) ) THEN
                  LMODE(IZ) = LMODE(IZ) - 1      ! 負荷状態モードが暖房側へシフトした
                  ICONV = 0
               ELSE IF ( (LMODE(IZ).LE.1).AND.
     -              (WK.GT.CNST(LMODE(IZ)+3,IZ)+EPS2) ) THEN
                  LMODE(IZ) = LMODE(IZ) + 1      ! 負荷状態モードが冷房側へシフトした
                  ICONV = 0
               END IF
            END IF

C           除去熱量を求める
            AN(IZ) = -GRADL(IZ)*BB(IZ) + WK
         END DO   ! End ゾーン loop

         IF ( ICONV.EQ.1 ) THEN   ! 全てのゾーンで負荷状態モードが変化しない
            DO IZ = 1, NZ
               RM(IZ) = BB(IZ)
            END DO
            RETURN
         END IF

      END DO   ! End 収束計算 loop

      CALL ERROR2(161,2)

      RETURN
      END
