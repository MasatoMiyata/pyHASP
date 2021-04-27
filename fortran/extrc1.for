C-----------------------------------------------------------------------
C     除去熱量計算のための前処理（その2）
C     LATEST REVISION   2020.03.24 (2005.01.30より)                               rev 20200324(T.Nagai)
C                       装置容量を季節別に設定する                                rev 20200324(T.Nagai)
C-----------------------------------------------------------------------
      SUBROUTINE EXTRC1(JHR,NHR,LOPC,LC,NAZ,ISEAS,KSCH,IOPTWK,IOPTG,
     -         IOPVG,SMRT1,SMRT2,LCG,VOAG,CLDG,P0,RMMN,RMMX,SPCAP,EXCAP,
     -         VFLOW)

      PARAMETER (MX=30000)
      DIMENSION X(MX),M(MX)
      EQUIVALENCE (X,M)
      COMMON /XMQ/X

      PARAMETER ( NSL=2 )              !     顕熱と潜熱
C     引数
      INTEGER     JHR                  ! I   時刻
      INTEGER     NHR                  ! I   1日のステップ数
      INTEGER     LOPC                 ! I   OPCOデータへのポインタ(L)
      INTEGER     LC                   ! I   SPACデータへのポインタ(L)
      INTEGER     NAZ                  ! I   ゾーン数を表わす整合寸法
      INTEGER     ISEAS                ! I   (本日の)翌日の季節 値=1:夏期、2:冬期、3:中間期
      INTEGER     KSCH                 ! I   (本日の)スケジュール値(1～3)
      INTEGER     IOPTWK               ! I   IOPTGと同じ（現在のスペースについての値）
      INTEGER     IOPTG(NAZ,NHR)       ! 0   空調運転状態フラグ、=0:停止中、
                                       !     =1:運転中、=2:起動、=3:停止
      INTEGER     IOPVG(NAZ,NHR)       ! O   外気導入状態フラグ、=0:カット中
                                       !     =1:導入中、=2:導入開始、=3:導入停止
      REAL        SMRT1(NAZ,NHR)       ! O   面積を持たない部位からの冷房負荷
      REAL        SMRT2(NAZ,NHR)       ! O   INFLの吸熱応答係数
      INTEGER     LCG(*)               ! O   XMQ配列のSPACデータへのポインタ（L）
                                       !     添字は現在のゾーンのグループ内における順番(=IZ)
      REAL        VOAG(*)              ! O   導入時の外気量(添字はグループ内の順番=IZ)
      REAL        CLDG(NAZ,NHR,NSL)    ! O   冷房負荷
      REAL        P0(NAZ,0:1,NHR,NSL)  ! O   瞬時蓄熱応答係数（吸熱される側が正）
                                       !     第2添字=0:二等辺三角
                                       !     第2添字=1:右側直角二等辺三角
      REAL        RMMN(NAZ,NSL)        ! O   各スペースの設定温湿度下限
      REAL        RMMX(NAZ,NSL)        ! O   各スペースの設定温湿度上限
      REAL        SPCAP(NAZ,NSL)       ! O   各スペースの装置容量（加熱、0以上）
      REAL        EXCAP(NAZ,NSL)       ! O   各スペースの装置容量（冷却、0以上）
      REAL        VFLOW(NAZ,NAZ,NHR)   ! O   第1添字目のスペースから第2添字目のスペースへの流入
                                       !     風量（体積流量、0以上、対角項は0とする）
C     X(JHR)                             I   冷房負荷（顕熱）
C     X(24+JHR)                          !   瞬時蓄熱応答係数補正項（顕熱）
C     X(48+JHR)                          I   冷房負荷（潜熱）
C     X(72+JHR)                          !   瞬時蓄熱応答係数補正項（潜熱）
C     M(LOPC+164)                        I   外気導入開始時刻 [時]
C     X(LOPC+165)                        I   外気導入量 [m3/m2h]
C     X(LOPC+4*ISEAS+i), i = -2, 1       I   現在の季節の設定温湿度の上下限
C     X(LC+2)                            !   床面積 [m2]
C     X(LC+i), i = 15, 16                !   瞬時蓄熱応答係数固定成分（顕熱、二等辺・直角三角）
C     X(LC+i), i = 25, 26                !   瞬時蓄熱応答係数固定成分（潜熱、二等辺・直角三角）
C     X(LC+i), i = 56, 59                I   装置容量（加熱・冷却、顕熱・潜熱）
C     X(LC+74)                           I   面積を持たない部位の冷房負荷（=INFLと強制空冷のHEAT）
C     X(LC+75)                           I   INFLの吸熱応答係数（瞬時） Σ0.288V （時変数）
C     M(LC+61)                           I/O 前日からの外気導入継続状態(=1:継続, =0:途切れた)
C     M(LC+101)                          I   現在のゾーンが同一グループの何ゾーン目か
C     XMQ(LC+102～201)                   I   CFLW関連データ

C     ローカル変数
      INTEGER     IZ                   !     現在のゾーンが同一グループの何ゾーン目か
      REAL        RFLW                 !     流入風量比率
      INTEGER     IOPVWK
      INTEGER     I
      INTEGER     L1

      IF(JHR.LT.1) CALL ERROR2(119,2)
      IF(JHR.GT.NHR) CALL ERROR2(118,2)


C     外気導入モード ( IOPVG )
      IF ( X(LOPC+165).LT.0.01 ) THEN
         IOPVWK = 0
      ELSE IF ( M(LC+61).EQ.1 ) THEN   ! 前日より外気導入継続中
         IOPVWK = IOPTWK
         IF(IOPTWK.EQ.0) CALL ERROR2(117,2)
         IF(IOPTWK.EQ.2) CALL ERROR2(116,2)
      ELSE IF ( JHR.LT.M(LOPC+164) ) THEN
         IOPVWK = 0
      ELSE IF ( JHR.GT.M(LOPC+164) ) THEN
         IOPVWK = IOPTWK
      ELSE IF ( (IOPTWK.EQ.0).OR.(IOPTWK.EQ.3) ) THEN
         IOPVWK = 0
      ELSE
         IOPVWK = 2
      END IF

      IF ( (IOPVWK.EQ.0).OR.(IOPVWK.EQ.3) ) THEN
         M(LC+61) = 0   ! たとえ前ステップまで外気導入が継続していたと
                        ! してもこのステップで途絶えた
      ELSE IF ( JHR.EQ.NHR ) THEN
         M(LC+61) = 1
      END IF


C     1日分のデータを保存するための代入
      IZ = M(LC+101)
      IF(IZ.GT.NAZ) CALL ERROR2(115,2)
      IOPTG(IZ,JHR) = IOPTWK
      IOPVG(IZ,JHR) = IOPVWK
      SMRT1(IZ,JHR) = X(LC+74)
      SMRT2(IZ,JHR) = X(LC+75)
      IF ( JHR.EQ.1 ) THEN   ! 1日に1回代入すれば十分
         LCG (IZ) = LC
         VOAG(IZ) = X(LOPC+165)*X(LC+2)
      END IF

      ! 顕熱
      CLDG(IZ,JHR,1) = X(JHR)
      P0(IZ,0,JHR,1) = X(LC+15) + X(24+JHR)   ! 顕熱、二等辺三角
      P0(IZ,1,JHR,1) = X(LC+16) + X(24+JHR)   ! 顕熱、右側直角三角
      IF ( JHR.EQ.1 ) THEN
         RMMX (IZ,1) = X(LOPC+4*ISEAS-2)
         RMMN (IZ,1) = X(LOPC+4*ISEAS-1)
         SPCAP(IZ,1) = X(LC+205+4*(ISEAS-1))*X(LC+2)                            ! rev 20200324(T.Nagai)
         EXCAP(IZ,1) = X(LC+203+4*(ISEAS-1))*X(LC+2)                            ! rev 20200324(T.Nagai)
      END IF
*TNRS
C     XLC74(    KSPAC,JHR) = X(LC+74)
C     XLC75(    KSPAC,JHR) = X(LC+75)
*TNRE

      ! 潜熱
      CLDG(IZ,JHR,2) = X(48+JHR)
      P0(IZ,0,JHR,2) = X(LC+25) + X(72+JHR)   ! 潜熱、二等辺三角
      P0(IZ,1,JHR,2) = X(LC+26) + X(72+JHR)   ! 潜熱、右側直角三角
      IF ( JHR.EQ.1 ) THEN
         RMMX (IZ,2) = X(LOPC+4*ISEAS)
         RMMN (IZ,2) = X(LOPC+4*ISEAS+1)
         SPCAP(IZ,2) = X(LC+206+4*(ISEAS-1))*X(LC+2)                            ! rev 20200324(T.Nagai)
         EXCAP(IZ,2) = X(LC+204+4*(ISEAS-1))*X(LC+2)                            ! rev 20200324(T.Nagai)
      END IF

      ! スペース間移動風量
      DO I = 1, NAZ   ! 流入元スペースループ
         L1 = LC+101+(I-1)*5
         IF ( M(L1+2).EQ.2 ) THEN
            IF ( (IOPTWK.EQ.1).OR.(IOPTWK.EQ.3) ) THEN
               RFLW = X(L1+4)   ! 空調on時の割合を使用
            ELSE
               RFLW = X(L1+5)   ! 空調off時の割合を使用
            END IF
         ELSE IF ( M(L1+2).EQ.1 ) THEN  ! DSCH使用
            RFLW = X(M(L1+3)+(KSCH-1)*24+JHR)
         END IF
         IF ( (M(L1+2).GE.1).AND.(I.NE.IZ) ) THEN   ! M(L1+2)=0は未定義の場合
            VFLOW(I,IZ,JHR) = RFLW*X(L1+1)
         ELSE
            VFLOW(I,IZ,JHR) = 0.0
         END IF
      END DO

      RETURN
      END
