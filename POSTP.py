import numpy as np
from RTVADJ import RTVADJ
from CLCMRT import CLCMRT

def POSTP(MZ,NZ,LCG,NSTP,IDLT,JHR0,NHR,NSIZE,BB,IPS,CRHO,
            VOA,OATX,REFWD,ISL,RMSET,NTRM,LSTP,LSTQ,NAZ,VFLOW,
            SMRT1,SMRT2,LSZSPC,RM,X,M):
    """予熱終了時の後処理（負荷等の抽出と蓄熱負荷の更新）

    # 引数
    INTEGER     MZ                      ! I   整合寸法（1グループあたりの最大ゾーン数）
    INTEGER     NZ                      ! I   現在のグループのゾーン数
    INTEGER     LCG(NZ)                 ! I   XMQ配列のSPACデータへのポインタ（L）
    INTEGER     NSTP                    ! I   予熱時間（ステップ）
    INTEGER     IDLT(NSTP)              ! I   予熱開始後、各ステップにおいて直後室温湿度を
                                        !     =0：未知数としない、=1：未知数とする
    INTEGER     JHR0                    ! I   予熱を開始した時刻
    INTEGER     NHR                     ! I   1日のステップ数
    INTEGER     NSIZE                   ! I   方程式の数（未知数の数）
    REAL*8      BB(NSIZE)               ! I   連立方程式の解（右辺ベクトル）
    INTEGER     IPS(0:NSTP)             ! I   予熱開始後、各時刻ステップの室温湿度が、未知ベク
                                        !     トルの何次元目から始まるか（=IPS+1次元目から
    REAL        CRHO                    ! I   空気の容積比熱
                                        !     （潜熱の場合は密度に蒸発潜熱を掛けたもの）
    REAL        VOA(MZ,0:1,NHR)         ! I   各ゾーンの外気量（体積流量、0以上）
    REAL        OATX(MZ,NHR)            ! I   導入外界温湿度（外調機考慮、基準温湿度からの
                                        !     偏差ではない）
    REAL        REFWD                   ! I   基準温湿度
    INTEGER     ISL                     ! I   =1:顕熱、=2:潜熱
    REAL        RMSET(NZ)               ! I   設定温湿度（基準温湿度からの偏差ではない）
    INTEGER     NTRM                    ! I   蓄熱応答係数の項数
    INTEGER     LSTP(NTRM)              ! I   蓄熱応答係数（瞬時分を除く）へのポインタ
    INTEGER     LSTQ(NTRM)              ! I   蓄熱負荷（瞬時分を除く）へのポインタ
    INTEGER     NAZ                     ! I   ゾーン数を表わす整合寸法
    REAL        VFLOW(NAZ,NAZ,NHR)      ! I   第1添字目のゾーンから第2添字目のゾーンへの流入
                                        !     風量（体積流量、0以上、対角項は0とする）
    REAL        SMRT1(NAZ,NHR)          ! I   面積を持たない部位からの冷房負荷
    REAL        SMRT2(NAZ,NHR)          ! I   INFLの吸熱応答係数
    INTEGER     LSZSPC(0:4)             ! I   XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,
                                        !     (3):WNDW, (4):INFL の変数の数
    INTEGER     LMODE(MZ,0:1,NHR)       ! O   各ゾーンの負荷状態モード（-2：暖房過負過,
                                        !     -1：暖房軽負荷,0：無負荷,1：冷房軽負荷,
                                        !     2：冷房過負荷,9：停止）
    REAL        AN(MZ,0:1,NHR)          ! O   各ゾーンの装置除去熱量
    REAL        RM(MZ,0:1,NHR)          ! I/O 各ゾーンの室温湿度（基準温湿度からの偏差）
                                        !     (引用されるのは予熱開始直前のデータのみ)
    REAL        RN(MZ,0:1,NHR)          ! O   各ゾーンの室除去熱量
    REAL        AMRT(MZ,0:1,NHR)        ! O   MRT
    REAL*8      WK(NTRM)

    # ローカル変数
    INTEGER     IZ                      !     現在のゾーンが何ゾーン目か（1<=IZ<=NZ）
    INTEGER     LC  
    INTEGER     KSTP2                   !     予熱開始後の経過ステップ
    INTEGER     IREPS   
    INTEGER     IREPE   
    INTEGER     IREP2                   !     現在の時刻ステップの直前か直後か
                                        !     =0：直前あるいは二等辺三角、=1：直後
    INTEGER     JHR2                    !     時刻
    REAL        RMWK
    INTEGER     J
    INTEGER     LSTPWK
    INTEGER     L
    INTEGER     IWL
    INTEGER     JZ
    INTEGER     ISTAT
    REAL        EOA                     !     外気負荷（実室温湿度基準）
    """
    
    LMODE = np.zeros([MZ+1, 2, NHR+1])
    AN    = np.zeros([MZ+1, 2, NHR+1])
    RN    = np.zeros([MZ+1, 2, NHR+1])
    AMRT  = np.zeros([MZ+1, 2, NHR+1])
    WK    = np.zeros(NTRM)

    for IZ in range(1, NZ+1):    # ゾーン loop

        LC = LCG[IZ]

        for KSTP2 in range(0, NSTP):   # 予熱ステップ loop

            JHR2 = JHR0 + KSTP2

            if (JHR2 > NHR):
                raise Exception("想定外エラー")

            if ( KSTP2 == 0 ):
                IREPS = 1
            else:
                IREPS = 0

            if (KSTP2 == NSTP) and (IDLT[NSTP] == 0):
                IREPE = 0
            else:
                IREPE = 1

            for IREP2 in range(IREPS, IREPE+1):  # 直前・直後 loop

                # 予熱時間帯の室温湿度・除去熱量を抜き出す
                if BB(IZ) >= 0:
                    LMODE[IZ,IREP2,JHR2] = 1.0
                else:
                    LMODE[IZ,IREP2,JHR2] = -1.0

                AN[IZ,IREP2,JHR2] = BB[IZ]
                if ( KSTP2 == 0 ):
                    RMWK = BB[ int(IPS[KSTP2]+IZ) ]
                elif (KSTP2 == NSTP) and (IREP2 == IREPE):
                    RMWK = RMSET[IZ] - REFWD
                else:
                    RMWK = BB[ int( IPS[KSTP2] + IDLT[KSTP2]*IREP2*NZ + IZ) ]
                RM[IZ,IREP2,JHR2] = RMWK

                # 予熱時間帯の外気負荷・室負荷、MRTを求める
                EOA = CRHO*VOA[IZ,IREP2,JHR2] *( OATX[IZ,JHR2] - (RMWK+REFWD) )
                RN[IZ,IREP2,JHR2] = AN[IZ,IREP2,JHR2] - EOA

                if ( ISL == 1 ):
                    (AMRT[IZ,IREP2,JHR2], X, M) = CLCMRT(
                        NZ,VFLOW[1,IZ,JHR2],MZ,RM[1,0,JHR2],
                        IZ,IREP2,CRHO,SMRT1[IZ,JHR2],SMRT2[IZ,JHR2],
                        LC,RN[IZ,IREP2,JHR2],X,M)

                # 予熱時間帯の室温湿度変動による蓄熱負荷を更新する
                if (KSTP2 == NSTP) and (IDLT[NSTP] == 0):
                    # 予熱終了直後側（シミュレーションモード）で更新する
                    pass
                else:
                    for J in range(1, NTRM+1):
                        LSTPWK = LC + LSTP[J]
                        if ( KSTP2 == 0 ):
                            WK[J] = -X[LSTPWK+1]*RMWK
                        elif ( IREP2 == 0 ):
                            WK[J] = X[LSTPWK+2]*WK[J] - ( X[LSTPWK] - X[LSTPWK+1] )*RMWK
                        else:
                            WK[J] = WK[J] - X[LSTPWK+1]*RMWK

                    if ( ISL == 1 ):

                        L = int( LCG[IZ] + LSZSPC[0] )

                        flag_RTVADJ = True
                        while flag_RTVADJ:
                            (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                            if ISTAT != 1:
                                flag_RTVADJ = False
                            else:

                                if ( KSTP2 == 0 ):
                                    X[L+13] = X[L+6]*RMWK
                                    X[L+14] = X[L+9]*RMWK
                                elif ( IREP2 == 0 ):
                                    X[L+13] = X[L+ 7]*X[L+13] + ( X[L+5] - X[L+6] )*RMWK
                                    X[L+14] = X[L+10]*X[L+14] + ( X[L+8] - X[L+9] )*RMWK
                                else:   
                                    X[L+13] = X[L+13] + X[L+6]*RMWK
                                    X[L+14] = X[L+14] + X[L+9]*RMWK

                                L = L + int(LSZSPC[int(M[L])])

        for J in range(1, NTRM+1):
            X[int(LC+LSTQ[J])] = X[int(LC+LSTQ[J])] + WK[J]

        # 予熱終了時に直後の計算をしない場合はNSTP-1までの
        # 変動によるNSTPのための蓄熱負荷。計算する場合はNSTP
        # までの変動によるNSTP+1のための蓄熱負荷。

        if ISL == 1:
            L = int( LCG[IZ] + LSZSPC[0] )

            flag_RTVADJ = True
            while flag_RTVADJ:
                (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                if ISTAT != 1:
                    flag_RTVADJ = False
                else:
                    X[L+11] = X[L+11] + X[L+13]
                    X[L+12] = X[L+12] + X[L+14]
                    L = L + int(LSZSPC[int(M[L])])  

    return LMODE, AN, RM, RN, AMRT, WK, X, M