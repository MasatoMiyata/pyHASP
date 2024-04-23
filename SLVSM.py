import numpy as np
from RTVADJ import RTVADJ
from COEFFS import COEFFS
from DGESV import DGESV
from mprint import mprint

def SLVSM(NZ,IOPT,EXCAP,SPCAP,NAZ,VFLOW,P0,CRHO,VOA,
        RMMX,RMMN,REFWD,IPEAK,ISEAS,NITER,FIXEDL,ISL,IREP,LCG,LSZSPC,NA,X,M):
    """シミュレーション計算

    引数:
    INTEGER     NZ              現在のグループのゾーン数
    INTEGER     IOPT(NZ)        現段の空調運転状態（0:停止,その他:稼動）
    REAL        EXCAP(NZ)       各スペースの装置容量（冷却、0以上）
    REAL        SPCAP(NZ)       各スペースの装置容量（加熱、0以上）
    INTEGER     NAZ             配列VFLOWの整合寸法
    REAL        VFLOW(NAZ,NZ)   第1添字目のゾーンから第2添字目のゾーンへの流入風量（体積流量、0以上、対角項は0となっていなければならない）
    REAL        P0(NZ)          各スペースの瞬時蓄熱応答係数（吸熱される場合が正）
    REAL        CRHO            空気の容積比熱（潜熱の場合は密度に蒸発潜熱を掛けたもの）
    REAL        VOA(NZ)         各ゾーンの外気量（体積流量、0以上）
    REAL        RMMX(NZ)        各スペースの設定温湿度上限
    REAL        RMMN(NZ)        各スペースの設定温湿度下限（RMMX,RMMNは基準温湿度からの偏差ではない）
    REAL        REFWD           基準温湿度
    INTEGER     IPEAK           1:ピーク計算モード、0:シミュレーションモード
    INTEGER     ISEAS           1:夏期、2:冬期、3:中間期
    INTEGER     NITER           最大収束計算数
    REAL        FIXEDL(NZ)      各ゾーンの、温湿度や除去熱量に依存しない固定流入熱量
    INTEGER     ISL             =1:顕熱、=2:潜熱
    INTEGER     IREP            現在の時刻ステップの直前か直後か =0：直前あるいは二等辺三角、=1：直後
    INTEGER     LCG(NZ)         XMQ配列のSPACデータへのポインタ（L）
    INTEGER     LSZSPC(0:4)     XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL, (3):WNDW, (4):INFL の変数の数
    INTEGER     LMODE(NZ)       各ゾーンの負荷状態モード（-2：暖房過負荷,-1：暖房軽負荷,0：無負荷,1：冷房軽負荷,2：冷房過負荷,9：停止）
    REAL        AN(NZ)          各ゾーンの装置除去熱量
    REAL        RM(NZ)          各ゾーンの室温湿度（基準温湿度からの偏差）
    INTEGER     NA              AAの整合寸法
    REAL*8      AA(NA,NZ)       連立方程式左辺係数行列 
    REAL*8      BB(NZ)          連立方程式右辺係数ベクトル
    INTEGER     IP(NZ)          

    ローカル変数:
    INTEGER     IZ  
    REAL        EXCAP1(MZ)      各ゾーンの装置容量（冷却、0以上）
    REAL        SPCAP1(MZ)      各ゾーンの装置容量（加熱、0以上）
    REAL*8      WK  
    INTEGER     J   
    REAL        GRADL(MZ)       各ゾーンの単位温湿度変化に対する除去熱量の変化（符合を逆にしたもの。正値）
    REAL        CNST(4,MZ)      第2添字目のゾーンの負荷状態（過負荷等）の分岐点を与える負荷の成分
                                第1添字目の添字1:暖房過負荷と暖房軽負荷の境界
                                添字2:暖房軽負荷と無負荷の境界
                                添字3:無負荷と冷房軽負荷の境界
                                添字4:冷房軽負荷と冷房過負荷の境界
    INTEGER     ITER    
    INTEGER     ICONV           収束判定フラグ（1:収束、0:非収束）
    INTEGER     INFO      
    INTEGER     L
    INTEGER     IWL
    INTEGER     JZ
    INTEGER     ISTAT
    """
    
    # 1グループあたり最大ゾーン数
    MZ = 20
    # 収束判定のための許容範囲（kcal/h）
    EPS2   = 0.01

    LMODE  = np.zeros(MZ+1)
    AN     = np.zeros(MZ+1)
    RM     = np.zeros(MZ+1)
    AA     = np.zeros([NA+1,NZ+1])
    BB     = np.zeros(NZ+1)
    IP     = np.zeros(NZ+1)

    EXCAP1 = np.zeros(MZ+1)
    SPCAP1 = np.zeros(MZ+1)
    GRADL  = np.zeros(MZ+1)
    CNST   = np.zeros([4+1,MZ+1])

    #---------------------------------------
    # データの検証
    #---------------------------------------
    if (NZ > MZ): 
        raise Exception("想定外エラー")
    
    for IZ in range(1, NZ+1):
        if (VOA[IZ] < 0): 
            raise Exception("想定外エラー")
    
        for J in range(1, NZ+1):
            if ( J == IZ ):
                if abs(VFLOW[IZ,J] > 0.01): 
                    raise Exception("想定外エラー")
            else:
                if (VFLOW[IZ,J] < 0):
                    raise Exception("想定外エラー")

        if (RMMN[IZ] > RMMX[IZ]): 
            raise Exception("想定外エラー")
        if (EXCAP[IZ] < 0): 
            raise Exception("想定外エラー")
        if (SPCAP[IZ] < 0): 
            raise Exception("想定外エラー")


    #---------------------------------------
    # データの検証
    #---------------------------------------
    for IZ in range(1, NZ+1):   # ゾーン loop

        # 初期負荷状態モードを無負荷（非空調時は停止）とする
        # IOPT:  現段の空調運転状態（0:停止,その他:稼動）
        # LMODE: 各ゾーンの負荷状態モード（-2：暖房過負荷,-1：暖房軽負荷,0：無負荷,1：冷房軽負荷,2：冷房過負荷,9：停止）
        if ( IOPT[IZ] == 0 ):
            LMODE[IZ] = 9
        else:
            LMODE[IZ] = 0

        # 装置容量をセットする
        if (IPEAK == 1) and (ISEAS <= 2):  # 容量計算 夏期・冬期モードである 
            # 装置容量を無限大にセットする
            EXCAP1[IZ] = 9.9E+10
            SPCAP1[IZ] = 9.9E+10
        else:  # シミュレーションモードあるいは容量計算モード中間期
            # 装置容量は引数で渡されたものをそのまま用いる
            EXCAP1[IZ] = EXCAP[IZ]
            SPCAP1[IZ] = SPCAP[IZ]

        # 熱平衡式の傾きを求める
        WK = 0.0
        for J in range(1, NZ+1):
            WK += VFLOW[J,IZ]
        GRADL[IZ] = P0[IZ] + CRHO*( VOA[IZ] + WK )

        # 負荷状態モードの分岐点を求める
        CNST[1,IZ] = -SPCAP1[IZ] + GRADL[IZ]*(RMMN[IZ]-REFWD)
        CNST[2,IZ] = GRADL[IZ]*(RMMN[IZ]-REFWD)
        CNST[3,IZ] = GRADL[IZ]*(RMMX[IZ]-REFWD)
        CNST[4,IZ] = EXCAP1[IZ] + GRADL[IZ]*(RMMX[IZ]-REFWD)


    for ITER in range(1, NITER+1):  # 収束計算 loop

        ICONV = 1

        mprint("--- SLVSM NZ", NZ)
        mprint("--- SLVSM ITER", ITER)

        AA = np.zeros([NA+1, NZ+1])    
        for IZ in range(1, NZ+1):   # ゾーン loop

            # 係数行列の該当部分に値をセットする
            AA,BB[IZ],X,M = COEFFS(LMODE[IZ],NZ,IZ,RMMX[IZ]-REFWD,RMMN[IZ]-REFWD,
                GRADL[IZ],CRHO,VFLOW[:,IZ],FIXEDL[IZ],EXCAP1[IZ],SPCAP1[IZ],
                ISL,IREP,LCG[IZ]+LSZSPC[0],LSZSPC,NA,AA,X,M)

        mprint("--- SLVSM AA", AA[1,:])
        mprint("--- SLVSM AA", AA[2,:])
        mprint("--- SLVSM BB", BB)
        mprint("--- SLVSM FIXEDL[1]", FIXEDL[1])
        mprint("--- SLVSM FIXEDL[2]", FIXEDL[2])

        # 方程式を解く
        (BB) = DGESV(AA, BB, NZ)

        mprint("--- SLVSM XX",BB)

        for IZ in range(1, NZ+1):  # ゾーン loop

            # 熱平衡式の切片を求める
            WK = 0.0
            for J in range(1, NZ+1):
                WK += CRHO*VFLOW[J,IZ]*BB[J]
    
            if ( ISL == 1 ):
                L = int( LCG[IZ] + LSZSPC[0] )

                flag_RTVADJ = True
                while flag_RTVADJ:
                    (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                    if ISTAT != 1:
                        flag_RTVADJ = False
                    else:
                        WK = WK + X[ int(L+3+IREP) ]*BB[int(JZ)]
                        L += int(LSZSPC[int(M[L])])
    
            WK += FIXEDL[IZ]

            # 負荷状態モードが変化したかどうかをチェックする
            if ( LMODE[IZ] != 9 ):   # 空調稼動状態で、
                if (LMODE[IZ] >= -1) and (WK < CNST[int(LMODE[IZ]+2),IZ]-EPS2):
                    LMODE[IZ] = LMODE[IZ] - 1      # 負荷状態モードが暖房側へシフトした
                    ICONV = 0
                elif (LMODE[IZ] <= 1) and (WK > CNST[int(LMODE[IZ]+3),IZ]+EPS2):
                    LMODE[IZ] = LMODE[IZ] + 1      # 負荷状態モードが冷房側へシフトした
                    ICONV = 0

            # 除去熱量を求める
            AN[IZ] = -GRADL[IZ]*BB[IZ] + WK
        
        if ( ICONV == 1 ):  # 全てのゾーンで負荷状態モードが変化しない

            for IZ in range(1, NZ+1):
                RM[IZ] = BB[IZ]

            # print(f"--- SLVSM ITER: {ITER}")
            # print(f"--- SLVSM RM: {RM[1]}")

            return LMODE,AN,RM,AA,BB,IP,X,M

    raise Exception("収束していません")
