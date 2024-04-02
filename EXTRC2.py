import numpy as np
import pandas as pd

from RTVADJ import RTVADJ
from COEFFP import COEFFP
from DGESV import DGESV
from POSTP import POSTP
from SLVSM import SLVSM
from CLCMRT import CLCMRT

def EXTRC2(NHR,IPEAK,ISEAS,NAZ,IOPTG,NZ,IOPVG,SMRT1,SMRT2,
        VOAG,LCG,CLDG,NWD,WD,REFWD,P0,NSTP,VFLOW,EXCAP,SPCAP,RMMX,RMMN,
        NITER,NUOT1,ID,MDW,MODE,IOUT,LSZSPC,X,M,result):
    """1日分の除去熱量を計算する

    引数：
    INTEGER     NHR                     ! I   1日のステップ数（=時刻）
    INTEGER     IPEAK                   ! I   1:ピーク計算モード、0:シミュレーションモード
    INTEGER     ISEAS                   ! I   1:夏期、2:冬期、3:中間期
    INTEGER     NAZ                     ! I   ゾーン数を表わす整合寸法
    INTEGER     IOPTG(NAZ,NHR)          ! I   空調運転状態フラグ、=0:停止中、=1:運転中、=2:起動、=3:停止
    INTEGER     NZ                      ! I   現在のグループのゾーン数
    INTEGER     IOPVG(NAZ,NHR)          ! I   外気導入状態フラグ、=0:カット中、=1:導入中、=2:導入開始、=3:導入停止
    REAL        SMRT1(NAZ,NHR)          ! I   面積を持たない部位からの冷房負荷
    REAL        SMRT2(NAZ,NHR)          ! I   INFLの吸熱応答係数
    REAL        VOAG(NZ)                ! I   導入時の外気量
    INTEGER     LCG(NZ)                 ! I   XMQ配列のSPACデータへのポインタ（L）
    REAL        CLDG(NAZ,NHR,NSL)       ! I   冷房負荷
    INTEGER     NWD                     ! I   WDの整合寸法（=7）
    REAL        WD(NWD,NHR)             ! I   外界気象（基準温湿度からの偏差ではない）
    REAL        REFWD(NSL)              ! I   基準温湿度
    REAL        P0(NAZ,0:1,NHR,NSL)     ! I   瞬時蓄熱応答係数（吸熱される側が正）第2添字=0:二等辺三角、=1:右側直角二等辺三角
    INTEGER     NSTP                    ! I   予熱時間（ステップ）
    REAL        VFLOW(NAZ,NAZ,NHR)      ! I   第1添字目のゾーンから第2添字目のゾーンへの流入風量（体積流量、0以上、対角項は0とする）
    REAL        EXCAP(NAZ,NSL)          ! I   各スペースの装置容量（冷却、0以上）
    REAL        SPCAP(NAZ,NSL)          ! I   各スペースの装置容量（加熱、0以上）
    REAL        RMMX(NAZ,NSL)           ! I   各スペースの設定温湿度上限
    REAL        RMMN(NAZ,NSL)           ! I   各スペースの設定温湿度下限（RMMX,RMMNは基準温湿度からの偏差ではない）
    INTEGER     NITER                   ! I   収束計算における許容繰り返し計算数
    INTEGER     NUOT1                   ! I   テキスト出力ファイルの装置番号（最初の装置番号）
    INTEGER     ID(3)                   ! I   年・月・日（出力情報）
    INTEGER     MDW                     ! I   曜日（出力情報）
    INTEGER     MODE                    ! I   =1:助走、=2:本計算、=3:最終日
    INTEGER     IOUT                    ! I   =0:簡易出力(1h1行)、=1:詳細出力(1h2行)
    INTEGER     LSZSPC(0:4)             ! I   XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL, (3):WNDW, (4):INFL の変数の数
    INTEGER     IBECS                   ! I   BECSへの受け渡しデータを、=1:出力する, 0:出力しない
    INTEGER     NUOB                    ! I   BECSへの受け渡しファイル用装置番号

    ローカル変数：
    REAL        CRHO(NSL)               !     空気の容積比熱（潜熱の場合は密度に蒸発潜熱を掛けたもの）
    INTEGER     NTRM(NSL)               !     蓄熱応答係数の項数
    INTEGER     LSTP(MTRM,NSL)          !     蓄熱応答係数（瞬時分を除く）へのポインタ
    INTEGER     LSTQ(MTRM,NSL)          !     蓄熱負荷（瞬時分を除く）へのポインタ
    INTEGER     ISL                     !     =1:顕熱、=2:潜熱
    INTEGER     KSTP                    !     予熱開始後の経過ステップ
    INTEGER     JHR                     !     時刻
    INTEGER     IWARM                   !     そのステップが予熱中(=1)かそれ以外(=0)か
    INTEGER     JHR0                    !     予熱を開始した時刻
    INTEGER     ICHNG                   !     そのステップでいずれかのゾーンで階段状の変化（空調発停、外気量の変化）があったかどうか
    INTEGER     ISTOP                   !     そのステップで全てのゾーンで空調停止となるか
    INTEGER     IDLT(MSTP)              !     予熱開始後、各ステップにおいて直後室温湿度を =0：未知数としない、=1：未知数とする
    INTEGER     IZ                      !     現在のゾーンが何ゾーン目か（1<=IZ<=NZ）
    INTEGER     IREP                    !     現在の時刻ステップの直前か直後か =0：直前あるいは二等辺三角、=1：直後
    REAL        RMSET(MZ)               !     各ゾーンの一定除去熱量計算時設定温湿度
    REAL        VOAWK(MZ,0:1,MHR)       !     各ゾーンの外気量（体積流量、0以上）
    REAL        OATX(MZ,MHR)            !     導入外気温湿度（外調機考慮、基準温湿度からの偏差ではない）
    INTEGER     IOPTWK(MZ)              !     各ゾーン・各段の空調運転状態（=1:運転、=0:停止）
    INTEGER     IWARM1                  !     現在の段（直前･直後を区別する）が予熱時間帯か
    REAL        FIXEDL(MZ)              !     各ゾーンの、未知変数に依存しない固定流入熱量
    INTEGER     LSTPWK  
    INTEGER     LSTQWK  
    REAL        PV(0:MSTP-1,MZ)         !     予熱開始後各ステップにおける蓄熱応答係数（二等辺三角、吸熱側が正）PV(0),PR(0)は現在時刻のもの
    REAL        PR(0:MSTP,MZ)           !     予熱開始後各ステップにおける蓄熱応答係数（右側直角三角、吸熱側が正）PV(0),PR(0)は現在時刻のもの
    INTEGER     NSIZE                   !     方程式の数（未知数の数）
    INTEGER     IPS(0:MSTP)             !     予熱開始後、各時刻ステップの室温湿度が、未知ベクトルの何次元目から始まるか（=IPS+1次元目から）
    INTEGER     LMODE(MZ,0:1,MHR,NSL)   !     各ゾーンの負荷状態モード（-2：暖房過負荷,-1：暖房軽負荷,0：無負荷,1：冷房軽負荷,2：冷房過負荷,9：停止）
    REAL        AN(MZ,0:1,MHR,NSL)      !     各ゾーンの装置除去熱量
    REAL        RM(MZ,0:1,MHR,NSL)      !     各ゾーンの室温湿度（基準温湿度からの偏差）
    REAL        EOA                     !     各ゾーンの外気負荷（実室温湿度基準）
    REAL        RN(MZ,0:1,MHR,NSL)      !     各ゾーンの室除去熱量
    REAL        AMRT(MZ,0:1,MHR)        !     MRT
    REAL*8      AA(NA,NA)               !     Work Array （連立方程式左辺係数行列）
    REAL*8      BB(NA)                  !     Work Array （連立方程式右辺係数ベクトル）
    INTEGER     IP(NA)                  !     Work Array
    INTEGER     INFO    
    REAL*8      WK(MTRM)                !     Work Array
    REAL        FF
    REAL        EOUT(4,NSL)
    REAL        EMRT
    INTEGER     LMODEB(NSL)
    INTEGER     LC
    INTEGER     NSTP1
    INTEGER     L
    INTEGER     IWL
    INTEGER     JZ
    INTEGER     ISTAT
    """
    
    # 顕熱と潜熱
    NSL=2
    # 1グループあたりの最大ゾーン数
    MZ=20
    # 整合寸法（=NHR）
    MHR=24
    # 許容最大方程式数（未知数の数）
    NA=100
    # 許容最大予熱ステップ数
    MSTP=24
    # 蓄熱応答係数の項数
    MTRM=2

    CRHO   = np.array([0, 0.288, 0.720])    # 単位依存（ISLと対応。最初の0はダミー）
    NTRM   = np.array([0, 2, 1])            # ISLと対応。最初の0はダミー
    LSTP   = np.array([
                    [0,0,0],
                    [0,17,27],
                    [0,20,0]])  # 潜熱の2項目はダミー（XMQ配列変更時注意）
    LSTQ   = np.array([
                    [0,0,0],
                    [0,23,30],
                    [0,24,0]])  # 潜熱の2項目はダミー（XMQ配列変更時注意）

    IDLT   = np.zeros(MSTP+1)
    RMSET  = np.zeros(MZ+1)                     # 設定温湿度上限・下限
    VOAWK  = np.zeros([MZ+1,2+1,MHR+1])
    OATX   = np.zeros([MZ+1,MHR+1])             # 導入外気温湿度
    IOPTWK = np.zeros(MZ+1)
    FIXEDL = np.zeros(MZ+1)
    PV     = np.zeros([MSTP,MZ+1])
    PR     = np.zeros([MSTP,MZ+1])
    IPS    = np.zeros(MSTP)
    LMODE  = np.zeros([MZ+1,2,MHR+1,NSL+1])
    AN     = np.zeros([MZ+1,2,MHR+1,NSL+1])
    RM     = np.zeros([MZ+1,2,MHR+1,NSL+1])
    RN     = np.zeros([MZ+1,2,MHR+1,NSL+1])
    AMRT   = np.zeros([MZ+1,2,MHR+1])

    AA     = np.zeros([NA+1, NA+1])
    BB     = np.zeros(NA+1)
    IP     = np.zeros(NA+1)

    WK     = np.zeros(MTRM+1)
    EOUT   = np.zeros([4+1, NSL+1])
    LMODEB = np.zeros(NSL+1)
    

    if (NZ > NAZ):
        raise Exception("想定外エラー")
    if (NZ > MZ):
        raise Exception("想定外エラー")
    if (NSTP > MSTP):
        raise Exception("想定外エラー")
    if ((IPEAK == 1) and (NSTP < 0)):
        raise Exception("想定外エラー")        
    if (NHR != MHR):
        raise Exception("想定外エラー")
    

    for ISL in range(1, NSL+1):   # 顕熱・潜熱 loop

        # print(f"--- EXTRC2 顕熱・潜熱ループ: {ISL}")

        #---------------------------------------
        # 各ゾーンの設定温湿度上限・下限
        #---------------------------------------
        for IZ in range(1, NZ+1):   # ゾーン loop
            if ISEAS == 2:   # 1:夏期、2:冬期、3:中間期
                RMSET[IZ] = RMMN[IZ,ISL]  # 各ゾーンの設定温湿度下限
            else:
                RMSET[IZ] = RMMX[IZ,ISL]   # 各ゾーンの設定温湿度上限
            
        KSTP  = NSTP   # 予熱時間（ステップ）
        NSTP1 = NSTP   # 予熱時間（ステップ）

        for JHR in range(1, NHR+1):    # 時刻ループ

            # print(f"--- EXTRC2 時刻ループ: {JHR}")

            #---------------------------------------
            # 導入外気温湿度（外調機考慮） (OATX)
            #---------------------------------------
            for IZ in [1, NZ+1]:

                ## OAHUデータへのポインタ(L), LCGはXMQ配列のSPACデータへのポインタ
                L = M[ int(LCG[IZ]+202) ]

                if ( L == 0 ):   # OAHUデータが指定されていない場合
                    OATX[IZ,JHR] = WD[ISL,JHR]   # 外気温度・湿度
                else:
                    OATX[IZ,JHR] = X[ int( L+80+(ISL-1)*25+JHR ) ]   # 外調機の出口温度・湿度
            
            #---------------------------------------
            # 予熱運転モードか否かの判定（IWARM, KSTP）
            #---------------------------------------
                    
            # IOPTG(NAZ,NHR) 空調運転状態フラグ、=0:停止中、=1:運転中、=2:起動、=3:停止
            if (IPEAK == 1) and (ISEAS <= 2) and (IOPTG[1,JHR] != 0):   # ピーク計算モードで夏期・冬期で、かつ、空調稼動ステップである

                if (IOPTG[1,JHR] == 2) and (NSTP >= 1):  # 空調が「起動」状態であり、予熱時間が 1 以上

                    # 予熱開始時刻であり、一定除去熱量の計算ステップである。
                    # グループに属する全てのゾーンで予熱時間が同じでないとダメ

                    if ( JHR == NHR ):  # 24時(0時)の起動に対するピーク計算は未対応
                        raise Exception("想定外エラー")
                    else:

                        KSTP  = 0       # 予熱開始後の経過ステップを初期化
                        IWARM = 1       # そのステップが予熱中(=1)かそれ以外(=0)か
                        JHR0  = JHR     # 予熱開始時刻の記録
                        NSTP1 = NSTP    # 予熱終了時間
                        
                        # 予熱終了時間の更新
                        for J in [1, NSTP]:
                            if ( JHR+J == NHR ):   # 予熱終了前に24時に達する
                                NSTP1 = J
                                break
                            else:
                                for IZ in range(1, NZ+1):
                                    if ( IOPTG[IZ,JHR+J] == 3 ):  # 予熱終了前に空調停止する
                                        NSTP1 = J
                                        break
                                else:
                                    continue  # for J ループに戻る
                            break
                
                elif (KSTP >= 0) and (KSTP < NSTP1):

                    # 予熱開始時刻以外であり、一定除去熱量の計算ステップである
                    KSTP += 1   # 予熱開始後の経過ステップ
                    IWARM = 1   # そのステップが予熱中(=1)かそれ以外(=0)か
        
                else:
                    # シミュレーション計算を行うステップである
                    IWARM = 0   # そのステップが予熱中(=1)かそれ以外(=0)か

            else:
                # シミュレーション計算を行うステップである
                IWARM = 0   # そのステップが予熱中(=1)かそれ以外(=0)か
    

            #---------------------------------------
            # いずれかのゾーンで階段状変化が生じるか(ICHNG, ISTOP, IDLT)
            #---------------------------------------

            ICHNG = 0  # そのステップでいずれかのゾーンで階段状の変化（空調発停、外気量の変化）があったかどうか
            ISTOP = 1  # そのステップで全てのゾーンで空調停止となるか

            for IZ in range(1, NZ+1):

                # IOPTG 空調運転状態フラグ、=0:停止中、=1:運転中、=2:起動、=3:停止
                # IOPVG 外気導入状態フラグ、=0:カット中 =1:導入中、=2:導入開始、=3:導入停止
                if (IOPTG[IZ,JHR] >= 2) or (IOPVG[IZ,JHR] >= 2):   # 空調起動あるいは停止時刻、外気導入開始・終了時刻    
                    ICHNG = 1
                if (IOPTG[IZ,JHR] != 3 ):
                    ISTOP = 0   # 全てのゾーンで空調停止となるわけではない

                # IWARM そのステップが予熱中(=1)かそれ以外(=0)か
                # KSTP 予熱開始後の経過ステップ
                # IDLT 予熱開始後、各ステップにおいて直後室温湿度を =0：未知数としない、=1：未知数とする
                if (IWARM == 1) and (KSTP >= 1):
                    if (ICHNG == 1) and (ISTOP == 0):
                        IDLT[KSTP] = 1
                    else:
                        IDLT[KSTP] = 0


            for IREP in [0, 1]:   # 直前・直後 loop

                # print(f"--- EXTRC2 直前・直後ループ: {IREP}")

                #---------------------------------------
                # 運転状態の判定
                #---------------------------------------
                for IZ in range(1, NZ+1):

                    # 外気導入量（VOAWK(NZ,IREP,JHR)）
                    # IOPVG 外気導入状態フラグ、=0:カット中 =1:導入中、=2:導入開始、=3:導入停止
                    if (IOPVG[IZ,JHR] == 2) and (IREP == 0):    # 導入開始直前は風量 0 とする。
                        VOAWK[IZ,IREP,JHR] = 0.0
                    elif (IOPVG[IZ,JHR] == 3) and (IREP == 1):  # 導入停止直後は風量 0 とする。
                        VOAWK[IZ,IREP,JHR] = 0.0
                    elif (IOPVG[IZ,JHR] >= 1):
                        VOAWK[IZ,IREP,JHR] = VOAG[IZ]
                    else:
                        VOAWK[IZ,IREP,JHR] = 0.0
                    
                    # 空調運転状態（IOPTWK(NZ))
                    # IOPTG 空調運転状態フラグ、=0:停止中、=1:運転中、=2:起動、=3:停止
                    if (IOPTG[IZ,JHR] == 2) and (IREP == 0):    # 空調起動直前は 停止 扱いとする
                        IOPTWK[IZ] = 0
                    elif (IOPTG[IZ,JHR] == 3) and (IREP == 1):  # 空調停止直後は 停止 扱いとする
                        IOPTWK[IZ] = 0
                    elif (IOPTG[IZ,JHR] >= 1):   # 運転中
                        IOPTWK[IZ] = 1
                    else:
                        IOPTWK[IZ] = 0
                    
                # 一定除去熱量計算中か（IWARM1）
                # IWARM そのステップが予熱中(=1)かそれ以外(=0)か
                if ( IWARM == 1 ):
                    if (KSTP == 0) and (IREP == 0):   # 予熱開始直前は 一定除去熱量計算 としない
                        IWARM1 = 0
                    elif (KSTP == NSTP1) and (IDLT[KSTP]==0)  and (IREP == 1):  # 予熱終了直後は 一定除去熱量計算 としない
                        IWARM1 = 0
                    else:
                        IWARM1 = 1
                else:
                    IWARM1 = 0
                
                #---------------------------------------
                # 各ゾーンの、未知変数に依存しない固定流入熱量 FIXEDL(NZ)
                #---------------------------------------
                for IZ in range(1, NZ+1):
                    
                    # 各ゾーンの、未知変数に依存しない固定流入熱量
                    # 冷房負荷 CLDG + 空気の容積比熱 * 外気導入量 * (外気温度/湿度 - 基準温度/湿度)
                    FIXEDL[IZ] = CLDG[IZ,JHR,ISL] + CRHO[ISL] * VOAWK[IZ,IREP,JHR] * ( OATX[IZ,JHR] - REFWD[ISL] )

                    # print(f"--- EXTRC2 FIXEDL①: {FIXEDL[IZ]}")
                    # print(f"--- EXTRC2   CLDG[IZ,JHR,ISL]: {CLDG[IZ,JHR,ISL]}")
                    # print(f"--- EXTRC2   CRHO[ISL]: {CRHO[ISL]}")
                    # print(f"--- EXTRC2   VOAWK[IZ,IREP,JHR]: {VOAWK[IZ,IREP,JHR]}")
                    # print(f"--- EXTRC2   OATX[IZ,JHR]: {OATX[IZ,JHR]}")
                    # print(f"--- EXTRC2   REFWD[ISL]: {REFWD[ISL]}")
                    
                    # NTRM 蓄熱応答係数の項数
                    for I in range(1, NTRM[ISL]+1):
                        FIXEDL[IZ] += X[ int( LCG[IZ]+LSTQ[I,ISL] ) ]
                        # print(f"---  X[ int( LCG[IZ]+LSTQ[I,ISL] ) ]: { X[ int( LCG[IZ]+LSTQ[I,ISL] ) ]}")
                    
                    # print(f"--- EXTRC2 FIXEDL②: {FIXEDL[IZ]}")

                    if ( ISL == 1 ):  # 顕熱

                        # 各スペースの「内壁」のループ
                        L = int(LCG[IZ] + LSZSPC[0])   # 最初のポインタ

                        flag_RTVADJ = True
                        while flag_RTVADJ:
                            (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                            if ISTAT != 1:
                                flag_RTVADJ = False
                            else:
                                # 前ステップまでの励振による現ステップでの貫流負荷 [kcal/h]
                                FIXEDL[IZ] += X[L+11] + X[L+12]
                                # 次の要素のポインタ
                                L += int(LSZSPC[int(M[L])])

                    # print(f"--- EXTRC2 FIXEDL③: {FIXEDL[IZ]}")

                    # 直前の室温湿度変動による直後の流入熱を加算する
                    # 直後の計算で一定除去熱量計算を行わない場合 か 予熱運転ではない状態で一定除去熱量計算を行う場合
                    if ( (IWARM1 == 0) and (IREP == 1) ) or ( (IWARM1 == 1) and (KSTP == 0) ):

                        if (IREP == 0):
                            raise Exception("例外が発生しました")

                        # P0 瞬時蓄熱応答係数（吸熱される側が正） 第2添字=0:二等辺三角、第2添字=1:右側直角二等辺三角
                        # RM 各ゾーンの室温湿度（基準温湿度からの偏差）
                        FIXEDL[IZ] = FIXEDL[IZ] - ( P0[IZ,0,JHR,ISL] - P0[IZ,1,JHR,ISL] ) * RM[IZ,0,JHR,ISL]

                        if ( ISL == 1 ):  # 顕熱

                            # 各スペースの「内壁」のループ
                            L = int(LCG[IZ] + LSZSPC[0])   # 最初のポインタ

                            flag_RTVADJ = True
                            while flag_RTVADJ:
                                (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                                if ISTAT != 1:
                                    flag_RTVADJ = False
                                else:
                                    # X[L+3]  貫流応答係数（顕熱） 瞬時応答係数、二等辺三角波励振
                                    # X[L+4]  貫流応答係数（顕熱） 瞬時応答係数、右側直角三角波励振
                                    FIXEDL[IZ] += ( X[L+3] - X[L+4] ) * RM[JZ,0,JHR,ISL]
                                    # 次の要素のポインタ
                                    L += int(LSZSPC[int(M[L])])

                    # print(f"--- EXTRC2 FIXEDL④: {FIXEDL[IZ]}")


                if ( IWARM1 == 1 ):  # 一定除去熱量を計算する
                        
                    #---------------------------------------
                    # 前処理（OUT: PV(0:NSTP1-1,NZ), PR(0:NSTP1,NZ), 添字0を除く）
                    # PR(0:MSTP,MZ)   予熱開始後各ステップにおける蓄熱応答係数（右側直角三角、吸熱側が正）PV(0),PR(0)は現在時刻のもの
                    # PV(0:MSTP-1,MZ) 予熱開始後各ステップにおける蓄熱応答係数（二等辺三角、吸熱側が正）PV(0),PR(0)は現在時刻のもの
                    #---------------------------------------
                    if ( KSTP == 0 ):   # 予熱開始時刻
                        
                        for IZ in range(1, NZ+1):
                            for J in range(1, NSTP1+1):   # 予熱時間中

                                # 予熱開始後各ステップにおける蓄熱応答係数（右側直角三角、吸熱側が正）
                                PR[J,IZ] = 0.0
                                if ( J <= NSTP1-1 ):
                                    PV[J,IZ] = 0.0

                                # NTRM 蓄熱応答係数の項数（顕熱 2、潜熱 1）
                                for I in range(1, NTRM[ISL]):

                                    # 室蓄熱応答係数へのポインタ
                                    # LSTPWK   二等辺三角形波励振 
                                    # LSTPWK+1 右側直角三角形波励振 
                                    # LSTPWK+2 公比 
                                    LSTPWK = int(LCG[IZ] + LSTP[I,ISL])

                                    # 右側直角三角形波励振
                                    PR[J,IZ] += X[ int(LSTPWK+1) ] * X[ int(LSTPWK+2) ]**(J-1)

                                    if ( J <= NSTP1-1 ):
                                        # 二等辺三角形波励振 
                                        PV[J,IZ] += X[LSTPWK] * X[LSTPWK+2]**(J-1)

                        # 係数行列の0クリア
                        for I in range(1, NA+1):
                            for J in range(1, NA+1):
                                AA[I,J] = 0.0
                            BB[I] = 0.0
                        
                        # IPS 予熱開始後、各時刻ステップの室温湿度が、未知ベクトルの何次元目から始まるか（=IPS+1次元目から）
                        IPS[KSTP] = NZ
                        # 方程式の数（未知数の数）   
                        NSIZE     = 0    

                    #---------------------------------------
                    # 方程式に係数を加算する
                    #---------------------------------------

                    # IREP 直前 
                    # ICHNG: そのステップでいずれかのゾーンで階段状の変化（空調発停、外気量の変化）があったかどうか
                    if (IREP == 0) or (ICHNG == 1):    # 変化の直前
                        for IZ in range(1, NZ+1):

                            PR[0,IZ] = P0[IZ,1,JHR,ISL]   # 右側直角三角形波励振
                            PV[0,IZ] = P0[IZ,0,JHR,ISL]   # 二等辺三角形波励振

                            (NSIZE,AA,BB,IPS,X,M) = COEFFP(IZ,KSTP,NZ,IREP,NSTP1,IDLT,VFLOW[:,IZ,JHR],
                                    PV[:,IZ],PR[:,IZ],CRHO[ISL],VOAWK[IZ,IREP,JHR],FIXEDL[IZ],
                                    RMSET,REFWD[ISL],ISL,LCG[IZ]+LSZSPC[0],LSZSPC,NA,NSIZE,AA,BB,IPS,X,M)


                    # print(f"--- EXTRC2 AA: {AA}")
                    # print(f"--- EXTRC2 BB: {BB}")

                    # 予熱開始直前までの室温湿度変動による蓄熱負荷を更新する
                    if ( IREP == 1 ):
                        for IZ in range(1, NZ+1):
                            for J in range(1, NTRM[ISL]+1):
                                LSTPWK = LCG[IZ] + LSTP[J,ISL]
                                LSTQWK = LCG[IZ] + LSTQ[J,ISL]
                                if ( KSTP == 0 ):
                                    X[LSTQWK] = X[LSTPWK+2]*X[LSTQWK] - ( X[LSTPWK] - X[LSTPWK+1] )*RM[IZ,0,JHR,ISL]
                                else:
                                    X[LSTQWK] = X[LSTPWK+2]*X[LSTQWK]

                            if ( ISL == 1 ):
                                L = LCG[IZ] + LSZSPC[0]

                                flag_RTVADJ = True
                                while flag_RTVADJ:
                                    (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                                    if ISTAT != 1:
                                        flag_RTVADJ = False
                                    else:

                                        if ( KSTP == 0 ):
                                            X[L+11] = X[L+ 7]*X[L+11] + (X[L+5]-X[L+6])*RM[JZ,0,JHR,ISL]
                                            X[L+12] = X[L+10]*X[L+12] + (X[L+8]-X[L+9])*RM[JZ,0,JHR,ISL]
                                        else:
                                            X[L+11] = X[L+ 7]*X[L+11]
                                            X[L+12] = X[L+10]*X[L+12]

                                        L = L + int(LSZSPC[int(M[L])])


                    # 予熱終了段であり方程式を解く
                    if (KSTP == NSTP1) and ( (IDLT[KSTP] == 0) or (IREP == 1) ):

                        if (NSIZE > NA): 
                            raise Exception("例外が発生しました")

                        # 方程式を解く
                        (BB) = DGESV(AA, BB)

                        # 予熱終了時の後処理（負荷等の抽出と蓄熱負荷の更新）
                        (LMODE[:,:,:,ISL],AN[:,:,:,ISL],RM[:,:,:,ISL],RN[:,:,:,ISL],AMRT,WK,X,M) = \
                            POSTP(MZ,NZ,LCG,NSTP1,IDLT,JHR0,NHR,NSIZE,BB,IPS,CRHO[ISL],
                                    VOAWK,OATX,REFWD[ISL],ISL,RMSET,NTRM[ISL],LSTP[:,ISL],
                                    LSTQ[:,ISL],NAZ,VFLOW,SMRT1,SMRT2,LSZSPC,
                                    RM[:,:,:,ISL],WK)


                else:  # シミュレーションを行う段である

                    # ICHNG  そのステップでいずれかのゾーンで階段状の変化（空調発停、外気量の変化）があったかどうか
                    # IREP   直前・直後
                    if ( ICHNG == 0 ) and ( IREP == 1 ):    # 階段状変化がない直後
                    
                        # 直後の計算は不要とする（直前と同じとする）
                        for IZ in range(1, NZ+1):

                            LMODE[IZ,1,JHR,ISL] = LMODE[IZ,0,JHR,ISL]
                            AN[IZ,1,JHR,ISL]    = AN[IZ,0,JHR,ISL]
                            RM[IZ,1,JHR,ISL]    = RM[IZ,0,JHR,ISL]
                            RN[IZ,1,JHR,ISL]    = RN[IZ,0,JHR,ISL]

                    else:

                        # 熱平衡式を解く
                        (LMODE[:,IREP,JHR,ISL],AN[:,IREP,JHR,ISL],RM[:,IREP,JHR,ISL],AA,BB,IP,X,M) = \
                            SLVSM(NZ,IOPTWK,EXCAP[:,ISL],SPCAP[:,ISL],NAZ,
                                VFLOW[:,:,JHR],P0[:,IREP,JHR,ISL],CRHO[ISL],
                                VOAWK[:,IREP,JHR],RMMX[:,ISL],RMMN[:,ISL],
                                REFWD[ISL],IPEAK,ISEAS,NITER,FIXEDL,ISL,IREP,LCG,LSZSPC,NA,X,M)
            
                        # 外気負荷・室負荷を求める
                        for IZ in range(1, NZ+1):

                            # 各ゾーンの外気負荷（実室温湿度基準）
                            EOA = CRHO[ISL] * VOAWK[IZ,IREP,JHR] * ( OATX[IZ,JHR] - (RM[IZ,IREP,JHR,ISL] + REFWD[ISL]) )
                            # 各ゾーンの室除去熱量（室負荷） = 各ゾーンの装置除去熱量から外気負荷を差し引いたもの
                            RN[IZ,IREP,JHR,ISL] = AN[IZ,IREP,JHR,ISL] - EOA

                    # print(f"--- EXTRC2 JHR: {JHR}")
                    # print(f"--- EXTRC2 RM[1,0,JHR,1]: {RM[1,0,JHR,1]}")
                    # print(f"--- EXTRC2 RM[1,1,JHR,1]: {RM[1,1,JHR,1]}")
                    # print(f"--- EXTRC2 RM[1,0,JHR,2]: {RM[1,0,JHR,2]}")
                    # print(f"--- EXTRC2 RM[1,1,JHR,2]: {RM[1,1,JHR,2]}")

                    # MRTの計算
                    if ( ISL == 1 ):  # 顕熱計算の場合

                        for IZ in range(1, NZ+1):

                            (AMRT[IZ,IREP,JHR], X, M) = CLCMRT(
                                NZ,VFLOW[:,IZ,JHR],MZ,RM[:,:,JHR,ISL],
                                IZ,IREP,CRHO[ISL],SMRT1[IZ,JHR],SMRT2[IZ,JHR],
                                LCG[IZ],RN[IZ,IREP,JHR,ISL],X,M)
                
                    # 蓄熱負荷の更新
                    if ( IREP == 1 ):  # 直後であれば
                                
                        for IZ in range(1, NZ+1):

                            # NTRM 蓄熱応答係数の項数
                            for J in range(1, NTRM[ISL]+1):

                                LSTPWK = int( LCG[IZ] + LSTP[J,ISL] )
                                LSTQWK = int( LCG[IZ] + LSTQ[J,ISL] )

                                X[LSTQWK] = X[LSTPWK+2] * X[LSTQWK] \
                                    - ( X[LSTPWK] - X[LSTPWK+1] ) * RM[IZ,0,JHR,ISL] - X[LSTPWK+1] * RM[IZ,1,JHR,ISL]
                            
                                # print(f"--- EXTRC2 蓄熱応答係数の項数 J: {J}")
                                # print(f"--- EXTRC2 LSTP[J,ISL]: {LSTP[J,ISL]}")
                                # print(f"--- EXTRC2 X[LSTQWK]: {X[LSTQWK]}")
                                # print(f"--- EXTRC2 X[LSTPWK]: {X[LSTPWK]}")
                                # print(f"--- EXTRC2 X[LSTPWK+1]: {X[LSTPWK+1]}")
                                # print(f"--- EXTRC2 X[LSTPWK+2]: {X[LSTPWK+2]}")

                                # print(f"--- EXTRC2 RM[IZ,0,JHR,ISL]: {RM[IZ,0,JHR,ISL]}")
                                # print(f"--- EXTRC2 RM[IZ,1,JHR,ISL]: {RM[IZ,1,JHR,ISL]}")

                            if ( ISL == 1 ): # 顕熱計算の場合

                                L = int( LCG[IZ] + LSZSPC[0] )

                                flag_RTVADJ = True
                                while flag_RTVADJ:

                                    (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                                    if ISTAT != 1:
                                        flag_RTVADJ = False
                                    else:
                                        
                                        X[L+11] = X[L+ 7]*X[L+11] + (X[L+5]-X[L+6])*RM[JZ,0,JHR,ISL] + X[L+6]*RM[JZ,1,JHR,ISL]
                                        X[L+12] = X[L+10]*X[L+12] + (X[L+8]-X[L+9])*RM[JZ,0,JHR,ISL] + X[L+9]*RM[JZ,1,JHR,ISL]

                                        L += int(LSZSPC[int(M[L])])


    # 計算結果の出力
    for IZ in range(1, NZ+1):   # ゾーンループ

        # スペースのポインタ
        LC = int(LCG[IZ])

        # Kcal/h から W/m2 への換算係数
        # X[int(LC+2)]  床面積
        FF = 1.163 / X[int(LC+2)]   

        for JHR in range(1, NHR+1):   # 時刻ループ

            for IREP in [0, 1]:   # 直前・直後ループ

                if ( IOUT == 1 ):

                    if ( MODE >= 2 ):
                            
                        for J in range(1,3+1):
                            print(ID[J])
                        print(MDW), 
                        print(JHR), 
                        print(IREP), 
                        for ISL in range(1,NSL+1):
                            print(RM[IZ,IREP,JHR,ISL]+REFWD[ISL])
                            print(CLDG[IZ,JHR,ISL]*FF)
                            print(RN[IZ,IREP,JHR,ISL]*FF)
                            print(AN[IZ,IREP,JHR,ISL]*FF)
                            print(LMODE[IZ,IREP,JHR,ISL])
                        print(AMRT[IZ,IREP,JHR]+REFWD[1])
    
                else:   # 簡易出力（1時間分の平均の出力）

                    for ISL in range(1, NSL+1):

                        EOUT[1,ISL] = RM[IZ,IREP,JHR,ISL] + REFWD[ISL]
                        EOUT[2,ISL] = CLDG[IZ,JHR,ISL]*FF
                        EOUT[3,ISL] = RN[IZ,IREP,JHR,ISL]*FF
                        EOUT[4,ISL] = AN[IZ,IREP,JHR,ISL]*FF
                        LMODEB[ISL] = LMODE[IZ,IREP,JHR,ISL]

                    EMRT = AMRT[IZ,IREP,JHR] + REFWD[1]

                    if (IREP == 0) and (MODE >= 2):  # 平均を取って出力

                        # NSL: 顕熱 or 潜熱
                        for ISL in range(1, NSL+1):

                            for I in range(1, 4+1):
                                EOUT[I,ISL] = 0.5*( X[ int(LC+86+(ISL-1)*7+I) ] + EOUT[I,ISL] )

                            if (M[ int(LC+93+(ISL-1)*7) ] == 9) and (LMODEB[ISL] == 9):
                                LMODEB[ISL] = 9
                            elif (M[ int(LC+93+(ISL-1)*7) ] != 9) and (LMODEB[ISL] != 9):
                                LMODEB[ISL] = 10
                            else:
                                raise Exception("例外が発生しました")

                        # リストとして格納
                        result[str(LC)]["calc_data"].append([
                                int(ID[1]), int(ID[2]), int(ID[3]), int(MDW), int(JHR), int(IREP),
                                EOUT[1,1], EOUT[2,1], EOUT[3,1], EOUT[4,1], LMODEB[1],
                                EOUT[1,2], EOUT[2,2], EOUT[3,2], EOUT[4,2], LMODEB[2],
                                0.5*(X[int(LC+92)]+EMRT),
                            ])
    
                    elif ( IREP == 1 ):  # 次ステップのために記憶
                        for ISL in range(1, NSL+1):
                            for I in range(1, 4+1):
                                X[ int(LC+86+(ISL-1)*7+I) ] = EOUT[I,ISL]
                            M[ int(LC+93+(ISL-1)*7) ] = LMODEB[ISL]
                        X[int(LC+92)] = EMRT

    # 導入外気量の外調機別加算
    for IZ in range(1, NZ+1):    # ゾーンループ
        L = M[int(LCG[IZ]+202)]   # OAHUデータへのポインタ(L)
        if ( L != 0 ):  # OAHUデータが指定されている場合
            for JHR in range(1, NHR+1):   # 時刻ループ
                for IREP in [0, 1]:   #直前・直後ループ
                    X[int(L+131+(JHR-1)*2+IREP)] = X[int(L+131+(JHR-1)*2+IREP)] + VOAWK[IZ,IREP,JHR]

    return X,M,result


if __name__ == '__main__':

    LSTQ   = np.array([
                [0,0,0],
                [0,23,24],
                [0,30,0]])  # 潜熱の2項目はダミー（XMQ配列変更時注意）
    
    print(LSTQ[1,1])

    pass
