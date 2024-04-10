def EXTRC1(JHR,NHR,LOPC,LC,NAZ,ISEAS,KSCH,IOPTWK,IOPTG,IOPVG,SMRT1,SMRT2,LCG,VOAG,CLDG,P0,RMMN,RMMX,SPCAP,EXCAP,VFLOW,X,M):
    """装置容量を季節別に設定する

    引数
    INTEGER     JHR                 ! I   時刻
    INTEGER     NHR                 ! I   1日のステップ数
    INTEGER     LOPC                ! I   OPCOデータへのポインタ(L)
    INTEGER     LC                  ! I   SPACデータへのポインタ(L)
    INTEGER     NAZ                 ! I   ゾーン数を表わす整合寸法
    INTEGER     ISEAS               ! I   (本日の)翌日の季節 値=1:夏期、2:冬期、3:中間期
    INTEGER     KSCH                ! I   (本日の)スケジュール値(1～3)
    INTEGER     IOPTWK              ! I   IOPTGと同じ（現在のスペースについての値）
    INTEGER     IOPTG(NAZ,NHR)      ! 0   空調運転状態フラグ、=0:停止中、
                                    !     =1:運転中、=2:起動、=3:停止
    INTEGER     IOPVG(NAZ,NHR)      ! O   外気導入状態フラグ、=0:カット中
                                    !     =1:導入中、=2:導入開始、=3:導入停止
    REAL        SMRT1(NAZ,NHR)      ! O   面積を持たない部位からの冷房負荷
    REAL        SMRT2(NAZ,NHR)      ! O   INFLの吸熱応答係数
    INTEGER     LCG(*)              ! O   XMQ配列のSPACデータへのポインタ（L）
                                    !     添字は現在のゾーンのグループ内における順番(=IZ)
    REAL        VOAG(*)             ! O   導入時の外気量(添字はグループ内の順番=IZ)
    REAL        CLDG(NAZ,NHR,NSL)   ! O   冷房負荷
    REAL        P0(NAZ,0:1,NHR,NSL) ! O   瞬時蓄熱応答係数（吸熱される側が正）
                                    !     第2添字=0:二等辺三角
                                    !     第2添字=1:右側直角二等辺三角
    REAL        RMMN(NAZ,NSL)       ! O   各スペースの設定温湿度下限
    REAL        RMMX(NAZ,NSL)       ! O   各スペースの設定温湿度上限
    REAL        SPCAP(NAZ,NSL)      ! O   各スペースの装置容量（加熱、0以上）
    REAL        EXCAP(NAZ,NSL)      ! O   各スペースの装置容量（冷却、0以上）
    REAL        VFLOW(NAZ,NAZ,NHR)  ! O   第1添字目のスペースから第2添字目のスペースへの流入
                                    !     風量（体積流量、0以上、対角項は0とする）

    X(JHR)                            I   冷房負荷（顕熱）
    X(24+JHR)                         !   瞬時蓄熱応答係数補正項（顕熱）
    X(48+JHR)                         I   冷房負荷（潜熱）
    X(72+JHR)                         !   瞬時蓄熱応答係数補正項（潜熱）
    M(LOPC+164)                       I   外気導入開始時刻 [時]
    X(LOPC+165)                       I   外気導入量 [m3/m2h]
    X(LOPC+4*ISEAS+i), i = -2, 1      I   現在の季節の設定温湿度の上下限
    X(LC+2)                           !   床面積 [m2]
    X(LC+i), i = 15, 16               !   瞬時蓄熱応答係数固定成分（顕熱、二等辺・直角三角）
    X(LC+i), i = 25, 26               !   瞬時蓄熱応答係数固定成分（潜熱、二等辺・直角三角）
    X(LC+i), i = 56, 59               I   装置容量（加熱・冷却、顕熱・潜熱）
    X(LC+74)                          I   面積を持たない部位の冷房負荷（=INFLと強制空冷のHEAT）
    X(LC+75)                          I   INFLの吸熱応答係数（瞬時） Σ0.288V （時変数）
    M(LC+61)                          I/O 前日からの外気導入継続状態(=1:継続, =0:途切れた)
    M(LC+101)                         I   現在のゾーンが同一グループの何ゾーン目か
    XMQ(LC+102～201)                  I   CFLW関連データ

    ローカル変数
        INTEGER     IZ                !     現在のゾーンが同一グループの何ゾーン目か
        REAL        RFLW              !     流入風量比率
        INTEGER     IOPVWK
        INTEGER     I
        INTEGER     L1
    """
    
    NSL = 2  # 顕熱と潜熱

    # OPCOのポインタ
    LOPC = int(LOPC)

    if (JHR < 1):
        raise Exception("例外が発生しました")
    if (JHR > NHR):
        raise Exception("例外が発生しました")

    # 外気導入モード ( IOPVG )
    # X[ int(LOPC+165) ] 外気導入量
    if ( X[ int(LOPC+165) ] < 0.01 ):
        IOPVWK = 0
    elif ( M[LC+61] == 1 ):   # 前日より外気導入継続中
        IOPVWK = IOPTWK
        if (IOPTWK == 0):
            raise Exception("例外が発生しました")
        if (IOPTWK == 2):
            raise Exception("例外が発生しました")
    elif ( int(JHR) < int(M[LOPC+164]) ):
        IOPVWK = 0
    elif ( int(JHR) > int(M[LOPC+164]) ):
        IOPVWK = IOPTWK
    elif (IOPTWK == 0) or (IOPTWK == 3):
        IOPVWK = 0
    else:
        IOPVWK = 2

    # print(f'-----------JHR: {JHR}')
    # print(f'-----------LOPC: {LOPC}')
    # print(f'-----------int(M[LOPC+164]): {int(M[LOPC+164])}')
    # print(f'-----------IOPVWK: {IOPVWK}')

    if (IOPVWK==0) or (IOPVWK == 3):
        M[LC+61] = 0   # たとえ前ステップまで外気導入が継続していたとしてもこのステップで途絶えた
    elif ( JHR == NHR ):
        M[LC+61] = 1

    # 1日分のデータを保存するための代入
    IZ = int(M[LC+101])

    if (IZ > NAZ):
        raise Exception("例外が発生しました")

    IOPTG[IZ,JHR] = IOPTWK
    IOPVG[IZ,JHR] = IOPVWK
    SMRT1[IZ,JHR] = X[LC+74]
    SMRT2[IZ,JHR] = X[LC+75]

    # X[LC+2] は 床面積
    if ( JHR == 1 ):   # 1日に1回代入すれば十分
        # XMQ配列のSPACデータへのポインタ
        LCG[IZ]  = LC
        # 導入時の外気量 VOAG
        VOAG[IZ] = X[LOPC+165]*X[LC+2]

    # 顕熱
    CLDG[IZ,JHR,1] = X[JHR]
    P0[IZ,0,JHR,1] = X[LC+15] + X[24+JHR]   # 顕熱、二等辺三角
    P0[IZ,1,JHR,1] = X[LC+16] + X[24+JHR]   # 顕熱、右側直角三角
    if JHR == 1:
        RMMX[IZ,1]  = X[ int(LOPC+4*ISEAS-2) ]
        RMMN[IZ,1]  = X[ int(LOPC+4*ISEAS-1) ]
        SPCAP[IZ,1] = X[ int(LC+205+4*(ISEAS-1)) ] * X[LC+2]
        EXCAP[IZ,1] = X[ int(LC+203+4*(ISEAS-1)) ] * X[LC+2]

    # 潜熱
    CLDG[IZ,JHR,2] = X[48+JHR]
    P0[IZ,0,JHR,2] = X[LC+25] + X[72+JHR]   # 潜熱、二等辺三角
    P0[IZ,1,JHR,2] = X[LC+26] + X[72+JHR]   # 潜熱、右側直角三角
    if JHR == 1:
        RMMX[IZ,2]  = X[ int(LOPC+4*ISEAS) ]
        RMMN[IZ,2]  = X[ int(LOPC+4*ISEAS+1) ]
        SPCAP[IZ,2] = X[ int(LC+206+4*(ISEAS-1)) ]*X[LC+2]
        EXCAP[IZ,2] = X[ int(LC+204+4*(ISEAS-1)) ]*X[LC+2]

    # スペース間移動風量
    for I in range(1, NAZ+1):   # 流入元スペースループ

        L1 = int( LC+101+(I-1)*5 )

        # CFLWスケジュールオプション（=2:CFLW入力どおりあるいは空調on･off指定）
        if M[L1+2] == 2:
            if (IOPTWK == 1) or (IOPTWK == 3):
                RFLW = X[L1+4]   # 空調on時の割合を使用
            else:
                RFLW = X[L1+5]   # 空調off時の割合を使用         
        elif ( M[L1+2] == 1 ):  # DSCH使用
            RFLW = X[ int( M[L1+3]+(KSCH-1)*24+JHR ) ]
        
        if (M[L1+2] >= 1) and (I != IZ):   # M(L1+2)=0は未定義の場合
            VFLOW[I,IZ,JHR] = RFLW * X[L1+1]
        else:
            VFLOW[I,IZ,JHR] = 0.0

    return IOPTG,IOPVG,SMRT1,SMRT2,LCG,VOAG,CLDG,P0,RMMN,RMMX,SPCAP,EXCAP,VFLOW,X,M