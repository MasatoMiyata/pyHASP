import math
import numpy as np
import pandas as pd
import xlrd



def read_textfile(filename:str, split_method=None):
    """
    テキストファイルを読み込む関数
    Args:
        filename (str): ファイル名称
    Returns:
        _type_: 行毎のデータ
    """
    with open(filename, 'r', encoding='shift_jis') as f:
        line_data = f.readlines()

    if split_method == None:
        
        data = line_data

    else:

        for line_num in range(0,len(line_data)):

            split_data = line_data[line_num].split(split_method)
            for split_num in range(0,len(split_data)):
                print(split_data[split_num])

    return data

def RHEAD(QA, IWFLG, RWFLG):
    """
    気象データのヘッダー部分の読み込み
    """

    # IWFLG[2]　日射・放射の単位 =0:10kJ/m2h, =1:kcal/m2h, =2:kJ/m2h、
    if (QA[15:19] == '10kJ'):
        IWFLG[2] = 0
    elif (QA[15:19] == 'kcal'):
        IWFLG[2] = 1
    elif (QA[15:19] == 'kJ  '):
        IWFLG[2] = 2
    else:
        raise Exception("ERROR: RHEAD")

    # IWFLG[3]　雲量モード =0:雲量, =1:夜間放射、
    if (QA[20:23] ==  'CA '):
        IWFLG[3] = 0
    elif (QA[20:23] == 'LNR'):
        IWFLG[3] = 1
    else:
        raise Exception("ERROR: RHEAD")

    # IWFLG[4]　気象データのカラム数(3以上9以下)
    IWFLG[4] = QA[24]
    if IWFLG[4] <= 3 or IWFLG[4] >= 9:
        raise Exception("ERROR: RHEAD")

    # RWFLG[1] 緯度[deg]（南緯の場合は負値）、
    IWK1 = float(QA[60:62])
    IWK2 = float(QA[62:65])

    if QA[65] == "N":
        RWFLG[1] = IWK1 + IWK2/600.0
    elif QA[65] == "S":
        RWFLG[1] = -IWK1 - IWK2/600.0
    else:
        raise Exception("ERROR: RHEAD")

    # RWFLG[2] 経度[deg]（西経の場合は負値）、
    IWK1 = float(QA[67:70])
    IWK2 = float(QA[70:73])

    if QA[73] == "E":
        RWFLG[2] = IWK1 + IWK2/600.0
    elif QA[65] == "W":
        RWFLG[2] = -IWK1 - IWK2/600.0
    else:
        raise Exception("ERROR: RHEAD")

    # RWFLG[3] 世界時と地方標準時の時差
    RWFLG[3] = float(QA[77:83])

    return IWFLG, RWFLG


def NDATE(MON,IDY):
    """
    **************** ANNUAL BASE DATING **********************************
    *     ARGUMENTS MON       - INPUT.  MONTH.
    *               IDY       - INPUT.  DAY OF THE MONTH.
    *     FUNCTION  NDATE               DAY OF THE YEAR.
    *     REQ. ROUTINES       - NONE.
    **********************************************************************
    """
    if MON-3 >= 0:
        y = int(30.57*MON - 31.06 - 1 + IDY)
    else:
        y = int(30.57*MON - 31.06 + 1 + IDY)

    return y

def NDATF(IYR,MON,IDY):
    """
    **************** 2004.02.07 BASE DATING ******************************
    *     1899年12月31日を1とした通算日数
    *     ARGUMENTS IYR       - INPUT.  YEAR.(1951-2050あるいは0)
    *               MON       - INPUT.  MONTH.
    *               IDY       - INPUT.  DAY OF THE MONTH.
    *     FUNCTION  NDATF               CUMULATIVE DAY.
    *     REQ. ROUTINES       - NONE.
    *     REMARKS   IYR=0のときは1999の月日に対する通算日数を返す
    **********************************************************************
    """

    NDS_x = range(1951,2051)
    NDS_y = [18628, 18993, 19359, 19724, 20089,   # 1951-55
            20454, 20820, 21185, 21550, 21915,    # 1956-60
            22281, 22646, 23011, 23376, 23742,    # 1961-65
            24107, 24472, 24837, 25203, 25568,    # 1966-70
            25933, 26298, 26664, 27029, 27394,    # 1971-75
            27759, 28125, 28490, 28855, 29220,    # 1976-80
            29586, 29951, 30316, 30681, 31047,    # 1981-85
            31412, 31777, 32142, 32508, 32873,    # 1986-90
            33238, 33603, 33969, 34334, 34699,    # 1991-95
            35064, 35430, 35795, 36160, 36525,    # 1996-2000
            36891, 37256, 37621, 37986, 38352,    # 2001-05
            38717, 39082, 39447, 39813, 40178,    # 2006-10
            40543, 40908, 41274, 41639, 42004,    # 2011-15
            42369, 42735, 43100, 43465, 43830,    # 2016-20
            44196, 44561, 44926, 45291, 45657,    # 2021-25
            46022, 46387, 46752, 47118, 47483,    # 2026-30
            47848, 48213, 48579, 48944, 49309,    # 2031-35
            49674, 50040, 50405, 50770, 51135,    # 2036-40
            51501, 51866, 52231, 52596, 52962,    # 2041-45
            53327, 53692, 54057, 54423, 54788]    # 2046-50
    
    NDS = dict(zip(NDS_x, NDS_y))
    
    if IYR == 0:
        y = NDS[1999] + NDATE(MON,IDY)
    else:
        y = NDS[IYR] + NDATE(MON,IDY)

        # 2000年以外に100で割り切れる年はない
        if IYR%4 == 0 and MON >= 3:
            y = y + 1

    return y


def MKMDW(ID, M):
    """
    **************** DATING (DAY OF THE WEEK) ****************************
    *     LATEST REVISION     - 1996.06.15
    *     ARGUMENTS ID(7,5)   - INPUT   DATE.
    *     FUNCTION  MKMDW               DAY OF THE WEEK.
    *     REQ. ROUTINES       - NDATE
    **********************************************************************
    """

    KDY = NDATE(ID[1,2],ID[1,3])

    mkmdw = ID[1,4]-1

    if (mkmdw == 0):
        mkmdw = 7
    if (mkmdw < 0):
        mkmdw = 8

    if (M[KDY+192] == 1):
        mkmdw = 9   
    
    return mkmdw

# ***          4.1. CODE NAME RETRIEVAL ********************************
def RETRIV(LO,QNAM,M):
    """
    CODE NAME RETRIEVAL
    文字列を数値化
    """
    QLIT = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789=+-*/(),.:"

    # 文字列の数値化 NNAM
    # 例えば、 N___ であれば 15010101、 E___  であれば 6010101
    NNAM = 0
    for i in range(0,4):
        N = QLIT.find(QNAM[i])+1
        if N==0:
            N=1
        NNAM = 100*NNAM + N

    # EXPSの場合、L0=100
    LC=LO
    LD=LO

    while True:

        if M[int(LC+1)] == NNAM:
            return NNAM,LC,LD

        LD = M[int(LC)]
        if LD == 0:       # M(LC)=0であれば空いている。
            return NNAM,LC,LD

        LC = LD


def NAME(NNAM):
    """
    RETRIVの逆処理
    数値を文字列化

    NAME("15010101")
    """
    QLIT = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789=+-*/(),.:"

    QNAM = ""
    NN = NNAM

    for I in [1,2,3,4]:

        # 冒頭の2桁を取り出す
        N=int(int(NN)/100**(4-I))

        # 文字列の連結演算子
        QNAM = QNAM + QLIT[N-1]

        # 処理済みの数値を削除
        NN = int(NN)-N*100**(4-I)

    return QNAM


# ***          4.2. DECODING ARITHMETIC EXPRESSION *********************
        
def ARITH(Q, NERR):
    # 床面積等を算術式で入力した場合に値を計算する関数
    # 実装不要とする。
    A = 0
    return A


# SATURATION HUMIDITY
def SATX(T):
    y = 1000.*math.exp(-5.58001+T*(0.0780136+T*(-2.87894E-04+T*(1.36152E-06+T*3.49024E-09)))-4.87306E-03*abs(T))
    return y

# SOLAR GAIN FACTOR
def GF(Z):
    y = Z*(2.392+Z*(-3.8636+Z*(3.7568-Z*1.3952)))
    return y

# WIND PRESSURE COEFFICIENT
def CF(Z):
    y=-0.01107+Z*(0.03675+Z*0.02332)
    return y


def display_XMQ_matrix(X,M,start, end):
    """XMQ変数を出力する関数

    Args:
        start (_type_): 出力を開始する行数
        end (_type_): 出力を終了する行数
    """

    set_range = range(start,end)
    print("***********************************************")
    for i in set_range:
        print( f"X {str(i)} :  {X[i]} " )
    print("***********************************************")
    for i in set_range:
        print( f"M {str(i)} :  {M[i]} " )
    print("***********************************************")


    # ファイルに書き込む
    with open('output_XMQ.txt', 'w') as file:
        for i, (item1, item2) in enumerate(zip(X, M), start=1):
            file.write(f"{i-1}: {item1}, {item2}\n")


def GVECTR(QWL,NL,MT,TH,HT,HO,TCM=np.zeros([2,100])):
    """_summary_

    Args:
        QWL (_type_): 識別子（文字列4）
        NL (_type_): 層の数
        MT (_type_): 建材番号のリスト
        TH (_type_): 長さのリスト
        HT (_type_): HC+HR
        HO (_type_): 建材データベースのファイル、H0=20　（QWLによって変わる）
        TCM (_type_, optional): 建材データベースを読みとった値. Defaults to np.zeros([2,100]).

    Returns:
        _type_: _description_
    """

    TRNS = np.zeros(10)
    ADMT = np.zeros(10)
    R = np.zeros(23)
    C = np.zeros(23)


    # 家具容量の応答に関する石野らの実験・調査データ（顕熱用）
    G0QC = [0.2893, 0.2518, 0.2311, 0.2119, 0.1790, 0.1794, 0.2187,
            0.2038, 0.1234, 0.0696, 0.0772, 0.0927, 0.0966, 0.0411,
            0.0465, 0.1398, 0.0741, 0.5120, 0.3653, 0.1982, 1.0000,
            0.2572]
    G1QC = [0.5579, 0.5260, 0.3129, 0.3720, 0.3038, 0.3176, 0.3820,
            0.5263, 0.4707, 0.3461, 0.2612, 0.3520, 0.2566, 0.2090,
            0.1535, 0.7415, 0.5722, 0.2513, 0.5668, 0.6222, 0.0000,
            1.6416]
    EPS =   [0.785 , 0.703 , 0.407 , 0.472 , 0.370 , 0.387 , 0.489 ,
            0.661 , 0.537 , 0.372 , 0.283 , 0.388 , 0.284 , 0.218 ,
            0.161 , 0.862 , 0.618 , 0.515 , 0.893 , 0.776 , 0.000 ,
            2.210]
    XN  =   [3.0  , 13.2  , 17.1  ,  7.7  ,  0.0  ,  9.8  ,  0.0  ,
            1.2  , 10.2  ,  5.3  ,  3.7  ,  1.4  , 11.4  ,  7.5  ,
            3.9  ,  0.0  ,  0.0  ,  3.6  ,  0.0  ,  0.5  ,  0.4  ,
            0.1]

    S =  [0., 0.00025, 0.001,  0.004,  0.016,  0.064,   0.256, 1.024, 4.096,  16.384]


    if QWL == "INIT":  # 特性値の読み込み

        for L in range(0,100):
        
            J  = int(HO.sheet_by_name("Sheet1").cell(L,0).value)
            IU = int(HO.sheet_by_name("Sheet1").cell(L,1).value)
            ALMD = float(HO.sheet_by_name("Sheet1").cell(L,2).value)
            CRHO = float(HO.sheet_by_name("Sheet1").cell(L,3).value)

            if J >= 1 and J <= 100:

                if (IU == 1):  # SI入力
                    if (J >= 90):  # 純抵抗
                        if (ALMD == 0):  # 未定義(非引用)の材料を想定
                            TCM[0,J-1] = 1.0*10**6
                        else:
                            TCM[0,J-1] = 1.0/(ALMD/0.86)   # [m2K/W]から[(m2hdeg/kcal)^(-1)]へ
                        TCM[1,J-1] = 0
                    else:
                        TCM[0,J-1]=ALMD*0.86   # [W/mK]から[kcal/mhdeg]へ
                        TCM[1,J-1]=CRHO/4.186  # [kJ/m3K]から[kcal/m3deg]へ

                else:

                    if (J >= 90):  # 純抵抗
                        if (ALMD == 0):  # 未定義(非引用)の材料を想定
                            TCM[0,J-1] = 1.0*10**6
                        else:
                            TCM[0,J-1] = 1.0/(ALMD)
                        TCM[1,J-1] = 0
                    else:
                        TCM[0,J-1]=ALMD
                        TCM[1,J-1]=CRHO

        return TCM
    
    elif QWL == "FURN":
        V = 2
        W = V*HO
        for J in range(0,10):
            TRNS[J]=0.
            ADMT[J]=W*S[J]/(S[J]+V)

        return TRNS,ADMT

    elif QWL == "FURS":
        
        for J in range(0,10):
                TRNS[J]=0.0
                SUM = 0.0
                for K in range(0,22):
                    if abs(EPS[K]) < 0.0001:
                        SUM=SUM+XN[K]*G0QC[K]
                    else:
                        SUM=SUM+XN[K]*(G0QC[K]+G1QC[K]/(S[J]+EPS[K]))
                ADMT[J]=SUM*0.01*HO*S[J]

        return TRNS,ADMT

    else:

        # 初項
        R[0] = 1/HT
        C[0] =0.0
        
        # 初項 0 があるため、NLと1個ずれることに注意。
        for L in range(0,int(NL)):
            if MT[L] > 0:

                # 建材番号
                I = int(MT[L])
                if I > 90:
                    TH[L] = 1.0
                R[L+1]=TH[L]/TCM[0,I-1]
                C[L+1]=TH[L]*TCM[1,I-1]
            else:
                R[L+1]=TH[L]   # 純熱抵抗
                C[L+1]=0.0

        # GWALでなければ一層追加
        if QWL != "GWAL":
            NL=NL+1
            R[int(NL)]=1/HO
            C[int(NL)]=0.

        # print("----NL----")
        # print(NL)
        # print("----R----")
        # print(R)
        # print("----C----")
        # print(C)


        for J in range(0,10):

            G0=1.0
            U1=1.0
            U2=0.0
            U3=0.0
            U4=1.0
        
            for L in range(0,int(NL+1)):
            
                W = math.sqrt(S[J]*R[L]*C[L])

                # print(f"L: {L}")
                # print(f"J: {J}")
                # print(f"W: {W}")

                if (W == 0):
                    V1=1.
                    V2=R[L]
                    V3=0.
                else:
                    V0=math.exp(-W)
                    G0=G0*V0
                    V1=0.5*(1.+V0**2)
                    V2=0.5*R[L]*(1.-V0**2)/W
                    V3=0.5*W*(1.-V0**2)/R[L]
                
                W1=U1
                W3=U3
                U1=W1*V1+U2*V3
                U2=W1*V2+U2*V1
                U3=W3*V1+U4*V3
                U4=W3*V2+U4*V1
            
            TRNS[J]=G0/U2
            ADMT[J]=U4/U2

    return TRNS,ADMT


# ***          4.4. CONVOLUTION PARAMETERS *****************************
# *
# *     数値Laplace逆変換し，convolution parametersを求める．
# *     HASP/ACLD/8501の収束計算でしばしばエラーになるのを改良
# *     2001-06-25, Akihiro Nagata (Revised by T.Nagai)

def CPARAM(nt, g):
    """数値Laplace逆変換し，convolution parametersを求める．

    Args:
        nt (_type_): 項数
        g (_type_): 応答
    """

    p = np.zeros(8+1)   # 1始まりとする

    h = np.zeros(9+1)   # 1始まりとする
    z = np.zeros([4+1,5+1])   # 1始まりとする
    zz = np.zeros(5+1)   # 1始まりとする
    e  = np.zeros(2+1)   # 1始まりとする
    ee  = np.zeros(2+1)   # 1始まりとする
    a   = np.zeros(2+1)   # 1始まりとする

    # sは0始まり
    s = [ 0.00000, 0.00025, 0.00100, 0.00400, 0.01600, 0.06400, 0.25600, 1.02400, 4.09600, 16.38400]
    # g = [1.004636764526E+00,1.002895593643E+00,9.977108836174E-01,9.773875474930E-01,9.024424552917E-01,6.781448125839E-01,2.917146384716E-01,4.119890555739E-02,7.415368454531E-04,2.908633121024E-07]


    for j in range(1, 9+1):
        h[j] = (g[j] - g[0])/s[j]
    for i in range(1, 8+1):
        p[i] = 0

    n = 1

    # print("----s-----")
    # print(s)
    # print("----g-----")
    # print(g)

#    12 continue
    goto12 = True

    while True:

        if goto12:
            rr0  = 1*10**20
            nn   = 2*n
            nn1  = nn + 1
            e[n] = s[9]
            nc   = 1

        #フラグリセット
        goto12 = True

        for i in range(1,nn+1):
            for k in range(1,nn1+1):
                z[i,k] = 0

        for j in range(1, 9+1):
            for i in range(1, n+1):
                zz[i]   = 1/(s[j] + e[i])
                zz[n+i] = zz[i]**2
            
            zz[nn1] = h[j]
            for i in range(1, nn+1):
                for k in range(1, nn1+1):
                    z[i,k] = z[i,k] + zz[i]*zz[k]

        # 判定（元のプログラムの go to 100）
        goto100 = False
        for i in range(1, nn+1):
            if (abs(z[i,i]) < 1*10**-20):
                w = 1
                nc = 100
                goto100 = True

        if goto100 == False:
            for i in range(1, nn+1):

                if (abs(z[i,i]) < 1*10**-20):
                    w = 1
                    nc = 100
                    print("ここに来たらおかしい")
                    break

                for j in range(i+1, nn1+1):
                    z[i,j] = z[i,j]/z[i,i]

                for k in range(1, nn+1):
                    if (i != k):
                        for j in range(i+1, nn1+1):
                            z[k,j] = z[k,j] - z[i,j]*z[k,i]
        
            for i in range(1, n+1):
                w = z[n+i,nn1]/(e[i]*z[i,nn1] + z[n+i,nn1])

                if (abs(w) < 10):
                    ee[i] = e[i]*math.exp(-w)
                else:
                    nc = 100

            # 残差の計算
                    
            rr = 0.0
            for j in range(1, 9+1):
                r = h[j]
                for i in range(1, n+1):
                    r = r - z[i,2*n+1]/(s[j] + ee[i])
                rr = rr + r*r
            rr = math.sqrt(rr/9.0)

            if (rr < rr0):
                rr0 = rr
                for i in range(1, n+1):
                    a[i] = z[i,2*n+1]
                    e[i] = ee[i]
                
                for i in range(n+1, 2+1):
                    a[i] = 0
                    e[i] = 0
            else:
                nc = 100

        if (abs(w) > 10**-4):
            if (nc >= 100): # not converged
                if (n == 1):  # 1項でも収束しない場合はエラー
                    NERR=0
                    raise Exception("収束しません")
                elif (rr0 >= rr1):  # 1項近似より残差が大きい
                    n = 1
                    a[1] = u
                    e[1] = v
                    print("ループを抜けます")
                    break
            else:
                nc = nc + 1
                goto12 = False  # go to 13

        elif ((nt == 2) and (n == 1) and (e[1] < 1)):
            u = z[1,3]
            v = e[1]
            n = 2
            rr1 = rr  # 1項で近似したときのRMSE
            goto12 = True
        
        else:
            break

    # Convolution parameters
    p[1] = g[0]
    p[2] = g[0]
    for i in range(1, n+1):
        u    = a[i]/e[i]
        v    = math.exp(-e[i])
        p[1] = p[1] + u*(1.-v)
        p[2] = p[2] + 2.*u*(1.-v)/(1.+v)
        p[3*i+0] = -u*(1.-v)**2
        p[3*i+1] = -u*(1.-v)**2/(1.+v)
        p[3*i+2] = v

    for i in range(n+1, 2+1):
        p[3*i+0] = 0
        p[3*i+1] = 0
        p[3*i+2] = 0

    # print('approximate step response:')
    # print(f'g[0] = {g[0]}')
    # print(f'a[1] = {a[1]}')
    # print(f'e[1] = {e[1]}')
    # print(f'a[2] = {a[2]}')
    # print(f'e[2] = {e[2]}')

    return p


#-----------------------------------------------------------------------
#     SPACデータのうち、次のIWALデータを探して先頭ポインタ等を返す
#-----------------------------------------------------------------------
def RTVADJ(LSZSPC, L, M):

    #       INTEGER     LSZSPC(0:4)          ! I   XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,
    #                                        !     (3):WNDW, (4):INFL の変数の数
    #       INTEGER     L                    ! I   SPACデータのうち、OWAL, IWAL等の先頭ポインタ
    #                                        !     （検索開始点）
    #                                        ! O   同 検索されたIWAL(adjacent)の先頭ポインタ
    #                                        !     (ISTAT=1,-2のときのみ有効)
    #       INTEGER     JZ                   ! O   検索されたIWAL(adjacent)の隣接スペースは、当該
    #                                        !     グループの何スペース目か(ISTAT=1のときのみ有効）
    #       INTEGER     ISTAT                ! O   =1 : IWAL(adjacent)が見つかった
    #                                        !     =0 : IWAL(adjacent)は見つからずに正常終了
    #                                        !     =-1: 異常終了
    #                                        !     =-2: adjacent wallにも関わらず隣接SPACが見つからない
    # C     M(L)                             I   =1:OWAL, =2:IWAL, =3:WNDW, 4:INFL, 5:SPAC終了
    # C     M(L+1) (IWALデータ)              I   隣室モード(=3のときadjacent wall)
    # C     Q(L+2) (IWALデータ)              I   隣室SPAC名(M(L+1)=3のとき有効)

    # ローカル変数
    JZ = 0
    ISTAT = 0

    # メインループ
    for II in range(1, 10000):

        # print(f"II: {II}")
        # print(f"L:{L}")
        # print(f"M[L]:{M[L]}")
        # print(f"M[L+1]:{M[L+1]}")
        # print(f"LSZSPC[int(M[L])]:{LSZSPC[int(M[L])]}")

        if M[L] < 1 or M[L] > 5:
            # print("--aルート---")
            raise Exception("RTVADJでエラーが発生しました")

        elif M[L] == 5:  # SPACデータ終了
            # print("--bルート---")
            ISTAT = 0
            return L, JZ, ISTAT
        
        elif M[L] != 2:  # IWAL以外
            # print("--cルート---")
            L += LSZSPC[int(M[L])]

        elif M[L+1] != 3:  # IWALだがadjacent wallではない
            # print("--dルート---")
            L += LSZSPC[int(M[L])]

        else:  # IWAL で adjacent wall
            # print("--eルート---")

            QSP = NAME(LSZSPC[L + 2])
            # print(f"QSP: {QSP}")

            (NNAM,LC,LD) = RETRIV(106,QSP,M)

            if LD != LC:
                ISTAT = -2  # adjacent wallだが隣接SPACは見つからない（ERROR5相当。ただしL だけは正しく返せる）
            else:
                JZ = M[LC+101]  # ! 隣接スペースは当該グループのうち何番目に登録されているか
                ISTAT = 1       # IWAL(adjacent)とその隣接スペースが見つかった
            
            return L, JZ, ISTAT

    # 異常終了
    ISTAT = -1
    
    return L, JZ, ISTAT


def INWD(NUW,IOPWE,NCLM,ICYCL,NUW_day):
    """
    **************** READ STANDARD WEATHER DATA **************************
    *     LATEST REVISION     - 2004.02.07
    *                         - 2020.04.03                                          ! add 20000403(T.Nagai)
    *     ARGUMENTS NUW       - I    気象データファイルの装置番号
    *               IOPWE     - I    =0:周期データ、=1:実在データ
    *               NCLM      - I    データカラム数（3以上9以下）                   ! add 20000403(T.Nagai)
    *               ICYCL     - I/O  現在のファイル読み込みサイクル数
    *               WD(7,24)  - O    気象データ（加工前）
    *               ID(7,5)   - O    日付データ（4桁で返す。ただしIOPWE=0のときの年は"0"）
    *               ISTAT     - O    =1:通常  0:ファイル終了(IOPWE=1のとき)
    *     REQ. ROUTINES       - NDATE
    *********************************************************************
    """

    WD = np.zeros([8,25])
    ID = np.zeros([8,6])

    line = 7 * NUW_day
    for I in [1,2,3,4,5,6,7]:

        for J in range(1,25):
            WD[I,J] = NUW[line+I][4*(J-1):4*J]

        ID[I,1] =  NUW[line+I][96:98]
        ID[I,2] =  NUW[line+I][98:100]
        ID[I,3] =  NUW[line+I][100:102]
        ID[I,4] =  NUW[line+I][102:103]
        ID[I,5] =  NUW[line+I][103:104]

    if IOPWE == 0:
        for I in [1,2,3,4,5,6,7]:
            ID[I,1]=0
    else:
        for I in [1,2,3,4,5,6,7]:

            # 年の4桁化
            if ID[I,1] >= 51:
                ID[I,1] = 1900+ID[I,1]
            else:
                ID[I,1] = 2000+ID[I,1]

    ISTAT = 1

    # 周期データには対応していない

    # 次の行数
    NUW_day += 1
    if len(NUW)-1 <= 7*(NUW_day):  # 最終行まで来たら繰り返す
        NUW_day = 0
        ICYCL += 1

    return ICYCL,WD,ID,ISTAT,NUW_day

# if __name__ == '__main__':

#     print(NAME("15010101"))
