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


def EXTRC0(JHR,LOPC,LC,ISEAS,KSCH,X,M):
    """除去熱量計算のための前処理（その1）

    引数
    INTEGER     JHR                 I   時刻
    INTEGER     LOPC                I   OPCOデータへのポインタ(L)
    INTEGER     LC                  I   SPACデータへのポインタ(L)
    INTEGER     ISEAS(2)            I   本日、翌日の季節 値=1:夏期、2:冬期、3:中間期
    INTEGER     KSCH(2)             I   本日、翌日のスケジュール値(1～3、3は空調は停止)
    INTEGER     IOPTWK              0   空調運転状態フラグ、=0:停止中、
                                        =1:運転中、=2:起動、=3:停止
    X(119,120,...,143)              I   空調用ダミースケジュール（=-1,0,0,..,0）
    X(LOPC+14,15,...,163)           I   空調発停操作（=1:起動、=-1:停止、=0:状態継続）
    M(LC+60)                        I   1時間前から現時刻までの運転状態（0 or 1)
    M(LC+60)                        O   現時刻から1時間後までの運転状態（0 or 1)

    ローカル変数
    INTEGER     LPNT(2)             !     今日、明日の空調スケジュールポインタ(0:00の操作)
    INTEGER     ISTATB              !     1時間前から現時刻までの運転状態（0 or 1)
    INTEGER     ISTATA              !     現時刻から1時間後までの運転状態（0 or 1)
    INTEGER     ISW                 !     =1:空調停止時はon, -1:空調時はoff, 0:状態継続
    INTEGER     I
    """

    LPNT = np.zeros(3)

    # 今日、明日の空調スケジュールポインタ(0:00の操作)
    for I  in [1,2]:
        if ( KSCH[I] == 3 ): # 1日中休止の場合
            LPNT[I] = 119   # ダミースケジュールを参照
        else:
            LPNT[I] = LOPC + 14 + (ISEAS[I]-1)*50 + (KSCH[I]-1)*25

    # 空調運転モード ( IOPTG )

    # 直前の空調運転状態
    ISTATB = M[int(LC+60)]

    # 現時刻のon-off操作
    if ( JHR == 24 ) and ( int(X[ int(LPNT[2]) ]) != 0 ) :
        ISW = int(X[ int(LPNT[2]) ])   # 翌日「0時」のon-off操作とする
    else:
        ISW = int(X[ int(LPNT[1]+JHR) ])

    if ( ISTATB == 0 ) and ( ISW == 1 ):
        IOPTWK = 2   # 起動
        ISTATA = 1
    elif ( ISTATB == 1 ) and ( ISW == -1 ):
        IOPTWK = 3   # 停止
        ISTATA = 0
    else:
        IOPTWK = ISTATB   # 停止中あるいは運転中
        ISTATA = ISTATB

    M[LC+60] = ISTATA   # 直後の空調運転状態

    return IOPTWK, M


def EXTRC1(JHR,NHR,LOPC,LC,NAZ,ISEAS,KSCH,IOPTWK,X,M):
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

    LOPC = int(LOPC)

    # 変数初期化
    LCG  = np.zeros([NAZ+1])
    VOAG = np.zeros([NAZ+1])

    IOPTG = np.zeros([NAZ+1,NHR+1])
    IOPVG = np.zeros([NAZ+1,NHR+1])
    SMRT1 = np.zeros([NAZ+1,NHR+1])
    SMRT2 = np.zeros([NAZ+1,NHR+1])
    CLDG  = np.zeros([NAZ+1,NHR+1,NSL+1])
    P0    = np.zeros([NAZ+1,2,NHR+1,NSL+1])

    RMMN = np.zeros([NAZ+1,NSL+1])
    RMMX = np.zeros([NAZ+1,NSL+1])
    SPCAP = np.zeros([NAZ+1,NSL+1])
    EXCAP = np.zeros([NAZ+1,NSL+1])

    VFLOW = np.zeros([NAZ+1,NAZ+1,NHR+1])


    if (JHR < 1):
        raise Exception("例外が発生しました")
    if (JHR > NHR):
        raise Exception("例外が発生しました")

    # 外気導入モード ( IOPVG )
    if ( X[ int(LOPC+165) ] < 0.01 ):
        IOPVWK = 0
    elif ( M[LC+61] == 1 ):   # 前日より外気導入継続中
        IOPVWK = IOPTWK
        if (IOPTWK == 0):
            raise Exception("例外が発生しました")
        if (IOPTWK == 2):
            raise Exception("例外が発生しました")
    elif ( JHR < M[LOPC+164] ):
        IOPVWK = 0
    elif ( JHR > M[LOPC+164] ):
        IOPVWK = IOPTWK
    elif (IOPTWK == 0) or (IOPTWK == 3):
        IOPVWK = 0
    else:
        IOPVWK = 2

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

    if ( JHR == 1 ):   # 1日に1回代入すれば十分
        LCG[IZ]  = LC
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

        if M[L1+2] == 2:
            if (IOPTWK == 1) or (IOPTWK == 3):
                RFLW = X[L1+4]   # 空調on時の割合を使用
            else:
                RFLW = X[L1+5]   # 空調off時の割合を使用            
        elif ( M[L1+2] == 1 ):  # DSCH使用
            RFLW = X[ int( M[L1+3]+(KSCH-1)*24+JHR ) ]
        
        if (M[L1+2] >= 1) and (I != Z):   # M(L1+2)=0は未定義の場合
            VFLOW[I,IZ,JHR] = RFLW*X[L1+1]
        else:
            VFLOW[I,IZ,JHR] = 0.0

    return IOPTG,IOPVG,SMRT1,SMRT2,LCG,VOAG,CLDG,P0,RMMN,RMMX,SPCAP,EXCAP,VFLOW


def COEFFP(IZ,KSTP,NZ,IREP,NSTP,IDLT,VFLOW,PV,PR,CRHO,VOA,FIXEDL,RMSET,REFWD,ISL,LSTRT,LSZSPC,NA,II,AA,BB,IPS,X,M):
    """
    C     一定除去熱量計算のための方程式を作成する
    C     LATEST REVISION   2012.03.05
    C     ・ このルーチンは、予熱時間が終了するまで、他のグループなど
    C        から同時に呼ばれてはならない
    C     ・ 引数のうち出力（II, AA, BB,IPS）については予熱時間終了時に
    C        正しい値となる。それまで、親ルーチンにおいてこれらの
    C        変数を引用・定義してはならない。
    C     ・ 時間ループの中の、直前・直後ループの中の、
    C        ゾーンループの中で呼ばれることを想定している。

    C     引数
    INTEGER     IZ              ! I   現在のゾーンが何ゾーン目か（1<=IZ<=NZ）
    INTEGER     KSTP            ! I   予熱開始後何ステップ目か(0<=KSTP<=NSTP)
    INTEGER     NZ              ! I   現在のグループのゾーン数
    INTEGER     IREP            ! I   現在の時刻ステップの直前か直後か
                                !     =0：直前あるいは二等辺三角、=1：直後
    INTEGER     NSTP            ! I   予熱時間（NSTP>=1）
    INTEGER     IDLT(NSTP)      ! I   予熱開始後、各ステップにおいて直後室温湿度を
                                !     =0：未知数としない、=1：未知数とする
                                !     （KSTPを含んでそれ以前の値のみ引用される）
    REAL        VFLOW(NZ)       ! I   各ゾーンからの流入風量（体積流量、正値）
    REAL        PV(0:NSTP-1)    ! I   予熱開始後各ステップにおける蓄熱応答係数
                                !     （二等辺三角、吸熱側が正）
    REAL        PR(0:NSTP)      ! I   予熱開始後各ステップにおける蓄熱応答係数
                                !     （右側直角三角、吸熱側が正）
                                !     PV(0), PR(0)はそれぞれ現在時刻のものを入力する
    REAL        CRHO            ! I   空気の容積比熱（潜熱の場合は密度に蒸発潜熱を掛けたもの）
    REAL        VOA             ! I   現在のゾーンに対する外気量（体積流量、0以上）
    REAL        FIXEDL          ! I   現在のゾーンの、未知変数に依存しない固定流入熱量
    REAL        RMSET(NZ)       ! I   各ゾーンの設定温湿度（予熱終了後に必ず達成される）
                                ! I   （基準温湿度からの偏差ではない）
    REAL        REFWD           ! I   基準温湿度
    INTEGER     ISL             ! I   =1:顕熱、=2:潜熱
    INTEGER     LSTRT           ! I   SPACデータのうち、OWAL, IWAL等の先頭ポインタ(検索開始点)
    INTEGER     LSZSPC(0:4)     ! I   XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,
                                !     (3):WNDW, (4):INFL の変数の数
    INTEGER     NA              ! I   行列AAの整合寸法
    INTEGER     II              ! I/O 未知数の数（行列のサイズ）
    REAL*8      AA(NA,NA)       ! I/O 連立方程式左辺係数行列
    REAL*8      BB(NA)          ! I/O 連立方程式右辺係数ベクトル
    INTEGER     IPS(0:NSTP)     ! I/O 予熱開始後、各時刻ステップの室温湿度が、未知ベクトルの
                                !     何次元目から始まるか（=IPS+1次元目から）

    C     ローカル変数
    INTEGER     I
    INTEGER     J
    REAL*8      VSUM            !     現在のゾーンに流入する風量の合計
    PARAMETER ( MZ=20 )         !     1グループあたり最大ゾーン数
    REAL        COEF(MZ)        !     現在のゾーンについての方程式のうち、添字目のゾーンの
                                !     現在の室温湿度に対する係数
    INTEGER     MSTP
    REAL*8      WK0, WK1
    INTEGER     L
    INTEGER     IWL
    INTEGER     JZ
    INTEGER     ISTAT
    """

    MZ = 20  # 1グループあたり最大ゾーン数
    COEF = np.zeros(MZ+1)

    if (NZ > MZ) or (IZ > NZ): 
        raise Exception("例外が発生しました")
    if (KSTP < 0) or (KSTP > NSTP):
        raise Exception("例外が発生しました")
    if (IREP != 0) and (IREP != 1):
        raise Exception("例外が発生しました")
    if (NSTP < 1):
        raise Exception("例外が発生しました")
    if (KSTP == 0) and (IREP == 0):
        raise Exception("例外が発生しました")

    # 行列要素のインデックスの更新
    if (IREP == 0) and (IZ == 1):
        if ( KSTP == 1 ):
            IPS[KSTP] = IPS[KSTP-1] + NZ   # 予熱開始直後のみ未知
        else:
            IPS[KSTP] = IPS[KSTP-1] + ( IDLT[KSTP-1] + 1 )*NZ
    II = II + 1
    if (II > NA): 
        raise Exception("例外が発生しました")

    # 現在のゾーンに流入する風量の合計
    VSUM = 0.0
    for J in range(1, NZ+1):
        VSUM = VSUM + VFLOW[J]
    
    # 現在の室温湿度（未知数）に対する係数（前処理）
    for J in range(1, NZ+1):
        if ( J == IZ ):
            if ( IREP == 1 ): # 現在が階段状変化直後である（予熱開始時刻を含む）
                COEF[J] = PR[0] + CRHO*( VOA + VSUM )
            else:
                COEF[J] = PV[0] + CRHO*( VOA + VSUM )
        else:
            COEF[J] = -CRHO*VFLOW[J]

    if ( ISL == 1 ):
        L = int(LSTRT)

        flag_RTVADJ = True
        while flag_RTVADJ:
            (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
            if ISTAT != 1:
                flag_RTVADJ = False
            else:
                if ( IREP == 1 ):
                    COEF[JZ] = COEF[JZ] - X[L+4]
                else:
                    COEF[JZ] = COEF[JZ] - X[L+3]
                L = L + int(LSZSPC[ int(M[L]) ])

    # 右辺ベクトルの初期セット
    BB[II] = FIXEDL

    # 行列の該当要素への代入

    # 一定除去熱量（未知数）の係数
    AA[II,IZ] = 1.0

    # 現在の時刻ステップの室温湿度に対する係数
    for MSTP in range(0, KSTP):
    
        if ( MSTP == KSTP ):
                
            if ( (KSTP >= 1) and (IREP == 1) ):

                # 階段状変化直後の場合（予熱開始直後を除く）
                AA[II,IPS[MSTP]+IZ] = PV[0] - PR[0]

                # 階段状変化直前の室温湿度に対する係数
                if ( ISL == 1 ):
                    L = LSTRT
                    flag_RTVADJ = True
                    while flag_RTVADJ:
                        (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                        if ISTAT != 1:
                            flag_RTVADJ = False
                        else:
                            AA[II,IPS[MSTP]+JZ] = AA[II,IPS[MSTP]+JZ] - ( X[L+3] - X[L+4] )
                            L = L + int( LSZSPC[ int(M[L]) ])

            if (MSTP == NSTP) and ((IDLT[NSTP] == 0) or (IREP == 1)):    # 予熱終了時刻で かつ 設定温湿度に達する側のステップ

                # 右辺ベクトルの修正
                WK1 = 0.0
                for J in range(1, NZ+1):
                    WK1 = WK1 + COEF[J] * ( RMSET[J] - REFWD )
                BB[II] = BB[II] - WK1
            else:
                for J in range(1, NZ+1):
                    if ( KSTP == 0 ):
                        AA[II, int(IPS(MSTP)+J) ] = COEF[J]
                    else:
                        AA[II, int(IPS(MSTP)+IREP*NZ+J) ] = COEF[J]
 
        # 予熱開始1時刻後から、1ステップ前までの室温湿度に対する係数（KSTP>=2の場合のみ）
        elif ( MSTP >= 1 ):

            AA[II,IPS[MSTP]+IZ] = PV[KSTP-MSTP] - PR[KSTP-MSTP]*IDLT[MSTP]

            if ( IDLT[MSTP] == 1 ):    # 予熱開始MSTP後に階段状変化が生じた
                AA[II, int(IPS[MSTP]+NZ+IZ) ] = PR[KSTP-MSTP]

            if ( ISL == 1 ):
                L = LSTRT

                flag_RTVADJ = True
                while flag_RTVADJ:
                    (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                    if ISTAT != 1:
                        flag_RTVADJ = False
                    else:

                        WK0 = X[L+5] * X[L+ 7]**(KSTP-MSTP-1) + X[L+8] * X[L+10]**(KSTP-MSTP-1)
                        WK1 = X[L+6] * X[L+ 7]**(KSTP-MSTP-1) + X[L+9] * X[L+10]**(KSTP-MSTP-1)
                        AA[II,IPS[MSTP]+JZ] = AA[II,IPS[MSTP]+JZ] - ( WK0 - WK1*IDLT[MSTP] )
                        if ( IDLT[MSTP] == 1 ):
                            AA[II, int(IPS[MSTP]+NZ+JZ)] = AA[II,int(IPS[MSTP]+NZ+JZ)] - WK1

                        L = L + int(LSZSPC[int(M[L])])

        else:   # 予熱開始直後の室温湿度に対する係数（KSTP>=1の場合のみ）

            AA[II,int(IPS[MSTP]+IZ)] = PR[KSTP]

            if ( ISL == 1 ):
                L = LSTRT

                flag_RTVADJ = True
                while flag_RTVADJ:
                    (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                    if ISTAT != 1:
                        flag_RTVADJ = False
                    else:
                        WK1 = X[L+6] * X[L+7]**(KSTP-1) + X[L+9]*X[L+10]**(KSTP-1)
                        AA[II, int(IPS(MSTP)+JZ)] = AA[II,int(IPS(MSTP)+JZ)] - WK1
                        L = L + int(LSZSPC[int(M[L])])


    if (KSTP == NSTP) and (IZ == NZ) and ((IDLT[NSTP] == 0) or (IREP == 1)):
        if (II != IPS[KSTP] + IDLT[KSTP]*NZ): 
            raise Exception("例外が発生しました")

    return II,AA,BB,IPS,X,M


def DGESV(A,B):
    # A * X = B を解く関数
    # ただし、AとBは 1始まりの行列とする
    # Bは一次元とする。
    # a0_lis = [0, 0, 0, 0]
    # a1_lis = [0, 1, 0, 0]
    # a2_lis = [0, 0, 2, 0]
    # a3_lis = [0, 0, 0, 4] 
    # A_matrix = np.array([a0_lis, a1_lis, a2_lis, a3_lis])
    # b = np.array([0,4,5,6])
    # X = DGESV(A_matrix, b)

    # 行列をずらす
    AA = np.zeros([A.shape[0]-1, A.shape[1]-1])
    for i in range(1, A.shape[0]):
        for j in range(1, A.shape[1]):
            AA[i-1][j-1] = A[i][j]

    BB = np.zeros(B.shape[0]-1)
    for i in range(1, B.shape[0]):
        BB[i-1] = B[i]

    XX = np.linalg.solve(AA, BB)

    X = np.zeros(XX.shape[0]+1)
    for i in range(0, XX.shape[0]):
        X[i+1] = XX[i]

    return X

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
                    pass
        #             CALL CLCMRT(NZ,VFLOW(1,IZ,JHR2),MZ,RM(1,0,JHR2),IZ,
        # -                        IREP2,CRHO,SMRT1(IZ,JHR2),SMRT2(IZ,JHR2),
        # -                        LC,RN(IZ,IREP2,JHR2),AMRT(IZ,IREP2,JHR2))


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


def EXTRC2(NHR,IPEAK,ISEAS,NAZ,IOPTG,NZ,IOPVG,SMRT1,SMRT2,
        VOAG,LCG,CLDG,NWD,WD,REFWD,P0,NSTP,VFLOW,EXCAP,SPCAP,RMMX,RMMN,
        NITER,NUOT1,ID,MDW,MODE,IOUT,LSZSPC,X,M):
    """1日分の除去熱量を計算する

    引数：
    INTEGER     NHR                     ! I   1日のステップ数
    INTEGER     IPEAK                   ! I   1:ピーク計算モード、0:シミュレーションモード
    INTEGER     ISEAS                   ! I   1:夏期、2:冬期、3:中間期
    INTEGER     NAZ                     ! I   ゾーン数を表わす整合寸法
    INTEGER     IOPTG(NAZ,NHR)          ! I   空調運転状態フラグ、=0:停止中、
                                        !     =1:運転中、=2:起動、=3:停止
    INTEGER     NZ                      ! I   現在のグループのゾーン数
    INTEGER     IOPVG(NAZ,NHR)          ! I   外気導入状態フラグ、=0:カット中
                                        !     =1:導入中、=2:導入開始、=3:導入停止
    REAL        SMRT1(NAZ,NHR)          ! I   面積を持たない部位からの冷房負荷
    REAL        SMRT2(NAZ,NHR)          ! I   INFLの吸熱応答係数
    REAL        VOAG(NZ)                ! I   導入時の外気量
    INTEGER     LCG(NZ)                 ! I   XMQ配列のSPACデータへのポインタ（L）
    REAL        CLDG(NAZ,NHR,NSL)       ! I   冷房負荷
    INTEGER     NWD                     ! I   WDの整合寸法（=7）
    REAL        WD(NWD,NHR)             ! I   外界気象（基準温湿度からの偏差ではない）
    REAL        REFWD(NSL)              ! I   基準温湿度
    REAL        P0(NAZ,0:1,NHR,NSL)     ! I   瞬時蓄熱応答係数（吸熱される側が正）
                                        !     第2添字=0:二等辺三角
                                        !     第2添字=1:右側直角二等辺三角
    INTEGER     NSTP                    ! I   予熱時間（ステップ）
    REAL        VFLOW(NAZ,NAZ,NHR)      ! I   第1添字目のゾーンから第2添字目のゾーンへの流入
                                        !     風量（体積流量、0以上、対角項は0とする）
    REAL        EXCAP(NAZ,NSL)          ! I   各スペースの装置容量（冷却、0以上）
    REAL        SPCAP(NAZ,NSL)          ! I   各スペースの装置容量（加熱、0以上）
    REAL        RMMX(NAZ,NSL)           ! I   各スペースの設定温湿度上限
    REAL        RMMN(NAZ,NSL)           ! I   各スペースの設定温湿度下限
                                        !     （RMMX,RMMNは基準温湿度からの偏差ではない）
    INTEGER     NITER                   ! I   収束計算における許容繰り返し計算数
    INTEGER     NUOT1                   ! I   テキスト出力ファイルの装置番号（最初の装置番号）
    INTEGER     ID(3)                   ! I   年・月・日（出力情報）
    INTEGER     MDW                     ! I   曜日（出力情報）
    INTEGER     MODE                    ! I   =1:助走、=2:本計算、=3:最終日
    INTEGER     IOUT                    ! I   =0:簡易出力(1h1行)、=1:詳細出力(1h2行)
    INTEGER     LSZSPC(0:4)             ! I   XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,
                                        !     (3):WNDW, (4):INFL の変数の数
    INTEGER     IBECS                   ! I   BECSへの受け渡しデータを、=1:出力する, 0:出力しない
    INTEGER     NUOB                    ! I   BECSへの受け渡しファイル用装置番号

    ローカル変数：
    REAL        CRHO(NSL)               !     空気の容積比熱
                                        !     （潜熱の場合は密度に蒸発潜熱を掛けたもの）
    INTEGER     NTRM(NSL)               !     蓄熱応答係数の項数
    INTEGER     LSTP(MTRM,NSL)          !     蓄熱応答係数（瞬時分を除く）へのポインタ
    INTEGER     LSTQ(MTRM,NSL)          !     蓄熱負荷（瞬時分を除く）へのポインタ
    INTEGER     ISL                     !     =1:顕熱、=2:潜熱
    INTEGER     KSTP                    !     予熱開始後の経過ステップ
    INTEGER     JHR                     !     時刻
    INTEGER     IWARM                   !     そのステップが予熱中(=1)かそれ以外(=0)か
    INTEGER     JHR0                    !     予熱を開始した時刻
    INTEGER     ICHNG                   !     そのステップでいずれかのゾーンで階段状の変化
                                        !     （空調発停、外気量の変化）があったかどうか
    INTEGER     ISTOP                   !     そのステップで全てのゾーンで空調停止となるか

    INTEGER     IDLT(MSTP)              !     予熱開始後、各ステップにおいて直後室温湿度を
                                        !     =0：未知数としない、=1：未知数とする
    INTEGER     IZ                      !     現在のゾーンが何ゾーン目か（1<=IZ<=NZ）
    INTEGER     IREP                    !     現在の時刻ステップの直前か直後か
                                        !     =0：直前あるいは二等辺三角、=1：直後

    REAL        RMSET(MZ)               !     各ゾーンの一定除去熱量計算時設定温湿度
    REAL        VOAWK(MZ,0:1,MHR)       !     各ゾーンの外気量（体積流量、0以上）
    REAL        OATX(MZ,MHR)            !     導入外気温湿度（外調機考慮、基準温湿度からの
                                        !     偏差ではない）
    INTEGER     IOPTWK(MZ)              !     各ゾーン・各段の空調運転状態（=1:運転、=0:停止）
    INTEGER     IWARM1                  !     現在の段（直前･直後を区別する）が予熱時間帯か
    REAL        FIXEDL(MZ)              !     各ゾーンの、未知変数に依存しない固定流入熱量
    INTEGER     LSTPWK  
    INTEGER     LSTQWK  
    REAL        PV(0:MSTP-1,MZ)         !     予熱開始後各ステップにおける蓄熱応答係数
                                        !     （二等辺三角、吸熱側が正）
    REAL        PR(0:MSTP,MZ)           !     予熱開始後各ステップにおける蓄熱応答係数
                                        !     （右側直角三角、吸熱側が正）
                                        !     PV(0),PR(0)は現在時刻のもの
    INTEGER     NSIZE                   !     方程式の数（未知数の数）
    INTEGER     IPS(0:MSTP)             !     予熱開始後、各時刻ステップの室温湿度が、未知ベク
                                        !     トルの何次元目から始まるか（=IPS+1次元目から）
    INTEGER     LMODE(MZ,0:1,MHR,NSL)   !     各ゾーンの負荷状態モード（-2：暖房過負荷,
                                        !     -1：暖房軽負荷,0：無負荷,1：冷房軽負荷,
                                        !     2：冷房過負荷,9：停止）
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

    CRHO = [0.288, 0.720]    # 単位依存
    NTRM = [2, 1]
    LSTP = [[17, 20], [27, 0]]   # 潜熱の2項目はダミー（XMQ配列変更時注意）
    LSTQ = [[23, 24], [30, 0]]   # 潜熱の2項目はダミー（XMQ配列変更時注意）

    IDLT = np.zeros(MSTP+1)
    RMSET = np.zeros(MZ+1)
    VOAWK = np.zeros([MZ+1,2+1,MHR+1])
    OATX  = np.zeros([MZ+1,MHR+1])
    IOPTWK = np.zeros(MZ+1)
    FIXEDL = np.zeros(MZ+1)
    PV = np.zeros([MSTP,MZ+1])
    PR = np.zeros([MSTP,MZ+1])
    IPS = np.zeros(MSTP)
    LMODE = np.zeros([MZ+1,2,MHR+1,NSL+1])
    AN = np.zeros([MZ+1,2,MHR+1,NSL+1])
    RM = np.zeros([MZ+1,2,MHR+1,NSL+1])
    RN = np.zeros([MZ+1,2,MHR+1,NSL+1])
    AMRT = np.zeros([MZ+1,2,MHR+1])

    AA = np.zeros([NA+1, NA+1])
    BB = np.zeros(NA+1)
    IP = np.zeros(NA+1)

    WK = np.zeros(MTRM+1)
    EOUT = np.zeros([4+1, NSL+1])
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

        for IZ in range(1, NZ+1):   # ゾーン loop
            if ISEAS == 2:   # 1:夏期、2:冬期、3:中間期
                RMSET[IZ] = RMMN[IZ,ISL]  # 各スペースの設定温湿度下限
            else:
                RMSET[IZ] = RMMX[IZ,ISL]   # 各スペースの設定温湿度上限
            
        KSTP  = NSTP   # 予熱時間（ステップ）
        NSTP1 = NSTP   # 予熱時間（ステップ）

        for JHR in range(1, NHR+1):    # 時刻ループ

            # 導入外気温湿度（外調機考慮） (OATX)
            for IZ in [1, NZ+1]:
                L = M[ int(LCG[IZ]+202) ]   # OAHUデータへのポインタ(L), LCGはXMQ配列のSPACデータへのポインタ
                if ( L == 0 ):   # OAHUデータが指定されていない場合
                    OATX[IZ,JHR] = WD[ISL,JHR]
                else:
                    OATX[IZ,JHR] = X[ int( L+80+(ISL-1)*25+JHR ) ]
            

            # IWARM:予熱ステップか、KSTP:予熱開始後の経過ステップ
            if (IPEAK == 1) and (ISEAS <= 2) and (IOPTG[1,JHR] != 0):   # ピーク計算モードで夏期・冬期で、かつ、空調稼動ステップである

                if (IOPTG[1,JHR] == 2) and (NSTP >= 1):

                    # 一定除去熱量の計算ステップである（予熱開始時刻）
                    # このグループの全てのゾーンで予熱時間が同じでないとダメ

                    if ( JHR == NHR ):  # 24時(0時)の起動に対するピーク計算は未対応
                        raise Exception("想定外エラー")
                    else:

                        KSTP  = 0
                        IWARM = 1
                        JHR0  = JHR     # 予熱開始時刻の記録
                        NSTP1 = NSTP
                        
                        for J in [1,NSTP]:
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

                    #一定除去熱量の計算ステップである（予熱開始時刻以外）
                    KSTP = KSTP + 1
                    IWARM = 1
        
                else:
                    # シミュレーション計算を行うステップである
                    IWARM = 0

            else:
                # シミュレーション計算を行うステップである
                IWARM = 0
    

            # いずれかのゾーンで階段状変化が生じるか(OUT: ICHNG, ISTOP, IDLT)
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

                    for IZ in range(1, NZ+1):

                        # 外気導入量（VOAWK(NZ,IREP,JHR)）
                        if (IOPVG[IZ,JHR] == 2) and (IREP == 0):
                            VOAWK[IZ,IREP,JHR] = 0.0
                        elif (IOPVG[IZ,JHR] == 3) and (IREP == 1):
                            VOAWK[IZ,IREP,JHR] = 0.0
                        elif (IOPVG[IZ,JHR] >= 1):
                            VOAWK[IZ,IREP,JHR] = VOAG[IZ]
                        else:
                            VOAWK[IZ,IREP,JHR] = 0.0
                        
                        # 空調運転状態（IOPTWK(NZ))
                        if (IOPTG[IZ,JHR] == 2) and (IREP == 0):   # 起動直後
                            IOPTWK[IZ] = 0
                        elif (IOPTG[IZ,JHR] == 3) and (IREP == 1):  # 停止直前
                            IOPTWK[IZ] = 0
                        elif (IOPTG[IZ,JHR] >= 1):   # 運転中
                            IOPTWK[IZ] = 1
                        else:
                            IOPTWK[IZ] = 0
                        
                    # 一定除去熱量計算中か（IWARM1）
                    if ( IWARM == 1 ):
                        if (KSTP == 0) and (IREP == 0):
                            IWARM1 = 0
                        elif (KSTP == NSTP1) and (IDLT[KSTP]==0)  and (IREP == 1):
                            IWARM1 = 0
                        else:
                            IWARM1 = 1
                    else:
                        IWARM1 = 0
                    

                    # FIXEDL(NZ)のセット
                    for IZ in range(1, NZ):
                        
                        # 各ゾーンの、未知変数に依存しない固定流入熱量
                        FIXEDL[IZ] = CLDG[IZ,JHR,ISL] + CRHO[ISL]*VOAWK[IZ,IREP,JHR] * ( OATX[IZ,JHR] - REFWD[ISL] )
                        
                        for I in range(1, NTRM[ISL]+1):
                            FIXEDL[IZ] = FIXEDL[IZ] + X[ int( LCG[IZ]+LSTQ[I,ISL] ) ]
                        
                        if ( ISL == 1 ):  # 顕熱
                            L = int(LCG[IZ] + LSZSPC[0])

                            flag_RTVADJ = True
                            while flag_RTVADJ:
                                (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                                if ISTAT != 1:
                                    flag_RTVADJ = False
                                else:
                                    FIXEDL[IZ] = FIXEDL[IZ] + X[L+11] + X[L+12]
                                    L = L + int(LSZSPC[int(M[L])])

                        # 直前の室温湿度変動による直後の流入熱を加算する
                        if ( (IWARM1 == 0) and (IREP == 1) ) or ( (IWARM1 == 1) and (KSTP == 0) ):

                            if (IREP == 0):
                                raise Exception("例外が発生しました")
                                                                        
                            # P0 瞬時蓄熱応答係数（吸熱される側が正） 第2添字=0:二等辺三角、第2添字=1:右側直角二等辺三角
                            # RM 各ゾーンの室温湿度（基準温湿度からの偏差）
                            FIXEDL[IZ] = FIXEDL[IZ] - ( P0[IZ,0,JHR,ISL] - P0[IZ,1,JHR,ISL] ) * RM[IZ,0,JHR,ISL]

                            if ( ISL == 1 ):  # 顕熱

                                L = int(LCG[IZ] + LSZSPC[0])

                                flag_RTVADJ = True
                                while flag_RTVADJ:
                                    (L,JZ,ISTAT) = RTVADJ(LSZSPC,L,M)
                                    if ISTAT != 1:
                                        flag_RTVADJ = False
                                    else:
                                        FIXEDL[IZ] = FIXEDL[IZ] + ( X[L+3] - X[L+4] ) * RM[JZ,0,JHR,ISL]
                                        L = L + int(LSZSPC[int(M[L])])


                    if ( IWARM1 == 1 ):  # 一定除去熱量を計算する

                        # 前処理（OUT: PV(0:NSTP1-1,NZ), PR(0:NSTP1,NZ), 添字0を除く）
                        if ( KSTP == 0 ):
                            
                            for IZ in range(1, NZ+1):
                                for J in range(1, NSTP1+1):

                                    # 予熱開始後各ステップにおける蓄熱応答係数（右側直角三角、吸熱側が正）
                                    PR[J,IZ] = 0.0
                                    if ( J <= NSTP1-1 ):
                                        PV[J,IZ] = 0.0

                                    # NTRM 蓄熱応答係数の項数（顕熱 2、潜熱 1）
                                    for I in range(1, NTRM[ISL]):
                                        LSTPWK = LCG[IZ] + LSTP[I,ISL]
                                        PR[J,IZ] = PR[J,IZ] + X[ int(LSTPWK+1) ] * X[ int(LSTPWK+2) ]**(J-1)
                                        if ( J <= NSTP1-1 ):
                                            PV[J,IZ] = PV[J,IZ] + X[LSTPWK] * X[LSTPWK+2]**(J-1)

                            # 係数行列の0クリア
                            for I in range(1, NA+1):
                                for J in range(1, NA+1):
                                    AA[I,J] = 0.0
                                BB[I] = 0.0
                            IPS[KSTP] = NZ   # IPS 予熱開始後、各時刻ステップの室温湿度が、未知ベクトルの何次元目から始まるか（=IPS+1次元目から）
                            NSIZE     = 0    # 方程式の数（未知数の数）
                            


                        # 方程式に係数を加算する
                        if (IREP == 0) or (ICHNG == 1):
                            for IZ in range(1, NZ+1):
                                PR[0,IZ] = P0[IZ,1,JHR,ISL]
                                PV[0,IZ] = P0[IZ,0,JHR,ISL]
                                (NSIZE,AA,BB,IPS,X,M) = COEFFP(IZ,KSTP,NZ,IREP,NSTP1,IDLT,VFLOW[1,IZ,JHR],
                                        PV[0,IZ],PR[0,IZ],CRHO[ISL],VOAWK[IZ,IREP,JHR],FIXEDL[IZ],
                                        RMSET,REFWD[ISL],ISL,LCG[IZ]+LSZSPC[0],LSZSPC,NA,NSIZE,AA,BB,IPS,X,M)


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
                        (LMODE[1,0,1,ISL],AN[1,0,1,ISL],RM[1,0,1,ISL],RN[1,0,1,ISL],AMRT,WK,X,M) = \
                            POSTP(MZ,NZ,LCG,NSTP1,IDLT,JHR0,NHR,NSIZE,BB,IPS,CRHO[ISL],
                                    VOAWK,OATX,REFWD[ISL],ISL,RMSET,NTRM[ISL],LSTP[1,ISL],
                                    LSTQ[1,ISL],NAZ,VFLOW,SMRT1,SMRT2,LSZSPC,
                                    RM[1,0,1,ISL],WK)


                    else:  # シミュレーションを行う段である
                        pass

    # C                 ! （OUT: LMODE,AN,RM,RN）
    #                   IF ( ( ICHNG.EQ.0 ).AND.( IREP.EQ.1 ) ) THEN
    #                   ! 階段状変化がなく直後の計算が不要である

    #                      DO IZ = 1, NZ
    #                         LMODE(IZ,IREP,JHR,ISL) = LMODE(IZ,0,JHR,ISL)
    #                         AN(IZ,IREP,JHR,ISL) = AN(IZ,0,JHR,ISL)
    #                         RM(IZ,IREP,JHR,ISL) = RM(IZ,0,JHR,ISL)
    #                         RN(IZ,IREP,JHR,ISL) = RN(IZ,0,JHR,ISL)
    #                      END DO

    #                   ELSE

    #                      ! 熱平衡式を解く
    #                      CALL SLVSM(NZ,IOPTWK,EXCAP(1,ISL),SPCAP(1,ISL),NAZ,
    #      -               VFLOW(1,1,JHR),P0(1,IREP,JHR,ISL),CRHO(ISL),
    #      -               VOAWK(1,IREP,JHR),RMMX(1,ISL),RMMN(1,ISL),
    #      -               REFWD(ISL),IPEAK,ISEAS,NITER,FIXEDL,
    #      -               ISL,IREP,LCG,LSZSPC,
    #      -               LMODE(1,IREP,JHR,ISL),AN(1,IREP,JHR,ISL),
    #      -               RM(1,IREP,JHR,ISL),NA,AA,BB,IP)

    #                      ! 外気負荷・室負荷を求める
    #                      DO IZ = 1, NZ
    #                         EOA = CRHO(ISL)
    #      -                       *VOAWK(IZ,IREP,JHR)*( OATX(IZ,JHR)
    #      -                             - (RM(IZ,IREP,JHR,ISL)+REFWD(ISL)) )
    #                         RN(IZ,IREP,JHR,ISL) = AN(IZ,IREP,JHR,ISL) - EOA
    #                      END DO
    #                   END IF

    # C                 MRTの計算
    #                   IF ( ISL.EQ.1 ) THEN
    #                      DO IZ = 1, NZ
    #                         CALL CLCMRT(NZ,VFLOW(1,IZ,JHR),MZ,
    #      -                    RM(1,0,JHR,ISL),
    #      -                    IZ,IREP,CRHO(ISL),SMRT1(IZ,JHR),SMRT2(IZ,JHR),
    #      -                    LCG(IZ),RN(IZ,IREP,JHR,ISL),AMRT(IZ,IREP,JHR))
    #                      END DO
    #                   END IF

    # C                 蓄熱負荷の更新
    #                   IF ( IREP.EQ.1 ) THEN
    #                      DO IZ = 1, NZ
    #                         DO J = 1, NTRM(ISL)
    #                            LSTPWK = LCG(IZ) + LSTP(J,ISL)
    #                            LSTQWK = LCG(IZ) + LSTQ(J,ISL)
    #                            X(LSTQWK) = X(LSTPWK+2)*X(LSTQWK)
    #      -                     - ( X(LSTPWK) - X(LSTPWK+1) )
    #      -                        *RM(IZ,0,JHR,ISL)
    #      -                     - X(LSTPWK+1)*RM(IZ,1,JHR,ISL)
    #                         END DO

    #                         IF ( ISL.EQ.1 ) THEN
    #                            L = LCG(IZ) + LSZSPC(0)
    #                            DO IWL = 1, 9999
    #                               CALL RTVADJ(LSZSPC,L,JZ,ISTAT)
    #                               IF ( ISTAT.NE.1 ) THEN
    #                                  GO TO 500
    #                               ELSE
    #                                  X(L+11)=X(L+7)*X(L+11)
    #      -                              +(X(L+5)-X(L+6))*RM(JZ,0,JHR,ISL)
    #      -                              +X(L+6)*RM(JZ,1,JHR,ISL)
    #                                  X(L+12)=X(L+10)*X(L+12)
    #      -                              +(X(L+8)-X(L+9))*RM(JZ,0,JHR,ISL)
    #      -                              +X(L+9)*RM(JZ,1,JHR,ISL)
    #                                  L = L + LSZSPC(M(L))
    #                               END IF
    #                            END DO
    #                         END IF
    #   500                   CONTINUE

    #                      END DO
    #                   END IF

    #                END IF

    #             END DO   ! 直前・直後 loop
    #          END DO   ! 時刻 loop
    #       END DO   ! 顕熱・潜熱 loop

    return X,M

# if __name__ == '__main__':

    #     print(NAME("15010101"))   
    