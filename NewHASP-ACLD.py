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

# filename = "./newHASP/Sample_Input_NewHASP1.txt"
# filename = "./newHASP/wndwtabl.dat"
# data = read_textfile(filename, ",")
# for line in range(0,len(data)):
#     print(data[line])

def RHEAD(QA, IWFLG, RWFLG):
    """
    気象データのヘッダー部分の読み込み
    """

    # IWFLG[1]　日射・放射の単位 =0:10kJ/m2h, =1:kcal/m2h, =2:kJ/m2h、
    if (QA[15:19] == '10kJ'):
        IWFLG[1] = 0
    elif (QA[15:19] == 'kcal'):
        IWFLG[1] = 1
    elif (QA[15:19] == 'kJ  '):
        IWFLG[1] = 2
    else:
        raise Exception("ERROR: RHEAD")

    # IWFLG[2]　雲量モード =0:雲量, =1:夜間放射、
    if (QA[20:23] ==  'CA '):
        IWFLG[2] = 0
    elif (QA[20:23] == 'LNR'):
        IWFLG[2] = 1
    else:
        raise Exception("ERROR: RHEAD")

    # IWFLG[3]　気象データのカラム数(3以上9以下)
    IWFLG[3] = QA[24]
    if IWFLG[3] <= 3 or IWFLG[3] >= 9:
        raise Exception("ERROR: RHEAD")

    # RWFLG[0] 緯度[deg]（南緯の場合は負値）、
    IWK1 = float(QA[60:62])
    IWK2 = float(QA[62:65])

    if QA[65] == "N":
        RWFLG[0] = IWK1 + IWK2/600.0
    elif QA[65] == "S":
        RWFLG[0] = -IWK1 - IWK2/600.0
    else:
        raise Exception("ERROR: RHEAD")

    # RWFLG[1] 経度[deg]（西経の場合は負値）、
    IWK1 = float(QA[67:70])
    IWK2 = float(QA[70:73])

    if QA[73] == "E":
        RWFLG[1] = IWK1 + IWK2/600.0
    elif QA[65] == "W":
        RWFLG[1] = -IWK1 - IWK2/600.0
    else:
        raise Exception("ERROR: RHEAD")

    # RWFLG[2] 世界時と地方標準時の時差
    RWFLG[2] = float(QA[77:83])

    return IWFLG, RWFLG

#-----------------------------------------------------------------------
# DYNAMIC HEAT LOAD PROGRAM FOR ENERGY SIMULATION
# HASP/ACLD/8501       CODED BY Y.MATSUO
# NewHASP/ACLD         REVISED BY T.NAGAI  
#-----------------------------------------------------------------------

# 1. JOB START

QVER='20200403'                                               

NUB=1    # 建物データファイルの装置番号
NUW=11   # 気象データファイルの装置番号
NUO=12   # ACLD連携のためのファイルの装置番号
NUOT=20  # CSVファイル出力先装置番号
NUWN=2   # 窓データファイル装置番号
NUBW=3   # WCON物性値データ入力装置番号
NUOW=10  # 外気温・外気湿度テキスト出力先装置番号 （weath.dat）
NUOB=13  # BECSへの受け渡しファイル用装置番号
NUDD=99  # デバッグ用（宮田追加）

MX=30000
HC=3.5
HR=4.5
HT=HC+HR
FC=HC/HT
FR=HR/HT
HO=20.0
DR=0.0174533  # 度からラジアンに変換するための係数

GAS = np.zeros(10)   # FURN(顕熱)による冷房負荷 GAS(0:9)
X = np.zeros(MX)
M = np.zeros(MX)

# EQUIVALENCE (X,M)    ! XとMの記憶領域は共有される
# COMMON /XMQ/X

MT = np.zeros(21)
TH = np.zeros(21)
G = np.zeros(10)  # G(0:9)
P = np.zeros(8)
WF = np.zeros((9,4))
GTR = np.zeros(10) # GTR(0:9)
GAD = np.zeros(10) # GAD(0:9)
GRM = np.zeros(10) # GRM(0:9)
GRL = np.zeros(10) # GRL(0:9)
WD = np.zeros((7,24))
ID = np.zeros((7,5))
SH = np.zeros(24)
CHSA = np.zeros(24)
CHCA = np.zeros(24)
WD8 = np.zeros(24)   # 外気飽和湿度－外気絶対湿度
ROH = np.zeros(3)
ROL = np.zeros(3)

MXGL=200   # MXGL    :特性値表1つあたり最大ガラス数
NBLD=3     # NBLD    :ブラインド色種別数
NTBL=4     # NTBL    :窓特性値表の数
MXVS=10    # MXVS    :通気量の最大サンプリング数
MXGT=100   # MXGT    :最大ガラス種別数

GLK = np.zeros((MXGL,NBLD+1,NTBL))   # K[kcal/m2hdeg]    (MXGL,0:NBLD,NTBL)
GLR = np.zeros((MXGL,NBLD+1,NTBL))   # SCR   (MXGL,0:NBLD,NTBL)
GLC = np.zeros((MXGL,NBLD+1,NTBL))   # SCC   (MXGL,0:NBLD,NTBL)

MGT = np.zeros((MXGL,NTBL))          # ガラス種別


GLD = np.zeros((MXVS,NBLD+1,int(MXGT/10)+1,6))   
        # 第4添字
        # =1:AFWのΔSC, =2:AFWのΔU,
#       # =3:PPWのΔSC(Xpull=0), =4:PPWのΔU(Xpull=0),
#       # =5:PPWのΔSC(Xpull=1), =6:PPWのΔU(Xpull=1)

GLKR = np.zeros(MXGT)     # 長波放射成分係数kLR（内側ブラインドなし）
# GLKRB          # 内側ブラインドのkLR
# GLKRBO         # ブラインドの総合熱伝達率に対する放射熱伝達率の比
NVS = np.zeros(6)         # 通気量のサンプリング数（添字は「GLD」第4添字と同）
GLWK = np.zeros((2,2))      # Work array(第2添字=1:ΔSC, =2:ΔU,  第1添字=1:ブラインド開, =2:閉)

FL = np.zeros((5,3))
AM = np.zeros((3,9))
MCNTL = np.zeros(32)   # 「CNTL」カードのデータ内容(XMQ配列に相当)           rev 20200403(T.Nagai)

#       COMMON /ETC/MCNTL

MDW = np.zeros(2)    # MDW(1)  :本日の曜日(=1:月,2:火,..,7:日,8:祝,9:特), MDW(2)  :明日の曜日
WDND = np.zeros((7,24))    # WDND    :明日の気象データ
IDND = np.zeros((7,5))    # IDND    :明日の日付データ
KSCH = np.zeros(2)    # KSCH(1) :本日のスケジュール(=1:全日,2:半日,3:1日中0%), KSCH(2) :明日のスケジュール

ISEAS = np.zeros(2)    #  ISEAS(1):本日の季節(=1:夏期,2:冬期,3:中間期), ISEAS(2):明日の季節

NAZ=20   # NAZ     :1グループあたりの最大スペース数(変更の場合は関連ルーチンのPARAMETER文を全て変更する必要あり)
NHR=24   # NHR     :1日のステップ数(24以外不可)
NSL=2    # NSL     :顕熱と潜熱(2以外不可)
NWD=7    # NWD     :気象データの種類(7以外不可)

IOPTG = np.zeros((NAZ,NHR))        # 空調運転状態フラグ、=0:停止中,=1:運転中,=2:起動,=3:停止
IOPVG = np.zeros((NAZ,NHR))        # 外気導入状態フラグ、=0:カット中,=1:導入中,=2:導入開始,=3:導入停止
SMRT1 = np.zeros((NAZ,NHR))        # 面積を持たない部位からの冷房負荷
SMRT2 = np.zeros((NAZ,NHR))        # INFLの吸熱応答係数（瞬時）
VOAG = np.zeros(NAZ)             # 導入時の外気量
LCG = np.zeros(NAZ)              # XMQ配列のSPACデータへのポインタ（L）
CLDG = np.zeros((NAZ,NHR,NSL))     # 冷房負荷
REFWD = np.zeros(NSL)            # 基準温湿度
P0 = np.zeros((NAZ,2,NHR,NSL))   # 瞬時蓄熱応答係数（吸熱される側が正）
                                #     第2添字=0:二等辺三角
                                #     第2添字=1:右側直角二等辺三角
EXCAP = np.zeros((NAZ,NSL))        # 各スペースの装置容量（冷却、0以上）
SPCAP = np.zeros((NAZ,NSL))        # 各スペースの装置容量（加熱、0以上）
RMMX  = np.zeros((NAZ,NSL))         # 各スペースの設定温湿度上限
RMMN  = np.zeros((NAZ,NSL))         # 各スペースの設定温湿度下限
                        # （RMMX,RMMNは基準温湿度からの偏差ではない）
# LOPC                  # OPCOデータへのポインタ(L)
# IZ                    # 当該スペースは現在のグループの何スペース目か

IDWK = np.zeros(3)               # 年・月・日
# NZ                    # 現在のグループのスペース数
VFLOW = np.zeros((NAZ,NAZ,NHR))    # 第1添字目のスペースから第2添字目のスペースへの流入
                        #     風量（体積流量、0以上、対角項は0とする）
# ICYCL                 # 気象データをRewindした回数（Rewind後）
# ICYCLO                # 気象データをRewindした回数（Rewind前）
# ISTAT                 # 気象データファイルの状態 =1:通常  0:ファイル終了(IOPWE=1のとき)
# ISTAT2                # SUBROUTINE RTVADJからの返り値
# IOPTWK                # 空調運転状態フラグ、=0:停止中、=1:運転中、=2:起動、=3:停止

WINCHR = np.zeros((3,2))       # 窓の物性値（第1添字=0:K, 1:SCC, 2:SCR、第2添字=0:ブラインド開時、1:閉時）
MFLWK = np.zeros(2)              # CFLWデータセット用Work array
XFLWK = np.zeros(2)              # CFLWデータセット用Work array
LSZSPC = np.zeros(5)           # XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,(3):WNDW, (4):INFL の変数の数
IWFLG = np.zeros(4)              # 気象データヘッダ行のデータ                add 20200403(T.Nagai)
                                # (1) =0:ヘッダ行がない、=1:ヘッダ行がある
                                # (2):日射・放射の単位 =0:10kJ/m2h,
                                #   =1:kcal/m2h, =2:kJ/m2h、
                                # (3) 雲量モード =0:雲量, =1:夜間放射、
                                # (4) 気象データのカラム数(3以上9以下)
RWFLG = np.zeros(3)              # 気象データヘッダ行のデータ                add 20200403(T.Nagai)
                                # (1) 緯度[deg]（南緯の場合は負値）、
                                # (2) 経度[deg]（西経の場合は負値）、
                                # (3) 世界時と地方標準時の時差
                                #  （日本の場合は9.0）
# QKY*4,
# QD*80,
# QJB*80,
# QSP*4,
# QERR*1
# QWK*200
# QPATH*200   # 出力データパス(".....\"まで)

# rev 20200403(T.Nagai)
LSZSPC = [218,16,26,47,8]                                               

# INSIDE SURFACE REFLECTANCE
ROH = [0.7, 0.5, 0.3]
ROL = [0.3, 0.2, 0.1]

# WINDOW GLASS DATA (Initialization)
# GLK = 3200*9.999
# GLR = 3200*9.999
# GLC = 3200*9.999
# GLD = 2640*9.999
# GLKR   = 102*9.999
# GLKRB  = 102*9.999
# GLKRBO = 102*9.999

# WF FOR LIGHTING FIXTURE
FL = [  0.4438,0.0534,0.8972,0.0362,0.0000,
        0.7321,0.0254,0.8926,0.0309,0.0000,
        1.0000,0.0000,0.0000,0.0000,0.0000  ]

# OCCUPANCY HEAT DISCHARGE
AM = [  79.,50.,-3.0,91.,53.,-3.1,102.,54.,-3.4,113.,55.,-3.6,
        125.,59.,-3.8,170.,65.,-5.6,194.,72.,-6.0,227.,85.,-6.3,
        329.,118.,-5.4 ]

#**********************************************************************************************************

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

#**********************************************************************************************************

# 入力ファイル 1行目 建物データファイル名称
NUB = read_textfile("./newHASP/Sample_Input_NewHASP1.txt")

# 入力ファイル 2行目 気象データファイル名称
NUW = read_textfile("./newHASP/36300110_SI.hasH")

# 気象データファイルの1行目の1カラム目が「*」の場合は、ヘッダ行あり、その 他の文字の場合はヘッダ行なしと見なされる。
if NUW[0][0] == "*":
    IWFLG[0] = 1
    IWFLG, RWFLG = RHEAD(NUW[0], IWFLG, RWFLG)
    MCNTL[2]  = IWFLG[2]
    MCNTL[3]  = IWFLG[1]
    MCNTL[30] = IWFLG[3]
else:
    IWFLG[0] = 0

# 入力ファイル 3行目 出力先のディレクトリ名称
QPATH = "./out/"

# 入力ファイル 4行目 窓のファイル WINDOW GLASS DATA (Read from file)
# NUWN = read_textfile("./newHASP/wndwtabl.dat", ",")

# K,SCC,SCRの読み込み
wb = xlrd.open_workbook("wndwtabl.xlsx")

for II in range(0, NTBL):

    if II == 0:
        sheets = wb.sheet_by_name("単板")
    elif II == 1:
        sheets = wb.sheet_by_name("複層A6mm")
    elif II == 2:
        sheets = wb.sheet_by_name("複層A12mm")
    elif II == 3:
        sheets = wb.sheet_by_name("複層ブラインド内蔵")
    else:
        raise Exception("シート名が不正です")

    for I in range(0, sheets.nrows-1):
        row_data = sheets.row_values(I+1)

        I1 = float(row_data[0])
        MGT[I][II] = float(row_data[1])

        for j in range(0,NBLD):

            if abs(float(row_data[3*j+2]) - 9.999) > 0.001:
                GLK[I][j][II] = float(row_data[3*j+2])*0.86  # [W/m2K]から[kcal/m2hdeg]へ変換
            else:
                GLK[I][j][II] = float(row_data[3*j+2])           

            GLC[I][j][II] = float(row_data[3*j+3])
            GLR[I][j][II] = float(row_data[3*j+4])

# AFW, PPW関連データの読み込み ＜省略＞

# 入力ファイル 5行目 建材のファイル 
NUBW = xlrd.open_workbook("wndwtabl.xlsx")

# 入力ファイル 6行目 ACSSへの連携のためのファイル ＜省略＞
# 入力ファイル 7行目 BECSへの連携のためのファイル ＜省略＞


#*****       2. PRELIMINARY PROCESS ***********************************

#***          2.1. BUILDING COMMON DATA *******************************

for line in range(1,len(NUB)):
    
    KEY = NUB[line][0:4]

    if KEY == "BUIL":

        # DCHECK(QD,560,NERR)

        W1 = float(NUB[line][11:17])  # 緯度
        W2 = float(NUB[line][17:23])  # 経度
        X[153] = float(NUB[line][23:29])  # 軒高
        X[154] = float(NUB[line][29:35])  # 地物反射率
        X[155] = float(NUB[line][35:41])  # 基準温度
        X[156] = float(NUB[line][41:47])  # 基準湿度
        X[158] = float(NUB[line][47:53])  # 限界日射取得
        W3 = float(NUB[line][53:59])  # 時差

        if IWFLG[0] == 1:  # 気象データにヘッダ行がある場合

            X[150] = math.sin(DR*RWFLG[0])   # 気象データに記された緯度の正弦
            X[151] = math.cos(DR*RWFLG[0])   # 気象データに記された緯度の余弦
            X[152] = RWFLG[1]/15 - RWFLG[2]  # 標準時との時差            

        else:

            X[150] = math.sin(DR*W1)   # 気象データに記された緯度の正弦
            X[151] = math.cos(DR*W1)   # 気象データに記された緯度の余弦
            X[152] = W2/15 - W3  # 標準時との時差          

        X[154] = 0.01 * X[154]    # 反射率を % から 比率 に。
        X[157] = SATX(X[155]) * X[156] / 100   # 基準湿度（絶対湿度）
        X[158] = 0.860 * X[158]    # W/m2 を kcal/m2h に変換

        REFWD[0] = X[155]   # 基準温度
        REFWD[1] = X[157]   # 基準湿度（絶対湿度）



    # elif KEY == "CNTL":
    #     print("未実装")
    # elif KEY == "HRAT":
    #     print("未実装")
    # elif KEY == "EXPS":
    #     print("未実装")
    # elif KEY == "WCON":
    #     print("未実装")
    # elif KEY == "WSCH":
    #     print("未実装")
    # elif KEY == "DSCH":
    #     print("未実装")
    # elif KEY == "SDAY":
    #     print("未実装")
    # elif KEY == "SEAS":
    #     print("未実装")
    # elif KEY == "OPCO":
    #     print("未実装")
    # elif KEY == "OSCH":
    #     print("未実装")
    # elif KEY == "OAHU":
    #     print("未実装")
    # else:
    #     print(KEY)



