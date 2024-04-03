#-----------------------------------------------------------------------
# DYNAMIC HEAT LOAD PROGRAM FOR ENERGY SIMULATION
# HASP/ACLD/8501       CODED BY Y.MATSUO
# NewHASP/ACLD         REVISED BY T.NAGAI 
# pyHASP/ACLD          CODED BY M.MIYATA
#-----------------------------------------------------------------------
import math
import numpy as np
import xlrd
import pandas as pd

from RHEAD import RHEAD
from NDATE import NDATE
from NDATF import NDATF
from MKMDW import MKMDW
from RETRIV import RETRIV
from NAME import NAME
from GVECTR import GVECTR
from CPARAM import CPARAM
from RTVADJ import RTVADJ
from INWD import INWD
from EXTRC0 import EXTRC0
from EXTRC1 import EXTRC1
from EXTRC2 import EXTRC2

from read_textfile import read_textfile
from mprint import mprint
from display_XMQ_matrix import display_XMQ_matrix

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

#-----------------------------------------------------------------------
# 0. INPUT
#-----------------------------------------------------------------------

inputfile_name   = "./input/test_001_single_room.txt"
climatefile_name = "./input/36300110_SI.hasH"


#-----------------------------------------------------------------------
# 1. JOB START
#-----------------------------------------------------------------------

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

# XMQ配列 X：実数, M：整数、Q：文字列（整数に変換）
X = np.zeros(MX)
M = np.zeros(MX)

MT = np.zeros(21)
TH = np.zeros(21)
G = np.zeros(10)  # G(0:9)
P = np.zeros(8)
WF = np.zeros((10,5))
GTR = np.zeros(10) # GTR(0:9)
GAD = np.zeros(10) # GAD(0:9)
GRM = np.zeros(10) # GRM(0:9)
GRL = np.zeros(10) # GRL(0:9)
WD = np.zeros((7+1,24+1))
ID = np.zeros((7+1,5+1))
SH = np.zeros(24+1)
CHSA = np.zeros(24+1)
CHCA = np.zeros(24+1)
WD8 = np.zeros(24+1)   # 外気飽和湿度－外気絶対湿度
ROH = np.zeros(3)
ROL = np.zeros(3)

MXGL=200   # MXGL    :特性値表1つあたり最大ガラス数
NBLD=3     # NBLD    :ブラインド色種別数
NTBL=4     # NTBL    :窓特性値表の数
MXVS=10    # MXVS    :通気量の最大サンプリング数
MXGT=100   # MXGT    :最大ガラス種別数

# 第2引数が0のときはブラインドなし
GLK = np.zeros((MXGL,NBLD+1,NTBL+1))   # K[kcal/m2hdeg]    (MXGL,0:NBLD,NTBL)
GLR = np.zeros((MXGL,NBLD+1,NTBL+1))   # SCR   (MXGL,0:NBLD,NTBL)
GLC = np.zeros((MXGL,NBLD+1,NTBL+1))   # SCC   (MXGL,0:NBLD,NTBL)
MGT = np.zeros((MXGL,NTBL+1))            # ガラス種別

GLD = np.zeros((MXVS+1,NBLD+1,int(MXGT/10)+1,6+1))   
        # 第4添字
        # =1:AFWのΔSC, =2:AFWのΔU,
#       # =3:PPWのΔSC(Xpull=0), =4:PPWのΔU(Xpull=0),
#       # =5:PPWのΔSC(Xpull=1), =6:PPWのΔU(Xpull=1)

GLKR = np.zeros(MXGT)     # 長波放射成分係数kLR（内側ブラインドなし）
# GLKRB          # 内側ブラインドのkLR
# GLKRBO         # ブラインドの総合熱伝達率に対する放射熱伝達率の比
NVS = np.zeros(6+1)         # 通気量のサンプリング数（添字は「GLD」第4添字と同）
GLWK = np.zeros((2,2))      # Work array(第2添字=1:ΔSC, =2:ΔU,  第1添字=1:ブラインド開, =2:閉)

MCNTL = np.zeros(32+1)   # 「CNTL」カードのデータ内容(XMQ配列に相当)           rev 20200403(T.Nagai)

#  COMMON /ETC/MCNTL

MDW = np.zeros(2+1)    # MDW(1)  :本日の曜日(=1:月,2:火,..,7:日,8:祝,9:特), MDW(2)  :明日の曜日
WDND = np.zeros((7,24))    # WDND    :明日の気象データ
IDND = np.zeros((7,5))    # IDND    :明日の日付データ
KSCH = np.zeros(2+1)    # KSCH(1) :本日のスケジュール(=1:全日,2:半日,3:1日中0%), KSCH(2) :明日のスケジュール

ISEAS = np.zeros(2+1)    #  ISEAS(1):本日の季節(=1:夏期,2:冬期,3:中間期), ISEAS(2):明日の季節

NAZ=20   # NAZ     :1グループあたりの最大スペース数(変更の場合は関連ルーチンのPARAMETER文を全て変更する必要あり)
NHR=24   # NHR     :1日のステップ数(24以外不可)
NSL=2    # NSL     :顕熱と潜熱(2以外不可)
NWD=7    # NWD     :気象データの種類(7以外不可)

# EXTRC1 関連
IOPTG = np.zeros([NAZ+1,NHR+1])       # 空調運転状態フラグ、=0:停止中,=1:運転中,=2:起動,=3:停止
IOPVG = np.zeros([NAZ+1,NHR+1])       # 外気導入状態フラグ、=0:カット中,=1:導入中,=2:導入開始,=3:導入停止
SMRT1 = np.zeros([NAZ+1,NHR+1])       # 面積を持たない部位からの冷房負荷
SMRT2 = np.zeros([NAZ+1,NHR+1])       # INFLの吸熱応答係数（瞬時）
VOAG  = np.zeros([NAZ+1])             # 導入時の外気量
LCG   = np.zeros([NAZ+1])             # XMQ配列のSPACデータへのポインタ（L）
CLDG  = np.zeros([NAZ+1,NHR+1,NSL+1])    # 冷房負荷
REFWD = np.zeros(NSL+1)                  # 基準温湿度
P0    = np.zeros([NAZ+1,2,NHR+1,NSL+1])  # 瞬時蓄熱応答係数（吸熱される側が正）第2添字=0:二等辺三角、=1:右側直角二等辺三角
EXCAP = np.zeros([NAZ+1,NSL+1])       # 各スペースの装置容量（冷却、0以上）
SPCAP = np.zeros([NAZ+1,NSL+1])       # 各スペースの装置容量（加熱、0以上）
RMMX  = np.zeros([NAZ+1,NSL+1])       # 各スペースの設定温湿度上限（RMMX,RMMNは基準温湿度からの偏差ではない）
RMMN  = np.zeros([NAZ+1,NSL+1])       # 各スペースの設定温湿度下限（RMMX,RMMNは基準温湿度からの偏差ではない）
VFLOW = np.zeros([NAZ+1,NAZ+1,NHR+1]) # 第1添字目のスペースから第2添字目のスペースへの流入

# LOPC                  # OPCOデータへのポインタ(L)
# IZ                    # 当該スペースは現在のグループの何スペース目か

IDWK = np.zeros(3+1)               # 年・月・日

# NZ                    # 現在のグループのスペース数

                        #     風量（体積流量、0以上、対角項は0とする）
# ICYCL                 # 気象データをRewindした回数（Rewind後）
# ICYCLO                # 気象データをRewindした回数（Rewind前）
# ISTAT                 # 気象データファイルの状態 =1:通常  0:ファイル終了(IOPWE=1のとき)
# ISTAT2                # SUBROUTINE RTVADJからの返り値
# IOPTWK                # 空調運転状態フラグ、=0:停止中、=1:運転中、=2:起動、=3:停止

WINCHR = np.zeros((3,2))        # 窓の物性値（第1添字=0:K, 1:SCC, 2:SCR、第2添字=0:ブラインド開時、1:閉時）
MFLWK = np.zeros(2)             # CFLWデータセット用Work array
XFLWK = np.zeros(2)             # CFLWデータセット用Work array
LSZSPC = np.zeros(5)            # XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL,(3):WNDW, (4):INFL の変数の数
IWFLG = np.zeros(4+1)           # 気象データヘッダ行のデータ                add 20200403(T.Nagai)
                                # (1) =0:ヘッダ行がない、=1:ヘッダ行がある
                                # (2):日射・放射の単位 =0:10kJ/m2h,
                                #   =1:kcal/m2h, =2:kJ/m2h、
                                # (3) 雲量モード =0:雲量, =1:夜間放射、
                                # (4) 気象データのカラム数(3以上9以下)
RWFLG = np.zeros(3+1)              # 気象データヘッダ行のデータ                add 20200403(T.Nagai)
                                # (1) 緯度[deg]（南緯の場合は負値）、
                                # (2) 経度[deg]（西経の場合は負値）、
                                # (3) 世界時と地方標準時の時差
                                #  （日本の場合は9.0）

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
FL = [  [0.4438,0.0534,0.8972,0.0362,0.0000],
        [0.7321,0.0254,0.8926,0.0309,0.0000],
        [1.0000,0.0000,0.0000,0.0000,0.0000]  ]

# OCCUPANCY HEAT DISCHARGE
AM = [  [79.,50.,-3.0],
        [91.,53.,-3.1],
        [102.,54.,-3.4],
        [113.,55.,-3.6],
        [125.,59.,-3.8],
        [170.,65.,-5.6],
        [194.,72.,-6.0],
        [227.,85.,-6.3],
        [329.,118.,-5.4] ]

#-----------------------------------------------------------------------
# ファイルの読み込み
#-----------------------------------------------------------------------

# 入力ファイル 1行目 建物データファイル名称
NUB = read_textfile(inputfile_name)

# 入力ファイル 2行目 気象データファイル名称
NUW = read_textfile(climatefile_name)

# 気象データファイルの1行目の1カラム目が「*」の場合は、ヘッダ行あり、
# その他の文字の場合はヘッダ行なしと見なされる。
if NUW[0][0] == "*":

    IWFLG[1] = 1   # ヘッダー行がある
    
    # 気象データのヘッダー部分の読み込み
    IWFLG, RWFLG = RHEAD(NUW[0], IWFLG, RWFLG)
    
    MCNTL[3]  = IWFLG[3]  # 雲量
    MCNTL[4]  = IWFLG[2]  # 日射の単位
    MCNTL[31] = IWFLG[4]  # データのカラム数（3以上9以下）

else:
    IWFLG[1] = 0   # ヘッダー行がない

# 入力ファイル 3行目 出力先のディレクトリ名称
QPATH = "./out/"

# K,SCC,SCRの読み込み
wb = xlrd.open_workbook("./input/wndwtabl.xlsx")

# NTBL    :窓特性値表の数
for II in range(1, NTBL+1):

    if II == 1:
        sheets = wb.sheet_by_name("単板")
    elif II == 2:
        sheets = wb.sheet_by_name("複層A6mm")
    elif II == 3:
        sheets = wb.sheet_by_name("複層A12mm")
    elif II == 4:
        sheets = wb.sheet_by_name("複層ブラインド内蔵")
    else:
        raise Exception("シート名が不正です")

    for I in range(0, sheets.nrows-1):

        # データ読み込み（2行目から）
        row_data = sheets.row_values(I+1)

        # ガラス品種番号（1～200）
        I1 = int(row_data[0])
        # ガラス種別番号（長波長放射成分係数を区別するため等に使用）
        MGT[I1][II] = float(row_data[1])

        # NBLD    :ブラインド色種別数(=4)
        for j in range(0,NBLD):

            if abs(float(row_data[3*j+2]) - 9.999) > 0.001:
                GLK[I1][j][II] = float(row_data[3*j+2])*0.86  # [W/m2K]から[kcal/m2hdeg]へ変換
            else:
                GLK[I1][j][II] = float(row_data[3*j+2])           

            GLC[I1][j][II] = float(row_data[3*j+3])
            GLR[I1][j][II] = float(row_data[3*j+4])

# AFW, PPW関連データの読み込み
for II in [1,2,3,4,5,6]:

    if II == 1:
        sheets = wb.sheet_by_name("AFW日射遮蔽補正")
    elif II == 2:
        sheets = wb.sheet_by_name("AFW熱貫流率補正")
    elif II == 3:
        sheets = wb.sheet_by_name("PPW日射遮蔽補正0")
    elif II == 4:
        sheets = wb.sheet_by_name("PPW熱貫流率補正0")
    elif II == 5:
        sheets = wb.sheet_by_name("PPW日射遮蔽補正1")
    elif II == 6:
        sheets = wb.sheet_by_name("PPW熱貫流率補正1")
    else:
        raise Exception("シート名が不正です")

    L1 = 9
    NVS[II] = 8  # 通気量の条件の数

    # 2行目
    row_data = sheets.row_values(1)
    IWK = float(row_data[0])
    for JJ in range(1, int(NVS[II])):
        GLD[JJ,0,0,II] = row_data[JJ]

    # 3行目以降
    for I in range(0, L1):
        for J in range(0, NBLD):
            row_data = sheets.row_values(2 + NBLD*I + J)
            IWK = float(row_data[0])
            for JJ in range(1, int(NVS[II])):
                GLD[JJ,J,I,II] = row_data[JJ]
                # 単位変換は省略

# kLR等の読み込み
sheets = wb.sheet_by_name("kLR")
row_data = sheets.row_values(1)
L1 = int(row_data[0])

for I in range(1,L1+1):
    row_data = sheets.row_values(I+1)
    L2 = int(row_data[0])
    GLKR[L2] = row_data[1]

# 熱伝達比率
sheets = wb.sheet_by_name("熱伝達比率")
row_data = sheets.row_values(1)
GLKRB = row_data[0]    # 内側ブラインドのkLR
GLKRBO = row_data[1]   # PPWブラインドにおける放射熱伝達率／総合熱伝達率


# 入力ファイル 5行目 建材のファイル 
NUBW = xlrd.open_workbook("./input/wcontabl.xlsx")
# print(NUBW.sheet_by_name("Sheet1").cell(0,0).value)


# 入力ファイル 6行目 ACSSへの連携のためのファイル ＜省略＞
# 入力ファイル 7行目 BECSへの連携のためのファイル ＜省略＞


#-----------------------------------------------------------------------
# 2. PRELIMINARY PROCESS
#-----------------------------------------------------------------------

#  2.1. BUILDING COMMON DATA

L = 1500

# Building Dataの終了行を探す。
bldg_end = 0
for line in range(1, len(NUB)):
    if len(NUB[line]) == 1:  # 空白行を探す
        bldg_end = line
        break
print(f"建物データの最終行: {bldg_end}")


for line in range(1,bldg_end):
    
    KEY = NUB[line][0:4]

    print(f"KEY: {KEY}")

    if KEY == "BUIL":

        # DCHECK(QD,560,NERR)

        W1 = float(NUB[line][11:17])  # 緯度
        W2 = float(NUB[line][17:23])  # 経度
        X[153] = float(NUB[line][23:29])  # 軒高

        
        if (NUB[line][29:35]) == "      " : # 地物反射率（デフォルト:10）
            X[154] =  10
        else:
            X[154] = float(NUB[line][29:35])  

        if (NUB[line][35:41]) == "      ": # 基準温度（デフォルト:24）
            X[155] = 24
        else:
            X[155] = float(NUB[line][35:41])  

        if (NUB[line][41:47]) == "      ": # 基準湿度（デフォルト:50）
            X[156] = 50
        else:
            X[156] = float(NUB[line][41:47])  

        if (NUB[line][47:53]) == "      ": # 限界日射取得（デフォルト:200）
            X[158] = 200
        else:
            X[158] = float(NUB[line][47:53])  

        if (NUB[line][53:59]) == "      ": # 時差（デフォルト:9.0）
            W3 = 0
        else:
            W3 = float(NUB[line][53:59])  


        if IWFLG[0] == 1:  # 気象データにヘッダ行がある場合

            X[150] = math.sin(DR*RWFLG[0])   # 気象データに記された緯度の正弦
            X[151] = math.cos(DR*RWFLG[0])   # 気象データに記された緯度の余弦
            X[152] = RWFLG[1]/15 - RWFLG[2]  # 標準時との時差            

        else:

            X[150] = math.sin(DR*W1)   # 気象データに記された緯度の正弦
            X[151] = math.cos(DR*W1)   # 気象データに記された緯度の余弦
            X[152] = W2/15 - W3        # 標準時との時差          


        X[154] = 0.01 * X[154]                    # 反射率を % から 比率 に。
        X[157] = SATX(X[155]) * X[156] / 100   # 基準湿度（絶対湿度）
        X[158] = 0.860 * X[158]                   # W/m2 を kcal/m2h に変換

        REFWD[1] = X[155]   # 基準温度
        REFWD[2] = X[157]   # 基準湿度（絶対湿度）


    elif KEY == "CNTL":

        # CALL DCHECK(QD,1001,NERR)

        MCNTL[1]  = float(NUB[line][11:14]) # 計算モード         
        MCNTL[2]  = float(NUB[line][14:17]) # 出力形式
        if IWFLG[0] == 0:
            MCNTL[3]  = float(NUB[line][17:20]) # 雲量モード  
            MCNTL[4]  = float(NUB[line][20:23]) # SIモード    
        MCNTL[5]  = float(NUB[line][23:26]) # データ形式 0:標準年気象データ、1:ピークデータ、2:実データ
        if NUB[line][26:29] != "   ":
            MCNTL[6]  = float(NUB[line][26:29]) # 助走開始 年  
        MCNTL[7]  = float(NUB[line][29:32]) # 助走開始 月  
        MCNTL[8]  = float(NUB[line][32:35]) # 助走開始 日  
        if NUB[line][35:38] != "   ":
            MCNTL[9]  = float(NUB[line][35:38]) # 本計算開始 年 
        MCNTL[10] = float(NUB[line][38:41]) # 本計算開始 月 
        MCNTL[11] = float(NUB[line][41:44]) # 本計算開始 日 
        if NUB[line][44:47] != "   ":
            MCNTL[12] = float(NUB[line][44:47]) # 計算終了 年   
        MCNTL[13] = float(NUB[line][47:50]) # 計算終了 月   
        MCNTL[14] = float(NUB[line][50:53]) # 計算終了 日   

        # 4.8 INITIALIZATION で予めデフォルト値が代入されている

        # 人体発熱顕熱比率算出用温度
        if NUB[line][59:63] != "   ":
            MCNTL[32] = float(NUB[line][59:63])

        # 年について、二桁（80）から四桁（1980）に変換
        # 51年以上の場合は1900年代とみなす

        for MCNTL_i in [6,9,12]:
            if MCNTL[5] == 2:  # 実気象データである場合
                if MCNTL[MCNTL_i] >= 51: 
                    MCNTL[MCNTL_i] += 1900
                else:
                    MCNTL[MCNTL_i] += 2000
            else:
                MCNTL[MCNTL_i]=0


        # 1899/12/31を1とした通算日数(EXCELと合わせるため)
        # 実在気象データ以外が指定されたときは1999年となる
        # 助走開始日の通算日数 MCNTL(19)
        # 本計算開始日の通算日数 MCNTL(20)
        # 計算終了日の通算日数 MCNTL(21)

        MCNTL[19] = NDATF(MCNTL[6],MCNTL[7],MCNTL[8])
        MCNTL[20] = NDATF(MCNTL[9],MCNTL[10],MCNTL[11])
        MCNTL[21] = NDATF(MCNTL[12],MCNTL[13],MCNTL[14])

    elif KEY == "HRAT":

        # CALL DCHECK(QD,1341,NERR)

        for i in range(0,9):
            L1 = 11 + 3*i
            if NUB[line][L1:L1+3] != "   ":
                MCNTL[22+i] = float(NUB[line][L1:L1+3]) 


    elif KEY == "EXPS":

        # CALL DCHECK(QD,597,NERR)

        # 名称の数値化と起点（初期値100）の検索
        # NNAMは数値化された EXPS名称 QD(6:9) 
        # 例えば、 N___ であれば 15010101、 E___  であれば 6010101
        (NNAM,LC,LD) = RETRIV(100,NUB[line][5:9],M)

        if LD != 0:
            print(f"EXPS: " + {NUB[line][5:9]})
            raise Exception("LDが0以外になります")

        # 行番号の保存 EXPSの場合、起点は LC=100
        M[int(LC)] = L     # 初期値 LC=100, L=1500 次のデータが入っている行番号

        # 起点 M(L) には 0 を入れる
        M[int(L)] = LD						 

        # EXPS名称 M(起点＋1)
        M[int(L+1)] = NNAM

        # 傾斜角 W2
        # 方位角 W1
        # 隣棟距離 X(L+12)
        # 隣棟高さ X(L+13)

        W2 = float(NUB[line][11:17])    # 傾斜角
        W1 = float(NUB[line][17:23])    # 方位角

        if len(NUB[line]) > 24:

            X[L+12] = float(NUB[line][23:26])   # 隣棟距離[m]
            X[L+13] = float(NUB[line][26:29])   # 隣棟高さ[m]

        X[L+2]  = W1				  # 傾斜角 X(起点+2)
        X[L+3]  = math.sin(DR*W1)	  # 傾斜角のsin X(起点+3)
        X[L+4]  = math.cos(DR*W1)	  # 傾斜角のcos X(起点+4)
        X[L+5]  = math.sin(DR*W2)	  # 方位角のsin X(起点+5)
        X[L+6]  = math.cos(DR*W2)	  # 方位角のcos X(起点+6)
        X[L+7]  = X[L+3] * X[L+5]     # 傾斜角のsinと方位角のsinの積
        X[L+8]  = X[L+3] * X[L+6]     # 傾斜角のsinと方位角のcosの積
        X[L+9]  = X[L+4] * X[L+5]     # 傾斜角のcosと方位角のsinの積
        X[L+10] = X[L+4] * X[L+6]     # 傾斜角のcosと方位角のcosの積
        X[L+11] = (1.0-X[L+6])/2.0    # (1-cos(方位角))/2

        if len(NUB[line]) > 30:

            X[1] = float(NUB[line][29:35])  # 庇の出[ZH] X(1)
            X[2] = float(NUB[line][35:41])  # 窓下[y1] X(2)
            X[3] = float(NUB[line][41:47])  # 窓高[y2] X(3)
            X[4] = float(NUB[line][47:53])  # 小壁[y3] X(4)

            X[L+14] = X[1]                  # 庇の出 X(起点+14)
            X[L+15] = X[3]                  # 窓高  X(起点+15)
            X[L+16] = X[2] + X[3]           # 窓下＋窓高  X(起点+16)
            X[L+17] = X[3] + X[4]           # 窓高+小壁  X(起点+17)
            X[L+18] = X[2] + X[3] + X[4]    # 窓下＋窓高+小壁  X(起点+18)

            X[1] = float(NUB[line][53:59])  # 袖庇の出[ZV] X(1)
            X[2] = float(NUB[line][59:65])  # 右袖壁[x1] X(2)
            X[3] = float(NUB[line][65:71])  # 窓幅[x2] X(3)
            X[4] = float(NUB[line][71:77])  # 左袖壁[x3] X(4)

            X[L+19] = X[1]                         # 袖庇の出 X(起点+19)
            X[L+20] = X[3]                         # 窓幅 X(起点+20)
            X[L+21] = X[2] + X[3]                  # 右袖壁+窓幅 X(起点+21)
            X[L+22] = X[3] + X[4]                  # 窓幅+左袖壁 X(起点+22)
            X[L+23] = X[2] + X[3] + X[4]           # 右袖壁+窓幅+左袖壁 X(起点+23)
            X[L+25] = X[L+15] * X[L+20]            # 窓面積  X(起点+25)
            X[L+24] = X[L+18] * X[L+23] - X[L+25]  # 壁面積  X(起点+24) 

            if X[L+18] == 0:
                X[L+26] = 0.0
            else:
                # X[L+18]: 外皮全体の幅、 X[L+14]: 庇の出
                X[L+26] = 0.5*(X[L+18]+X[L+14]-math.sqrt(X[L+18]**2+X[L+14]**2))/X[L+18]
        
            if X[L+23] == 0:
                X[L+27] = 0.0
            else:
                # X[L+23]: 外皮全体の高さ、 X[L+19]: 袖庇の出
                X[L+27] = (X[L+23]+X[L+19]-math.sqrt(X[L+23]**2+X[L+19]**2))/X[L+23]
    
        L = L+76


    elif KEY == "WCON":

        # DCHECK(QD,615,NERR)
    
        # 名称の数値化と起点（初期値102）の検索
        (NNAM,LC,LD) = RETRIV(102,NUB[line][5:9],M)

        if LD != 0:
            print(f"WCON: " + {NUB[line][5:9]})
            raise Exception("LDが0以外になります")
        
        M[int(LC)] = L
        M[int(L)] = LD

        # WCON名称 M(起点＋1)
        M[L+1] = NNAM

        # 入力された層の数
        NL = int((len(NUB[line])-12) /6)

        for i in range(NL):

            L1 = L + 2*i + 3
            K1 = 6*i + 12

            # ! 材番（3桁） M1
            if NUB[line][K1-1:K1+2] == "   ":
                M1 = 0
            else:
                M1 = float(NUB[line][K1-1:K1+2])
    
            # ! 厚さ（3桁） W1
            if NUB[line][K1+2:K1+5] == "   ":
                W1 = 0
            else:
                W1 = float(NUB[line][K1+2:K1+5])

            if M1 != 0:
                M[L1] = M1              # 材料番号 M(L+3)
                X[L1+1] = 0.001*W1      # 材料厚さ[mm → m] M(L+4)
    
        # 層の数 M(L+2)
        M[L+2] = NL

        # Lの更新
        L = L + 2*NL + 3


    elif KEY == "WSCH":

        # CALL DCHECK(QD,661,NERR)        

        # WSCH名称の数値化と起点の検索
        (NNAM,LC,LD) = RETRIV(108,NUB[line][5:9],M)
        
        if LD != 0:
            print(f"WSCH: " + {NUB[line][5:9]})
            raise Exception("LDが0以外になります")

        M[int(LC)] = int(L)
        M[int(L)]  = int(LD)

        # WSCH名称 M(起点＋1)
        M[int(L+1)] = NNAM
        M[int(L+2)] = int(NUB[line][11:14])  # 月曜
        M[int(L+3)] = int(NUB[line][14:17])  # 火曜
        M[int(L+4)] = int(NUB[line][17:20])  # 水曜
        M[int(L+5)] = int(NUB[line][20:23])  # 木曜
        M[int(L+6)] = int(NUB[line][23:26])  # 金曜
        M[int(L+7)] = int(NUB[line][26:29])  # 土曜
        M[int(L+8)] = int(NUB[line][29:32])  # 日曜
        M[int(L+9)] = int(NUB[line][32:35])  # 祝日
        M[int(L+10)] = int(NUB[line][35:38])  # 特別日

        # Lの更新
        L = L + 11
    
    elif KEY == "DSCH":

        # DSCH名称の数値化と起点の検索
        (NNAM,LC,LD) = RETRIV(104,NUB[line][5:9],M)

        if LD != 0:
            print(f"DSCH: " + {NUB[line][5:9]})
            raise Exception("LDが0以外になります")

        M[int(LC)] = int(L)
        M[int(L)] = int(LD)

        # DSCH名称 M(起点＋1)
        M[L+1] = NNAM

        # 変数初期化 X(L+2) から X(L+73)   73は 24時間×3パターン + 1（名称分）
        for i in range(L+2,L+73+1):
            X[int(i)] = 0

        # 指定可能なスケジュールは3つ
        for i in [1,2,3]:

            # 読み込む行数
            n = line + (i-1)

            if i == 1 or (i > 1 and NUB[n][0:4] == "+   "):

                # もし空白であれば 24時間停止（1時から24時まで0）とする。
                if NUB[n][11:14] == "   " or len(NUB[n]) < 15:
                    NUB[n] = NUB[n][0:9] + "    1  0 24"

                K1 = 12
                L1 = L + 24*i - 23

                # スケジュール 開始時刻 M1
                M1 = int(NUB[n][K1-1:K1+2])

                while K1 < len(NUB[n])-8 and M1 != 0:

                    # スケジュール 比率% W
                    # スケジュール 終了時刻 M2
                    W  = int(NUB[n][K1+2:K1+5])
                    M2 = int(NUB[n][K1+5:K1+8])

                    for m in range(M1, M2+1):
                        X[L1 + m] = 0.01 * W

                    M1 = M2
                    K1 = K1 + 6

        # Lの更新
        L = L + 74

    elif KEY == "SDAY":

        # 特別日のカウント
        N = 1
        for i in range(1,35): # ! データ行のループ(34:365日を特別日とする場合の最大行数)
            if (len(NUB[line+i]) > 4) and (NUB[line+i][0:4] == "+   "):
                N += 1
            else:
                break

        for i in range(1,N+1): # ! データ行のループ(34:365日を特別日とする場合の最大行数)

            # 読み込む行数
            n = line + (i-1)

            if i == 1 or (i > 1 and NUB[n][0:4] == "+   "):

                for j in range(0, int(len(NUB[n][11:-1])/6) ):

                    M1 = 171 + 2*j
                    
                    M[M1]   = NUB[n][11+6*j  :11+6*j+3]   # 月
                    M[M1+1] = NUB[n][11+6*j+3:11+6*j+6]   # 日

                    # 通し日の算出 M2
                    M2 = NDATE(M[M1],M[M1+1]) 
                    
                    # M(193)=1/1、M(558)=12/31 特別日なら 1
                    M[192+M2] = 1   

        # 特別日の日数 M(170)
        M[170] = N 

    elif KEY == "SEAS":

        # CALL DCHECK(QD,894,NERR)

        for i in range(0,12):
            L1 = 3*i + 11
            M[980+i+1] = int(NUB[line][L1:L1+3])

    elif KEY == "OPCO":

        # CALL DCHECK(QD,1131,NERR)

        # 名称の数値化と起点（初期値145）の検索
        (NNAM,LC,LD) = RETRIV(145,NUB[line][5:9],M)

        if LD != 0:
            print(f"OPCO: " + {NUB[line][5:9]})
            raise Exception("LDが0以外になります")
    
        print(f"OPCOの起点番号 LC: {LC}")
        print(f"OPCOの行番号 L: {L}")

        M[int(LC)] = L
        M[int(L)] = LD

        # OPCO名称 M(起点＋1)
        M[L+1] = NNAM
        
        # 外気導入開始時刻（デフォルト値は0=終日導入しない）  M(L+164)  
        if NUB[line][11:14] == "   ":
            M[L+164] = 0
        else:
            M[L+164] = float(NUB[line][11:14])

        # 運転終了時間（スケジュール1） M1
        if NUB[line][14:17] == "   ":
            M1 = -1
        else:
            M1 = int(NUB[line][14:17])

        # ! 運転終了時間（スケジュール2） M2
        if NUB[line][17:20] == "   ":
            M2 = -1
        else:
            M2 = int(NUB[line][17:20])

        # ! 外気導入量（デフォルト値は 0）  X(L+165)
        if NUB[line][74:80] == "   ":
            X[L+165] = 0
        else:
            X[L+165] = float(NUB[line][74:80])  
    
        # ! DB上限、下限、RH上限、下限、予熱時間 （夏期） のデフォルト値
        if NUB[line][26:29] == "   ":
            NUB[line][26:29] = 26

        if NUB[line][29:32] == "   ":
            NUB[line][29:32] = 26

        if NUB[line][32:35] == "   ":
            NUB[line][32:35] = 50

        if NUB[line][35:38] == "   ":
            NUB[line][35:38] = 50

        if NUB[line][38:41] == "   ":
            NUB[line][38:41] = 1

        # ! DB上限、下限、RH上限、下限、予熱時間 （冬期） のデフォルト値
        if NUB[line][44:47] == "   ":
            NUB[line][44:47] = 22

        if NUB[line][47:50] == "   ":
            NUB[line][47:50] = 22

        if NUB[line][50:53] == "   ":
            NUB[line][50:53] = 40

        if NUB[line][53:56] == "   ":
            NUB[line][53:56] = 40

        if NUB[line][56:59] == "   ":
            NUB[line][56:59] = 2

        # ! DB上限、下限、RH上限、下限、予熱時間 （中間期） のデフォルト値
        # IF(QD(63:65).EQ.'   ') QD(63:65)='24.'
        # IF(QD(66:68).EQ.'   ') QD(66:68)='24.'
        # IF(QD(69:71).EQ.'   ') QD(69:71)='50.'
        # IF(QD(72:74).EQ.'   ') QD(72:74)='50.'

        if NUB[line][62:65] == "   ":
            NUB[line][62:65] = 24

        if NUB[line][65:68] == "   ":
            NUB[line][65:68] = 24

        if NUB[line][68:71] == "   ":
            NUB[line][68:71] = 50

        if NUB[line][71:74] == "   ":
            NUB[line][71:74] = 50


        # 夏期、冬期、中間期のループ
        for II in [1,2,3]:

            IWK=26+(II-1)*18
            L1=L+1+(II-1)*4

            print(f"OPCO II: {II}")
            print(f"OPCO IWK: {IWK}")
            print(f"OPCO L1: {L1}")

            # DB上限 X(L+2), X(L+6), X(L+10)
            # DB下限 X(L+3), X(L+7), X(L+11) 
            # RH上限 X(L+4), X(L+8), X(L+12)
            # RH下限 X(L+5), X(L+9), X(L+13)
            X[L1+1] = NUB[line][IWK:IWK+3]
            X[L1+2] = NUB[line][IWK+3:IWK+6]
            X[L1+3] = NUB[line][IWK+6:IWK+9]
            X[L1+4] = NUB[line][IWK+9:IWK+12]

            if X[L1+1] < X[L1+2]:
                raise Exception("室温の下限値が上限値を超えています")
            if X[L1+3] < X[L1+4]:
                raise Exception("湿度の下限値が上限値を超えています")

            # 最大気温、最大相対湿度から最大絶対湿度を求める
            X[L1+3] = SATX( X[L1+1]) * X[L1+3] /100
            X[L1+4] = SATX( X[L1+2]) * X[L1+4] /100
            
            # 予熱時間を読み込む    ：newHASPのコメントが間違っている？
            if II <= 2:
                M[L+165+II] = NUB[line][IWK+12:IWK+15]
                print(f"OPCO L+165+II: {L+165+II}")

            # 運転開始時刻
            IWK=23+(II-1)*18
            (NNAM,LC,LD) = RETRIV(147,NUB[line][IWK:IWK+3]+" ",M)

            I=L+14+(II-1)*50    # スケジュール2種類（24時間×2=48）
            print(f"OPCO I: {I}")

            if LD != LC:  # 該当するOSCHデータが見つからない場合

                if NUB[line][IWK:IWK+3] != "   ":
                    # 運転開始時刻 M3
                    M3 = int(NUB[line][IWK:IWK+3])
                    # 運転開始時刻 X(L+14)
                    X[I+M3] = 1.
                    X[I+25+M3] = 1.

                if M1 != -1:
                    # 運転終了時刻（スケジュール1）
                    X[I+M1] = -1
                if M2 != -1:
                    # 運転終了時刻（スケジュール2）
                    X[I+25+M2] = -1

            else:   # 該当するOSCHデータが見つかった場合はそちらのスケジュールに従う

                for M4 in [1,2]:  # スケジュール1か2
                    for M5 in [1,2,3,4,5]:  # 各スケジュールにおいて、5セットの開始時刻と終了時刻を指定可能
                        L1=LC+1+(M4-1)*10+(M5-1)*2
                        if M[L1+1] != -1:
                            X[I+(M4-1)*25+M(L1+1)] = 1.
                        if M[L1+2] != -1:
                            X[I+(M4-1)*25+M(L1+2)] = -1.

        L=L+168

    elif KEY == "OSCH":
    
        # OSCH名称の数値化と起点の検索 （3文字であるため空白を入れて4文字に）
        (NNAM,LC,LD) = RETRIV(147,NUB[line][5:8]+" ",M)

        if LD != 0:
            print(f"OSCH: " + {NUB[line][5:8]})
            raise Exception("LDが0以外になります")

        M[int(LC)] = L
        M[int(L)] = LD

        # WCON名称 M(起点＋1)
        M[L+1] = NNAM

        for II in [1,2]:  # スケジュール1か2
            for I in [1,2,3,4,5]: # 各スケジュールにおいて、5セットの開始時刻と終了時刻を指定可能

                IWK=11+(II-1)*36+(I-1)*6

                if NUB[line][IWK:IWK+3] == "   ":
                    NUB[line][IWK:IWK+3] = " -1"
                if NUB[line][IWK+3:IWK+6] == "   ":
                    NUB[line][IWK+3:IWK+6] = " -1"

                # 開始時刻
                M[L+1+(II-1)*10+(I-1)*2+1] = int(NUB[line][IWK:IWK+3])
                # 終了時刻
                M[L+1+(II-1)*10+(I-1)*2+2] = int(NUB[line][IWK+3:IWK+6])

    
        L=L+22

    elif KEY == "OAHU":

        # CALL DCHECK(QD,1370,NERR)

        # 名称の数値化と起点（初期値98）の検索
        (NNAM,LC,LD) = RETRIV(98,NUB[line][5:9],M)

        if LD != 0:
            print(f"OAHU: " + {NUB[line][5:9]})
            raise Exception("LDが0以外になります")
        
        M[int(LC)] = L
        M[int(L)] = LD

        # WCON名称 M(起点＋1)
        M[L+1] = NNAM

        # 全熱交換効率[%] X(L+2)
        if NUB[line][11:14] == "   ":
            NUB[line][11:14] = "  0"
        X[L+2] = float(NUB[line][11:14])
        X[L+2] = X[L+2]*0.01

        for II in [1,2,3]:  # 季節ループ
            
            IWK=14+(II-1)*18
            L1=L+2+(II-1)*9

            # 全熱交換器 熱回収用排気条件（温度、湿度）
            if NUB[line][IWK:IWK+3] == "   ":
                if NUB[line][IWK+3:IWK+6] != "   ":
                    raise Exception("OAHU エラー")
                M[L1+1] = 0 
            else:
                # 熱回収用排気条件（温度)
                X[L1+2] = float(NUB[line][IWK:IWK+3])

                if NUB[line][IWK+3:IWK+6] == "   ":
                    M[L1+1] = 1   # 温度のみ
                else:
                    M[L1+1] = 2   # 温度と湿度の指定
                    X[L1+3] = float(NUB[line][IWK+3:IWK+6])
                    X[L1+3] = SATX(X[L1+2])*X[L1+3]*0.01

            # 外調機 （DB上限、下限、RH上限、下限）
            if I in [1,2]:  # 上下限ループ

                I1=IWK+6+(I-1)*3

                if NUB[line][I1:I1+3] == "   ":
                    if NUB[line][I1+6:I1+9] != "   ":     # これあっているか？？？[IWK+3:IWK+6]では？
                        raise Exception("OAHU エラー")
                    M[L1+3+I] = 0
                else:
                    # 上限の読み込み
                    X[L1+5+I] = float(NUB[line][I1:I1+3])
                    if NUB[line][I1+6:I1+9] != "   ":
                        M[L1+3+I]=1
                    else:
                        M[L1+3+I]=2
                        # 下限の読み込み
                        X[L1+7+I] = NUB[line][I1+6:I1+9]
                        X[L1+7+I] = SATX(X[L1+5+I])*X(L1+7+I)*0.01

        # Lの更新 （183個 間隔）
        L=L+183

print("---建物データの読み込み終了---")

NRM = 0
IZ = 1
LCGB = L   # 現在のグループの先頭スペースのSPACデータポインタ(L)
TCM = GVECTR("INIT",NL,MT,TH,HT,NUBW)

for line in range(bldg_end+1,len(NUB)):

    KEY = NUB[line][0:4]

    if KEY == "CMPL":

        print("---SPACデータの読み込み終了---")
        break

    elif KEY == "SPAC":

        # SPAC名
        QSP=NUB[line][5:9]
        (NNAM,LC,LD) = RETRIV(106,QSP,M)

        print(f"SPAC: 室名{QSP}")

        if LD != 0:
            print(f"SPAC: " + {NUB[line][5:9]})
            raise Exception("LDが0以外になります")
        
        M[int(LC)] = L
        M[int(L)] = LD
        NRM=NRM+1
        # WCON名称 M(起点＋1)
        M[L+1] = NNAM

        # WSCH 名
        (NNAM,LC,LD) = RETRIV(108,NUB[line][9:13],M)
        if LD != LC:
            print(f"SPAC: " + {NUB[line][9:13]})
            raise Exception("LDがLCと異なります")
        else:
            M[L+34] = LC+1
        
        M[L+35] = 0
        M[L+47] = 0
        M[L+51] = 0
        M[L+55] = 0    # SOPCデータを指定しないスペースはこの値のままとなる
        M[L+60] = 0    # 1時間前から現時刻までの運転状態（初期値）
        M[L+61] = 0    # 前日からの外気導入継続状態（初期値）
    
        IFURS = 0      # 0以外の家具顕熱容量が指定されたかどうか

        for II in [1,2]:
            L1 = L+86+(II-1)*7
            X[L1+1] = REFWD[II]  # 基準温湿度
            X[L1+6] = REFWD[II]  # 基準温湿度
            for I in [2,3,4,5]:
                X[L1+I] = 0.0
            M[L1+7] = 9
        
        # 床面積（関数ARITHは実装しない）
        X[L+2] = float(NUB[line][41:])
        X[L+5] = float(NUB[line][14:20])  # 地上高[m]
        X[L+3] = float(NUB[line][20:26])  # 階高[m]
        X[L+4] = float(NUB[line][26:32])  # 天井高[m]

        X[L+6] = 0.004*(X[L+5]-X[153]/2.)
        X[L+7] = math.sqrt(X[L+5]/25.)

        if X[L+7] < 1:
            X[L+7] = 1

        if NUB[line][35:38] == "   ":
            NUB[line][35:38] = "  3"
        
        if NUB[line][32:35] == "   ":   # エラーが出るので処理を追加
            M[L+43] = 0
        else:
            M[L+43] = int(NUB[line][32:35])
            
        X[L+44] = NUB[line][35:38]

        # 昼光利用 (なし：0、あり：1/2設計照度 = LIGHを読み込んだ時に入力）
        M1 = M[L+43]
        if M1 == 0:
            M1 = 1

        X[L+46] = ROH[M1-1]/(X[L+2]*(1.-ROH[M1-1]*ROL[M1-1]))
        X[L+45] = ROL[M1-1]*X[L+46]

        LL=L
        L=L+LSZSPC[0]

        for J in [0,1,2,3,4,5,6,7,8,9]:
            GAS[J]=0.
            GRM[J]=0.
            GRL[J]=0.
    
        ARM = 0

        # SPACの終了行を探す。
        spec_end = 0
        for line_ex in range(line, len(NUB)):
            if len(NUB[line_ex]) == 1:  # 空白行を探す
                spec_end = line_ex
                break
        
        for line_ex in range(line+1, spec_end+1):

            KEY_SPAC = NUB[line_ex][0:4]
            print(f"KEY_SPAC: {KEY_SPAC}")

            if KEY_SPAC == "OWAL":

                M[L] = 1
                A = float(NUB[line_ex][41:])
                ARM=ARM+A

                # WCONを検索
                (NNAM,LC,LD) = RETRIV(102,NUB[line_ex][5:9],M)
                if LD != LC:
                    raise Exception("LDがLCと異なります")
                
                print(f"WCON LC: {LC}")

                # 層の構成数
                NL=M[int(LC+2)]

                # 蒸発比[%]
                if NUB[line_ex][20:23] == '   ':
                    L1 = 0       
                else:
                    L1 = int(NUB[line_ex][20:23])

                # 植栽と土壌の間の熱抵抗[m2K/W]
                if NUB[line_ex][23:29] == '      ':
                    W2 = 0.2      
                else:
                    W2 = float(NUB[line_ex][23:29])

                W2=W2/0.86   # [m2K/W] --> [m2hdeg/kcal]

                if L1 > 0:
                    W1 = 0.60*60*L1*0.01
                    HOX = HO+W1*1.0   # 飽和絶対湿度の温度変化に対する微分を1.0[g/kg(DA)/deg]とする
                    X[L+15] = W1/HOX
                    NL = NL+1
                    MT[NL] = -1   # 純熱抵抗の意味
                    TH[NL] = W2   # 熱抵抗
                else:
                    HOX=HO
                    X[L+15]=0.0

                for I in range(0,int(M[int(LC+2)])):
                    L1 = LC+2*(I)+3
                    # 建材番号
                    MT[I]=M[int(L1)]
                    # 長さ
                    TH[I]=X[int(L1+1)]

                # print("---TCM---")
                # print(TCM)

                GTR,GAD = GVECTR('OWAL',NL,MT,TH,HT,HOX,TCM)

                # print("---NL---")
                # print(NL)
                # print("---MT---")
                # print(MT)
                # print("---TH---")
                # print(TH)
                # print("---HT---")
                # print(HT)
                # print("---HOX---")
                # print(HOX)

                # print("---GTR---")
                # print(GTR)
                # print("---GAD---")
                # print(GAD)

                for J in range(0,9+1):
                    GRM[J]=GRM[J]+A*GAD[J]

                P = CPARAM(2,GTR)

                X[L+2]=A*P[1]
                X[L+3]=A*P[3]
                X[L+4]=P[5]
                X[L+5]=A*P[6]
                X[L+6]=P[8]

                # EXPSを検索
                (NNAM,LC,LD) = RETRIV(100,NUB[line_ex][9:13],M)
                if LD != LC:
                    raise Exception("LDがLCと異なります")

                print(f"EXPS LC: {LC}")
                print(f"EXPS LL: {LL}")

                M[L+1]=int(LC)
                V1=X[int(LC)+26]
                V2=X[int(LC)+11]
                V4=X[int(LC)+27]
                V3=0.0

                if X[int(LC)+12] > 0:
                    W = math.sqrt(X[int(LC)+12]**2 + (X[int(LC)+13]-X[int(LL)+5])**2)
                    W = (X[int(LC)+6]*X[int(LC)+12]-X[int(LC)+5]*(X[int(LC)+13]-X[int(LL)+5]))/W
                    V3 = (1.0 - W)/2.0

                if (V2 > V3):
                    if V1+V2 > 1:
                        U1 = 0.0
                        U2 = (1.0-V1-V3)*(1.0-V4)
                    else:
                        U1=(1.0-V1-V2)*(1.0-V4)
                        U2=(V2-V3)*(1.0-V4)
                else:
                    if V1+V3 > 1:
                        U1=0.0
                        U2=0.0
                    else:
                        U1=(1.0-V1-V3)*(1.0-V4)
                        U2=0.0

                # 日射吸収率
                if NUB[line_ex][14:17] == "   ":
                    W1 = 80
                else:
                    W1 = float(NUB[line_ex][14:17])
    
                # 長波放射率
                if NUB[line_ex][17:20] == "   ":
                    W2 = 90
                else:
                    W2 = float(NUB[line_ex][17:20])

                X[L+9]  = 0.01*W1/HOX
                X[L+10] = X[154]*U2*X[L+9]
                X[L+11] = (U1+X[154]*U2)*X[L+9]
                X[L+12] = 0.01*W2*U1/HOX

                if X[int(LC)+12] != 0.0:
                    W = (X[int(LC)+13]-X[int(LL)+5])/X[int(LC)+12]
                    X[L+13] = W*X[int(LC)+3]
                    X[L+14] = W*X[int(LC)+4]

                L=L+LSZSPC[1]
    

            elif KEY_SPAC == "IWAL":

                # A：面積[m2]
                A = float(NUB[line_ex][41:])
                ARM=ARM+A

                # WCONを検索
                (NNAM,LC,LD) = RETRIV(102,NUB[line_ex][5:9],M)
                if LD != LC:
                    raise Exception("LDがLCと異なります")

                # 層の構成数
                NL=M[int(LC)+2]

                for I in range(0,int(M[int(LC+2)])):
                    L1 = LC+2*(I)+3
                    # 建材番号
                    MT[I]=M[int(L1)]
                    # 長さ
                    TH[I]=X[int(L1+1)]
                
                # print(NUB[line_ex][5:9])
                # print("---NL---")
                # print(NL)
                # print("---MT---")
                # print(MT)
                # print("---TH---")
                # print(TH)
                
                GTR,GAD = GVECTR('IWAL',NL,MT,TH,HT,HT,TCM)

                if NUB[line_ex][11:14] != "   ":
                    I = int(NUB[line_ex][11:14])  # 隣室モード
                else:
                    I = 0

                if NUB[line_ex][14:20] != "      ":
                    W1 = float(NUB[line_ex][14:20]) # 隣室条件α
                else:
                    W1 = 0

                if I == 0 and W1 == 0:  # 隣室設定なし

                    for J in range(0,10):
                        GRM[J] = GRM[J] + A*(GAD[J]-GTR[J])
                    # ここで処理終了
                        
                else:
                    M[L]=2
                    M[L+1]=I
                    for J in range(0,10):
                        if M[L+1] == 0:
                            GRM[J] = GRM[J] + A*(GAD[J]-(1.0-W1)*GTR[J])
                        else:
                            GRM[J] = GRM[J] + A*GAD[J]

                    if M[L+1] == 3:

                        # 隣室SPAC名
                        (NNAM,LC,LD) = RETRIV(102,NUB[line_ex][20:24],M) # NNAMへの変換機能のみ利用
                        M[L+2] = NNAM

                        for J in range(0,10):
                            X[L+16+J] = A*GTR[J]

                    else:
                        P = CPARAM(2,GTR)
                        X[L+3] = A*P[1]
                        X[L+4] = A*P[2]
                        X[L+5] = A*P[3]
                        X[L+6] = A*P[4]
                        X[L+7] = P[5]
                        X[L+8] = A*P[6]
                        X[L+9] = A*P[7]
                        X[L+10] = P[8]
                            

                    X[L+11] = 0.0
                    X[L+12] = 0.0
                    X[L+13] = 0.0
                    X[L+14] = 0.0
                    X[L+15] = W1

                    L=L+LSZSPC[2]

            elif KEY_SPAC == "GWAL":
                
                A = float(NUB[line_ex][41:])
                ARM=ARM+A
            
                # WCONを検索
                (NNAM,LC,LD) = RETRIV(102,NUB[line_ex][5:9],M)
                if LD != LC:
                    raise Exception("LDがLCと異なります")

                # 層の構成数
                NL = M[int(LC)+2]
                W = 2.0

                for I in range(0,int(M[int(LC+2)])-1):
                    L1 = LC+2*(I)+3
                    # 建材番号
                    MT[I]=M[int(L1)]
                    # 長さ
                    TH[I]=X[int(L1+1)]
                    W = W - TH[I]

                MT[NL] = M[int(M[int(LC+2)])-1]
                TH[NL] = W

                # print(NUB[line_ex][5:9])
                # print("---NL---")
                # print(NL)
                # print("---MT---")
                # print(MT)
                # print("---TH---")
                # print(TH)
                
                GTR,GAD = GVECTR('GWAL',NL,MT,TH,HT,0,TCM)

                for J in range(0,10):
                    GRM[J] = GRM[J] + A*(GAD[J]-GTR[J])

            elif KEY_SPAC == "BECO":
                
                # 部材延長[m]
                A = float(NUB[line_ex][41:])
                
                # WCONを検索
                (NNAM,LC,LD) = RETRIV(102,NUB[line_ex][5:9],M)
                if LD != LC:
                    raise Exception("LDがLCと異なります")

                U1 = float(NUB[line_ex][26:32])  # 断面形状長辺[m]
                U2 = float(NUB[line_ex][32:38])  # 断面形状短辺[m]

                U0 = 2.0*(U1+U2)
                A = U0*A
                ARM = ARM+A
            
                NL=M[LC+2]

                for I in range(0,int(M[int(LC+2)])-1):
                    L1 = LC+2*(I)+3
                    II = 2*NL-I
                    W = U1*U2
                    U1 = U1-2.*X[L1+2]
                    U2 = U2-2.*X[L1+2]
                    MT[I] = M[L1+1]
                    TH[I] = (W-U1*U2)/U0
                    MT[II] = MT[I]
                    TH[II] =TH[I]
                    
                MT[NL] = M(int(M[int(LC+2)])-1)
                TH[NL] = U1*U2/U0

                GTR,GAD = GVECTR('BECO',2*NL-1,MT,TH,HT,HT,TCM)

                for J in range(0,10):
                    GRM[J] = GRM[J] + A*(GAD[J]-GTR[J])


            elif KEY_SPAC == "WNDW":

                # WNDWデータを示すID
                M[L] = 3

                # 窓面積
                A = float(NUB[line_ex][41:])
                ARM += A

                if (NUB[line_ex][5:9] == "    ") or (NUB[line_ex][5:9] == "SNGL"):                    
                    ITB=1   # テーブル番号
                    IAP=0   # IAP=0:普通,1:AFW,2:PPW
                elif (NUB[line_ex][7:9] == "06"):
                    ITB=2
                elif (NUB[line_ex][7:9] == "12"):
                    ITB=3
                elif (NUB[line_ex][5:9] == "DLBT"):
                    ITB=4
                    IAP=0
                elif (NUB[line_ex][5:9] == "AFWN"):
                    ITB=4
                    IAP=1
                else:
                    raise Exception("WNDWの窓種グループが不正です")

                if ITB == 2 or ITB == 3:
                    if (NUB[line_ex][5:7] == "DL"):
                        IAP=0
                    elif (NUB[line_ex][5:7] == "PP"):
                        IAP=2
                    else:
                        raise Exception("WNDWの窓種グループが不正です")
                    
                M1 = NUB[line_ex][14:17]  # 品種番号
                if NUB[line_ex][17:20] == "   ":  # ブラインド番号
                    M2 = 0
                else:
                    M2 = NUB[line_ex][17:20]

                if abs(GLK[int(M1),0,int(ITB)]-9.999) < 0.001:
                    raise Exception("WNDWの設定が不正です")
                if abs(GLC[int(M1),0,int(ITB)]-9.999) < 0.001:
                    raise Exception("WNDWの設定が不正です")
                if abs(GLR[int(M1),0,int(ITB)]-9.999) < 0.001:
                    raise Exception("WNDWの設定が不正です")          
                if abs(GLK[int(M1),int(M2),int(ITB)]-9.999) < 0.001:
                    raise Exception("WNDWの設定が不正です")
                if abs(GLC[int(M1),int(M2),int(ITB)]-9.999) < 0.001:
                    raise Exception("WNDWの設定が不正です")
                if abs(GLR[int(M1),int(M2),int(ITB)]-9.999) < 0.001:
                    raise Exception("WNDWの設定が不正です")       

                # ブラインド開時の Kw×Aw kcal/h℃]
                X[L+2] = A * GLK[int(M1),0,int(ITB)]
                # ブラインド開時の SCC× Aw [m2]
                X[L+3] = A * GLC[int(M1),0,int(ITB)]
                # ブラインド開時の SCR ×Aw [m2]
                X[L+4] = A * GLR[int(M1),0,int(ITB)]
                # ブラインド閉時の Kw×Aw [kcal/h℃]
                X[L+5] = A * GLK[int(M1),int(M2),int(ITB)]
                # ブラインド閉時のSCC× Aw [m2]
                X[L+6] = A * GLC[int(M1),int(M2),int(ITB)]
                # ブラインド閉時のSCR Aw [m2]
                X[L+7] = A * GLR[int(M1),int(M2),int(ITB)]

                X[L+38] = IAP
                X[L+45] = GLKR[int(MGT[int(M1),int(ITB)])]

                if (abs(X[L+45]-9.999) < 0.001):
                    raise Exception("WNDWの設定が不正です") 
                
                if (ITB == 4) or (M2 == 0):  # 室内側にブラインドなし
                    X[L+46] = X[L+45]   # ブラインド開時のkLRと同じ
                else:
                    if abs(GLKRB-9.999) < 0.001:
                        raise Exception("WNDWの設定が不正です") 
                    X[L+46] = GLKRB

                if (IAP >= 1):  # # IAP=0:普通,1:AFW,2:PPW
                    
                    print("省略")

                    # # 窓通気量
                    # if NUB[line_ex][20:26] == "      ":
                    #     W1 = 0
                    # else:
                    #     W1 = float(NUB[line_ex][20:26])
                    #     W1=W1/3.6   # [m3/m2h]から[L/m2s]への変換

                    # # 窓排気率
                    # if NUB[line_ex][26:30] == "   ":
                    #     W2 = 40
                    # else:
                    #     W2 = float(NUB[line_ex][26:30])
                    #     W2 = W2*0.01

                    # for II in [1,2]:  # ΔSC,ΔUループ
                    #     for I in [1,2]:  # ブラインド開、閉ループ
                    #         I1=(IAP-1)*2+II
                    #         if (I == 1):
                    #             M3=0
                    #         else:
                    #             M3=M2
                    #
                    #         GLWK[I,II] = A*DLTGL(IAP,W1,W2, \
                    #             NVS(I1),GLD(1,0,0,I1),GLD(1,M3,MGT(M1,ITB)/10,I1),\
                    #             NVS(I1+2),GLD(1,0,0,I1+2),GLD(1,M3,MGT(M1,ITB)/10,I1+2))
                    #
                    #        DO 258 II=1,2   ! ΔSC,ΔUループ
                    #        DO 258 I=1,2    ! ブラインド開、閉ループ
                    #         I1=(IAP-1)*2+II
                    #         IF(I.EQ.1) THEN
                    #          M3=0
                    #         ELSE
                    #          M3=M2
                    #         END IF
                    #         IF(ABS(GLD(1,0,0,I1)-9.999).LT.0.001) CALL ERROR(53,NERR)
                    #         IF(ABS(GLD(1,0,0,I1+2)-9.999).LT.0.001) CALL ERROR(53,NERR)
                    #         IF(ABS(GLD(1,M3,MGT(M1,ITB)/10,I1)-9.999).LT.0.001)
                    #      -   CALL ERROR(53,NERR)
                    #         IF(ABS(GLD(1,M3,MGT(M1,ITB)/10,I1+2)-9.999).LT.0.001)
                    #      -   CALL ERROR(53,NERR)
                    #         GLWK(I,II)=A*DLTGL(IAP,W1,W2,
                    #      -         NVS(I1),GLD(1,0,0,I1),GLD(1,M3,MGT(M1,ITB)/10,I1),
                    #      -         NVS(I1+2),GLD(1,0,0,I1+2),GLD(1,M3,MGT(M1,ITB)/10,I1+2))
                    #         ! A*ΔSC, A*ΔU
                    #   258  CONTINUE
                    #        IF(IAP.EQ.1) THEN   ! AFW
                    #         X(L+41)=GLWK(1,1)*X(L+45)
                    #         X(L+44)=GLWK(2,1)*X(L+46)
                    #        ELSE   ! PPW
                    #         X(L+41)=0.0
                    #         IF(M2.EQ.0) THEN
                    #          X(L+44)=0.0
                    #         ELSE
                    #          I1=3
                    #          IF(ABS(GLKRBO-9.999).LT.0.001) CALL ERROR(53,NERR)
                    #          X(L+44)=(GLKRBO-GLKRB)*X(L+6)/(1-GLKRB)
                    #      -   +GLKRBO*A*DLTGL(IAP,W1,1.0,
                    #      -         NVS(I1),GLD(1,0,0,I1),GLD(1,M2,MGT(M1,ITB)/10,I1),
                    #      -         NVS(I1+2),GLD(1,0,0,I1+2),GLD(1,M2,MGT(M1,ITB)/10,I1+2))
                    #         END IF
                    #        END IF
                    #        DO 259 I=1,2   ! ブラインド開、閉ループ
                    #         X(L+39+(I-1)*3)=GLWK(I,2)
                    #         X(L+40+(I-1)*3)=GLWK(I,1)-X(L+41+(I-1)*3)
                    #   259  CONTINUE
                    #       END IF
                        

                # 限界日射取得[kcal/h]
                # X[158] BUIL 限界日射量 [kcal/m2h]
                X[L+8] = A * X[158]

                # X[L+5] ブラインド閉時の Kw×Aw [kcal/h℃]
                for J in range(0,10):
                    GRM[J] += X[L+5]

                # EXPSを検索
                (NNAM,LC,LD) = RETRIV(100,NUB[line_ex][9:13],M)
                if LD != LC:
                    raise Exception("LDがLCと異なります")
            
                # EXPSへのポインタ
                M[L+1] = int(LC)

                # ψ1 （外面から上庇を見る形態係数）
                V1 = X[int(LC)+26]
                # (1-cosβ)/2 （外面から地平以下を見る形態係数）
                V2 = X[int(LC)+11]
                # ψ2 （外面から上庇を見る形態係数）
                V4 = X[int(LC)+27]

                # X[int(LC)+12]  Da（隣棟距離[m]）
                # X[int(LC)+13]  Ha（隣棟高さ[m]）
                # X[int(LC)+ 5]  方位角のsin
                if (X[int(LC)+12] == 0):
                    V3=0.0
                else:
                    W  = math.sqrt(X[int(LC)+12]**2+(X[int(LC)+13]-X[int(LL)+5])**2)
                    W  = (X[int(LC)+6]*X[int(LC)+12]-X[int(LC)+5]*(X[int(LC)+13]-X[int(LL)+5]))/W
                    V3 = (1.-W)/2.

                if V2 > V3:
                    if V1+V2 > 1:
                        U1=0.0
                        U2=(1.-V1-V3)*(1.-V4)
                    else:
                        U1=(1.-V1-V2)*(1.-V4)
                        U2=(V2-V3)*(1.-V4)
                else:
                    if V1+V3 > 1:
                        U1=0.
                        U2=0.
                    else:
                        U1=(1.-V1-V3)*(1.-V4)
                        U2=0.
                
                # X[154]  BUIL 地物反射率[%]  
                # ρGψG・g (gは散乱日射に対する標準ガラスの日射取得率=0.808）[-]
                X[L+9]  =0.808*X[154]*U2

                # ψs・g [-]
                X[L+11] =0.808*U1

                # (ψs＋ρGψG)・g [-]
                X[L+10] =X[L+9]+X[L+11]

                # ψsεl ／αo [℃／(kcal/m2h)]
                X[L+12] =0.9*U1/HO

                # X[L+13] (Ha-Hf)sinα／Da [-]
                # X[L+14] (Ha-Hf)cosα／Da [-]
                if X[int(LC)+12] == 0:
                    X[L+13] = 0.
                    X[L+14] = 0.
                else:
                    W = (X[int(LC)+13]-X[int(LL)+5])/X[int(LC)+12]
                    X[L+13] = W*X[int(LC)+3]
                    X[L+14] = W*X[int(LC)+4]
                
                # X[int(LL)+43] 昼光利用の有無
                # X[L+15]  1/2×(1-x0/(x02+y2)**(1/2))×109 [lx/(kcal/m2h)]
                # X[L+16]  Awρ1ρ2／(Af・(1-ρ1ρ2))×109 [lx/(kcal/m2h)]
                # X[L+17]  Awρ2／(Af・(1-ρ1ρ2))×109 [lx/(kcal/m2h)]
                # X[L+18]  X(L+15)+0.5*X(L+16)+0.5*X(L+17) [lx/(kcal/m2h)]
                # X[L+19]  消灯面積率 x0W／Af　[-]
                if X[int(LL)+43] == 0:

                    for I in [15,16,17,18,19]:
                        X[L+I] = 0.0

                else:

                    U = NUB[line_ex][29:35]
                    W = NUB[line_ex][35:41]
                    V = A / W

                    if (U < 0.75): 
                        V = V + U - 0.75

                    V1 = 109.*0.5*(1.-X[int(LL)+44]/math.sqrt(X[int(LL)+44]**2+V**2))
                    V2 = 109.*A*X[int(LL)+45]
                    V3 = 109.*A*X[int(LL)+46]

                    X[L+15] = V1
                    X[L+16] = V2
                    X[L+17] = V3
                    X[L+18] = V1+(V2+V3)/2.
                    X[L+19] = X[int(LL)+44]*W/X[int(LL)+2]
                
                # スケジュールオプション(0:品種番号の物性値どおり,1:on時%指定,2:DSCH使用）
                # DSCHポインタ(L+1),
                # 空調on時割合 [-],
                for I in [0,1,2,3,4,5]:
                    M[L+20+I*3] = 0   # デフォルトで物性値は品種番号の物性値のとおり
            
                # 継続行の処理は省略
                if NUB[line_ex+1][0:4] == "+   ":
                    raise Exception("継続行の処理は省略")

                L=L+LSZSPC[3]

            elif KEY_SPAC == "INFL":

                M[L] = 4

                # EXPSを検索
                (NNAM,LC,LD) = RETRIV(100,NUB[line_ex][9:13],M)
                if LD != LC:
                    raise Exception("LDがLCと異なります")
                else:
                    M[L+1] = LC
                
                # 計算方法
                M[L+2] = int(NUB[line_ex][14:17])

                if (M[L+2] == 0):
                    # 隙間特性または換気回数
                    if NUB[line_ex][20:26] == "      ":
                        W = 5.0
                    else:
                        W = float(NUB[line_ex][20:26])

                    A = float(NUB[line_ex][41:])
                    X[L+3] = W*A

                else:
                    W = float(NUB[line_ex][20:26])
                    X[L+3] = W*X[int(LL)+2]*X[int(LL)+4]

                if len(NUB[line_ex]) <= 27 or NUB[line_ex][27:31] == "    ":

                    M[L+4]=0   # オリジナル換気量で一定、あるいは空調on・off時%の値を使用
                    if len(NUB[line_ex]) <= 27 or NUB[line_ex][32:35] == "   ":
                        X[L+6] = 1.0
                    else:
                        X[L+6] = float(NUB[line_ex][32:35])

                    if len(NUB[line_ex]) <= 27 or NUB[line_ex][35:38] == "   ":
                        X[L+7] = 1.0
                    else:
                        X[L+7] = float(NUB[line_ex][35:38])

                else:

                    M[L+4] = 1   # DSCH使用

                    # DSCHを検索
                    (NNAM,LC,LD) = RETRIV(104,NUB[line_ex][27:31],M)
                    if LD != LC:
                        raise Exception("LDがLCと異なります")
                    else:
                        M[L+5] = LC+1

                L=L+LSZSPC[4]

            elif KEY_SPAC == "LIGH":

                # DSCHを検索
                (NNAM,LC,LD) = RETRIV(104,NUB[line_ex][5:9],M)
                if LD != LC:
                    raise Exception("LDがLCと異なります")
                else:
                    # LL は SPACのポインタ
                    M[LL+35] = LC+1    # LIGHへのポインタを保存（+1しているのはDSCH名称を参照するため）

                # M1: 器具形式
                # 1 蛍光灯 埋め込み器具 <デフォルト>
                # 2 蛍光灯 直付け器具
                # 3 蛍光灯 吊り下げ器具
                # 4 白熱灯 埋め込み器具
                # 5 白熱灯 直付け器具
                # 6 白熱灯 吊り下げ器具
                if NUB[line_ex][14:17] == '   ':
                    M1 = 1       
                else:
                    M1 = int(NUB[line_ex][14:17])

                # W: 電気容量
                if NUB[line_ex][17:23] == '      ':
                    W = 20      
                else:
                    W = float(NUB[line_ex][17:23])

                # 電気容量の単位(=1: W/m2、=2: kW)
                if len(NUB[line_ex]) <= 25 or NUB[line_ex][23:26] == '   ':
                    M2 = 1       
                else:
                    M2 = int(NUB[line_ex][23:26])

                # 単位変換 kW へ
                if (M2 == 1):
                    # X[int(LL)+2]: スペースの床面積
                    W = 0.001 * W * X[int(LL)+2]
                
                # 蛍光灯の場合は15%相当を安定器損失として加算
                if (M1 <= 3):
                    W = 1.15 * W

                # kWからkcalへ
                W = 860.0 * W

                # 0:埋め込み器具、1:直付け器具、2:吊り下げ器具
                M1 =int( (M1-1)%3 )
                
                # 照明熱取得：P0 瞬時応答係数[kcal/h]
                X[int(LL)+36] = FL[M1][0] * W
                # 照明熱取得：P1 1項目の1ステップ後の応答係数[kcal/h]
                X[int(LL)+37] = FL[M1][1] * W
                # 照明熱取得：R1 1項目の公比[-]
                X[int(LL)+38] = FL[M1][2]
                # 照明熱取得 P2 2項目の1ステップ後の応答係数[kcal/h]
                X[int(LL)+39] = FL[M1][3] * W
                # 照明熱取得：R2 1項目の公比[-]
                X[int(LL)+40] = FL[M1][4]
            
                # X[int(LL)+43]  昼光利用 (なし：0、あり：1/2設計照度）
                if (X[int(LL)+43] != 0):
                    if NUB[line_ex][32:38] == '      ':
                        W = 700    
                    else:
                        W = int(NUB[line_ex][32:38])
                    X[int(LL)+43] = W/2.


            elif KEY_SPAC == "OCUP":

                # DSCHを検索
                (NNAM,LC,LD) = RETRIV(104,NUB[line_ex][5:9],M)
                if LD != LC:
                    raise Exception("LDがLCと異なります")
                else:
                    # OCUPスケジュールへのポインタ
                    M[LL+51] = LC+1

                # 作業指数（1～9）
                if NUB[line_ex][14:17] == '   ':
                    M1 = 3       
                else:
                    M1 = int(NUB[line_ex][14:17])

                # 人数
                if NUB[line_ex][17:23] == '      ':
                    W = 0.2       
                else:
                    W = float(NUB[line_ex][17:23])

                # 単位（1: 人/m2、 2:人）
                if len(NUB[line_ex]) <= 25 or NUB[line_ex][23:26] == '   ':
                    M2 = 1
                else:
                    M2 = int(NUB[line_ex][23:26])
                    
                # 単位変換 人
                if (M2 == 1):
                    # X[int(LL)+2] 床面積
                    W = W * X[int(LL)+2]

                X[int(LL)+52] = W * AM[M1-1][0]   # 全熱
                X[int(LL)+53] = W * AM[M1-1][1]   # 顕熱分
                X[int(LL)+54] = W * AM[M1-1][2]   # 単位温度上昇あたりの顕熱変化量

            elif KEY_SPAC == "HEAT":

                # DSCHを検索
                (NNAM,LC,LD) = RETRIV(104,NUB[line_ex][5:9],M)
                if LD != LC:
                    raise Exception("LDがLCと異なります")
                else:
                    M[LL+47] = LC+1

                # 冷却方式(1: 自然放熱、 2: 強制空冷)
                if NUB[line_ex][14:17] == '   ':
                    M1 = 1       
                else:
                    M1 = int(NUB[line_ex][14:17])

                # 単位 1: W/m2、 2: kW
                if NUB[line_ex][29:32] == '   ':
                    M2 = 1       
                else:
                    M2 = int(NUB[line_ex][29:32])

                W = float(NUB[line_ex][17:23])
                X[int(LL)+50] = float(NUB[line_ex][23:29])

                if (M2 == 1):
                    W = 0.001*W*X[int(LL)+2]
                    X[int(LL)+50] = 0.001*X[int(LL)+50]*X[int(LL)+2]

                W=860.*W
                X[int(LL)+50] = 860.*X[int(LL)+50]

                if (M1 == 1):
                    # 自然放熱（放射伝熱を伴う）
                    X[int(LL)+48] = HC*W/HT
                    X[int(LL)+49] = HR*W/HT
                else:
                    # 強制空冷（機器内部からファン等で強制的に排熱。対流伝熱で瞬時に負荷に）
                    X[int(LL)+48] = W
                    X[int(LL)+49] = 0.


            elif KEY_SPAC == "FURN":

                # 顕熱容量 (kJ/m2K)
                if len(NUB[line_ex]) < 15 or [line_ex][14:20] == '      ':
                    W1 = 40   
                else:
                    W1 = float(NUB[line_ex][14:20])

                # 潜熱容量 (kJ/m2K)
                if len(NUB[line_ex]) < 21 or NUB[line_ex][20:26] == '      ':
                    W2 = 80
                else:
                    W2 = float(NUB[line_ex][20:26])

                W1 = W1 * X[int(LL)+2] / 4.186
                W2 = W2 * X[int(LL)+2] / 4.186

                if (abs(W1)>0.1):
                    IFURS=1

                GTR,GAD = GVECTR('FURS',0,MT,TH,HT,W1,TCM)
                for J in range(0,10):
                    GAS[J] = GAS[J] + GAD[J]

                GTR,GAD = GVECTR('FURN',0,MT,TH,HT,W2,TCM)
                for J in range(0,10):
                    GRL[J] = GRL[J] + GAD[J]


            elif KEY_SPAC == "SOPC":

                # OPCOを検索
                (NNAM,LC,LD) = RETRIV(145,NUB[line_ex][5:9],M)
                if LD != LC:
                    raise Exception("LDがLCと異なります")
                else:
                    M[int(LL)+55] = LC

                X[int(LL+56+0)] = float(NUB[line_ex][11:17])  # 除去熱量（顕熱）
                X[int(LL+56+1)] = float(NUB[line_ex][17:23])  # 除去熱量（潜熱）
                X[int(LL+56+2)] = float(NUB[line_ex][23:29])  # 供給熱量（顕熱）
                X[int(LL+56+3)] = float(NUB[line_ex][29:35])  # 供給熱量（潜熱）

                for I in [0,1,2,3]:
                    X[int(LL)+56+I] = 0.860 * X[int(LL)+56+I]

                # OAHUの検索
                if NUB[line_ex][37:41] != "    ":
                    # OPCOを検索
                    (NNAM,LC,LD) = RETRIV(98,NUB[line_ex][37:41],M)
                    if LD != LC:
                        raise Exception("LDがLCと異なります")
                    else:
                        M[int(LL)+202] = LC

                for II in [0,1,2]:  # 季節ループ

                    QWK = 'CDHS' 

                    # 装置容量の設定                                                        
                    for I in [0,1,2,3]:  # CDHSループ    

                        J=41+4*II+I+1

                        if (NUB[line_ex][J-1] == ' ') or (NUB[line_ex][J-1] == QWK[I]):
                            X[ LL+203+4*II+I ] = X[ LL+56+I ]            
                        elif (NUB[line_ex][J-1] == '-'): 

                            s = list(QWK)
                            s[I] = "-"
                            QWK = "".join(s)

                            X[ LL+203+4*II+I]  = 0.0                                             

                    # 人体発熱顕熱比率算出用温度                                            
                    if (MCNTL[32] == 0):                                                
                        X[ LL+215+II ] = X[155]                                                  
                    elif (QWK[0] == 'C') and (QWK[2] == '-') :                  
                        X[ LL+215+II ] = X[ M(LL+55)+2+4*II ]           
                    elif (QWK[0] == '-') and (QWK[2] == 'H') :                 
                        X[ LL+215+II ] = X[ M(LL+55)+3+4*II ]                                      
                    else:                                                                   
                        X[ LL+215+II ] = 0.5*(X[ M(LL+55)+2+4*II ] + X[ M(LL+55)+3+4*II ] )        

            else:

                # ***          2.20. SPACE WEIGHTING FACTOR ****************************
                                                
                # print(f"室名： {QSP}")

                M[L] = 5
                X[LL+63] = ARM

                for J in range(0,10):
                    G[J] = (HC*ARM-FC*GRM[J])/(HC*ARM+FR*GRM[J])

                P = CPARAM(1,G)
                X[LL+8]  = P[1]
                X[LL+9]  = P[3]
                X[LL+10] = P[5]
            
                for J in range(0,10):
                    G[J] = HC*ARM*GRM[J]/(HC*ARM+FR*GRM[J])+GAS[J]
                
                P = CPARAM(2,G)
                for I in range(1,9):
                    X[LL+I+14] = P[I]

                P = CPARAM(1,GRL)
                for I in range(1,6):
                    X[LL+I+24] = P[I]

                L2=LL+LSZSPC[0]

                flag_RTVADJ = True
                while flag_RTVADJ:

                    (L2, JZ, ISTAT2) = RTVADJ(LSZSPC, L2, M)

                    if (ISTAT2 == 1) or (ISTAT2 == -2):
                        for J in range(0,10):
                            G[J] = HC*ARM/(HC*ARM+FR*GRM[J])*X[int(L2)+16+J]
                        P = CPARAM(2,G)
                        for J in range(1,9):
                            X[int(L2)+I+2]=P[I]
                        L2 = L2 + LSZSPC(M[int(L2)])
                    else:
                        flag_RTVADJ = False
                
                    if (ISTAT2 == 1) or (ISTAT2 == -2):
                        for J in range(0,10):
                            G[J] = HC*ARM/(HC*ARM+FR*GRM[J])*X(L2+16+J)
                        P = CPARAM(2,G)
                        for I in range(0,9):
                            X[int(L2)+I+2] = P[I]
                        L2=L2+LSZSPC[int(M[L2])]

                if IFURS >= 1:
                    P = CPARAM(2,GAS)   # 家具の蓄熱応答係数のみ計算（簡易MRT計算用）
                    for I in range(1,9):
                        X[LL+63+I] = P[I]
                        
                M[LL+101] = IZ

                if KEY == ":   ":  # スペースの結合(グループの継続)が指示された場合
                    raise Exception('まだ作成していません')

                elif KEY == "CFLW":  # 空気移動量が指定された場合(現在のグループのうち最後のスペースのはず)
                    raise Exception('まだ作成していません')

                else:  
                    
                    if (IZ >= 2):
                        # IWAL(adjacent)の参照error check
                        L1=LCGB
                        for I in range(1,IZ+1):
                            L2=L1+LSZSPC[0]

                            flag_RTVADJ_2 = True
                            while flag_RTVADJ_2:
                                (L2, JZ, ISTAT2) = RTVADJ(LSZSPC, L2, M)
                                if ISTAT2 == -2:
                                    raise Exception("隣室条件が不正です")
                                elif ISTAT2 == -1:
                                    raise Exception("隣室条件が不正です")
                                elif (ISTAT2==0) and (I != IZ):
                                    L1=M[int(L1)]
                                    flag_RTVADJ_2 = False
                                elif (ISTAT2==1):
                                    L2=L2+LSZSPC(int(M[L2]))

                    # 次のグループのためのセット
                    IZ = 1
                    LCGB=L+1

                L=L+1
                # print(f"L: {L}")
                # print(f"LCGB: {LCGB}")


# ***          2.21. OUTPUT 1 (JOB NAME AND SPACE WF) ******************

LL = int(M[106])  # SPACポインタ
NRM = 0
result = {}

while (LL != 0):

    NRM += 1

    WF[1,1] = X[LL+15]
    WF[1,2] = X[LL+16]
    U1 = X[LL+17]
    V1 = X[LL+18]
    R1 = X[LL+19]
    U2 = X[LL+20]
    V2 = X[LL+21]
    R2 = X[LL+22]

    for J in range(2,9):
        WF[J,1] = U1+U2
        WF[J,2] = V1+V2
        U1 = U1*R1
        V1 = V1*R1
        U2 = U2*R2
        V2 = V2*R2

    WF[9,1] = (U1/(1.-R1)+U2/(1.-R2))/(U1/((1.-R1)*R1)+U2/((1.-R2)*R2))
    WF[9,2] = (V1/(1.-R1)+V2/(1.-R2))/(V1/((1.-R1)*R1)+V2/((1.-R2)*R2))

    WF[1,3] = X[LL+25]
    WF[1,4] = X[LL+26]
    U1 = X[LL+27]
    V1 = X[LL+28]

    for J in range(2,9):
        WF[J,3] = U1
        WF[J,4] = V1
        U1 = U1 * X[LL+29]
        V1 = V1 * X[LL+29]
        
    WF[9,3] = X[LL+29]
    WF[9,4] = X[LL+29]
    
    QSP = NAME(M[LL + 1])

    # 結果格納用のデータを作成
    result[str(LL)] = {
                "name": QSP,
                "calc_data": [],
            }

    # 次のスペースに移動
    LL = int(M[LL])


#-----------------------------------------------------------------------
# 3. MAIN PROCESS
#-----------------------------------------------------------------------
        
# 3.1. DATING AND JOB CONTROL

# 気象データをRewindした回数
ICYCL = 1

# mprint("MCNTL[5]")   # データ形式 0:標準年気象データ（周期データ）、1:ピークデータ（周期データ）、2:実データ （2で割ることで、=0:周期データ, =1:実在データ）
# mprint("MCNTL[31]")  # データカラム数


# 気象データを読み込む
NUW_day = 0  # 気象データを読み込んだ日数

NUW_flag = True
while NUW_flag:

    # 気象データの読み込み
    ICYCL,WDND,IDND,ISTAT,NUW_day = INWD(NUW, int(MCNTL[5]/2), int(MCNTL[31]), int(ICYCL),NUW_day)

    # 周期の処理
    if MCNTL[5] != 1:     # 気象データ形式 = ピークデータ以外

        # 読み込んだ気象データの日付
        KDYF = NDATF(IDND[1,1],IDND[1,2],IDND[1,3])   # 1899/12/31を1とした通算日数

        if (MCNTL[5] == 0 and ICYCL == 2) or (MCNTL[5] == 2 and KDYF > MCNTL[19]):
            # 気象データ形式が「標準年気象データ」で、繰り返し2回目
            # 気象データ形式が「実データ」で、読み込んだ日付が助走開始日より後
            raise Exception("エラー")
        elif (MCNTL[5] == 0 and KDYF != MCNTL[19]) or (MCNTL[5] == 2 and KDYF < MCNTL[19]):
            # 気象データ形式が「標準年気象データ」で、読み込んだ日付が助走開始日とは異なる
            # 気象データ形式が「実データ」で、読み込んだ日付が助走開始日より前
            # print(f"{int(IDND[1,1])}年 {int(IDND[1,2])}月 {int(IDND[1,3])}日")
            continue
        else:
            break


MODE = 1   # 1: 助走計算、2:本計算、3:計算終了
KWK  = 0   # SOLAR POSITION を skip するかどうか

flag_day = True

while flag_day:

    for I in range(1,8):
        for J in range(1,25):
            # 気象データ
            WD[I,J] = WDND[I,J]
        for J in range(1,6):
            # 日付
            ID[I,J]=IDND[I,J]

    # 通算日数
    KDY  = NDATE(ID[1,2],ID[1,3])
    # 通算日数 (1899.12.31を1とした日数）           
    KDYF = NDATF(ID[1,1],ID[1,2],ID[1,3])   

    MDW[1]   = MKMDW(ID, M)   # 曜日（スケジュール）
    ISEAS[1] = int(M[980+int(ID[1,2])])  # 季節
    IDWK[1]  = int(ID[1,1])   # 計算日の年
    IDWK[2]  = int(ID[1,2])   # 計算日の月
    IDWK[3]  = int(ID[1,3])   # 計算日の日

    print(f"計算日： {int(IDWK[1])}年 {int(IDWK[2])}月 {int(IDWK[3])}日")

    # 気象データをRewindした回数（Rewind前）
    ICYCLO = ICYCL

    # 気象データを読み込む（１日分）
    # WDND: 気象データ（加工前）
    # IDND: 日付データ
    # ISTAT: 1=通常、0=ファイル終了（IOPWE=1、つまり実在データのとき）
    ICYCL,WDND,IDND,ISTAT,NUW_day = INWD(NUW, int(MCNTL[5]/2), int(MCNTL[31]), int(ICYCL), NUW_day) 

    if ISTAT == 0: # 実在気象データの最終日＝計算最終日
        if (KDYF != MCNTL[21]): # 計算終了日と一致しない場合
            raise Exception("エラーです")
        MDW[2]   = MDW[1]    # 明日の曜日 ＝ 最終日と同じとする
        ISEAS[2] = ISEAS[1]  # 明日の季節 ＝ 最終日と同じとする
    else:
        MDW[2]   = MKMDW(IDND, M)      # 明日の曜日
        ISEAS[2] = M[980+int(IDND[1,2])]  # 明日の季節

    #------------------------
    # モードの切り替え
    #------------------------
    if MODE == 1:  # 助走計算

        if MCNTL[5] == 1:   # 気象データ形式 = ピークデータ
            if ICYCLO == MCNTL[15]:   # 計算サイクル数が指定のサイクル数の際に「本計算」となる
                MODE = 2
        else:
            if KDYF == MCNTL[20]:     # 本計算開始日
                MODE = 2

    if MODE == 2:
        if MCNTL[5] == 1:   # 気象データ形式 = ピークデータ
            if ICYCL == MCNTL[15]+1:    # 計算サイクル数が指定のサイクル数を超えたら終了
                MODE = 3   # 計算終了
        else:
            if KDYF == MCNTL[21]:      # 計算終了日に達したら
                MODE = 3   # 計算終了


    # ***          3.2 WEATHER DATA ****************************************

    for J in range(1,25):
        
        if (MCNTL[31] == 3):
            WD[1,J] = 0.1*(WD[1,J]-500.)
        else:
            WD[1,J] = 0.1*WD[1,J]
        
        WD[2,J] = 0.1*WD[2,J]

        if (MCNTL[3] == 0):
            WD[5,J] = 4.88*(0.01*(WD[1,J]+273.16))**4 * (1.-0.062*WD[5,J])*(0.49-2.1*math.sqrt(WD[2,J]/(WD[2,J]+622.0)))

        if (MCNTL[4] == 0):
            WD[3,J] = WD[3,J]/0.4186
            WD[4,J] = WD[4,J]/0.4186
            if (MCNTL[3] == 1):
                WD[5,J] = WD[5,J]/0.4186
        elif (MCNTL[4] == 2):
            WD[3,J] = WD[3,J]/4.186  
            WD[4,J] = WD[4,J]/4.186
            if (MCNTL[3] == 1):
                WD[5,J] = WD[5,J]/4.186 

        WD[6,J] = (WD[6,J]-8.)*22.5
        WD[7,J] = 0.1*WD[7,J]
        WD8[J] = SATX(WD[1,J])-WD[2,J]   # 飽差（外気飽和絶対湿度－外気絶対湿度)

    # print("気象データ")
    # print(WD[:,1])
    # print(WD[1,2])
    # print(WD[1,3])
    # print(WD[1,4])
    # print(WD[1,5])
    # print(WD[1,6])
        
    # ***          3.3. OUTPUT 2 (WEATHER DATA) ****************************
    # skip

    # M1：週番号, KDY (1/1からの通算日数)  
    M1 = int((KDY+6)/7)

    # KWK: 週番号保存・処理したらM1を更新 = 週に1回だけ処理を行う。
    if M1 != KWK:   # GOTO 540

        KWK = M1

        # **           3.4. SOLAR POSITION ******************************
        W  = 0.0171672*(KDY+3)
        W1 = 0.362213-23.2476*math.cos(W+0.153231)-0.336891*math.cos(2.*W+0.207099)-0.185265*math.cos(3.*W+0.620129)
        W2 = -0.000279+0.122772*math.cos(W+1.49831)-0.165458*math.cos(2.*W-1.26155)-0.005354*math.cos(3.*W-1.1571)
        SD = math.sin(W1*DR)
        CD = math.cos(W1*DR)
        ET = (W2+X[152]-12.)
        SS = X[150]*SD
        SC = X[150]*CD
        CS = X[151]*SD
        CC = X[151]*CD

        for J in range(1,25):
            W1 = 15.*(ET+J)*DR
            SH[J] = SS+CC*math.cos(W1)
            CHSA[J] = CD*math.sin(W1)
            CHCA[J] = -CS+SC*math.cos(W1)

        # ***          3.5. 'EXPS' UPDATE **************************************

        # M[100]はEXPSのポインタ
        L = int(M[100])

        while L != 0:

            for J in range(1,25):

                if SH[J] <= 0:
                    X[L+J+27] = 0.
                    X[L+J+51] = 0.
                    L = int(M[L])
                    break   # for文を抜ける

                SS = SH[J] * X[L+6] + CHCA[J] * X[L+9] + CHSA[J] * X[L+7]

                if SS <= 0:
                    X[L+J+27] = 0.
                    X[L+J+51] = 0.  
                    L = int(M[L])
                    break   # for文を抜ける

                if (X[L+14]==0) and (X[L+19]==0):
                    X[L+J+27] = SS 
                    X[L+J+51] = GF(SS**2)
                    L = int(M[L])
                    break   # for文を抜ける

                CS = CHSA[J] * X[L+4] - CHCA[J] * X[L+3]
                CC = -SH[J]  * X[L+5] + CHCA[J] * X[L+10] + CHSA[J] * X[L+8]
                U  = X[L+19] * abs(CS)/SS
                V  = X[L+14] * abs(CC)/SS

                if (U >= X[L+23]) or (V >= X[L+18]):
                    X[L+J+27] = 0. 
                    X[L+J+51] = 0. 
                    L = int(M[L])
                    break   # for文を抜ける

                ST = (X[L+23]-U)*(X[L+18]-V)

                if (X[L+25] == 0):
                    SG = 0
                else:

                    if (CS < 0):
                        UG = X[L+22]-U
                        if (UG > X[L+20]):
                            UG = X[L+20]
                        if (UG < 0):
                            UG=0.
                    else:
                        UG = X[L+21]-U
                        if (UG > X[L+20]):
                            UG = X[L+20]
                        if (UG < 0.):
                            UG=0.

                    if (CC < 0):
                        VG=X(L+17)-V
                        if (VG > X[L+15]):
                            VG = X[L+15]
                        if (VG < 0.):
                            VG = 0.
                    else:
                        VG = X[L+16]-V
                        if (VG > X[L+15]):
                            VG = X[L+15]
                        if (VG < 0):
                            VG = 0.

                    SG = UG*VG
                    X[L+J+51] = GF(SS**2)*SG/X[L+25]     

                if (X[L+24] == 0):  # 壁面全体面積-窓面積=0                           
                    X[L+J+27] = 0.                                 
                else:                                                                 
                    X[L+J+27] = SS*(ST-SG)/X[L+24]                                         

                # 次のEXPSへ
                L = int(M[L])


    # ***          3.5.5 OAHU PRE-PROCESS **********************************

    LL = M[98]

    while LL != 0:

        L1 = int(LL+2+(ISEAS[1]-1)*9)

        for II in [1,2]:
            X[LL+30+(II-1)*25] = X[LL+54+(II-1)*25]   # 計算初日は0となる
            X[LL+80+(II-1)*25] = X[LL+104+(II-1)*25]   # 計算初日は0となる
        X[LL+130] = X[LL+178]    # 計算初日は0となる

        for J in range(0,25):
            for II in [1,2]:   # 顕熱・潜熱ループ

                # 全熱交出口温湿度
                if (M[L1+1] >= II):
                    W1 = X[LL+2] * X[L1+1+II] + (1.0-X[LL+2]) * WD[II,J]
                else:
                    W1 = WD[II,J]

                X[LL+30+(II-1)*25+J] = W1

                # 外調機出口温湿度
                if (M[L1+4] >= II) and (W1 > X[L1+6+(II-1)*2]):
                    W2 = X[L1+6+(II-1)*2]
                elif (M[L1+5] >= II) and (W1 < X[L1+7+(II-1)*2]):
                    W2 = X[L1+7+(II-1)*2]
                else:
                    W2 = W1

                X[LL+80+(II-1)*25+J] = W2

            for II in [1,2]:
                X[LL+130+(J-1)*2+I] = 0.0    # 積算風量ゼロクリア

            LL = int(M[LL])

    # ***          3.6. SPACE LOOP START ***********************************

    # SPACのポインタ
    LC = int(M[106])

    print(f"計算対象SPAC: {NAME(M[LC+1])}, L = {LC}")

    LCGB = LC
    LCO  = LC  # 追加（元のfortranにはない）

    KSPAC = 0
    
    # スペースループのフラグ
    flag_space = True

    while flag_space:

        # KSPAC：既に処理したSPACの数
        # LC：次に処理するSPACポインタ（終了時は0となる）、LCO：既に処理したSPACポインタ
        # LC+101 は「グループ内の連番」
        # print(f'LC: {LC}, LCO: {LCO}')
        # print(f'M[LC+101]: {M[LC+101]}, M[LCO+101]+1: {M[LCO+101]+1}')

        if (KSPAC != 0) and (M[LC+101] != M[LCO+101]+1):
            
            # 新しいグループに移ったとき(最初のグループを除く) = 同一グループの処理が終わったとき
            LC1  = LCGB    # 同一グループで最初に処理したSPACポインタ
            LCGB = LC      # 次に処理するSPACポインタ

            if (M[LCO+55]!=0):  # SOPCデータによってOPCOデータを引用した場合
                
                # グループに属するスペース数をカウント
                # NAZ : 1グループあたりの最大スペース数（20）
                # NZ  : グループに属するスペース数
                NZ = 1
                for II in range(1, NAZ+1):
                    if (LC1 == LCO):
                        break
                    else:
                        LC1 = M[LC1]
                        NZ = NZ+1

                # #  ISEAS[1]:本日の季節(=1:夏期,2:冬期,3:中間期), ISEAS[2]:明日の季節
                II = min(2,ISEAS[1])

                # mprint("EXTRC2 NHR: ", NHR)                 # 1日のステップ数 (24で固定)
                # mprint("EXTRC2 MCNTL[1]: ", MCNTL[1])       # 計算モード =1:ピーク計算モード、=0:シミュレーションモード
                # mprint("EXTRC2 ISEAS[1]: ", ISEAS[1])       # 本日の季節 =1:夏期、=2:冬期、=3:中間期
                # mprint("EXTRC2 NAZ: ",NAZ)                  # 1グループあたりの最大スペース数（20で固定）
                # mprint("EXTRC2 IOPTG: ",IOPTG)              # 空調運転状態フラグ= 0:停止中、=1:運転中、=2:起動、=3:停止 <EXTRC1>
                # mprint("EXTRC2 NZ: ",NZ)                    # グループに属するスペース数
                # mprint("EXTRC2 IOPVG: ",IOPVG)              # 外気導入状態フラグ、=0:カット中、=1:導入中、=2:導入開始、=3:導入停止 <EXTRC1>
                # mprint("EXTRC2 SMRT1: ",SMRT1)              # 面積を持たない部位からの冷房負荷 <EXTRC1>
                # mprint("EXTRC2 SMRT2: ",SMRT2)              # INFLの吸熱応答係数 <EXTRC1>
                # mprint("EXTRC2 VOAG: ",VOAG)                # 導入時の外気量 <EXTRC1>
                # mprint("EXTRC2 LCG: ",LCG)                  # XMQ配列のSPACデータへのポインタ（L）<EXTRC1>
                # mprint("EXTRC2 CLDG: ",CLDG)                # 冷房負荷 <EXTRC1>
                # mprint("EXTRC2 NWD: ",NWD)                  # WDの整合寸法（=7）
                # mprint("EXTRC2 WD: ",WD)                    # 外界気象（基準温湿度からの偏差ではない）
                # mprint("EXTRC2 REFWD: ",REFWD)              # 基準温湿度
                # mprint("EXTRC2 P0: ",P0)                    # 瞬時蓄熱応答係数（吸熱される側が正）第2添字 =0:二等辺三角、=1:右側直角二等辺三角 <EXTRC1>
                # mprint("EXTRC2 M[ int(LOPC+165+II) ]", M[ int(LOPC+165+II) ])   # 予熱時間（ステップ）
                # mprint("EXTRC2 VFLOW: ",VFLOW)              # 第1添字目のスペースから第2添字目のスペースへの流入、風量（体積流量、0以上、対角項は0とする）
                # mprint("EXTRC2 EXCAP: ",EXCAP)              # 各スペースの装置容量（冷却、0以上） <EXTRC1>
                # mprint("EXTRC2 SPCAP: ",SPCAP)              # 各スペースの装置容量（加熱、0以上） <EXTRC1>
                # mprint("EXTRC2 RMMX: ",RMMX)                # 各スペースの設定温湿度上限 <EXTRC1>
                # mprint("EXTRC2 RMMN: ",RMMN)                # 各スペースの設定温湿度下限 <EXTRC1>
                # mprint("EXTRC2 NUOT+KSPAC-NZ: ", NUOT+KSPAC-NZ)   # テキスト出力ファイルの装置番号（最初の装置番号）
                # mprint("EXTRC2 IDWK: ", IDWK)               # 年・月・日
                # mprint("EXTRC2 MDW[1]: ",MDW[1])            # MDW(1)  :本日の曜日(=1:月,2:火,..,7:日,8:祝,9:特)
                # mprint("EXTRC2 MODE: ", MODE)               # =1:助走、=2:本計算、=3:最終日
                # mprint("EXTRC2 MCNTL[2]: ", MCNTL[2])       # 出力モード =0:簡易出力(1h1行)、=1:詳細出力(1h2行)
                # mprint("EXTRC2 LSZSPC: ", LSZSPC)           # XMQ配列のうち、(0):SPAC, (1):OWAL, (2):IWAL, (3):WNDW, (4):INFL の変数の数

                (X,M,result) = EXTRC2(NHR,MCNTL[1],ISEAS[1],NAZ,IOPTG,NZ,IOPVG,SMRT1,SMRT2,VOAG,LCG,CLDG,NWD,WD,REFWD,
                        P0,M[int(LOPC+165+II)],VFLOW,EXCAP,SPCAP,RMMX,RMMN,10,
                        NUOT+KSPAC-NZ,IDWK,MDW[1],MODE,MCNTL[2],LSZSPC,X,M,result)

        # 全スペース終了
        if LC == 0:
            flag_space = False
            break

        # スペース数をカウント
        KSPAC += 1

        M1 = int(M[LC+34])   # WSCHポインタ

        # KSCH(1) :本日のスケジュール(=1:全日,2:半日,3:1日中0%), KSCH(2) :明日のスケジュール
        KSCH[1] = M[int(M1+MDW[1])]   # スケジュールパターン

        L1 = int((KSCH[1]-1)*24)
        LL = int(M[LC+35]+L1)   # DSCHポインタ（LIGH）
        LH = int(M[LC+47]+L1)   # DSCHポインタ（HEAT）
        LO = int(M[LC+51]+L1)   # DSCHポインタ（OCUP）

        if (M[LC+35] == 0):
            LL=0
        if (M[LC+47] == 0):
            LH=0
        if (M[LC+51] == 0):
            LO=0

        # OPCOデータのポインタ
        LOPC = M[LC+55]
        if (LOPC != 0):  # SOPCデータによってOPCOデータを引用した場合
            # KSCH(2) :明日のスケジュール
            # MDW(2)  :明日の曜日(=1:月,2:火,..,7:日,8:祝,9:特)
            KSCH[2] = M[int(M1+MDW[2])]

        # ***          3.7. HOURLY LOOP START **********************************

        for J in range(1,25):
            
            ACC1 = 0.0  # 顕熱負荷・対流成分（そのまま冷房負荷に）
            ACC2 = 0.0  # 顕熱負荷・放射成分（室の重み係数でたたみ込み）
            ACC3 = 0.0  # 潜熱負荷（そのまま冷房負荷に）
            ACC4 = 0.0  # 補正項（顕熱）
            ACC5 = 0.0  # 補正項（潜熱）
            ACC6 = 0.0  # 消灯面積率の和

            X[LC+74] = 0.0  # 面積を持たない部位の冷房負荷（=INFLと強制空冷が選ばれたときのHEAT）
            X[LC+75] = 0.0  # INFLの吸熱応答係数（瞬時） Σ0.288V

            L = int(LC + LSZSPC[0])
            
            if LOPC != 0:  # LOPC: OPCOデータへのポインタ(L)

                # IOPTWK 空調運転状態フラグ =0:停止中、=1:運転中、=2:起動、=3:停止
                IOPTWK,M = EXTRC0(J,LOPC,LC,ISEAS,KSCH,X,M)   # 空調運転状態(IOPTWK,M(LC+60))

                # mprint("J")
                # mprint("LOPC")
                # mprint("LC")
                # mprint("ISEAS")
                # mprint("KSCH")
                # mprint("IOPTWK")

            while M[L] != 5:

                if M[L] == 1:

                    # ***          3.8. HEAT GAIN THROUGH OUTSIDE WALL **********************

                    LE = int(M[L+1])

                    if (CHCA[J]*X[LE+4]+CHSA[J]*X[LE+3] < 0):
                        W = WD[3,J]
                    else:
                        if (X[L+13]*CHSA[J]+X[L+14]*CHCA[J] < SH[J]):
                            W = WD[3,J]
                        else:
                            W = 0

                    EXC = WD[1,J] - X[155] + W * (X[int(LE+J+27)]*X[L+9]+SH[J]*X[L+10]) \
                        + WD[4,J] * X[L+11] - WD[5,J]*X[L+12] - WD8[J]*X[L+15]

                    W = X[L+7] + X[L+8] + EXC*X[L+2]
                    ACC1 += W * FC
                    ACC2 += W * FR
                    X[L+7] = X[L+7]*X[L+4] + EXC*X[L+3]
                    X[L+8] = X[L+8]*X[L+6] + EXC*X[L+5]

                    # mprint("(1) 3.8. HEAT GAIN THROUGH OUTSIDE WALL: ACC1", ACC1)
                    # mprint("3.8 外壁 ACC1:", ACC1)

                    L = L+LSZSPC[1]

                elif M[L] == 2:

                    # ***          3.9. HEAT GAIN THROUGH INSIDE WALL **********************

                    if M[L+1] != 3:

                        if M[L+1] == 0:
                            EXC = (WD[1,J] - X[155]) * X[L+15]
                        elif M[L+1] == 1:
                            EXC = WD[1,J] + X[L+15] - X[155]
                        else:
                            EXC = X[L+15] - X[155]

                        W = X[L+11] + X[L+12] + EXC*X[L+3]
                        ACC1 += W * FC
                        ACC2 += W * FR
                        X[L+11] = X[L+11]*X[L+7] + EXC*X[L+5]
                        X[L+12] = X[L+12]*X[L+10] + EXC*X[L+8]
    
                    # mprint("(2) 3.9. HEAT GAIN THROUGH INSIDE WALL: ACC1", ACC1)
                    # mprint("3.9 内壁 ACC1:", ACC1)

                    L = L+LSZSPC[2]

                elif M[L] == 3:

                    # ***          3.10. HEAT GAIN THROUGH WINDOW **************************

                    LE = int(M[L+1])  # EXPSへのポインタ

                    # X[LE+3]  傾斜角のsin
                    # X[LE+4]  傾斜角のcos
                    if (CHCA[J] * X[LE+4] + CHSA[J] * X[LE+3]) < 0:
                        # WD[3,J] 法線面直達日射量
                        W = WD[3,J]
                    else:
                        # X[LE+13]  Ha （隣棟高さ[m]）
                        # X[LE+14]  Zh （庇の出[-]）
                        if (X[L+13]*CHSA[J] + X[L+14]*CHCA[J]) < SH[J]:
                            W = WD[3,J]
                        else:
                            W = 0
                
                    # WD[1,J] 外気温
                    # X[155] 基準温度
                    # WD[5,J] 夜間放射量
                    # X[L+12] 
                            
                    EXC1 = (WD[1,J] - X[155]) - WD[5,J]*X[L+12]

                    V1 = W * X[LE+J+51]
                    V2 = W * SH[J] * X[L+9]
                    V3 = WD[4,J] * X[L+11]
                    V4 = WD[4,J] * X[L+9]
                    EXC2 = V1+V2+V3+V4
                
                    for II in [0,1]:        # ブラインド開閉ループ
                        for I in [0,1,2]:   # K,SCC,SCRループ

                            L1 = int(L+20+II*9+I*3)  # XMQ配列の添字（スケジュールオプション）
                            W  = X[int(L+2+II*3+I)]  # オリジナルの物性値
                        
                            if M[L1] == 0:   #オリジナルのまま
                                WINCHR[I,II] = W
                            elif M[L1] == 1: # on時%の値を使用
                                if (IOPTWK == 1) or (IOPTWK == 3): # 稼動中あるいは停止時
                                    WINCHR[I,II] = X[L1+2]*W
                                else:
                                    WINCHR[I,II]=W
                            else:   # DSCHを使用
                                WINCHR[I,II] = X[ int(M[L1+1]+(KSCH[1]-1)*24+J) ] * W

                    if (M[L+38] >= 1) and ( (IOPTWK==1)or(IOPTWK==3) ):  # 稼動中あるいは停止時
                        for II in [0,1]:        # ブラインド開閉ループ
                            for I in [0,1,2]:   # K,SCC,SCRループ
                                WINCHR[I,II] = WINCHR[I,II] + X[int(L+39+II*3+I)]
                

                    if (X[LO+J] == 0):

                        # 641の処理
                        W1   = EXC1 * WINCHR[0,1]
                        ACC1 = ACC1 + W1 * (1.0-X[L+46]) + EXC2*WINCHR[1,1]
                        ACC2 = ACC2 + W1 * X[L+46] + EXC2*WINCHR[2,1]
                        if X[LC+43] != 0:
                            W = X[L+18] * (V1+V2+V3+V4) * WINCHR[2,1]
                            if W > X[LC+43]:
                                ACC6 += X[L+19]

                    else:

                        W = EXC2 *WINCHR[2,0]

                        if  W > X[L+8]:

                            # 641の処理
                            W1   = EXC1 * WINCHR[0,1]
                            ACC1 += W1 * (1.0-X[L+46]) + EXC2*WINCHR[1,1]
                            ACC2 += W1 * X[L+46] + EXC2*WINCHR[2,1]
                            if X[LC+43] != 0:
                                W = X[L+18] * (V1+V2+V3+V4) * WINCHR[2,1]
                                if W > X[LC+43]:
                                    ACC6 += X[L+19]

                        else:

                            W1   = EXC1 * WINCHR[0,0]
                            ACC1 += W1*(1.0-X[L+45]) + EXC2*WINCHR[1,0]
                            ACC2 += W1*X[L+45]+W
                            ACC4 += WINCHR[0,0]-X[L+5]
                    
                            if (X[LC+43] != 0):                        
                                W = (X[L+15]*V3+X[L+16]*(V1+V3)+X[L+17]*(V2+V4)) * WINCHR[2,0]
                                if W > X[LC+43]:
                                    ACC6 += X[L+19]

                    # mprint("(3) 3.10. HEAT GAIN THROUGH WINDOW: ACC1", ACC1)
                    # mprint("3.10 窓 ACC1:", ACC1)

                    L = L+LSZSPC[3]                    

                elif M[L] == 4:

                    # ***          3.11. INFILTRATION **************************************

                    # EXPSへのポインタ
                    LE = int(M[L+1])

                    W  = abs(X[LE+2] - WD[6,J])
                    U  = CF( math.cos(W*DR) ) * X[LC+7] * WD[7,J]**2 + (WD[1,J]-X[155])*X[LC+6]
                
                    if M[L+4] == 0:  # オリジナル換気量で一定、あるいは空調on・off時%の値を使用

                        if (IOPTWK == 1) or (IOPTWK == 3):    # 稼動中あるいは停止時
                            W = X[L+6]
                        else:
                            W = X[L+7]

                    else:   # DSCH使用
                        W = X[ int( M[L+5] + (KSCH[1]-1)*24+J) ]
                    
                    if (M[L+2] == 0):

                        if (U > 0):
                            V = W * X[L+3] * U**0.67
                        else:
                            V=0

                    else:

                        if (U > 0) or (M[L+2] == 2):
                            V = W * X[L+3]
                        else:
                            V=0.
                
                    ACC1 += 0.288 * V *(WD[1,J]-X[155])
                    ACC3 += 0.720 * V *(WD[2,J]-X[157])
                    ACC4 += 0.288 * V 
                    ACC5 += 0.720 * V

                    X[LC+74] = X[LC+74] + 0.288*V*(WD[1,J]-X[155])
                    X[LC+75] = X[LC+75] + 0.288*V

                    # mprint("W", W)
                    # mprint("L", L)
                    # mprint("X[L+3]", X[L+3])

                    # mprint("(4) 3.11. INFILTRATION: ACC1", ACC1)
                    # mprint("3.11 隙間風 ACC1:", ACC1)
                
                    L = L+LSZSPC[4]

            if M[L] == 5:

                # ***          3.12. INTERNAL HEAT *************************************

                if (LL != 0):   # 照明

                    # X[LC+43] 昼光利用 (なし：=0、あり：=1/2 設計照度）
                    if X[LC+43] == 0:   
                        ACC6 = 0
                    if ACC6 > 1:
                        ACC6 = 1

                    # LL: DSCHポインタ（LIGH） → X[LL+J]は照明のスケジュール
                    # MCNTL[22] HRAT 発熱割合 LIGH 夏期 %
                    # MCNTL[23] HRAT 発熱割合 LIGH 冬期 %
                    # MCNTL[24] HRAT 発熱割合 LIGH 中間期 %
                    W  = X[LL+J] * MCNTL[ int(21+ISEAS[1]) ] /100 * (1-ACC6)   # 季節別発熱割合を掛ける

                    # X[LC+36]  照明熱取得：P0 瞬時応答係数[kcal/h]
                    # X[LC+41]  照明熱取得：Q1 前ステップまでの励振による現ステップでの照明起源の1項目の熱取得 [kcal/h]
                    # X[LC+42]  照明熱取得：Q2 前ステップまでの励振による現ステップでの照明起源の2項目の熱取得 [kcal/h]
                    W1 = X[LC+41] + X[LC+42] + W * X[LC+36]

                    # X[LC+37]  照明熱取得：P1 1項目の1ステップ後の応答係数[kcal/h]
                    # X[LC+38]  照明熱取得：R1 1項目の公比[-]
                    # X[LC+39]  照明熱取得 P2 2項目の1ステップ後の応答係数[kcal/h]
                    # X[LC+40]  照明熱取得：R2 1項目の公比[-]
                    X[LC+41] = X[LC+41] * X[LC+38] + W * X[LC+37]
                    X[LC+42] = X[LC+42] * X[LC+40] + W * X[LC+39]

                    ACC1 += W1 * FC
                    ACC2 += W1 * FR
            
                    # mprint("3.12 照明発熱 ACC1:", ACC1)

                if LH != 0:   # 機器

                    # LH: DSCHポインタ（HEAT） → X[LH+J]は機器発熱のスケジュール
                    # MCNTL[28] HRAT 発熱割合 HEAT 夏期 %
                    # MCNTL[29] HRAT 発熱割合 HEAT 冬期 %
                    # MCNTL[30] HRAT 発熱割合 HEAT 中間期 %
                    W1 = X[LH+J] * MCNTL[ int(27+ISEAS[1]) ]/100.   # 季節別発熱割合を掛ける

                    # X[LC+48]  対流放熱量[kcal/h]
                    # X[LC+49]  放射放熱量[kcal/h]
                    # X[LC+50]  潜熱放熱量[kcal/h]
                    ACC1 += W1 * X[LC+48]
                    ACC2 += W1 * X[LC+49]
                    ACC3 += W1 * X[LC+50]
                
                    # X[LC+74] 面積を持たない部位の冷房負荷（=INFLと強制空冷が選ばれたときのHEAT）
                    if abs(X[LC+49]) < 0.001:  # 強制対流式の場合
                        X[LC+74] += W1 * X[LC+48]

                    # mprint("3.12 機器発熱 ACC1:", ACC1)

                if LO != 0:   # 人体

                    # LO: DSCHポインタ（OCUP) → X[LO+J]は人体のスケジュール
                    # MCNTL[25] HRAT 発熱割合 OCUP 夏期 %
                    # MCNTL[26] HRAT 発熱割合 OCUP 冬期 %
                    # MCNTL[27] HRAT 発熱割合 OCUP 中間期 %
                    W1  = X[LO+J] * MCNTL[ int(24+ISEAS[1]) ]/100.   # 季節別発熱割合を掛ける

                    # X[LC+53]  AM2×人数（24℃における顕熱）[kcal/h]
                    # X[LC+54]  AM3×人数（単位温度上昇あたりの顕熱増加量）[kcal/h℃]
                    # X[ int(LC+214+ISEAS[1]) ]  室温（各季節）
                    W2 = X[LC+53] + X[LC+54] * (X[ int(LC+214+ISEAS[1]) ]-24.)          

                    ACC1 += W1 * W2 * FC
                    ACC2 += W1 * W2 * FR

                    # X[LC+52]  AM1×人数（全熱）[kcal/h]
                    ACC3 += W1 * (X[LC+52] - W2)

                    # mprint("3.12 人体発熱 ACC1:", ACC1)
                    # mprint("3.12 人体発熱 ACC1:", ACC1)
                    # mprint("3.12 人体発熱 ACC1:", ACC1)

                # mprint("(5) 3.12. INTERNAL HEAT: ACC1", ACC1)

            # ***          3.13 CONVERT HEAT GAIN TO COOLING LOAD ******************
            
            # X[LC+8] : 冷房負荷重み係数：P0 瞬時応答係数[kcal/h℃]
            # X[LC+9] : 冷房負荷重み係数：P1 1ステップ後の応答係数[kcal/h℃]
            # X[LC+10] : 冷房負荷重み係数：R1 公比
            # X[LC+11] : 冷房負荷重み係数：Q1 前ステップまでの励振による現ステップでの放射成分起源の冷房負荷 [kcal/h]
                    
            X[J]     = ACC1 + X[LC+11] + ACC2 * X[LC+8]
            X[LC+11] = X[LC+11] * X[LC+10] + ACC2 * X[LC+9]
            X[J+24]  = ACC4
            X[J+48]  = ACC3
            X[J+72]  = ACC5

            # mprint("3.13: J", J)
            # mprint("3.13: ACC1", ACC1)
            # mprint("3.13: ACC2", ACC2)
            # mprint("3.13: X[LC+11]", X[LC+11])
            # mprint("3.13: X[LC+8]", X[LC+8])


            # ***                CALCULATION EXTRACTING LOAD   *********************

            if (LOPC != 0 ):

                # 装置容量を季節別に設定する
                # INTEGER     IOPTG(NAZ,NHR)      ! O   空調運転状態フラグ、=0:停止中、=1:運転中、=2:起動、=3:停止
                # INTEGER     IOPVG(NAZ,NHR)      ! O   外気導入状態フラグ、=0:カット中、=1:導入中、=2:導入開始、=3:導入停止
                # REAL        SMRT1(NAZ,NHR)      ! O   面積を持たない部位からの冷房負荷
                # REAL        SMRT2(NAZ,NHR)      ! O   INFLの吸熱応答係数
                # INTEGER     LCG(NAZ)            ! O   XMQ配列のSPACデータへのポインタ（L）添字は現在のゾーンのグループ内における順番(=IZ)
                # REAL        VOAG(NAZ)           ! O   導入時の外気量(添字はグループ内の順番=IZ)
                # REAL        CLDG(NAZ,NHR,NSL)   ! O   冷房負荷
                # REAL        P0(NAZ,0:1,NHR,NSL) ! O   瞬時蓄熱応答係数（吸熱される側が正）第2添字 =0:二等辺三角 =1:右側直角二等辺三角
                # REAL        RMMN(NAZ,NSL)       ! O   各スペースの設定温湿度下限
                # REAL        RMMX(NAZ,NSL)       ! O   各スペースの設定温湿度上限
                # REAL        SPCAP(NAZ,NSL)      ! O   各スペースの装置容量（加熱、0以上）
                # REAL        EXCAP(NAZ,NSL)      ! O   各スペースの装置容量（冷却、0以上）
                # REAL        VFLOW(NAZ,NAZ,NHR)  ! O   第1添字目のスペースから第2添字目のスペースへの流入
                #                                 !     風量（体積流量、0以上、対角項は0とする）

                (IOPTG,IOPVG,SMRT1,SMRT2,LCG,VOAG,CLDG,P0,RMMN,RMMX,SPCAP,EXCAP,VFLOW,X,M) = \
                    EXTRC1(J,NHR,LOPC,LC,NAZ,ISEAS[1],KSCH[1],IOPTWK,
                            IOPTG,IOPVG,SMRT1,SMRT2,LCG,VOAG,CLDG,P0,RMMN,RMMX,SPCAP,EXCAP,VFLOW,
                            X,M)

                # mprint("EXTRC1: J", J)
                # mprint("EXTRC1: NHR", NHR)
                # mprint("EXTRC1: LOPC", LOPC)
                # mprint("EXTRC1: LC", LC)
                # mprint("EXTRC1: NAZ", NAZ)
                # mprint("EXTRC1: NHR", NHR)
                # mprint("EXTRC1: ISEAS[1]", ISEAS[1])
                # mprint("EXTRC1: KSCH[1]", KSCH[1])
                # mprint("EXTRC1: IOPTWK", IOPTWK)

                # mprint("EXTRC1: IOPTG", IOPTG[1])
                # mprint("EXTRC1: IOPVG", IOPVG[1])
                # mprint("EXTRC1: SMRT1", SMRT1[1])
                # mprint("EXTRC1: SMRT2", SMRT2[1])
                # mprint("EXTRC1: LCG", LCG[1])
                # mprint("EXTRC1: VOAG", VOAG[1])
                # mprint("EXTRC1: CLDG", CLDG[1])
                # mprint("EXTRC1: P0-0", P0[1,0])
                # mprint("EXTRC1: P0-1", P0[1,1])
                # mprint("EXTRC1: RMMN", RMMN[1])
                # mprint("EXTRC1: RMMX", RMMX[1])
                # mprint("EXTRC1: SPCAP", SPCAP[1])
                # mprint("EXTRC1: EXCAP", EXCAP[1])
                # mprint("EXTRC1: VFLOW", VFLOW[1,1,:])

        LCO = int(LC)     # 処理をしたSPACのポインタ
        LC  = int(M[LC])  # 次に処理するSPACのポインタ

        if MODE == 3:
            flag_day = False
            break


#-----------------------------------------------------------------------
# 99. 結果の出力
#-----------------------------------------------------------------------

# XMQデータの出力
display_XMQ_matrix(X,M,2000,3000)


# 計算結果の出力
cols = ["YEAR","MO","DY","YB","HR","IREP",
        "ROOM-T","CLOD-S","RHEX-S","AHEX-S","FS",
        "ROOM-H","CLOD-L","RHEX-L","AHEX-L","FL","MRT"]

LL = int(M[106])
while (LL != 0):

    pd.DataFrame(result[str(LL)]["calc_data"], columns=cols).to_csv("pyHASP_"+result[str(LL)]["name"]+".csv")

    # 次のスペースに移動
    LL = int(M[LL])
