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

        if M[LC+1] == NNAM:
            return NNAM,LC,LD

        LD = M[LC]
        if LD == 0:
            return NNAM,LC,LD

        LC = LD


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



