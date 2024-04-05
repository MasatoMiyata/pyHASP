import numpy as np

#-----------------------------------------------------------------------
# 外気温湿度の出力
#-----------------------------------------------------------------------

def WEOUT(NHR,NWD,WD,ID,MDW,MODE,IOUT,NUOW,WOUT,weather_data):

    #   INTEGER     NHR                  ! I   1日のステップ数
    #   INTEGER     NWD                  ! I   WDの整合寸法（=7）
    #   REAL        WD(NWD,NHR)          ! I   外界気象（基準温湿度からの偏差ではない）
    #   INTEGER     ID(3)                ! I   年・月・日（出力情報）
    #   INTEGER     MDW                  ! I   曜日（出力情報）
    #   INTEGER     MODE                 ! I   =1:助走、=2:本計算、=3:最終日
    #   INTEGER     IOUT                 ! I   =0:簡易出力(1h1行)、=1:詳細出力(1h2行)
    #   INTEGER     NUOW                 ! I   装置番号（外気温・外気湿度ファイル出力）
    # 
    #   REAL        WETH(0:1,MHR,NSL)    !     外気温・絶対湿度（基準温湿度からの偏差ではない）
    #                                    !     (第一添字はIREP)
    #   REAL        WOUT(-1:0,NSL)       !     外気温・絶対湿度（基準温湿度からの偏差ではない）
    #                                    !     (第一添字は0:1ステップ前, 1:現ステップ)

    NSL = 2    # 顕熱と潜熱
    MHR = 24   # 整合寸法（=NHR）
    
    WETH = np.zeros([2,MHR+1,NSL+1])

    for JHR in range(1, NHR+1):   # 時刻ループ

        for IREP in [0, 1]:    # 直前・直後ループ

            # 外気温湿度のセット
            for ISL in range(1, NSL+1):   # 顕熱・潜熱 loop
                WETH[IREP,JHR,ISL] = WD[ISL,JHR]
            
            # 出力
            if IOUT == 1: # 詳細出力

                if ( MODE >= 2 ):
                    print(ID[1])
                    print(ID[2])
                    print(ID[3])
                    print(MDW)
                    print(JHR)
                    print(IREP)
                    print(WETH[IREP,JHR,1])
                    print(WETH[IREP,JHR,2])

            else:   # 簡易出力（1時間分の平均の出力）

                for ISL in range(1, NSL+1):
                    WOUT[1,ISL] = WETH[IREP,JHR,ISL]
                
                if ( (IREP == 0) and (MODE >= 2) ):   

                    # 平均を取って出力
                    for ISL in range(1, NSL+1):
                        WOUT[1,ISL] = 0.5*(WOUT[0,ISL] + WOUT[1,ISL])

                    weather_data.append([
                        ID[1],
                        ID[2],
                        ID[3],
                        MDW,
                        JHR,
                        IREP,
                        WOUT[1,1],
                        WOUT[1,2]
                    ])

                elif ( IREP == 1 ):   # 次ステップのために記憶

                    for ISL in range(1, NSL+1):
                        WOUT[0,ISL] = WOUT[1,ISL] 


    return weather_data, WOUT
