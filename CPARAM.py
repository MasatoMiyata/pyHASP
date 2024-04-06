import math
import numpy as np

# ***          4.4. CONVOLUTION PARAMETERS *****************************
# *
# *     数値Laplace逆変換し，convolution parametersを求める．
# *     HASP/ACLD/8501の収束計算でしばしばエラーになるのを改良
# *     2001-06-25, Akihiro Nagata (Revised by T.Nagai)

def CPARAM(nt, g):
    """伝達関数の数値逆変換
    数値Laplace逆変換し，convolution parametersを求める．

    Args:
        nt (_type_): 項数
        g (_type_): 応答
    """

    p   = np.zeros(8+1)         # 1始まりとする
    h   = np.zeros(9+1)         # 1始まりとする
    z   = np.zeros([4+1,5+1])   # 1始まりとする
    zz  = np.zeros(5+1)         # 1始まりとする
    e   = np.zeros(2+1)         # 1始まりとする
    ee  = np.zeros(2+1)         # 1始まりとする
    a   = np.zeros(2+1)         # 1始まりとする

    # sは0始まり
    s = [ 0.00000, 0.00025, 0.00100, 0.00400, 0.01600, 0.06400, 0.25600, 1.02400, 4.09600, 16.38400]
    # g = [1.004636764526E+00,1.002895593643E+00,9.977108836174E-01,9.773875474930E-01,9.024424552917E-01,6.781448125839E-01,2.917146384716E-01,4.119890555739E-02,7.415368454531E-04,2.908633121024E-07]
    
    for j in range(1, 9+1):
        h[j] = (g[j] - g[0])/s[j]

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



if __name__ == '__main__':


    P = [0, 1.66922074E-03,   5.60675981E-03,   2.61105206E-02,   1.32991960E-02 , 0.963315725,      -3.05636767E-02,  -1.72367357E-02,  0.773170769]
    print(f"fortran の P: {P}")

    GTR = [0.578689754,      0.574019492,      0.560385227,      0.510925353,      0.370261252,      0.151284918 ,      2.23989319E-02,   6.01702603E-04,   6.33586581E-07,   9.68184517E-13]
    P = CPARAM(2,GTR)
    print(f"fortran の GTRを使った場合のpythonの計算値: {P}")

    GTR = [5.78689769e-01, 5.74019918e-01, 5.60385347e-01, 5.10925622e-01,3.70261227e-01, 1.51284928e-01, 2.23989372e-02, 6.01702781e-04,6.33586716e-07, 9.68185037e-13]
    P = CPARAM(2,GTR)
    print(f"pythonの計算値: {P}")
    

    