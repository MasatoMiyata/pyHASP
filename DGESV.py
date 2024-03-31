import numpy as np

def DGESV(A, B, num_x=1):
    """A * X = B を解く関数
    ただし、AとBは 1始まりの行列とする。
    num_x は 未知数の個数
    """

    # 行列をずらす
    if num_x == 1:

        AA = np.zeros(num_x)
        BB = np.zeros(num_x)
        XX = np.zeros(num_x)

        for i in range(0, num_x):
            AA[i] = A[i+1][i+1]
            BB[i] = B[i+1]    
            XX[i] = BB[i]/AA[i] 

    else:

        AA = np.zeros([num_x, num_x])
        BB = np.zeros(num_x)

        for i in range(0, num_x):
            BB[i] = B[i+1]
            for j in range(0, num_x):
                AA[i][j] = A[i+1][j+1]

        # 方程式を解く
        XX = np.linalg.solve(AA, BB)

    # 行列をずらす
    X = np.zeros(XX.shape[0]+1)
    for i in range(0, XX.shape[0]):
        X[i+1] = XX[i]

    return X


if __name__ == '__main__':

    # 例題① 答えは X = [0 50]
    A = np.array(
        [   [0, 0,],
            [0, 2,]    ] )
    b = np.array([0,100])
    X = DGESV(A, b)

    # 例題② 答えは X = [0.0 4.0 2.5 1.5]
    # A = np.array(
    #     [   [0, 0, 0, 0],
    #         [0, 1, 0, 0], 
    #         [0, 0, 2, 0], 
    #         [0, 0, 0, 4]    ] )
    # b = np.array([0,4,5,6])
    # X = DGESV(A, b)
