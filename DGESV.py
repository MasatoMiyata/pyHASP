import numpy as np

def DGESV(A,B):

    # A * X = B を解く関数
    # ただし、AとBは 1始まりの行列とする
    # Bは一次元とする。

    print(f"A: {A}")
    print(f"B: {B}")

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


if __name__ == '__main__':

    # 例題① 答えは X = [0 50]
    A = np.array(
        [   [0, 0,],
            [0, 2,]    ] )
    b = np.array([0,100])
    X = DGESV(A, b)
    print(f"X: {X}")

    # 例題② 答えは X = [0.0 4.0 2.5 1.5]
    # A = np.array(
    #     [   [0, 0, 0, 0],
    #         [0, 1, 0, 0], 
    #         [0, 0, 2, 0], 
    #         [0, 0, 0, 4]    ] )
    # b = np.array([0,4,5,6])
    # X = DGESV(A, b)
    # print(f"X: {X}")
