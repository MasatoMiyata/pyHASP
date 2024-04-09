def display_XMQ_matrix(X,M,start, end):
    """XMQ変数を出力する関数

    Args:
        start (_type_): 出力を開始する行数
        end (_type_): 出力を終了する行数
    """

    # set_range = range(start,end)
    # print("***********************************************")
    # for i in set_range:
    #     print( f"X {str(i)} :  {X[i]} " )
    # print("***********************************************")
    # for i in set_range:
    #     print( f"M {str(i)} :  {M[i]} " )
    # print("***********************************************")


    # ファイルに書き込む
    with open('display_XMQ_matrix_out.txt', 'w') as file:
        for i, (item1, item2) in enumerate(zip(X, M), start=1):
            file.write(f"{i-1}: {item1}, {item2}\n")
