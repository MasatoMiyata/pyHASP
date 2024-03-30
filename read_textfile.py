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
