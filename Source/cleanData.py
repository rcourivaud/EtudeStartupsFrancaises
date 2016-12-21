import pandas as pd
import re


df = pd.read_csv('CSV\StartUpDataBase.csv', sep=';',  index_col=False, header=0);
m = re.search('(?<=abc)def', 'abcdef')

print(df.head())