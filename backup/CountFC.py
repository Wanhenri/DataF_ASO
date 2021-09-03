import pandas as pd

df1 = pd.read_csv('Teste_para_Python.csv', sep= ";")
#pd.set_option('display.max_columns', None)
#pd.reset_option('max_columns')
#df1.head(3)

df2 = pd.read_csv('Teste_para_Python_shape.csv', sep= ";")
#pd.set_option('display.max_columns', None)
#pd.reset_option('max_columns')
#df2.head(3)

#range(start, stop, step)
x = range(0, 1319, 1)
for n in x:
  print(n)
  df2.loc[df2['WDPA_PI'] == df1['WDPA_PI'][n], 'count'] = df1['count'][n]

df2.to_csv('tes.csv',sep=';',index_label=None,index=False)