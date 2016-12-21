import pandas as pd
import numpy as np


def cleanSalary():
    df = pd.read_csv('Salaires2012.csv', sep=';')
    ids= [x.strip().replace('\ufeff', '').replace('12', '').replace('SNHM','S') for x in list(df.columns)]
    df.columns = ids
    codegeo = pd.Series(df['CODGEO'])
    dept = codegeo.apply(getDepartement)
    df['Dept'] = dept
    
    df.to_csv('Salaires2012_clean.csv', sep=';')
   
    
def getDepartement(x):
    return str(x)[0:2]
    
def getSex(x):
    if(H in x):
        return 'homme'
    if(F in x):
        return 'femme'

def getJob(x):
    if(C in x):
        return 'cadre'
    if(P in x):
        return 'cadre'
    if(E in x):
        return 'cadre'
    if(O in x):
        return 'cadre'
        
def getAge(x):
    f = len(x)
    try:
        age = int(str(x)[f-2:f])
    except:
        return np.nan
 

cleanSalary()