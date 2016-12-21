import pandas as pd
import requests
from bs4 import BeautifulSoup
import pprint
import json
from datetime import datetime
from dateutil import rrule
from pandas import concat
import matplotlib.pyplot as plt
import datetime
import calendar


def getAcademies():
    url = "http://www.enseignementsup-recherche.gouv.fr/cid20269/liste-des-universites-francaises.html"
    # proxies = {"http": "http://147.215.1.189:3128/",}
    r = requests.get(url)
    # r = requests.get(url, proxies = proxies)
    # print(r.text)
    soup = BeautifulSoup(r.text)
    byAcademy = soup.find(class_="texte_contenu_max")
    Academies = byAcademy.find_all('p')
    Academy_list = []
    for academy in Academies:
   
        stringsplit = str(academy).split('"')
        try:
            Academy_list.append(stringsplit[3])
        except:
            pass
        
    Academy_list.remove('http://www.ac-spm.fr')
    Academy_list.remove('http://www.ac-wf.wf')
    print(Academy_list)

def getUniversitybyAcademy(url):
    url = 'http://www.enseignementsup-recherche.gouv.fr/cid20206/aix-marseille.html'
    r = requests.get(url)
    soup = BeautifulSoup(r.text)
    allUniv = soup.find(class_="texte_contenu_max")
    Universities = allUniv.find_all('p')
    names_list = []
    adress_list = []
    print(len(Universities))
    for i,univ in enumerate(Universities):
        print(i)
        if(i%2==0):
            univ.text
            names_list.append(univ.text)
        else:
            print('impair')
            adress_list.append(univ.text)
            
    print(len(names_list) ) 

getUniversitybyAcademy("")
    