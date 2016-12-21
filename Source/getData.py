import pandas as pd
import requests
from bs4 import BeautifulSoup
import pprint
import numpy as np

def getStartUpDataBase():
    dic = getStartUpListUrl()
    index_number =0
    for k in dic :
        index_number = index_number + len(dic[k])
        
    df = pd.DataFrame(index = range(index_number), columns=getIds())
    i=0
    for k in dic:
        for startup in dic[k]:        
            dic_startup = getInformations(startup)
            dic_startup['Classe'] = k
            df.loc[i]=pd.Series(dic_startup)
            i=i+1
            
    print(df.head())
    df.to_csv('StartUpDataBase_type.csv', sep=';')
        
        

def getStartUpListUrl():
    url = "http://www.usine-digitale.fr"
    url_appli = "/annuaire-start-up/application/"
    url_web = "/annuaire-start-up/start-up-du-web/"
    url_energie ="/annuaire-start-up/start-up-energie-et-environnement/"
    url_info = "/annuaire-start-up/start-up-informatique/"
    url_bio ="/annuaire-start-up/start-up-biotech-et-sante/"
    url_eco = "/annuaire-start-up/start-up-economie-et-social/"
    url_industrie = "/annuaire-start-up/start-up-industrie/"
    url_loisir ="/annuaire-start-up/start-up-jeux-et-loisirs/"
    url_list_category = [url_appli, url_web, url_energie, url_info, 
                url_bio, url_eco, url_industrie, url_loisir]
    
    dic_cat = {"http://www.usine-digitale.fr/annuaire-start-up/application/":'application',"http://www.usine-digitale.fr/annuaire-start-up/start-up-du-web/":"web",
            "http://www.usine-digitale.fr/annuaire-start-up/start-up-energie-et-environnement/":'environnement', "http://www.usine-digitale.fr/annuaire-start-up/start-up-informatique/":"informatique", 
            "http://www.usine-digitale.fr/annuaire-start-up/start-up-biotech-et-sante/":"sante", "http://www.usine-digitale.fr/annuaire-start-up/start-up-economie-et-social/":"eco_social",
            "http://www.usine-digitale.fr/annuaire-start-up/start-up-industrie/":'industrie', "http://www.usine-digitale.fr/annuaire-start-up/start-up-jeux-et-loisirs/":"jeux_loisirs"}
 
    url_list_category = [url+x for x in url_list_category]         
    dic_url_startups = {}           
    for category in url_list_category:
        print(category)
        all_pages = getAllPagesByCategory(category)
        print(len(all_pages))
        list_startups_cat = []
        for page in all_pages:
            list_startups = getStartUpsOnPage(page)
            list_startups_cat = list_startups_cat + list_startups
        dic_url_startups[dic_cat[category]] = list_startups_cat
    return dic_url_startups
    
def getAllPagesByCategory(url):
    # url="http://www.usine-digitale.fr/annuaire-start-up/start-up-du-web/"
    r = requests.get(url)
    soup = BeautifulSoup(r.text)
    nums = soup.find_all(class_='num')
    number_pages = int(nums[len(nums)-1].text)
    list_pages = [url]
    for i in range(2,number_pages+1):        
        list_pages.append(url + str(i) + '/')        
    return list_pages
    
def getStartUpsOnPage(url):
    # url="http://www.usine-digitale.fr/annuaire-start-up/start-up-du-web/"
    r = requests.get(url)
    soup = BeautifulSoup(r.text)
    list_url = []
    sections = soup.find_all(class_="blocType1")
    for section in sections:
        list_url.append("http://www.usine-digitale.fr" + section.a['href'])
        
    return list_url
    
    
def getInformations(url):
    # url="http://www.usine-digitale.fr/annuaire-start-up/studizen,347395"
    r = requests.get(url)
    soup = BeautifulSoup(r.text)
    name = soup.find(class_="titreFicheStartUp").text
    startup_dic = {}
    startup_dic['name']=name
    infos = soup.find(id="infoPratiq").find(class_="deco").find_all('li')
    for info in infos:
        key = info.find('div').text
        value = info.find('p').text
        startup_dic[key]=value    
    return startup_dic
    
    
def getIds():
    url="http://www.usine-digitale.fr/annuaire-start-up/studizen,347395"
    r = requests.get(url)
    soup = BeautifulSoup(r.text)
    Id_list =['Classe']
    infos = soup.find(id="infoPratiq").find(class_="deco").find_all('li')
    for info in infos:
        key = info.find('div').text
        Id_list.append(key)
        
    return Id_list

getStartUpDataBase()
    
# getAllPagesByCategory("")
# getStartUpsOnPage("")
# getStartUpDataBase()
