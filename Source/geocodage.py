from geopy.geocoders import Nominatim
import pandas as pd

df = pd.read_csv('CSV/StartUpDataBase.csv', sep=';')


df['GPS'] = df[']apply(getGPSCoordinate)



def getGPSCoordinate(adress):
    geolocator = Nominatim()
    location = geolocator.geocode(adress)
    return (location.latitude, location.longitude)

    