# -*- coding: utf-8 -*-
import requests
import csv
from bs4 import BeautifulSoup


import sys  
reload(sys)  
sys.setdefaultencoding('ascii')  

data=[]

for id in range(7,125):
    s = "%03d" % id
    station = {
    's2elect':s,
    'submit': '確定'
    }

    res = requests.post('http://web.metro.taipei/c/TicketALLresult.asp',data=station)

    soup = BeautifulSoup(res.text)


    for part in soup.find_all("tr")[4:124]:
        if len(part)==7:
            entity=[]
            entity.append(part.find_all('td')[0].text.encode('latin-1').decode('utf-8'))
            entity.append(part.find_all('td')[2].text.encode('latin-1').decode('utf-8'))
            entity.append(part.find_all('td')[6].text.encode('latin-1').decode('utf-8'))
            data.append(entity)
        else:
            break
        

f = open("MRT_Time.csv","w")  
w = csv.writer(f) 
w.writerows(data)  
f.close()  
