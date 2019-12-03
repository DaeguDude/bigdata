
import requests
from bs4 import BeautifulSoup
import pandas as pd

url = 'https://finance.naver.com/item/sise.nhn?code=005930'

# send a HTTP request to the URL of the webpage I want to access
r = requests.get(url)

data = r.text

# making the soup
soup = BeautifulSoup(data, 'html.parser')



newurl= "https://finance.naver.com" + soup.find('iframe', attrs={'title': '일별 시세'})['src']
newurl = (newurl + "&page=1")
dfs=pd.read_html(newurl)
df=dfs[0]
df1 = df.dropna(how='any', axis=0)

newurl= "https://finance.naver.com" + soup.find('iframe', attrs={'title': '일별 시세'})['src']
newurl = (newurl + "&page=590")
dfs=pd.read_html(newurl)
df=dfs[0]
df2 = df.dropna(how='any', axis=0)

frames = [df1, df2]
result = pd.concat(frames)
print(result)




