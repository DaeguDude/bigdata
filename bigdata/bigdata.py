
import requests
from bs4 import BeautifulSoup
import pandas as pd

url = 'https://finance.naver.com/item/sise.nhn?code=005930'

# send a HTTP request to the URL of the webpage I want to access
r = requests.get(url)

data = r.text

# making the soup
soup = BeautifulSoup(data, 'html.parser')


# frame is the place where we will store all the lists of stock prices
frames = []

# now we will scrape from page 1 to page 590. Where the samsung stock started
for i in range(1, 591):
  newurl= "https://finance.naver.com" + soup.find('iframe', attrs={'title': '일별 시세'})['src']
  newurl = f'{newurl}&page={i}'
  dfs=pd.read_html(newurl)
  df=dfs[0]
  df = df.dropna(how='any', axis=0)
  frames.append(df)

# All the stock prices are merged into one dataframe 
stock_data = pd.concat(frames)
print(type(stock_data))


# writer = pd.ExcelWriter('samsung-stock.xlsx')
# stock_data.to_excel(writer, 'Sheet1', index=False)
# writer.save()












