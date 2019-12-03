revenue = c(49.78, 50.94, 47.82, 53.33, 50.55, 61.00, 62.05, 65.98,
            60.56, 58.48, 65.46, 59.27, 52.39, 56.13, 62.00)

profit = c(6.68, 8.14, 5.20, 9.22, 9.90, 14.07, 14.53, 15.15,
            15.64, 14.87, 17.57, 10.80, 6.23, 6.60, 7.78)

quarter = c("2016_1Q", "2016_2Q", "2016_3Q", "2016_4Q", "2017_1Q", "2017_2Q", "2017_3Q", "2017_4Q",
            "2018_1Q", "2018_2Q", "2018_3Q", "2018_4Q", "2019_1Q", "2019_2Q", "2019_3Q")

samsung_data = data.frame(분기=quarter, 매출=revenue, 이익=profit)

library(ggplot2)


# 이익실적 그래프를 그림
# ------------------------------------------------------------------------
# This has to be checked again, because I want to give it a name for
# each line
# ------------------------------------------------------------------------ 
ggplot(samsung_data, aes(x=quarter)) + 
  geom_line(aes(y = revenue), group=1, color = "darkred") +
  geom_point(aes(y = revenue)) +
  
  # 이익
  geom_line(aes(y = profit), group=1, color="steelblue") +
  geom_point(aes(y = profit))

# ------------------------------------------------------------------------
# 삼성전자 주식가격을 엑셀파일에서 읽고, 시도표를 그림
# install.packages("readxl")
library("readxl")

# after stock split to present
after_stock_split <- read_excel("samsung-stock-price.xlsx", sheet=1)

# past to stock split
before_stock_split <- read_excel("samsung-stock-price.xlsx", sheet=2)

# extract the price from the data
after_stock_split$종가
before_stock_split$종가


# timeseries graph for both before & after
plot(after_stock_split$종가, type="l", 
     main="Samsung Stock Price After Split",
     xlab="t", ylab="xt(price)")
plot(before_stock_split$종가, type="l",
     main="Samsung Stock Price Before Split",
     xlab="t", ylab="xt(price)")
