## 기울기를 이용한 부호검정
sign_test_normal<-function(data,sign='eql')	#정규근사를 이용한 랜덤워크 검정함수
{		
  st<-0					#기울기 값이 들어갈 들어있는 개체 생성						
  for (i in 1:(length(data)-2))
  {
    st[i]<-(data[i+2]-data[i+1])/(data[i+1]-data[i])		#기울기구해서 넣음
    
  }
  st<-st[which(st!=Inf & st!=-Inf & st!=0)]  #Inf와 -Inf, 0는 제외 
  ###############################################################
  # Sanghak histogram 
  # hist(st, breaks=50)
  wt<-ifelse(st>0,1,0)		#기울기가 0보다 크면 1 아니면 0으로 변환
  b<-sum(wt,na.rm=TRUE)		#1인 것의 개수의 합을 구함
  statistic<-(b-(length(wt))/2)/sqrt((length(wt))/4)#검정통계량
  
  if(sign=='eql')		#H1 : rho!=1
  {
    if (statistic>0) {p_value=2*(1-pnorm(statistic))}		#p-value도출
    else {p_value=2*(pnorm(statistic))}
  }
  else if(sign=='lt')	#H1 : rho<1
  {
    p_value<-pnorm(statistic)
  }
  else if(sign=='gt')	#H1 : rho>1
  {
    p_value<-1-pnorm(statistic)
  }
  else
  {
    {geterrmessage('sign 은 eql , lt , gt만 가능합니다.')}
  }
  
  stat<-list(기울기=st,검정통계량=statistic,p_value=p_value,b=b)
  return(stat) 
}



# 삼성전자 매출, 이익, 분기
revenue = c(49.78, 50.94, 47.82, 53.33, 50.55, 61.00, 62.05, 65.98,
            60.56, 58.48, 65.46, 59.27, 52.39, 56.13, 62.00)

profit = c(6.68, 8.14, 5.20, 9.22, 9.90, 14.07, 14.53, 15.15,
            15.64, 14.87, 17.57, 10.80, 6.23, 6.60, 7.78)

quarter = c("2016_1Q", "2016_2Q", "2016_3Q", "2016_4Q", "2017_1Q", "2017_2Q", "2017_3Q", "2017_4Q",
            "2018_1Q", "2018_2Q", "2018_3Q", "2018_4Q", "2019_1Q", "2019_2Q", "2019_3Q")

# 삼성전자 데이터를 데이터프레임으로 만듬
samsung_data = data.frame(분기=quarter, 매출=revenue, 이익=profit)

# 그래프를 그리기 위하여 ggplt을 씀
library(ggplot2)


# 이익실적 그래프를 그림
# ------------------------------------------------------------------------ 
ggplot(samsung_data, aes(x=quarter)) + 
  # 매출
  geom_line(aes(y = revenue), group=1, color = "darkred") +
  geom_point(aes(y = revenue)) +
  
  # 이익
  geom_line(aes(y = profit), group=1, color="steelblue") +
  geom_point(aes(y = profit)) +
  ggtitle("Samsung Revenue and Profit Comparison")


# ------------------------------------------------------------------------
# 삼성전자 주식가격을 엑셀파일에서 읽고, 시도표를 그림
# install.packages("readxl")
library("readxl")

# 과거 -> 액면분할 전
before_stock_split <- read_excel("samsung-stock-price.xlsx", sheet=2)

# 액면분할 후 -> 현재
after_stock_split <- read_excel("samsung-stock-price.xlsx", sheet=1)


# 일별가걱 추출
price_before_split = before_stock_split$종가
price_after_split = after_stock_split$종가


# 액면분할 전, 후 시도표
plot(before_stock_split$종가, type="l",
     main="Samsung Stock Price Before Split",
     xlab="t", ylab="xt(price)")
plot(after_stock_split$종가, type="l", 
     main="Samsung Stock Price After Split",
     xlab="t", ylab="xt(price)")

# ------------------------------------------------------------------------
# 기울기 검정 수행
test_z<-sign_test_normal(price_after_split,sign='lt')

#기울기들의 히스토그램
s = 0
b = 0
for (i in 1:length(price_after_split)-2) {
  s[i] = (price_after_split[i+2] - price_after_split[i+1]) / 
    (price_after_split[i+1] - price_after_split[i])
}
s <-s[which(s!=Inf & s!=-Inf & s!=0)]  #Inf와 -Inf, 0는 제외
b <-ifelse(s>= 0,1,0) # s[i]가 0보다같거나크면 1, 아니면 0
b <- sum(b==1) # 1보다 큰 것의 갯수를 넣어줌
num_si = length(s) # inf, -inf, 0을 제외한 기울기 개수

# b들의 분포
hist(s, breaks=50, main="Si Distribution(number of Si's=356)")

# p value
# 동전던지기로 p value를 구할 것임
coinflip <- c("heads", "tails")

# I will store number of heads to b, each time the event occurs
# 기울기 개수 356개
# 앞면과 뒷면이 나올 확률이 같고(랜덤워크)
# 동전 던지기를 356번 한다면 185가 헤드가 나올 확률은 얼마일까?
# 그것이 p-value가 될 것임

# 동전던지기 356번을 시행하여 헤드의 개수를 num_heads에 넣음
# head가 나온 갯수를 세어서 num_heads에 넣음
# 이것을 50000번 시행하여 p-value 계산
b_s = 0
num_trials = 50000
for (i in 1:num_trials) {
  coinflip_result =  sample(coinflip, size = num_si, replace=TRUE)
  num_heads = sum(coinflip_result == 'heads')
  b_s[i] = num_heads
}

# b들의 히스토그램
hist(b_s, xlim=c(0,num_si), xlab="number of heads", main="b's histogram", freq=FALSE)
curve(dnorm(x, mean=mean(b_s), sd=sd(b_s)), add=TRUE, col="red")

# 50000번의 시행 중에서 185보다 작거나 같은 것이 몇 개 있는가?
b_less_eqaul_185 = sum(b_s<=b)

# 그것을 50000번으로 나누어 주면 p_value 나옴
p_value = b_less_eqaul_185 / num_trials

# ------------------------------------------------------------------------
# qqplot
