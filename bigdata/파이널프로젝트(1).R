# install.packages("readxl")
library("readxl")


# past to stock split
before_stock_split <- read_excel("samsung-stock-price.xlsx", sheet=2)

# after stock split to present
after_stock_split <- read_excel("samsung-stock-price.xlsx", sheet=1)



# extract the price from the data
price_before_split = before_stock_split$종가
price_after_split = after_stock_split$종가



# timeseries graph for both before & after
plot(price_before_split, type="l",
     main="Samsung Stock Price Before Split",
     xlab="t", ylab="xt(price)")

plot(price_after_split, type="l", 
     main="Samsung Stock Price After Split",
     xlab="t", ylab="xt(price)")


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


test_z<-sign_test_normal(price_after_split,sign='eql')
test_z

#기울기들의 히스토그램
s = 0
for (i in 1:length(price_after_split)-2) {
  s[i] = (price_after_split[i+2] - price_after_split[i+1]) / 
         (price_after_split[i+1] - price_after_split[i])
}

hist(s, breaks=50, main="Si Distribution(n=386, Number of Si's=n-2)")


