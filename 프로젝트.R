##1. 타임시리즈 데이터를 생성하는 함수
timeseries_data<-function(rho,nsmp,dtb,		#rho는 rho,nsmp는 표본 수,dtb는 분포
                          rnorm_mean=0,rnorm_std=1,	#정규분포 옵션(평균,표준편차)
                          runif_min=-1,runif_max=1,	#균등분포 옵션(최소,최대)
                          rcauchy_loc=0,rcauchy_scale=1,	#코시분포 옵션(위치모수,척도모수)
                          rchisq_df=1				#카이제곱분포 옵션(자유도)
)
{
  
  if(dtb=='rnorm'){n<-rnorm(nsmp,rnorm_mean,rnorm_std)}		#오차 생성
  else if (dtb=='runif'){n<-runif(nsmp,runif_min,runif_max)}
  else if (dtb=='rcauchy'){n<-rcauchy(nsmp,rcauchy_loc,rcauchy_scale)}
  else if (dtb=='rchisq'){n<-rchisq(nsmp,df=rchisq_df)-1}
  else{geterrmessage('이 함수는 정규분포, 균등분포,코시분포에서만 표본추출이 가능합니다.')}
  
  rh<-vector()
  xx<-vector()	#객체 생성
  
  for (i in 1:length(n)) 	# rho 생성 1, rho, rho**2, ... rho**(t-1)
  {
    rh[i]<-(rho)**(i-1)
  }		
  
  for (i in 1:length(n))
  {
    xx[i]<-sum(rh[1:i]*n[i:1])	#1*Et + rho*Et-1 + .....
  }
  return(xx)
}


#n=100일때 rho=-2,-1,-0.9,-0.5,0,0.5,0.9,1,2 데이터생성 예시
data_test<-timeseries_data(-2,100,'rnorm')
data_test
data_test<-timeseries_data(-1,100,'rnorm')
data_test
data_test<-timeseries_data(-0.9,100,'rnorm')
data_test
data_test<-timeseries_data(-0.5,100,'rnorm')
data_test
data_test<-timeseries_data(0,100,'rnorm')
data_test
data_test<-timeseries_data(0.5,100,'rnorm')
data_test
data_test<-timeseries_data(0.9,100,'rnorm')
data_test
data_test<-timeseries_data(1,100,'rnorm')
data_test
data_test<-timeseries_data(2,100,'rnorm')
data_test

data_test<-timeseries_data(-2,100,'rcauchy')
data_test
data_test<-timeseries_data(-1,100,'rcauchy')
data_test
data_test<-timeseries_data(-0.9,100,'rcauchy')
data_test
data_test<-timeseries_data(-0.5,100,'rcauchy')
data_test
data_test<-timeseries_data(0,100,'rcauchy')
data_test
data_test<-timeseries_data(0.5,100,'rcauchy')
data_test
data_test<-timeseries_data(0.9,100,'rcauchy')
data_test
data_test<-timeseries_data(1,100,'rcauchy')
data_test
data_test<-timeseries_data(2,100,'rcauchy')
data_test

data_test<-timeseries_data(-2,100,'rchisq')
data_test
data_test<-timeseries_data(-1,100,'rchisq')
data_test
data_test<-timeseries_data(-0.9,100,'rchisq')
data_test
data_test<-timeseries_data(-0.5,100,'rchisq')
data_test
data_test<-timeseries_data(0,100,'rchisq')
data_test
data_test<-timeseries_data(0.5,100,'rchisq')
data_test
data_test<-timeseries_data(0.9,100,'rchisq')
data_test
data_test<-timeseries_data(1,100,'rchisq')
data_test
data_test<-timeseries_data(2,100,'rchisq')
data_test

#error의 분포 확인
set.seed(123)
hist(rnorm(1000),breaks=30,probability=TRUE)#정규분포

set.seed(123)
hist(rcauchy(1000),breaks=100000,probability=TRUE)#코시분포
hist(rcauchy(1000),probability=TRUE)
hist(rcauchy(1000),breaks=30,probability=TRUE)
range(rcauchy(1000))

set.seed(123)
hist(rchisq(1000,df=1)-1,breaks=30,probability=TRUE)#카이제곱분포


#t와 Xt간의 그림
set.seed(1234)
plot(timeseries_data(-1.2,100,'rnorm'),type='l',xlab='t',ylab='Xt',main='rho=-1.2')
set.seed(1234)
plot(timeseries_data(-1.2,100,'runif'),type='l',xlab='t',ylab='Xt',main='rho=-1.2')
set.seed(1234)
plot(timeseries_data(-1.2,100,'rcauchy'),type='l',xlab='t',ylab='Xt',main='rho=-1.2')

set.seed(1234)
plot(timeseries_data(-1,100,'rnorm'),type='l',xlab='t',ylab='Xt',main='rho=-1')
set.seed(234)
plot(timeseries_data(-1,100,'runif'),type='l',xlab='t',ylab='Xt',main='rho=-1')
set.seed(3)
plot(timeseries_data(-1,100,'rcauchy'),type='l',xlab='t',ylab='Xt',main='rho=-1')

set.seed(1234)
plot(timeseries_data(-0.5,100,'rnorm'),type='l',xlab='t',ylab='Xt',main='rho=-0.5')
set.seed(1234)
plot(timeseries_data(-0.5,100,'runif'),type='l',xlab='t',ylab='Xt',main='rho=-0.5')
set.seed(1234)
plot(timeseries_data(-0.5,100,'rcauchy'),type='l',xlab='t',ylab='Xt',main='rho=-0.5')

set.seed(1234)
plot(timeseries_data(0,100,'rnorm'),type='l',xlab='t',ylab='Xt',main='rho=0')
set.seed(1234)
plot(timeseries_data(0,100,'runif'),type='l',xlab='t',ylab='Xt',main='rho=0')
set.seed(1234)
plot(timeseries_data(0,100,'rcauchy'),type='l',xlab='t',ylab='Xt',main='rho=0')

set.seed(1234)
plot(timeseries_data(0.5,100,'rnorm'),type='l',xlab='t',ylab='Xt',main='rho=0.5')
set.seed(1234)
plot(timeseries_data(0.5,100,'runif'),type='l',xlab='t',ylab='Xt',main='rho=0.5')
set.seed(234)
plot(timeseries_data(0.5,100,'rcauchy'),type='l',xlab='t',ylab='Xt',main='rho=0.5')

set.seed(123)
plot(timeseries_data(1,100,'rnorm'),type='l',xlab='t',ylab='Xt',main='rho=1')
set.seed(123)
plot(timeseries_data(1,100,'runif'),type='l',xlab='t',ylab='Xt',main='rho=1')
set.seed(123)
plot(timeseries_data(1,100,'rcauchy'),type='l',xlab='t',ylab='Xt',main='rho=1')

set.seed(123)
plot(timeseries_data(1.2,100,'rnorm'),type='l',xlab='t',ylab='Xt',main='rho=1.2')
set.seed(123)
plot(timeseries_data(1.2,100,'runif'),type='l',xlab='t',ylab='Xt',main='rho=1.2')
set.seed(123)
plot(timeseries_data(1.2,100,'rcauchy'),type='l',xlab='t',ylab='Xt',main='rho=1.2')



## 기울기를 이용한 부호검정
sign_test_normal<-function(data,sign='eql')	#정규근사를 이용한 랜덤워크 검정함수
{		
  st<-0					#기울기 값이 들어갈 들어있는 개체 생성						
  for (i in 1:(length(data)-2))
  {
    st[i]<-(data[i+2]-data[i+1])/(data[i+1]-data[i])		#기울기구해서 넣음
  }
  st<-st[which(st!=Inf & st!=-Inf & st!=0)]  #Inf와 -Inf, 0는 제외 
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


#기울기를 이용한 부호검정 실험

#rho=0.3, 정규분포, sign='eql'
set.seed(123)
data_test<-timeseries_data(0.3,100,'rnorm')
test_z<-sign_test_normal(data_test,sign='eql')
test_z

#rho=0.3, 정규분포, sign='lt'
set.seed(123)
data_test<-timeseries_data(0.3,100,'rnorm')
test_z<-sign_test_normal(data_test,sign='lt')
test_z

#rho=0.3, 정규분포, sign='gt'
set.seed(123)
data_test<-timeseries_data(0.3,100,'rnorm')
test_z<-sign_test_normal(data_test,sign='gt')
test_z

#b들의 히스토그램
data_z<-0
for (i in 1:100)				 #실험을 100번 반복.
{
  test<-timeseries_data(1,1000,'rnorm')#rho는 1, 데이터는 1000개 사용.
  data_test<-sign_test_normal(test)
  data_z[i]<-data_test[[4]]		 #b는 data_test의 4번째 리스트에 존재
}
hist(data_z,main='b들의 분포',breaks=30)


#pvalue들의 히스토그램
data_z<-0
for (i in 1:100)				 #실험을 100번 반복.
{
  test<-timeseries_data(1,1000,'rnorm')#rho는 1, 데이터는 1000개 사용.
  data_test<-sign_test_normal(test)
  data_z[i]<-data_test[[3]]		 #pvalue는 data_test의 3번째 리스트에 존재
}
hist(data_z,main='pvalue들의 분포')



#들의 히스토그램
data_z<-0
for (i in 1:100)				 #실험을 100번 반복.
{
  test<-timeseries_data(1,1000,'rnorm')#rho는 1, 데이터는 1000개 사용.
  data_test<-sign_test_normal(test)
  data_z[i]<-data_test[[2]]		 #pvalue는 data_test의 3번째 리스트에 존재
}
hist(data_z,main='pvalue들의 분포')


