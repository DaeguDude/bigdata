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

#t와 Xt간의 그림

# rnorm
par(mfrow=c(2,2))
set.seed(1234)
plot(timeseries_data(1,100,'rnorm'),xlim=c(0,50), ylim=c(-50,50), type='l',xlab='t',ylab='Xt',main='rho=1')
set.seed(1234)
plot(timeseries_data(0,100,'rnorm'),xlim=c(0,50), ylim=c(-50,50), type='l',xlab='t',ylab='Xt',main='rho=0')
set.seed(1234)
plot(timeseries_data(1.1,100,'rnorm'),xlim=c(0,50), ylim=c(-50,50),type='l',xlab='t',ylab='Xt',main='rho=1.1')
set.seed(1234)
plot(timeseries_data(-1.1,100,'rnorm'),xlim=c(0,50), ylim=c(-50,50),type='l',xlab='t',ylab='Xt',main='rho=-1.1')

# rcauchy
par(mfrow=c(2,2))
set.seed(1234)
plot(timeseries_data(1,100,'rcauchy'),xlim=c(0,50), ylim=c(-50,50), type='l',xlab='t',ylab='Xt',main='rho=1')
set.seed(1234)
plot(timeseries_data(0,100,'rcauchy'),xlim=c(0,50), ylim=c(-50,50), type='l',xlab='t',ylab='Xt',main='rho=0')
set.seed(1234)
plot(timeseries_data(1.1,100,'rcauchy'),xlim=c(0,50), ylim=c(-50,50),type='l',xlab='t',ylab='Xt',main='rho=1.1')
set.seed(1234)
plot(timeseries_data(-1.1,100,'rcauchy'),xlim=c(0,50), ylim=c(-50,50),type='l',xlab='t',ylab='Xt',main='rho=-1.1')


# x^2(1)-1 chi-squarepar(mfrow=c(2,2))
set.seed(1234)
plot(timeseries_data(1,100,'rchisq'),xlim=c(0,50), ylim=c(-50,50), type='l',xlab='t',ylab='Xt',main='rho=1')
set.seed(1234)
plot(timeseries_data(0,100,'rchisq'),xlim=c(0,50), ylim=c(-50,50), type='l',xlab='t',ylab='Xt',main='rho=0')
set.seed(1234)
plot(timeseries_data(1.1,100,'rchisq'),xlim=c(0,50), ylim=c(-50,50),type='l',xlab='t',ylab='Xt',main='rho=1.1')
set.seed(1234)
plot(timeseries_data(-1.1,100,'rchisq'),xlim=c(0,50), ylim=c(-50,50),type='l',xlab='t',ylab='Xt',main='rho=-1.1')


