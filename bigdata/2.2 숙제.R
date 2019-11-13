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
} # timeseries_data function


#xt_plot 함수

xt_plot<-function(rho,nsmp,dtb,	#rho는 rho,nsmp는 표본 수,dtb는 분포
                  rnorm_mean=0,rnorm_std=1,	#정규분포 옵션(평균,표준편차)
                  runif_min=-1,runif_max=1,	#균등분포 옵션(최소,최대)
                  rcauchy_loc=0,rcauchy_scale=1	#코시분포 옵션(위치모수,척도모수)
)
{
  xt<-timeseries_data(rho,nsmp,dtb,	#rho는 rho,nsmp는 표본 수,dtb는 분포
                      rnorm_mean=0,rnorm_std=1,	#정규분포 옵션(평균,표준편차)
                      runif_min=-1,runif_max=1,	#균등분포 옵션(최소,최대)
                      rcauchy_loc=0,rcauchy_scale=1	#코시분포 옵션(위치모수,척도모수)
  )
  std_xt<-sd(xt)			    #Xt들의 표준편차를 구함.
  mean_xt<-mean(xt)			    #Xt들의 평균을 구함.
  xt_1<-xt[1:(length(xt)-1)]        #Xt들의 plot을 그릴 데이터 생성
  xt<-xt[2:length(xt)]
  
 
  #Xt와 Xt+1의 산점도
  plot(xt~xt_1,
       xlab='Xt',			#x축의 라벨지정
       ylab='Xt+1',		#y축의 라벨지정
       main=paste('rho = ',rho,', slope = ',round(lm(xt~xt_1+0)$coef[1],3)),#플롯의 이름지정
       xlim=c(mean_xt-4*std_xt,mean_xt+4*std_xt),	#x축의 범위지정 범위는 평균+- 4*표쥰편차
       ylim=c(mean_xt-4*std_xt,mean_xt+4*std_xt))	#y축의 범위지정 범위는 평균+- 4*표쥰편차
  abline(lm(xt~xt_1+0))	#회귀식을 받아서 플롯에 그리는 함수
  print(lm(xt~xt_1+0))	#회귀분석 결과를 출력. 알파 값과 베타 값을 확인 가능
} # xt_plot

#실험시작
par(mfrow=c(2,2))		#그래프를 2행2열의 매트릭스 형태로 그린다는 옵션
set.seed(123)
xt_plot(0,100,'rchisq')
xt_plot(1,100,'rchisq')
xt_plot(1.1,100,'rchisq')
xt_plot(-1.1,100,'rchisq')