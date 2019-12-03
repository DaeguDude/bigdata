# -*- coding: utf-8 -*-
'''
!!!!!!!!!실습과제 3. 정규분포 모집단의 모평균에 대한 검ㅈ멍통계량을 구하는 함수 one_t를 작성하라.
!!!!!!!!!1) 함수의 인수는 y, mu_0이다
!!!!!!!!!2) y의 표본평균 barY, 표본분산 Sy를 계산한다
!!!!!!!!!3) t_stat = (barY-mu_0) / sqrt(Sy/n) 을 계산한다 
4) t_stat의 양측 t-검정의 p-value인 p_val_t_stat을 계산한다
5) t_stat과 p_val_t_stat을 함께 list로 return한다
'''

import numpy as np
import math # sqrt쓰기위해서
import statistics as stat
from scipy import stats


t_val_and_p_val = []

def one_t(y, mu_0):
    #표본평균
    bar_y = stat.mean(y)
    #표본분산
    s_y = stat.variance(y)
    
    t_stat = (bar_y-mu_0) / math.sqrt(s_y/len(y))
    
    # degrees of freedom
    df = len(y)
    # 4)pvalue 
    p_val_t_stat = stats.t.sf(np.abs(t_stat), df-1)*2  # two-sided pvalue = Prob(abs(t)>tt)
    
    t_val_and_p_val.append(t_stat)
    t_val_and_p_val.append(p_val_t_stat)
    
    return t_val_and_p_val
    

# 시드지정
np.random.seed(123)
y = np.random.normal(size=30)


