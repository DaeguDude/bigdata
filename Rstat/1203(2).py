# -*- coding: utf-8 -*-

# 실습과제 5. 1부터 n까지 하나씩 홀수여부를 출력하는 함수 odd_f를 작성하라.
# 여기서 n을 이 함수 odd_f의 인수로 지정하라

# 예: 1은 홀수이다
# 예: 2는 홀수가 아니다

i = 1
print(i + '는 1입니다')
def odd_f(n):
    for i in range(1, n+1):
        if(i % 2 == 1):
            print(f'{i} 는 홀수이다')
        else:
            print(f'{i}는 홀수가 아니다')