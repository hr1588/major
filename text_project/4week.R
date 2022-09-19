mtcars

## SUBSET 함수

?subset

# 조건식이 참인 데이터만 추출, select를 이용해 조건식이 참인 필드만 추출 가능
subset(mtcars, subset = (mpg>20), select = mpg) # subset(데이터셋, subset = (조건식), select = 필드명)

subset(mtcars, subset = (cyl == 4 & am == 0), select = c(mpg,hp,wt,am))

subset(mtcars, subset = (mpg < mean(mpg)), select = mpg)


## 상관분석

?USArrests

a = subset(USArrests, select = -UrbanPop) # urbanpop 변수를 제외한 나머지를 추출
?cor()
cor(a)
b = subset(USArrests, select = c(Murder, Assault))
cor(b)

## tibble < tibble(), tribble(), as_tibble(), print()

as.data.frame()
tibble()

install.packages("tibble")
library(tibble)

v1 <- c("a001","a002","a003")
v2 <- c("Mouse","Keyboard","USB")
v3 <- c(30000,35000,40000)

product <- tibble(id=v1,name=v2,price=v3)
copy <- data.frame(id=v1,name=v2,price=v3)

str(product)
str(copy)

# ~뒤에 필드명을 기입하고, 열마다 줄을 바꿔서 tibble과 동일한 데이터셋 제작 가능

tribble(
  ~id, ~name,~price,
  "a001","Mouse",30000,
  "a002","Keyboard",35000,
  "a003","USB",40000
)

d = tibble(id=c(1,2,3),
       data = list(tibble(x=1, y=2),
                   tibble(x=4:5,y=6:7),
                   tibble(x=10)))
d[[2]]
d$data
d[1] # tibble 형태 id 출력
d[2] # tibble 형태 data 출력
str(d[[1]]) # numeric
str(d[1]) # tibble

d[2] # tibble 형태만 출력
d[[2]] # tibble 형태 list 출력
str(d[[2]]) # tibble
str(d[2]) # tibble

d[2][1] # d[2]와 동일
d[2][[1]] # d[[2]]와 동일

d[[2]][1] # data의 첫번째 list 출력
d[[2]][[1]] # 위와 동일 형태

typeof(d$data[2]) # list
typeof(d$data[[2]]) # list


## as_tibble

str(iris)
iris

as_tibble(iris) # head를 치지않아도 10개 행까지만 보이고, 열도 화면에 맞춰서 출력, 깔끔한 출력이 장점

install.packages("Lahman")
library(Lahman)

str(Batting)
Batting
tail(Batting)

batting.tbl <- as_tibble(Batting)
batting.tbl

## apply (반복문, for,while 함수보다 효과적인 경우가 많음)

?apply
x <- matrix(1:20,4,5)
x

apply(X=x, MARGIN = 1, FUN = max) # margin = 1 (행), 각각의 행에서 fun(최대값) 출력
apply(X=x, MARGIN = 2, FUN = max) # margin = 2 (열), 각각의 열에서 fun(최대값) 출력
apply(X=x, MARGIN = 2, FUN = mean) # 열에 대한 평균
apply(X=x, MARGIN = 2, FUN = min)  # 열에 대한 최솟값
apply(X=x, MARGIN = 2, FUN = sum)  # 열에 대한 합

y = array(1:24,c(4,3,2))
y

apply(y,1,paste, collapse = ",") # 행을 중심으로 서로 다른차원의 행을 , 구분자를 사용해 결합

a = c(1,5,9,13,17,21)
a

paste(a)
paste(a, collapse = ",") # a를 , 구분자로 하나로 결합

apply(y, 2, paste, collapse = ",") # 열로 결합
apply(y, 3, paste, collapse = ",") # margin = 3 (차원을 기준으로 결합)

apply(y, c(1,2),paste,collapse=",") # 첫번째 차원과 두번째 차원의 동일한 위치에 있는 데이터를 결합

str(Titanic)
Titanic

apply(Titanic, 1, sum) # 등급별로 몇명이 탑승했는가?
apply(Titanic, 4, sum) # 생존
apply(Titanic, 2, sum) # 성별 
apply(Titanic, 3, sum) # 성인과 아동

apply(Titanic, "Class", sum) # 숫자 대신 필드명 사용 가능
apply(Titanic, c(1,4), sum) # 등급별 생존 현황

## lapply(list형태로 리턴), sapply(항상 list 형태가 아닌 데이터 타입을 자동적으로 지정)
# 같은 작업을 수행하지만, 리턴되는 결과가 다름
# lapply(데이터셋, 함수) / sapply(데이터셋, 함수)

exams <- list(spring_2020 = c(78,60,89,90,96,54),
              spring_2021 = c(85,78,69,90,95),
              spring_2022 = c(98,96,94,89,99,100,87),
              spring_2023 = c(86,98,76,89,57,79))
exams

lapply(exams, length) # 매년 수강한 학생은 몇명인가?
sapply(exams, length)

mode(sapply(exams, length))
mode(lapply(exams, length))

sapply(exams, mean) # 매해 시험 성적의 평균
sapply(exams, sd) # 매해 시험 성적의 표준편차

sapply(exams, range) # 매해 시험 성적의 최솟값과 최댓값, 자동적으로 2가지를 가져오는 matrix 구조
lapply(exams, range) # list 형태로 리턴

str(iris)
lapply(iris, class) # 각각 변수들의 데이터 타입 확인

sapply(iris, class)
sapply(iris, mean) # 변수들에 대한 평균치

a = subset(iris, select = -(Species))
sapply(a, mean)

sapply(iris, function(x) ifelse(is.numeric(x),mean(x),NA)) # ifelse문 활용



