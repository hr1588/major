### Lotte Data #####
### Loading and installing packages ###
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(sqldf)){
  install.packages("sqldf")
  library(sqldf)
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if(!require(lubridate)){ #날짜 데이터를 변환
  install.packages("lubridate")
  library(lubridate)
}

# dplyr 라이브러리도 부착
library(dplyr)
library(ggplot2)

### Define analysis data ####
file=choose.files()
customer <- read.table(file, header=T, sep=",")
file1 = choose.files()
purchaseList <- read.table(file1, header=T, sep=",")

### 테이블 조인

tb <- sqldf("select a.id, a.성별, a.연령, b.거래일자, b.상품대분류명, b.상품중분류명, b.구매건수, b.거래식별ID, b.구매금액,b.점포ID
            from customer as a, purchaseList as b
            where a.id = b.id")

head(tb)

## Create date field ##

####### lubridate 패키지 이용 ###
tb$거래일자 <- ymd(tb$거래일자) # 문자 거래일자 데이터를 yyyy-mm-dd형태(date data)로 변환
tb$거래월 <- month(tb$거래일자) # month 함수 활용 거래월 필드 추가

class(tb$거래일자)

### Data exploration

purchaseList %>% group_by(점포ID) %>% 
  summarise(매출=sum(구매금액))

s1 <- tb %>% 
      group_by(거래월, 상품대분류명, 점포ID) %>%
      summarise(amount = sum(round(구매금액/1000,0)),summarise(cnt = sum(구매건수)))
      

head(s1) 

# 월별로, 점포 ID별로 boxplot

s1

with(s1,boxplot(amount~거래월))
with(s1,boxplot(amount~점포ID))
with(s1,boxplot(cnt~점포ID))

load(file = "data/s1.rda")

head(s1)
head(tb)

 
### 220427 대면수업

s1 <- subset(s1, 거래월!=10) # 거래월에서 10월을 제외

# 점포별 매출 상자그림

g <- ggplot(s1, aes(점포ID, amount, fill=점포ID, options(scipen = 100)))+
  geom_boxplot() +
  scale_fill_brewer(palette="Set2")+ # 색상 지정
  stat_summary(fun.y = "mean",  fill="red", geom = "point", shape = 23, size = 3)+ # 평균 지점에 빨간 마름모 점 생성
  stat_boxplot(geom = "errorbar") +
  labs(title = "롯데 매장 매출액 Boxplot", subtitle = "매출액 vs 점포명", x="점포명", y="매출액", 
       caption = "source : 롯데데이터, red = 평균") # 제목, 부제목, 축 제목, 캡션 설정

?stat_summary
g

(g1 <- ggplot(s2,aes(상품대분류명, amount))+
      geom_point(aes(col=점포ID, size=cnt))+
      labs(title = "Bubble Chart", subtitle = "점포별 : 품목 vs 매출액",
           x = "품목", y = "매출액"))

# 데이터 수정(차트가 일관적이지 못함)

s2 <- tb %>% 
  group_by(상품대분류명, 점포ID) %>%
  summarise(amount = sum(round(구매금액/1000,0)),cnt = sum(구매건수))


### 220504 대면수업

head(s1)

s1$amount = round(s1$amount/1000,0)

(g2 <- ggplot(s1, aes(거래월, 점포ID))+
  geom_tile(aes(fill=amount))+ # 변수가 3개 이상일 때 사용하면 유용
  scale_fill_gradientn(colors = brewer.pal(n=5, name = "RdBu"))) # n은 색의 수를 정해야함


(g2 <- ggplot(s1, aes(거래월, 상품대분류명))+ # x, y축 지정
    geom_tile(aes(fill=amount))+ # 색상 범주 지정(양의 정도)
    facet_wrap(~점포ID)+ # 점포ID별 세분화
    scale_fill_gradientn(colors = brewer.pal(n=5, name = "RdBu")))

(g2 <- ggplot(s1, aes(거래월, 상품대분류명))+ # x, y축 지정
    geom_tile(aes(fill=cnt))+ # 색상 범주 지정(양의 정도)
    facet_wrap(~점포ID)+ # 점포ID별 세분화
    scale_fill_gradientn(colors = brewer.pal(n=5, name = "RdBu"))) # 색상 그라이언트 설정, 색깔 개수 지정


# RFM으로 고객을 분류한 데이터를 불러옴
library(didrooRFM)
library(sqldf)

customerData <- subset(tb, select=c(거래식별ID, ID, 거래일자, 구매금액)) # findRFM에 맞게 데이터 설정
result <- findRFM(customerData,4,3,3)
table(result$FinalCustomerClass)

load(file = "data/data.rda") # rda 파일 가져오기
table(data$FinalCustomerClass, data$점포ID) # 점포별 고객 클래스 확인

(g <- ggplot(data, aes(점포ID)) + # x축 지정
  geom_bar(aes(fill = FinalCustomerClass), position = position_stack(reverse = TRUE)))

(g <- ggplot(temp, aes(x=점포ID, y=amount)) +  # x축 범주형, y축 계량형(수치)
    geom_point(aes(col=FinalCustomerClass)))  # ggplot 종류 선택, 색깔 채울 요소 지정(묶음)

temp<- data %>% group_by(점포ID, FinalCustomerClass) %>% 
  summarize(amount=mean(amount), cnt=mean(cnt))


################ 과제
library(tidytext)

a <- tb %>% 
  group_by(성별,상품대분류명) %>%
  summarise(amount = sum(round(구매금액/1000,0)), cnt = sum(구매건수))

(a1 <- ggplot(a,aes(reorder(상품대분류명,amount), amount))+
  geom_col(aes(fill=성별))+
  coord_flip() +
  facet_wrap(~ 성별, scales = "free_y")+
  labs(x="품목", y ="구매금액", title = "성별 상품 구매금액",
       subtitle = "F = 여성 / M = 남성")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20, color = "#004C99"),
        axis.title = element_text(size = 13, color = "#336600"),
        legend.text = element_text(face = "bold", size = 13),
        legend.title = element_text(face = "bold", size=15),
        legend.box.background = element_rect(fill = "#9999FF"), 
        legend.box.margin = margin(5,5,5,5),
        plot.subtitle = element_text(hjust = 1, size = 10, face = "bold")))+
  scale_y_continuous(breaks = seq(0,30000,5000))




