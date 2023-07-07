# 예제 1
x <- c(2,5,8,5,7,10,11,3,4,7,12,15)
y <- c(1:12)
plot(x,y,main = "메인",sub = "부제", xlab = "숫자", ylab = "value") # 산점도

op <- par(mfrow=c(1,2))

plot(cars$speed, cars$dist, main = "산점도", sub = "변수 관계",
     cex = 0.5, col = "#FF0000", xlab = "속도", ylab = "거리",
     xlim = c(0,30), ylim = c(0,140), pch = 0)

text(10,20, "확인", col = "blue")

# lines
par(mfrow = c(1,1))
plot(cars, main = "speed")
lines(lowess(cars), lty = 2, lwd = 1)

# abline(회귀분석)
# cars 데이터를 이용하여 속도에 대한 제동거리의 신뢰구간을 구하라.

z <- lm(dist~speed, data = cars) # 종속 : dist, 독립 : speed
plot(cars, main = "제동 거리")
abline(z, col = "red")
abline(h=mean(cars$dist), lty = 2, col="blue")
abline(v=mean(cars$speed), lty = 2, col="green")

# boxplot 예제
v1 <- c(10,12,15,11,20)
v2 <- c(5,7,15,8,9)
v3 <- c(11,20,15,18,13)

boxplot(v1,v2,v3,col=c("blue","yellow","pink"),
        names = c("blue","yellow","pink"),
        horizontal = T)

# histogram
hist(cars$speed)
legend("topright",c("Speed 빈도"))

# histogram 연습
x <- rnorm(100, mean = 5, sd =1) # 평균이 5이고 표준편차가 1인 정규분포에서 100의 샘플 생성
hist(x)
hist(x, freq = F) # freq = F : 빈도가 아닌 밀도로 표시
curve(dnorm(x, mean = 5, sd = 1), add = T) # 평균이 5이고 표준편차가 1인 정규분포의 밀도 함수 곡선
# add = T : 겹쳐 그림

graphics.off()
par(mfrow=c(1,2))

height <- c(182,175,167,172,163,178,181,150,166,155)
hist(height, main = "height")
hist(height, main = "height", prob = T, col = 'blue')
lines(density(height), col = 'red') # 추정된 확률밀도를 그래프로 그림

stem(height) # 줄기와 잎 그림

# barplot
library(RColorBrewer)
graphics.off()

VADeaths

# 묶은 막대그래프
barplot(VADeaths, col = heat.colors(5),
        border = "dark blue", legend = rownames(VADeaths), beside = T)
title(main = list("사망율", font = 2))

# 누적 막대그래프
barplot(VADeaths, col = brewer.pal(5,"GnBu"),
        border = "dark blue", legend = rownames(VADeaths))
title(main = list("사망율", font = 2))

# barplot 활용(중요) 문제3 관련

data = read.csv("파일 이름", header = T)
str(data)
t <- aggregate(data$count, by = list(area = data$region), sum) # 지역별로 count를 sum
data <- t$x
names(data) <- t$area
barplot(sort(data, decreasing = T), main = paste("부산시 주요구별 과목별 병원현황"),
        beside = T, ylab = "병원수", ylim = c(0,300), col = brewer.pal(16,"RdYlGn"))
abline(h = seq(0,300,50), lty = 3, lwd = 0.2)

# 모자이크 그림

c <- data$count
dim(c) <- c(10,16)
rownames(c) <- data$medical[1:10] # 행 이름 부여
colnames(c) <- unique(data$region) # 열 이름 부여
mosaicplot(c, color = rainbow(10), main = "")
c1 <- t(c)
mosaicplot(c1, color = rainbow(10), main = "")

#### 문제 4,5 ggplot 사용
library(ggplot2)

#ggplot bar chart -> same output

ggplot(diamonds, aes(clarity, fill = cut))+geom_bar()

# 1단계 : 미적 매핑
# ggplot을 생성하면서 데이터와 미적 매핑 정보를 지정
g <- ggplot(data= diamonds, aes(carat,price))

# 2단계 : 통계 변환
# 3단계 : 기하 객체 적용, 각각의 개하 객체는 미적 매핑 정보를 상속
# 기하객체를 여러 개 지정할 경우, 레이어 형태로 중첩되어 표시

g <- g+geom_point(aes(color = clarity))

# 상속 받은 x,y와 지정한 color 정보를 사용하여 산점도를 그림

g <- g+geom_smooth() # 상속 받은 x,y로 회귀선을 그림
g

# 4단계 : 위치 조정(생략)
g <- g+facet_wrap(~cut)
g <- g+labs(title = "차트", x = "무게", y = "가격")
summary(g) # 그래프의 정보 표시

# mpg 데이터
# 모델 : 범주형, 배기량 : 연속형, 실린더 : 이산형

table(mpg$manufacturer)

mpg_select = mpg[mpg$manufacturer %in% c("audi","ford","hyundai","toyota"),]

# bubble chart
(g <- ggplot(mpg_select,aes(displ,cty))+
    geom_point(aes(col=manufacturer, size = hwy))+
    labs(title = "버블 차트"))

# 상관계수(선형 관계 확인)
library(ggcorrplot)

data <- subset(mpg, select = c(displ,cty,hwy,cyl)) 
corr <- round(cor(data),3)

ggcorrplot(corr, hc.order = T,
           type = "upper",
           lab = T,
           lab_size = 3,
           method = "circle",
           colors = c("tomato2","white","springgreen3"),
           title = "correlogram",
           ggtheme = theme_bw())

# Marginal Hist/Boxplot(여러 개)
library(ggExtra)

g <- ggplot(mpg,aes(cty,hwy))+
  geom_count()+
  geom_smooth(method = "lm", se = F)

ggMarginal(g, type = "histogram", fill = "transparent")
ggMarginal(g, type = "boxplot", , fill = "transparent")
ggMarginal(g, type = "density", , fill = "transparent") # 정규 분포

# boxplot
g <- ggplot(mpg, aes(class,cty))
g + geom_boxplot()+
  labs(title = "boxplot",
       caption = "source : mpg")+
  theme_bw()

# viloin chart
g <- ggplot(mpg, aes(class,cty))
g + geom_violin()+
  labs(title = "viloin",
       caption = "source : mpg")+
  theme_bw()

# density plot
g <- ggplot(mpg, aes(cty))
g + geom_density(aes(fill = factor(cyl)), alpha = 0.5)+
  scale_fill_brewer(palette = "Pastel1")+
  labs(title = "분포차트")+
  theme(legend.position = "botton")

## File input/output
library(xlsx)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(sqldf)
library(lubridate)

###### 권역별 인구, grdp

file <- choose.files() #file open
popData <- read.xlsx(file, 1, encoding = "UTF-8")
str(popData) #데이터 구조확인
dim(popdata)

popdata.temp = select(popdata, area, sido, pop10, pop18) # subset보다 간결하고 빠름
popdata.area = summarise(group_by(popdata.temp,area),year10=sum(pop10, na.rm = T),year18=sum(pop18, na.rm = T))
popdata.area = mutate(popdata.area,year10.pcnt = year10/sum(year10)*100, year18.pcnt = year18/sum(year18)*100,diff.pcnt=year18.pcnt-year10.pcnt)
popdata.area

###### 권역별 연도 인구 비율 ㅊ이

newdata = read.xlsx(file.choose(),1)
View(newdata)
newdata$area = factor(newdata$area,levels = c(1:7), labels = c("수도권","동남권","대경권","충청권","전라권","강원도","제주도"))

newdata.area = summarise(group_by(newdata,area), year10 = sum(pop10, na.rm = T),  year18 = sum(pop18, na.rm = T),
                         year20 = sum(pop20, na.rm = T))
newdata.area
newdata.area = mutate(newdata.area, year10.pcnt = year10/sum(year10)*100, year20.pcnt = year20/sum(year20)*100,diff.pcnt=year20.pcnt-year10.pcnt)

names(newdata.area) = c("지역","2010년","2018년","2020년","2010년 비율","2020년 비율","10년간 인구 비율 차이")

###### sqldf 활용 

pop.area<-sqldf("select area, sum(pop10) as area10, sum(pop18) as area18 
       from popData group by area")
tot10<-sum(pop.area$area10)
tot18<-sum(pop.area$area18)
pop.pnt10 <- pop.area$area10/tot10 * 100
pop.pnt18 <- pop.area$area18/tot18 * 100
pop.area1 <- cbind(pop.area, pop.pnt10, pop.pnt18)


###### pie를 활용한 지역별 인구, grdp 현황 ########

file = file.choose()
popData <- read.csv(file, header=T)
popData
pop
pop <- ddply(popData, .(area), summarise, pop.a10 = sum(pop10, na.rm = T),
             pop.a20 = sum(pop20, na.rm = T), grdp.a10 = sum(grdp10,na.rm = T),
             grdp.a20 = sum(grdp20,na.rm = T))

typeof(popData$pop20)
as.numeric(popData$pop20)


pop.pcnt10 <- with(pop,round(pop.a10 / sum(pop.a10)*100,1)) 
pop.pcnt20 <- with(pop,round(pop.a20 / sum(pop.a20)*100,1))
label1 <- paste(pop$area,"(",pop.pcnt10,"%)")
label2 <- paste(pop$area,"(",pop.pcnt20,"%)")
par(mfrow=c(1,2)) # 1?? 2??
with(pop, pie(pop.a10, labels = label1, col = rainbow(length(area)),
              main = "2010년 지역별 인구현황"))
with(pop, pie(pop.a20, labels = label2, col = rainbow(length(area)),
              main = "2020년 지역별 인구현황"))

pop
grdp.pcnt10 = with(pop,round(grdp.a10 / sum(grdp.a10)*100,1))
grdp.pcnt20 = with(pop,round(grdp.a20 / sum(grdp.a20)*100,1))

label3 = paste(pop$area,"(",grdp.pcnt10,"%)")
label4 = paste(pop$area,"(",grdp.pcnt18,"%)")

par(mfrow=c(2,2))
with(pop, pie(pop.a10, labels = label1, col = rainbow(length(area)),
              main = "2010년 지역별 인구현황"))
with(pop, pie(pop.a20, labels = label2, col = rainbow(length(area)),
              main = "2020년 지역별 인구현황"))
with(pop, pie(grdp.a10,labels = label3, col = rainbow(length(area)),
              main = "2010년 지역별 GRDP현황"))
with(pop, pie(grdp.a20,labels = label4, col = rainbow(length(area)),
              main = "2020년 지역별 GRDP현황"))


# boxplot

bp <- ggplot(iris, aes(Species, Sepal.Length))+ # ggplot(data, aes(x,y))
  geom_boxplot(aes(fill=Species))+ # geom_boxplot(aes(fill=채울 내용))
  theme_minimal() +# 테마 변경 theme_
  theme(legend.position = "top") # 범례 위치 조정(위로)

(bp + scale_fill_brewer(palette = "Set1")) # 색상 지정, Dark1, Set1,...

# 산점도

sp = ggplot(iris, aes(Sepal.Width, Sepal.Length))+
  geom_point(aes(color = Species))+
  theme_classic()+
  theme(legend.position = "bottom")+
  ggtitle("* IRIS Scatter Plot *")

(sp + scale_fill_brewer(palette = "Set3"))

# viridis color : sequential color
library(viridis)

sp = ggplot(iris, aes(Sepal.Width, Sepal.Length))+
  geom_point(aes(color = Sepal.Width))+
  scale_color_viridis(option = "D")+
  theme_classic()+
  theme(legend.position = "bottom")+
  ggtitle("* IRIS Scatter Plot *")


###### RFM 분석 ########## 자세한 내용은 6주차 수업 코드 참고
library(didrooRFM)
library(dplyr)

TransNo <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
CustomerID <- c('c01', 'c02', 'c03', 'c06', 'c05', 'c01', 'c02', 'c01', 'c03',
                'c01', 'c04', 'c07', 'c06', 'c02', 'c08', 'c08', 'c09', 'c10')
DateofPurch <- as.Date(c("2016-01-15", "2016-02-20", "2016-04-10", "2016-07-15",
                         "2016-07-15", "2017-01-15", "2017-03-10", "2017-05-10",
                         "2017-05-10", "2017-06-05", "2017-07-01", "2017-09-15",
                         "2017-10-10", "2017-11-12", "2017-12-03", "2017-12-10",
                         "2017-12-20", "2017-12-30"))
Amount <- c(24000, 10000, 12000, 60000, 110000, 15000, 60000, 30000, 8000, 18000,
            20000, 15000, 50000, 30000, 6000, 7000, 18000, 8000)

# RFM 입력 데이터 프레임 작성
customerData <- data.frame(TransNo, CustomerID, DateofPurch, Amount)

# dplyr 활용 고객별 RFM 분석

data <- customerData %>%
  group_by(CustomerID) %>% 
  summarise(F = n(),
            M = sum(Amount),
            Date = max(DateofPurch))

### data file 읽음 
file <- choose.files() #file open
sale <- read.csv(file,header=T)
### data 정리
sale <- rename(sale, cust_id = 癤풻ust_id)
head(sale)
tId <- c(1:298)
sale$sale_date <- as.Date(sale$sale_date)
sale <- cbind(tId, sale)
saleResult <- findRFM(sale)
saleResult$FinalCustomerClass
table(saleResult$FinalCustomerClass)
saleResult$FinalScore
with(saleResult, boxplot(MeanValue~FinalCustomerClass))
with(saleResult, boxplot(NoTransaction~FinalCustomerClass))

with(saleResult, boxplot(FinalScore~FinalCustomerClass,
                         col=brewer.pal(4, "Pastel2"),
                         xlab="고객분류",
                         ylab="평균거래금액"))
hist(saleResult$MeanValue)
hist(saleResult$NoTransaction)
hist(as.numeric(saleResult$LastTransaction))

# 기준날 - 최근 구매 날짜 

data$R = as.numeric(as.Date("2017-12-31") - as.Date(data$Date)) 

# 4분위수, probs = c() => 분위 수 지정

quantile(data$R, probs = c(0.2,0.4,0.6,0.8))
quantile(data$F, probs = c(0.2,0.4,0.6,0.8))
quantile(data$M, probs = c(0.2,0.4,0.6,0.8,0.9))

# 지정된 분위 수의 총 구매 금액에 따른 고객별 점수 부여(M)

data$Mscore = ifelse(data$M >= 110000, 5 , 
                     ifelse(data$M >= 46800,4,
                            ifelse(data$M >= 19200,3,
                                   ifelse(data$M >=14600,2,1))))


# RFM 점수 계산, findRFM은 기본적으로 5분위수 사용 findRFM(데이터셋, a,b,c) 이떄 a,b,c는 RFM의 가중치, 가중평균 return

result <- findRFM(customerData, 4,3,3)

# 고객 클래스 별 RFM 평균

RFM = result %>% 
  group_by(FinalCustomerClass) %>% 
  summarise(frq = n(),
            sal = mean(MeanValue),
            rec = mean(R))

result$R = as.numeric(as.Date("2017-12-31") - as.Date(result$LastTransaction)) 


##### mtcars 활용 시각화 cyl = 기통수 hp = power wt = 무게 mpg = 연비 disp = 배기량

(mc = ggplot(mtcars, aes(disp,mpg))+
    geom_point(aes(color=hp, size=wt, shape = as.factor(cyl)))+
    scale_color_viridis(option = "D")+
    scale_size(range = c(0,6))+
    theme_minimal()+
    theme(legend.position = "top")+
    xlab("배기량")+ylab("연비")+
    labs(title = "자동차 32대 연비와 배기량"))


###### cars 활용 산점도 

plot(cars$speed, cars$dist, main="산점도 그림", sub="두변수간의 관계",
     cex=0.5, col="#FF0000", xlab="속도", ylab="거리",
     xlim=c(0,30), ylim=c(0, 140), pch=0)
text(10, 20, "확인하세요", col="blue")

###### abline(회귀선)

z <- lm(dist ~ speed, data=cars)
plot(cars, main="Stopping Distance versus Speed")
abline(z, col="red")
abline (h=mean(cars$dist), lty=2, col="blue")
abline (v=mean(cars$speed), lty=2, col="green")

#boxplot 예제

v1 <- c(10,12,15,11,20)
v2 <- c(5,7,15,8,9)
v3 <- c(11,20,15,18,13)
boxplot(v1,v2,v3,col=brewer.pal(3,"Pastel2"),
        names=c("Blue","Yellow","Pink"),
        horizontal=T)

#histogram 그래프

hist(cars$speed, main="속도의 히스토그램", cex=0.5, 
     col="#FFFF00", xlab="속도", ylab="빈도")
legend("right",c("Speed 빈도"))

# 평균이 5이고 표준편차가 1인 정규분포에서 100의 샘플을 생성한다.
hist(x)
# freq=F : 빈도가 아닌 밀도로 표시
hist(x, freq=F)
# 평균이 5이고 표준편차가 1인 정규분포의 밀도 함수 곡선을 그린다.
#add=T : 겹쳐 그림.
curve(dnorm(x, mean=5, sd=1))

# 누적막대그래프
barplot(VADeaths, col = heat.colors(5),
        border = "dark blue", legend = rownames(VADeaths), beside = T) # beside를 쓰면 묶음막대 그래프
title(main = list("버지니아주 사망율", font = 2))

########### 부산시 병원 데이터 활용 ###########

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-14.0.2') #엑셀파일을 읽기 위해
file=file.choose()
data10 = read.xlsx(file, sheetIndex = 1, header=T, encoding = "UTF-8")
data20 = read.xlsx(file, sheetIndex = 2, header=T, encoding = "UTF-8")

# reshape2에서 제공하는 변환은 크게 melt(wide to long), cast(long to aggregate)
d10 <- melt(id=1, data10)
d20 <- melt(id=1, data20)

# 2020년 데이터를 시도별로 long하에 데이터 변환
sum.d10 <- d10  %>%
  group_by(sido) %>%
  summarise(count10 = sum(value))

barplot(sum.d10$count10, main=paste("2010년 부산시 주요구별 과목별 병원현황"),
        ylab="병원수",ylim=c(0,400), col=brewer.pal(16,"Dark2"), 
        names=sum.d10$sido, cex.names=0.6)
abline(h=seq(0,400,50),lty=3) # 눈금선 생성

#정렬해서 보여줌(직)
data<-sum.d10$count10
names(data)<-sum.d10$sido
barplot(sort(data, decreasing = T), main=paste("부산시 주요구별 과목별 병원현황"),
        ylab="병원수",ylim=c(0,400), col=cm.colors(16),cex.names=0.6)
abline(h=seq(0,400,50),lty=3, lwd=0.1) # cm, head, rainbow.colors
?abline

# 2010년과 2020년도 병원 현황 비교

#2020년 데이터 
sum.d20 <- d20  %>%
  group_by(sido) %>%
  summarise(count20 = sum(value))

data.t <- data.frame(sum.d20$sido, sum.d10$count10, sum.d20$count20)
with(data.t, barplot(cbind(sum.d10.count10,sum.d20.count20)~sum.d20.sido, main=paste("2010과 2020년 부산시 주요구별 과목별 병원현황"),
                     beside=T,ylab="병원수",xlab="구군별", ylim=c(0,450), col=c("red", "blue"), 
                     names=sum.d20.sido, cex.names=0.6))

graphics.off()

#모자이크 그림
#데이터 변환과정입니다.(값이 0인 학과 삭제)
dd10<-subset(d10, variable !='진단검사의학과' & variable !='핵의학과'  & variable !='병리과' & variable !='결핵과' & variable !='흉부외과'
             & variable !='재활의학과' & variable !='신경과'  & variable !='영상의학과' & variable !='가정의학과' & variable !='신경외과' )
c <- dd10$value
c

dim(c) <- c(16,14)
rownames(c) <- dd10$sido[1:16] #행이름 부여
colnames(c) <- unique(dd10$variable) #열 이름 부여
c
mosaicplot(c, color=rainbow(14), main="부산 지역별 진료과목 현황")
c1<-t(c) # 행렬 변환
mosaicplot(c1, color=topo.colors(16), cex=0.5, main="2010년 부산 지역별 진료과목 현황")

# 2020 데이터 활용

par(mfrow=c(1,2))

a = d20 %>% group_by(variable) %>% summarise(n=value)

dd20<-subset(d20, variable !='진단검사의학과' & variable !='핵의학과'  & variable !='병리과' & variable !='결핵과' & variable !='흉부외과'
             & variable !='재활의학과' & variable !='신경과'  & variable !='영상의학과' & variable !='가정의학과' & variable !='신경외과' )

d <- dd20$value
dim(d) <- c(16,16)
d
rownames(d) <- dd20$sido[1:16]
colnames(d) <- unique(dd20$variable)
d
mosaicplot(d, color = .colors(16))
d1 = t(d)
mosaicplot(d1, color = topo.colors(16), cex= 0.5, main = "2020년 부산 지역별 진료과목 현황")

#### diamond ggplot ####

# 10주차 비대면 수업

library(ggplot2)
library(RColorBrewer)

###########################################
# diamonds 데이터 설명(다이아몬드의 캐럿, 커팅 정보)
# price : 가격 ($)
# carat : 다이아몬드의 무게
# cut : cut 품질
# colour : J ~ D (D가 최상품)
# clarity : 투명도, I1 ~ IF (IF가 최상품)
#  - I1, SI1, SI2, VS1, VS2, VVS1, VVS2, IF
# x, y : 크기 (mm),
# z : 깊이 (mm)
# table : 다이아몬드 꼭지의 폭과 제일 넓은 곳의 거리
##########################################
head(diamonds)
plot(diamonds$carat, diamonds$price, type="p") #graphics 
qplot(carat, price, data = diamonds, geom="point") #ggplot2 
### ggplot으로 변경
g <- ggplot(diamonds, aes(x=carat, y= price))
g <- g+ geom_point() 
g

#qplot bar chart
qplot(clarity, data=diamonds, fill=cut, geom="bar")
#ggplot bar chart -> same output
graphics.off()
ggplot(diamonds, aes(clarity, fill=cut))+geom_bar()


# 1 단계 : 미적 매핑
# ggplot()을 생성하면서 데이터와 미적 매핑 정보를 지정합니다.(x축 caret y축은 price)
# data : 데이터 지정
# aes() : 시각적 속성 지정
# x – X축 데이터 지정
# y – Y축 데이터 지정
g <-ggplot(diamonds, aes(carat, price))

# 2 단계 : 통계 변환 (생략)
# 3 단계 : 기하객체 적용
# 각각의 기하 객체는 미적 매핑 정보를 상속 받습니다.
# 기하객체를 여러 개 지정할 경우, 레이어(Layer) 형태로 중첩되어 표시됩니다.
# 상속 받은 x, y와 여기서 지정한 color 정보를 사용하여 산점도를 그림
# 상속 받은 x, y로 회귀선을 그립니다.

g<- g+geom_point(aes(color=clarity))+geom_smooth()


# 제목과 x축과 y축 label를 적음

g<- g+labs(title="다이어몬드 차트", x='무게', y='가격')

# 4 단계 : 위치 조정 

g<-g+facet_wrap(~cut)

# 그래프를 화면에 표시
g
summary(g) # 그래프의 정보 표시
ggsave("D:/r_workspace/temp.png") # 그래프를 ~.png라는 이름의 이미지 파일로 저장합니다



###################################################
# mpg 데이터 셑 
# 'data.frame': 234 obs. of  11 variables:
#  $ manufacturer(제조회사): chr  "audi" "audi" "audi" "audi" ...
#  $ model(모델)       : chr  "a4" "a4" "a4" "a4" ...
#  $ displ(배기량)       : num  1.8 1.8 2 2 2.8 2.8 3.1 1.8 1.8 2 ...
#  $ year(생산연도)        : int  1999 1999 2008 2008 1999 1999 2008 1999 1999 2008 ...
#  $ cyl(실린더 개수)         : int  4 4 4 4 6 6 6 4 4 4 ...
#  $ trans(변속기 종류)       : chr  "auto(l5)" "manual(m5)" "manual(m6)" "auto(av)" ...
#  $ drv(구동 방식)         : chr  "f" "f" "f" "f" ...
#  $ cty(도시 연비)         : int  18 21 20 21 16 18 18 18 16 20 ...
#  $ hwy(고속도로 연비)         : int  29 29 31 30 26 26 27 26 25 28 ...
#  $ fl(연료 종류)          : chr  "p" "p" "p" "p" ...
#  $ class(자동차 종류)       : chr  "compact" "compact" "compact" "compact" ..
##############################################################3

data(mpg, package="ggplot2")
head(mpg)
# boxplot
g <- ggplot(mpg, aes(class, cty, fill=class))
g<- g + geom_boxplot() + 
  scale_fill_brewer(palette = "Set2")+
  labs(title="Box plot", 
       subtitle="도시연비 vs 자동차 종류",
       caption="Source: mpg",
       x="자동차종류",
       y="도시연비") 
g

data(mpg, package="ggplot2")
mpg$manufacturer
table(mpg$manufacturer)

## audi, ford, hyundai, toyota만 추출

mpg_select = mpg[mpg$manufacturer %in% c("audi", "ford", "hyundai", "toyota"),]
mpg_select

# bubble chart 차트 
g <- ggplot(mpg_select, aes(x=displ, y=cty)) + 
  geom_point(aes(col=manufacturer, size=hwy)) +
  labs(title="Bubble Chart", subtitle="mpg : 배기량 vs 도시연비",
       x = "배기량", y="도시연비")
g
graphics.off()

##### Corrleogram #################

if(!require(ggcorrplot)){
  install.packages("ggcorrplot")
  library(ggcorrplot)
}

data1<- subset(mpg, select=c(displ,cty,hwy,cyl))
corr <- round(cor(data1), 3)
corr

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "upper", 
           # 상관계수를 표시
           lab = TRUE,
           lab_size = 3,
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mcar", 
           ggtheme=theme_bw)

data(mpg, package="ggplot2")

if(!require(ggExtra)){
  install.packages("ggExtra")
  library(ggExtra)
}

# Marginal Histogram / Boxplot

(g <- ggplot(mpg, aes(cty, hwy)) + 
    geom_count() + 
    geom_smooth(method = "lm", se =F)) # lm => linear model, se => 오차

g
ggMarginal(g, type = "histogram",fill="transparent") # 가장자리에 히스토그램으로 분포 확인

# boxplot을 그려봄

ggMarginal(g, type = "boxplot", fill="transparent") 

# density를 그려봄 

ggMarginal(g, type = "density", fill="transparent") 
ggMarginal(g, type = "violin", fill="transparent") 


##################viloin chart #####################
g <- ggplot(mpg, aes(class, cty))
g + geom_violin() + 
  labs(title="Box plot", 
       subtitle="도시연비 vs 자동차 종류",
       caption="Source: mpg",
       x="자동차종류",
       y="도시연비") +
  theme_bw()

g + geom_boxplot() + 
  labs(title="Box plot", 
       subtitle="도시연비 vs 자동차 종류",
       caption="Source: mpg",
       x="자동차종류",
       y="도시연비") +
  theme_bw()


#######################Density plot #############
g <- ggplot(mpg, aes(cty))
g + geom_density(aes(fill=factor(cyl)), alpha=0.5) + # 투명도 : 0.5
  scale_fill_brewer(palette = "Pastel1") +
  labs(title="분포차트", 
       subtitle="실린더수에 의한 도시연비",
       caption="Source: mpg",
       x="도시연비",
       fill="# 실린드수") +
  theme(legend.position = "bottom")


##### Corrleogram #################

if(!require(ggcorrplot)){
  install.packages("ggcorrplot")
  library(ggcorrplot)
}

data1<- subset(mpg, select=c(displ,cty,hwy,cyl))
corr <- round(cor(data1), 3)
corr

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "upper", 
           # 상관계수를 표시
           lab = TRUE,
           lab_size = 3,
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mcar", 
           ggtheme=theme_bw)

data(mpg, package="ggplot2")

if(!require(ggExtra)){
  install.packages("ggExtra")
  library(ggExtra)
}

# Marginal Histogram / Boxplot

(g <- ggplot(mpg, aes(cty, hwy)) + 
    geom_count() + 
    geom_smooth(method = "lm", se =F)) # lm => linear model, se => 오차

g
ggMarginal(g, type = "histogram",fill="transparent") # 가장자리에 히스토그램으로 분포 확인

# boxplot을 그려봄

ggMarginal(g, type = "boxplot", fill="transparent") 

# density를 그려봄 

ggMarginal(g, type = "density", fill="transparent") 
ggMarginal(g, type = "violin", fill="transparent") 


##################viloin chart #####################
g <- ggplot(mpg, aes(class, cty))
g + geom_violin() + 
  labs(title="Box plot", 
       subtitle="도시연비 vs 자동차 종류",
       caption="Source: mpg",
       x="자동차종류",
       y="도시연비") +
  theme_bw()

g + geom_boxplot() + 
  labs(title="Box plot", 
       subtitle="도시연비 vs 자동차 종류",
       caption="Source: mpg",
       x="자동차종류",
       y="도시연비") +
  theme_bw()


#######################Density plot #############
g <- ggplot(mpg, aes(cty))
g + geom_density(aes(fill=factor(cyl)), alpha=0.5) + # 투명도 : 0.5
  scale_fill_brewer(palette = "Pastel1") +
  labs(title="분포차트", 
       subtitle="실린더수에 의한 도시연비",
       caption="Source: mpg",
       x="도시연비",
       fill="# 실린드수") +
  theme(legend.position = "bottom")

################# 작년 시험문제 풀이 ######################

# 1.1

file <- choose.files() #file open
popData <- read.xlsx(file, 1, encoding = "UTF-8")
dim(popData)

# 1.2
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# 2.1
library(plyr)

file = file.choose()
popData <- read.csv(file, header=T)

pop <- ddply(popData, .(area), summarise, pop.a10 = sum(pop10, na.rm = T),
             pop.a20 = sum(pop20, na.rm = T), grdp.a10 = sum(grdp10,na.rm = T),
             grdp.a20 = sum(grdp20,na.rm = T))

typeof(popData$pop20)
as.numeric(popData$pop20)
pop

pop.pcnt10 <- with(pop,round(pop.a10 / sum(pop.a10)*100,1)) # 인구밀도
pop.pcnt20 <- with(pop,round(pop.a20 / sum(pop.a20)*100,1))
label1 <- paste(pop$area,"(",pop.pcnt10,"%)")
label2 <- paste(pop$area,"(",pop.pcnt20,"%)")
par(mfrow=c(1,2)) # 1?? 2??
with(pop, pie(pop.a10, labels = label1, col = rainbow(length(area)),
              main = "2010년 지역별 인구현황"))
with(pop, pie(pop.a20, labels = label2, col = rainbow(length(area)),
              main = "2020년 지역별 인구현황"))

pop
grdp.pcnt10 = with(pop,round(grdp.a10 / sum(grdp.a10)*100,1)) # grdp
grdp.pcnt20 = with(pop,round(grdp.a20 / sum(grdp.a20)*100,1))

label3 = paste(pop$area,"(",grdp.pcnt10,"%)")
label4 = paste(pop$area,"(",grdp.pcnt18,"%)")

pop.pcnt10
grdp.pcnt10

pop$area = factor(pop$area,levels = c(1:7), labels = c("수도권","동남권","대경권","충청권","전라권","강원도","제주도"))

# 2.2
mean(pop.pcnt10)
median(pop.pcnt10)
min(pop.pcnt10)
max(pop.pcnt10)

# 3.1

data <- pop %>%
  group_by(area) %>%
  summarise(인구 = max(pop.a10), grdp = max(grdp.a10))

data <- pop %>%
  group_by(area)

data

# 3.2

data
barplot(sort(pop$pop.a10), col = heat.colors(5),
        border = "dark blue",  beside = T) # beside를 쓰면 묶음막대 그래프
title(main = "1717709 이동현", xlab = "지역", ylab = "인구수")

# 3.3
# 인구 수와 grdp 순위

# 4.1
pop_select = pop[pop$area %in% c("수도권","동남권","대경권","전라권"),]
pop_select
dim(pop_select)

# 4.2
# 수도권의 grdp가 제일 크다.

# 4.3

par(mfrow=c(2,2))

ggplot(pop, aes(area,grdp.a10))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set2")+
  labs()

ggplot(pop, aes(area,pop.a10))+
  geom_boxplot()

# 4.4

data1<- subset(mpg, select=c(displ,cty,hwy,cyl))
corr <- round(cor(data1), 3)
corr

# 4.5
library(ggcorrplot)

ggcorrplot(corr, hc.order = TRUE, 
           type = "upper", 
           lab = TRUE,
           lab_size = 3,
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="1717709 이동현",
           ggtheme=theme_bw)

# 4.6
cor.test(pop$pop.a10, pop$pop.a20)
# 상관계수 및 p-value, 신뢰구간을 확인

# 5
# bubble chart
(g <- ggplot(데이터, aes(1인당 grdp, 인구밀도))+
    geom_point(aes(col=국가, size = 100명 당 의사 수))+
    labs(title = "버블 차트"))


################ 롯데 데이터 ####################

# dplyr 라이브러리도 부착
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(sqldf)


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


################## 사회 안전 지수 및 시군구 데이터 #############

busan <- read.csv("./data/busan_sigungu.csv")
# index = 사회 안전 지수(경제 활동, 생활 영역, 건강, 거주 관련의 4가지로 구성)의 평균
# cor(busan[,4:8]) # 사회 안전 지수 상관계수 확인

library(GISTools)
library(maptools)
library(ggplot2)
library(dplyr)
library(rgdal)
library(RColorBrewer)

sigungushp = readOGR("sigungu/sig.shp")
summary(sigungushp)
df_map = fortify(sigungushp) # R 데이터 프레임으로 변경
head(df_map,20)

df_map_info = sigungushp@data
head(df_map_info,50)

df_map_info[, "id"] = (1:nrow(df_map_info)) - 1 # 지도와 data를 merge하기 위해 ID생성
df_map_info[, "SIDO"] = as.numeric(substr(df_map_info$SIG_CD,
                                          start = 1, stop = 2)) # 시도 코드 생성

df_map_info_busan = df_map_info[df_map_info$SIDO==26,] # 부산만 추출
df_map_busan = df_map[df_map$id %in% df_map_info_busan$id, ] # id를 활용해 map 데이터 추출

# map과 사회안전지수 데이터 결합
busan_map <- merge(df_map_busan, busan, by = 'id')

head(busan_map)
ggplot(busan_map, aes(x=long, y=lat, group=group, fill=index))+
  geom_polygon()+
  scale_fill_gradientn(colors = brewer.pal(9,name = "YlGnBu"))+
  theme(legend.position = "none")

geom_text(c, aes(long,lat,group=group,label=index))

## 220525 대면수업

coor <- as.data.frame(busan_map %>%
                        group_by(SIG_KOR_NM) %>%
                        summarise(long=median(long), lat=median(lat), index=mean(index)))
m<- ggplot() +
  geom_polygon(data=busan_map, aes(x=long, y=lat, group=group, fill=index))+
  scale_fill_gradientn(colors =brewer.pal(10,name="RdBu"))+ 
  geom_text(data=coor, aes(long, lat, label=paste(SIG_KOR_NM, "(", index, ")")),col="dimgray")

m

########################## 감성분석 ####################################


install.packages("rvest")
library(rvest)
library(stringr)


#1. 감성분석할 데이터를 수집

reviews <- c("다 좋았는데 마지막 전투신이 좀 허무했고 뭐지 싶을 정도로 빨리 끝남",
             "전작에 비해 아쉬움 ㅁㅁ",
             "믿고 보는 범죄의 도시! 계속된 재미",
             "저도 범죄의 도시 정주행중 ㅋㅋ 그냥 1편이나 2편이나 비슷하게 재미 있는 것 같음",
             "범죄의 도시 정주행중!! 마동석 멋져",
             "확실히 2편은 뭔가 약해..쩝",
             "1편보단 못하다... 처음이랑 끝에 밖에 액션신도 없고.. 기대보단 좀... 매력이 없다",
             "악당 리얼 최민식 닮음 나만 느낌???",
             "전작에 비해 아쉽지만 여전히 어벤져서 라인업 중 가장 세련된 시리즈",
             "추천순보고 재미없는 줄 알았네 ㅋㅋㅋ 엄청 재밌는데, 액션은 평범하지만 어벤져스 까지의 내용전개에 진짜 꼭 필요한 내용, 꿀잼")
reviews[1]
reviews

## 2. 사전 만들기
pos.words <- c("멋져", "재미", "믿고", "정주행중", "세련된", "꿀잼")
neg.words <- c("기대보단", "못하다", "아쉬움", "약해..쩝", "허무했고")

### 3. 극성평가(음수면 부정적, 양수면 긍정적)

pos.cnt <- str_count(reviews[3], pos.words)
pos.cnt
p<-sum(pos.cnt)

neg.cnt <- str_count(reviews[3], neg.words)
neg.cnt
n <- sum(neg.cnt)

if(p+n==0){
  polarity = 0
} else{
  polarity <- (p-n)/(p+n)
}
polarity

### 전체 리뷰 극성평가

m <- NULL

for(i in 1:length(reviews)){
  pos.cnt <- str_count(reviews[i], pos.words)
  p<-sum(pos.cnt)
  neg.cnt <- str_count(reviews[i], neg.words)
  n <- sum(neg.cnt)
  
  if(p+n==0){
    polarity = 0
  } else{
    polarity <- (p-n)/(p+n)
  }
  m <- c(m, polarity)
}

table(m)

#################### 웹 크롤링 ####################

library(rvest)
library(stringr)
url.review="https://movie.naver.com/movie/point/af/list.nhn?page="
reviews.all = NULL
for(page in  21:40){
  url.review.page = paste(url.review, page, sep="")
  html = read_html(url.review.page, encoding = "utf-8")
  reviews = html_nodes(html, ".title") %>% html_text()
  reviews.all = c(reviews.all, reviews)
}
reviews.all

titles <- str_match(reviews.all, "\n\t\t\t\t(.*)\n\t\t\t") #str_match 문자열 추출, 소괄호는 (  ) 문자열 추출
titles <- titles[,2]
titles
table(titles)

reviews <- str_replace(reviews.all, titles, "") #str_replace 
reviews <- gsub("신고", "", reviews)
reviews <- gsub("\t|\n", "", reviews)
reviews
reviews[20]


index<- str_locate(reviews, '총 10점')[,1]+7
str<-substr(reviews, 1, index)
reviews = str_replace(reviews, str, "")
reviews = str_replace(reviews, "0", "")
reviews

save(reviews, file="reviews.rda")
save(titles, file="titles.rda")
# save(predict, file="predict.rda")

####  predict 

positive <- readLines(file.choose(), encoding = "UTF-8")
negative <- readLines(file.choose(), encoding = "UTF-8")

predict = NULL
scores = NULL

m <- length(reviews)

for(i in 1:m){
  words = str_split(reviews[i], '\\s+') #문자열 나누기
  words = unlist(words) #리스트를 vector로 바꿈
  words <- gsub("[^A-Za-z0-9ㄱ-힣]", "", words)
  words
  
  # words의 단어를 positive에서 matching
  
  pos.matches = intersect(words, positive)
  pos.cnt=length(pos.matches)
  
  neg.matches = intersect(words, negative)
  neg.cnt=length(neg.matches)
  
  score = sum(pos.cnt) - sum(neg.cnt) 
  scores = c(scores, score)
  
  if(pos.cnt > neg.cnt){
    predict <- c(predict, 1)
  } else if(pos.cnt < neg.cnt){
    predict<-c(predict, -1)
  }  else {
    predict<-c(predict, 0)  
  }
  
  #### polarity 평가 ###
  
  scores
  
  p <- length(predict[predict==1])
  n <- length(predict[predict==-1])
  
  table(predict)
  
  polarity <- (p-n)/(p+n)
  polarity
  
  sort(table(titles))
  
  pie(table(predict[i]))
  
  load("titles-2.rda")
  load("predict-1.rda")
  load("reviews-2.rda")
  
  titles[1]
  predict[1]
  predict
  
  
  #### 특정 영화에 대한 극성평가 ##
  
  i = which(titles=="그대가 조국")
  i
  s1 <- predict[i]
  p <- length(s1[s1==1])
  n <- length(s1[s1==-1])
  s1
  
  polarity <- (p-n)/(p+n)
  polarity
  
  ### 차트 #####
  pie(table(predict[i]), col = c("red","yellow","blue"))
  table(titles)
  
  ###########
  
  # 감정 점수 부여, 감정 극성 분류
  word_tweet <- word_tweet_raw %>%
    left_join(dic, by = "word") %>% # 감정 점수 부여
    mutate(polarity = ifelse(is.na(polarity), 0, polarity), # NA를 0으로 변환
           sentiment = ifelse(polarity == 2, "긍정", # 감정 범주 분류
                              ifelse(polarity == -2, "부정", "중립")))
  
  
  
##################### shiny ###########################

  library(shiny)
  library(ggplot2)
  library(RColorBrewer)
  library(dplyr)
  load(file="data/data1.rda")
  
  ui <- fluidPage(
    
    # Application title
    headerPanel("Lotte DataViz"), 
    
    
    # Sidebar with a slider input a variable
    sidebarLayout(
      sidebarPanel(
        # radio button위젯 사용
        radioButtons("spot", "Branch Office of Lotte :",
                     c("AA지점" = "AA",
                       "BB지점" = "BB",
                       "CC지점" = "CC",
                       "DD지점" = "DD")),
        hr(),
        tags$img(src="inf.jpg", width="200px", height="200px")),
      
      # Show output 
      mainPanel(
        h2(textOutput("caption")),
        plotOutput("plot"),
        tableOutput("table")
        
        
        
      )
    )  
    
  )
  #### server
  
  server <- function(input, output) {
    # reactive function(title과 dataInput)
    
    dataInput <- reactive({
      temp <- input$spot
      subset(data1, 점포ID==temp)
    })
    
    title <- reactive(
      paste(input$spot, "지점 매출현황")
    )
    
    # title을 renderText에 전송
    output$caption <- renderText(title())
    
    
    # plot을 renderPlot에 전송, 여러줄을 쓸때는 집합기호{} 사용
    output$plot <- renderPlot({
      ggplot(dataInput(),aes(거래월,상품대분류명))+
        geom_tile(aes(fill=amount))+
        scale_fill_gradientn(colors = brewer.pal(5,name = "RdBu"))+
        ggtitle("월별 거래처에 대한 매출액")+
        xlab("거래월")+
        ylab("상품대분류명")+
        scale_x_continuous(breaks = seq(1:12))
    })
    
    #지점에 대한 매출액과 거래건수를 table형태로 rendering하여 전송, 이때 df로 타입 변경
    output$table <- renderTable({
      temp <- as.data.frame(dataInput() %>% 
                              group_by(거래월) %>% 
                              summarise(amount = sum(amount), 건수 = sum(cnt)))
      temp
    })
  }
  
  
  #Run the application 
  shinyApp(ui = ui, server = server)
  


