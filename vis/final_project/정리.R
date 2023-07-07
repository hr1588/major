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

# Lesson05 ggplot 수업전 01 참조

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

# 나머지는 각 project 확인




























