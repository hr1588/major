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



