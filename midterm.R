# 패키지 설치
library(dplyr)
library(RColorBrewer)
library(sqldf)
library(ggplot2)
rm(list=ls())

# 데이터 전처리(2009년 폐기물 통계)

t09 <- read.csv(file.choose(),header = T)
t09 <- t09 %>% filter(시도.2. == "발생량")
t09 <- t09[,-c(2,3)]
t09 <- t09[-c(1),c(1,2)] # 데이터 정제

names(t09) <- c("지역","총합계")

area <- data.frame(지역 = c('서울','부산','대구','인천','광주','대전','울산','경기','강원','충북',
                          '충남','전북','전남','경남','경북','제주'),
                     area = c(1,2,3,1,5,4,2,1,6,4,4,5,5,2,3,7))

t2009 <- merge(t09, area, by ="지역",all = TRUE)
t2009$지역 = factor(t2009$area, levels = c(1:7), 
                  labels = c("수도권", "동남권", "대경권", "충청권", "전라권"
                             , "강원도", "제주도"))
t2009 <- t2009[,-c(3)]
t2009 <- sqldf("select * from t2009 group by 지역")

t2009$총합계 <- as.numeric(t2009$총합계) # 그래프 제작을 위한 수치화
typeof(t2009$총합계)

t2009$비율 = prop.table(t2009$총합계) # 비율 생성

(g2009 <- ggplot(t2009,aes(x="",y=총합계,fill=지역))+
  geom_bar(width = 1, stat = "identity", color="white")+
  coord_polar("y",start=0)+
  scale_fill_brewer(palette = "GnBu")+
  theme_void()+
  geom_text(aes(label = paste0(round(비율*100,1),"%")),
            position = position_stack(vjust = 0.5))+
  labs(title = "2009년 지역별 폐기물 발생현황",
       subtitle = "종량제, 재활용품, 음식물"))

# 데이터 전처리(2010년 폐기물 통계)

t10 <- read.csv(file.choose(),header = T)
t10 <- t10[-c(1,2,3,4),c(1,2)] # 데이터 정제
names(t10) <- c("지역","총합계")
t2010 <- merge(t10, area, by ="지역",all = TRUE)
t2010$지역 = factor(t2010$area, levels = c(1:7), 
                  labels = c("수도권", "동남권", "대경권", "충청권", "전라권"
                             , "강원도", "제주도"))
t2010 <- t2010[,-c(3)]
t2010 <- sqldf("select * from t2010 group by 지역")

t2010$총합계 <- as.numeric(t2010$총합계) # 그래프 제작을 위한 수치화
typeof(t2010$총합계)
t2010$비율 = prop.table(t2010$총합계)

(g2010 <- ggplot(t2010,aes(x="",y=총합계,fill=지역))+
    geom_bar(width = 1, stat = "identity", color="white")+
    coord_polar("y",start=0)+
    scale_fill_brewer(palette = "GnBu")+
    theme_void()+
    geom_text(aes(label = paste0(round(비율*100,1),"%")),
              position = position_stack(vjust = 0.5))+
    labs(title = "2010년 지역별 폐기물 발생현황",
         subtitle = "종량제, 재활용품, 음식물"))

# 데이터 전처리(2011년 폐기물 통계)

t11 <- read.csv(file.choose(),header = T)
t11 <- t11[-c(1,2,3,4),c(1,2)] # 데이터 정제
names(t11) <- c("지역","총합계")
t2011 <- merge(t11, area, by ="지역",all = TRUE)
t2011$지역 = factor(t2011$area, levels = c(1:7), 
                  labels = c("수도권", "동남권", "대경권", "충청권", "전라권"
                             , "강원도", "제주도"))
t2011 <- t2011[,-c(3)]
t2011 <- sqldf("select * from t2011 group by 지역")

t2011$총합계 <- as.numeric(t2011$총합계) # 그래프 제작을 위한 수치화
typeof(t2011$총합계)
t2011$비율 = prop.table(t2011$총합계)

(g2011 <- ggplot(t2011,aes(x="",y=총합계,fill=지역))+
    geom_bar(width = 1, stat = "identity", color="white")+
    coord_polar("y",start=0)+
    scale_fill_brewer(palette = "GnBu")+
    theme_void()+
    geom_text(aes(label = paste0(round(비율*100,1),"%")),
              position = position_stack(vjust = 0.5))+
    labs(title = "2011년 지역별 폐기물 발생현황",
         subtitle = "종량제, 재활용품, 음식물"))

# 데이터 전처리(2012년 폐기물 통계)

t12 <- read.csv(file.choose(),header = T)
t12 <- t12[-c(1,2,3,4,12),c(1,2)] # 데이터 정제
names(t12) <- c("지역","총합계")
t2012 <- merge(t12, area, by ="지역",all = TRUE)
t2012$지역 = factor(t2012$area, levels = c(1:7), 
                  labels = c("수도권", "동남권", "대경권", "충청권", "전라권"
                             , "강원도", "제주도"))
t2012 <- t2012[,-c(3)]
t2012 <- sqldf("select * from t2012 group by 지역")

t2012$총합계 <- as.numeric(t2012$총합계) 
t2012$비율 = prop.table(t2012$총합계)

(g2012 <- ggplot(t2012,aes(x="",y=총합계,fill=지역))+
    geom_bar(width = 1, stat = "identity", color="white")+
    coord_polar("y",start=0)+
    scale_fill_brewer(palette = "GnBu")+
    theme_void()+
    geom_text(aes(label = paste0(round(비율*100,1),"%")),
              position = position_stack(vjust = 0.5))+
    labs(title = "2012년 지역별 폐기물 발생현황",
         subtitle = "종량제, 재활용품, 음식물"))

# 데이터 전처리(2013년 폐기물 통계)

t13 <- read.csv(file.choose(),header = T)
t13 <- t13[-c(1,2,3,4,12),c(1,2)] # 데이터 정제
names(t13) <- c("지역","총합계")
t2013 <- merge(t13, area, by ="지역",all = TRUE)
t2013$지역 = factor(t2013$area, levels = c(1:7), 
                  labels = c("수도권", "동남권", "대경권", "충청권", "전라권"
                             , "강원도", "제주도"))
t2013 <- t2013[,-c(3)]
t2013 <- sqldf("select * from t2013 group by 지역")

t2013$총합계 <- as.numeric(t2013$총합계) 
t2013$비율 = prop.table(t2013$총합계)

(g2013 <- ggplot(t2013,aes(x="",y=총합계,fill=지역))+
    geom_bar(width = 1, stat = "identity", color="white")+
    coord_polar("y",start=0)+
    scale_fill_brewer(palette = "GnBu")+
    theme_void()+
    geom_text(aes(label = paste0(round(비율*100,1),"%")),
              position = position_stack(vjust = 0.5))+
    labs(title = "2013년 지역별 폐기물 발생현황",
         subtitle = "종량제, 재활용품, 음식물"))

# 데이터 전처리(2014년 폐기물 통계)

t14 <- read.csv(file.choose(),header = T)
t14 <- t14[-c(1,2,3,4,12),c(1,2)] # 데이터 정제
names(t14) <- c("지역","총합계")
t2014 <- merge(t14, area, by ="지역",all = TRUE)
t2014$지역 = factor(t2014$area, levels = c(1:7), 
                  labels = c("수도권", "동남권", "대경권", "충청권", "전라권"
                             , "강원도", "제주도"))
t2014 <- t2014[,-c(3)]
t2014 <- sqldf("select * from t2014 group by 지역")

t2014$총합계 <- as.numeric(t2014$총합계) 
t2014$비율 = prop.table(t2014$총합계)

(g2014 <- ggplot(t2014,aes(x="",y=총합계,fill=지역))+
    geom_bar(width = 1, stat = "identity", color="white")+
    coord_polar("y",start=0)+
    scale_fill_brewer(palette = "GnBu")+
    theme_void()+
    geom_text(aes(label = paste0(round(비율*100,1),"%")),
              position = position_stack(vjust = 0.5))+
    labs(title = "2014년 지역별 폐기물 발생현황",
         subtitle = "종량제, 재활용품, 음식물"))

# 데이터 전처리(2015년 폐기물 통계)

t15 <- read.csv(file.choose(),header = T)
t15 <- t15[-c(1,2,3,4,12),c(1,2)] # 데이터 정제
names(t15) <- c("지역","총합계")
t2015 <- merge(t15, area, by ="지역",all = TRUE)
t2015$지역 = factor(t2015$area, levels = c(1:7), 
                  labels = c("수도권", "동남권", "대경권", "충청권", "전라권"
                             , "강원도", "제주도"))
t2015 <- t2015[,-c(3)]
t2015 <- sqldf("select * from t2015 group by 지역")

t2015$총합계 <- as.numeric(t2015$총합계) 
t2015$비율 = prop.table(t2015$총합계)

(g2015 <- ggplot(t2015,aes(x="",y=총합계,fill=지역))+
    geom_bar(width = 1, stat = "identity", color="white")+
    coord_polar("y",start=0)+
    scale_fill_brewer(palette = "GnBu")+
    theme_void()+
    geom_text(aes(label = paste0(round(비율*100,1),"%")),
              position = position_stack(vjust = 0.5))+
    labs(title = "2015년 지역별 폐기물 발생현황",
         subtitle = "종량제, 재활용품, 음식물"))

# 데이터 전처리(2016년 폐기물 통계)

t16 <- read.csv(file.choose(),header = T)
t16 <- t16[-c(1,2,3,4,12),c(1,2)] # 데이터 정제
names(t16) <- c("지역","총합계")
t2016 <- merge(t16, area, by ="지역",all = TRUE)
t2016$지역 = factor(t2016$area, levels = c(1:7), 
                  labels = c("수도권", "동남권", "대경권", "충청권", "전라권"
                             , "강원도", "제주도"))
t2016 <- t2016[,-c(3)]
t2016 <- sqldf("select * from t2016 group by 지역")

t2016$총합계 <- as.numeric(t2016$총합계) 
t2016$비율 = prop.table(t2016$총합계)

(g2016 <- ggplot(t2016,aes(x="",y=총합계,fill=지역))+
    geom_bar(width = 1, stat = "identity", color="white")+
    coord_polar("y",start=0)+
    scale_fill_brewer(palette = "GnBu")+
    theme_void()+
    geom_text(aes(label = paste0(round(비율*100,1),"%")),
              position = position_stack(vjust = 0.5))+
    labs(title = "2016년 지역별 폐기물 발생현황",
         subtitle = "종량제, 재활용품, 음식물"))

# 데이터 전처리(2017년 폐기물 통계)

t17 <- read.csv(file.choose(),header = T)
t17 <- t17[-c(1,2,3,4,12),c(1,2)] # 데이터 정제
names(t17) <- c("지역","총합계")
t2017 <- merge(t17, area, by ="지역",all = TRUE)
t2017$지역 = factor(t2017$area, levels = c(1:7), 
                  labels = c("수도권", "동남권", "대경권", "충청권", "전라권"
                             , "강원도", "제주도"))
t2017 <- t2017[,-c(3)]
t2017 <- sqldf("select * from t2017 group by 지역")

t2017$총합계 <- as.numeric(t2017$총합계) 
t2017$비율 = prop.table(t2017$총합계)

(g2017 <- ggplot(t2017,aes(x="",y=총합계,fill=지역))+
    geom_bar(width = 1, stat = "identity", color="white")+
    coord_polar("y",start=0)+
    scale_fill_brewer(palette = "GnBu")+
    theme_void()+
    geom_text(aes(label = paste0(round(비율*100,1),"%")),
              position = position_stack(vjust = 0.5))+
    labs(title = "2017년 지역별 폐기물 발생현황",
         subtitle = "종량제, 재활용품, 음식물"))

# 데이터 전처리(2018년 폐기물 통계)

t18 <- read.csv(file.choose(),header = T)
t18 <- t18[-c(1,2,3,4,12),c(1,2)] # 데이터 정제
names(t18) <- c("지역","총합계")
t2018 <- merge(t18, area, by ="지역",all = TRUE)
t2018$지역 = factor(t2018$area, levels = c(1:7), 
                  labels = c("수도권", "동남권", "대경권", "충청권", "전라권"
                             , "강원도", "제주도"))
t2018 <- t2018[,-c(3)]
t2018 <- sqldf("select * from t2018 group by 지역")

t2018$총합계 <- as.numeric(t2018$총합계) 
t2018$비율 = prop.table(t2018$총합계)

(g2018 <- ggplot(t2018,aes(x="",y=총합계,fill=지역))+
    geom_bar(width = 1, stat = "identity", color="white")+
    coord_polar("y",start=0)+
    scale_fill_brewer(palette = "GnBu")+
    theme_void()+
    geom_text(aes(label = paste0(round(비율*100,1),"%")),
              position = position_stack(vjust = 0.5))+
    labs(title = "2018년 지역별 폐기물 발생현황",
         subtitle = "종량제, 재활용품, 음식물"))

# 데이터 전처리(2019년 폐기물 통계)

t19 <- read.csv(file.choose(),header = T)
t19 <- t19[-c(1,2,3,4,12),c(1,2)] # 데이터 정제
names(t19) <- c("지역","총합계")
t2019 <- merge(t19, area, by ="지역",all = TRUE)
t2019$지역 = factor(t2019$area, levels = c(1:7), 
                  labels = c("수도권", "동남권", "대경권", "충청권", "전라권"
                             , "강원도", "제주도"))
t2019 <- t2019[,-c(3)]
t2019 <- sqldf("select * from t2019 group by 지역")

t2019$총합계 <- as.numeric(t2019$총합계) 
t2019$비율 = prop.table(t2019$총합계)

(g2019 <- ggplot(t2019,aes(x="",y=총합계,fill=지역))+
    geom_bar(width = 1, stat = "identity", color="white")+
    coord_polar("y",start=0)+
    scale_fill_brewer(palette = "GnBu")+
    theme_void()+
    geom_text(aes(label = paste0(round(비율*100,1),"%")),
              position = position_stack(vjust = 0.5))+
    labs(title = "2019년 지역별 폐기물 발생현황",
         subtitle = "종량제, 재활용품, 음식물"))

#### 인구통계(2009년)

p09 <- read.csv(file.choose(), header = T)
p09 <- p09 %>% filter(소재지.시군구.별.2. == "소계")
p09 <- p09[-c(1),c(1,3)]
names(p09) <- c("지역","인구")
p09
p09$지역 <- c("서울","부산","대구","인천","광주","대전","울산","경기","강원","충북","충남","전북","전남","경북","경남","제주")

p2009 <- merge(p09, area, by = "지역", all = TRUE)
p2009
p2009$지역 = factor(p2009$area, levels = c(1:7), 
                    labels = c("수도권", "동남권", "대경권", "충청권", "전라권"
                               , "강원도", "제주도"))
p2009 <- p2009[,-c(3)]

### sql활용, 권역별 최종 표 제작(2009년)

p2009 <- sqldf("select * from p2009 group by 지역")
p2009
t2009

m2009 <- sqldf("select t2009.지역, t2009.총합계, p2009.인구
               from t2009, p2009 where t2009.지역 = p2009.지역")

m2009$인구 <- as.numeric(m2009$인구) 
m2009$one = (m2009$총합계*1000/m2009$인구) # 1인 쓰레기량은 kg화해서 계산
m2009

#### 인구통계(2019년)

p19 <- read.csv(file.choose(), header = T)
p19 <- p19 %>% filter(소재지.시군구.별.2. == "소계")
p19 <- p19[-c(1,9),c(1,3)]
names(p19) <- c("지역","인구")
p19$지역 <- c("서울","부산","대구","인천","광주","대전","울산","경기","강원","충북","충남","전북","전남","경북","경남","제주")

p2019 <- merge(p19, area, by = "지역", all = TRUE)
p2019
p2019$지역 = factor(p2019$area, levels = c(1:7), 
                  labels = c("수도권", "동남권", "대경권", "충청권", "전라권"
                             , "강원도", "제주도"))
p2019 <- p2019[,-c(3)]

### sql활용, 권역별 최종 표 제작(2019년)

p2019 <- sqldf("select * from p2019 group by 지역")
p2019
t2019

# 2009년과 2019년의 지역별 1인당 폐기물량 비교 그래프 작성


m2019 <- sqldf("select t2019.지역, t2019.총합계, p2019.인구
               from t2019, p2019 where t2019.지역 = p2019.지역")

m2019$인구 <- as.numeric(m2019$인구) 
m2019$one = (m2019$총합계*1000/m2019$인구) # 1인 쓰레기량은 kg화해서 계산
m2019

m2019$two <- m2009$one
names(m2019) = c("지역","총합계","인구","십년전","현재")

library(grid) # 화살표 제작

m2019
g <- ggplot(m2019, aes(지역,십년전,group=1))+
  geom_line(color="red",linetype=5, size = 2)+
  geom_point(color="red",shape = 21, size=5,fill="white",stroke=5)

(g1 <- g+geom_line(aes(지역,현재),color = "blue", size = 2)+
    geom_point(aes(지역,현재),shape=21,color="blue",size=5,fill="white",stroke=5)+
  theme_bw()+
  labs(title = "권역별 1인 쓰레기 배출량",
       subtitle = "빨강 - 2009년 / 파랑 - 2019년 (단위 : kg)",
       x = "지역", y = "배출량")+
    annotate("text", x=3, y = 1.1, label = "2009년")+
    annotate("text", x=4.5, y = 1, label = "2019년")+
    annotate("segment", x = 2.9, y = 1.08, xend=2.5, yend = 1, colour="black", arrow=arrow(angle = 20), size=2)+
    annotate("segment", x = 4.5, y = 0.97, xend=4.8, yend = 0.92, colour="black", arrow=arrow(angle = 20),size=2)+
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#00CCCC"),
        plot.subtitle = element_text(face = "bold", hjust = 1, color = "#010101"),
        axis.title.x = element_text(face = "bold", size = 13),
        axis.title.y = element_text(face = "bold", size = 13)))


#### 1인 배출량 1등인 강원도의 쓰레기 배출 확인(10년사이 비교) - 2009년

gw2009 <- read.csv(file.choose(),header = T)
gw2009 <- gw2009 %>% filter(시도.2. == "발생량")
gw2009 <- gw2009[c(10),-c(2,3,4,5,6,13,18)]
gw2009 <- gw2009[,-c(1)]
names(gw2009) <- c("가연성 음식물", "가연성 폐지류", "가연성 폐목재류", "가연성 폐고무류",
                "가연성 플라스틱류","가연성 기타", "불연성 폐유리류", "불연성 폐금속류", "불연성 폐토사류",
                "불연성 기타", "종이류", "폐유리병류", "캔류", "플라스틱류", "합성수지류","폐전기전자제품",
                "폐전지류","타이어","윤활유","폐형광등","고철류","폐의류","영농폐기물", "폐가구류", "기타 재활용품", "음식물류 폐기물")

gw2009 <- as.data.frame(t(gw2009))
gw2009
names(gw2009) <- c("품목")
gw2009$num <- c(1:26)
gw2009$num <- c("가연성 음식물", "가연성 폐지류", "가연성 폐목재류", "가연성 폐고무류",
                "가연성 플라스틱류","가연성 기타", "불연성 폐유리류", "불연성 폐금속류", "불연성 폐토사류",
                "불연성 기타", "종이류", "폐유리병류", "캔류", "플라스틱류", "합성수지류","폐전기전자제품",
                "폐전지류","타이어","윤활유","폐형광등","고철류","폐의류","영농폐기물", "폐가구류", "기타 재활용품", "음식물류 폐기물")

names(gw2009) <- c("품목","폐기물종류","비율")
gw2009$품목 <- as.numeric(gw2009$품목) 
gw2009$비율 <- prop.table(gw2009$품목)
boxplot(gw2009$품목)

gw2009$품목 <- ifelse(gw2009$품목 < 30, NA, gw2009$품목) # 극소량의 값 결측치 처리
gw2009 <- na.omit(gw2009)

g2 <- ggplot(gw2009,aes(x=reorder(폐기물종류,품목),y=품목))+
  geom_bar(width=0.7,stat = "identity",aes(fill=품목))+
  coord_flip()+
  scale_fill_gradient(low="green", high = "red")+
  labs(title = "강원도 품목별 폐기물 배출량",
       subtitle = "2009년 일일 평균(단위 : 톤)",
       x = "폐기물 종류",
       y = "배출량")+
  theme(plot.title = element_text(size=20, hjust = 0.5, face = "bold", color = "darkblue"),
        plot.subtitle = element_text(hjust = 1, face = "bold"),
        axis.title.x = element_text(face = "bold", size = 13),
        axis.title.y = element_text(face = "bold", size = 13),
        axis.text.x = element_text(angle = 45))+
  scale_y_continuous(breaks = seq(0,300,20))
  
g2  


#### 1인 배출량 1등인 강원도의 쓰레기 배출 확인(10년사이 비교) - 2019년

gw2019 <- read.csv(file.choose(),header = T)
gw2019 <- gw2019[c(14),-c(2,3,4,12,21)]
gw2019 <- gw2019[,-c(1)]

gw2019 <- as.data.frame(t(gw2019))
gw2019
gw2019$폐기물종류 =  c("가연성 음식물", "가연성 폐지류", "가연성 폐합성수지류","가연성 폐목재류","가연성 폐섬유류","가연성 폐고무류",
                  "가연성 기타", "불연성 연탄재류","불연성 폐유리류", "불연성 폐금속류", "불연성 폐토사류","불연성 폐타일 및 도자기",
                  "불연성 기타","건설폐재류","배출불명 기타","종이팩","종이류","폐유리병류", "금속캔류", "비닐류","발포수지류","페트병",
                  "재활용 기타 폐합성수지류","재활용 폐고무류","폐전기전자제품","폐전지류","영농폐기물 농약","영농폐기물 폐비닐",
                  "폐형광등","고철류","폐의류","폐섬유류","폐가구류","폐식용유","재활용잔재물","재활용 기타","음식물류 폐기물")
names(gw2019) <- c("품목","폐기물종류")

gw2019$품목 <- as.numeric(gw2019$품목)
gw2019$품목 <- ifelse(gw2019$품목 < 30, NA, gw2019$품목)
gw2019 <- na.omit(gw2019)

g3 <- ggplot(gw2019,aes(x=reorder(폐기물종류,품목),y=품목))+
  geom_bar(width=0.7,stat = "identity",aes(fill=품목))+
  coord_flip()+
  scale_fill_gradient(low="green", high = "red")+
  labs(title = "강원도 품목별 폐기물 배출량",
       subtitle = "2019년 일일 평균(단위 : 톤)",
       x = "폐기물 종류",
       y = "배출량")+
  theme(plot.title = element_text(size=20, hjust = 0.5, face = "bold", color = "darkblue"),
        plot.subtitle = element_text(hjust = 1, face = "bold"),
        axis.title.x = element_text(face = "bold", size = 13),
        axis.title.y = element_text(face = "bold", size = 13),
        axis.text.x = element_text(angle = 45))+
  scale_y_continuous(breaks = seq(0,400,50))
  
# v02 - 1인당 폐기물 배출량을 지도로 도식

# 패키지 로드
library(GISTools)
library(maptools)
library(rgdal)

sidoshp <- readOGR("sido/ctp_rvn.shp")
korea <- fortify(sidoshp) # R데이터로 변환
ggplot(korea, aes(x=long,y=lat, group = group, color = id))+
  geom_polygon(fill="white")+
  theme(legend.position = "none")









                     