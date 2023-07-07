rm(list=ls())

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









