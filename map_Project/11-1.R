#220511 대면수업

# 패키지 설치
install.packages("GISTools")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
library(GISTools)
library(maptools)
library(ggplot2)
library(dplyr)
library(rgdal)
library(RColorBrewer)

# 데이터 불러오기
sidoshp <- readOGR("sido/ctp_rvn.shp")
class(sidoshp)
summary(sidoshp)
slotNames(sidoshp)
sidoshp@data
sidoshp@polygons


### 시도 지도 표시 ###

korea <- fortify(sidoshp) # R데이터로 변환
ggplot(korea, aes(x=long,y=lat, group = group, color = id))+
  geom_polygon(fill="white")+
  theme(legend.position = "none")
 # x : 경도, y : 위도

head(korea)

ggplot(korea[korea$id == 16,], aes(x=long,y=lat, group = group, color = id))+
  geom_polygon(fill="white")+
  theme(legend.position = "none")

# ggplot에서 공간 데이터를 사용하기 위해 좌표 변환
sidoshp@proj4string

#spTransform()함수를 사용하여 좌표계 정보를 변환
korea <- spTransform(sidoshp, CRS("+proj=longlat"))
korea@proj4string

pop <- read.csv(file.choose(),header = T)
pop <- subset(pop, id!=7)
korea <- fortify(korea)

korea <- subset(korea, id!=7)
korea_map <- merge(korea, pop, by = "id")

head(korea_map)

ggplot(korea_map, aes(long,lat, group=group, fill=diff), options("scipen"=100))+
  geom_polygon()+
  scale_fill_gradientn(colors = brewer.pal(9, name = "RdYlBu"))
  



