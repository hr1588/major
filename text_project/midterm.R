# 텍스트 전처리

raw_park = readLines("speech_park.txt",encoding="UTF-8") #연설문 불러오기
head(raw_park)
class(raw_moon)

library(stringr) 

# 불필요한 문자 제거(정규 표현식 활용)
park <- raw_park %>% str_replace_all("[^가-힣]", " ")
head(park,7)

# 불필요한 공백 제거

park <- str_trim(park) 
park <- str_squish(park) # 연속된 공백 제거
head(park)

# 문자열 벡터 tibble 구조로 변환

library(dplyr)
park <- as_tibble(park)
park
class(park)

# 토큰화
# tidytext, unnest_token() 활용

library(tidytext)

park_space <- park %>% 
  unnest_tokens(input = value, output = word, token = "words")

park_space

# 단어 빈도 분석
park_space <- park_space %>% count(word, sort = T)
park_space

# 자주 사용된 상위 20개 단어 추출
top_20_park <- park_space %>% head(20)
top_20_park


# 막대 그래프 만들기

library(ggplot2)

ggplot(top_20_park,aes(x=reorder(word,n),y=n,fill=n))+ # 단어 빈도순 정렬(reorder)
  geom_col()+ # 막대 그래프
  coord_flip()+ # x/y 변환
  scale_fill_gradient(low = "blue", high = "red")


library(ggwordcloud)

ggplot(park_space, aes(label = word, size = n, color = n))+
  geom_text_wordcloud(seed = 1234)+
  scale_radius(limits = c(3,NA),
               range = c(3,30)) +
  theme_minimal()+
  scale_color_gradient(low="green", high = "red") +
  ggtitle("박근혜")

?ggwordcloud



