
# 텍스트 전처리

raw_moon = readLines("speech_moon.txt",encoding="UTF-8") #연설문 불러오기
raw_moon
head(raw_moon)
class(raw_moon)

txt <- "치킨은!! 맛있다, xyz 정말 맛있다 !@#"
txt

library(stringr)

# 불필요한 문자 제거(정규 표현식 활용)
str_replace_all(txt, "[^가-힣]", replacement = " ") # 한글이 아닌것을 공백으로 치환
moon <-  raw_moon %>% str_replace_all("[^가-힣]", " ")
head(moon,7)

# 불필요한 공백 제거

moon <- str_trim(moon) 
head(moon,7)

moon <- str_squish(moon) # 연속된 공백 제거
head(moon,7)


# 문자열 벡터 tibble 구조로 변환

library(dplyr)
moon <- as_tibble(moon)
moon
class(moon)

# 토큰화
# tidytext, unnest_token() 활용

install.packages("tidytext")
library(tidytext)
?tidytext
?unnest_tokens

text <- tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")
text %>% unnest_tokens(input=value, output = word, token = "sentences") # 문장
text %>% unnest_tokens(input=value, output = word, token = "words") # 단어
text %>% unnest_tokens(input=value, output = word, token = "characters") # 음절

moon_space <- moon %>% 
  unnest_tokens(input = value, output = word, token = "words")

moon_space

# 단어 빈도 분석
moon_space1 <- moon_space %>% count(word, sort = T) %>% 
  filter(str_count(word)>1) # 개수를 세서 정렬, 두글자 이상만 남기기
moon_space1

# 자주 사용된 상위 20개 단어 추출
top_20_moon <- moon_space1 %>% head(20)
top_20_moon

# 막대 그래프 만들기
ggplot(top_20_moon,aes(x=reorder(word,n),y=n))+ # 단어 빈도순 정렬(reorder)
  geom_col()+ # 막대 그래프
  coord_flip() # x/y 변환

# 워드 클라우드
install.packages("ggwordcloud")
library(ggwordcloud)

ggplot(moon_space1, aes(label = word, size = n))+
  geom_text_wordcloud(seed = 1234)+
  scale_radius(limits = c(3,NA), # 최소, 최대 단어 빈도
               range = c(3,30))+  # 최소, 최대 글자 크기
  theme_minimal()+
  scale_color_gradient(low = "darkred",high = "red")
?ggwordcloud








