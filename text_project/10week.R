rm(list=ls())
text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetar$ian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")

source <- c("BBC","FOX","CNN")

library(dplyr) # dplyr내부 tibble 활용
text.df <- tibble(source = source, text = text)
text.df
class(text.df)

# tokenization
library(tidytext)

# input => 가져올 데이터, output => 가져온 데이터의 컬럼 명, tbl => 데이터 프레임
unnest_tokens(tbl = text.df, output = word, input = text) 

tidy.docs <- text.df %>% 
unnest_tokens(output = word, input = text) # 위와 동일한 형태

print(tidy.docs, n=Inf) # 처음부터 끝까지 출력

tidy.docs %>% count(source) %>% arrange(desc(n)) # source 기준으로 개수를 세고 내림차순 정렬

# 불필요한 단어 제거

?anti_join()
stop_words
anti_join(tidy.docs, stop_words, by = "word")  #아래와 동일한 결과
tidy.docs <- tidy.docs %>% anti_join(stop_words, by = "word") # stop_words에 저장된 불용어 삭제

word.remove <- tibble(word = c("http", "bbc.in", "1g0j4agg")) # 단어 지정

tidy.docs <- tidy.docs %>% anti_join(word.remove, by = "word") # 지정한 단어 삭제
tidy.docs

grep("\\d+", tidy.docs$word) # 정규 표현식을 활용 word에서 숫자가 있는 위치 확인

tidy.docs <- tidy.docs[-grep("\\d+", tidy.docs$word),] # 숫자 제거

text.df$text <- gsub("(f|ht)tp\\S+\\s*","",text.df$text) # 정규 표현식 활용 url 제거
text.df$text

text.df$text <- gsub("\\d+","",text.df$text) # 정규 표현식 활용 숫자 제거

# 정규 표현식 이후 위와 동일한 작업 실시

tidy.docs <-  text.df %>%
  unnest_tokens(output = word, input = text)
tidy.docs <- tidy.docs %>% anti_join(stop_words, by = "word") 

# 불필요하다고 생각하는 단어 제거
tidy.docs$word <- gsub("ian","",tidy.docs$word)
tidy.docs

# 유의어 처리
tidy.docs$word <-gsub("economists","economy",tidy.docs$word)
tidy.docs

# corpus를 tidy-text로 변환
library(tm)

corpus.docs <- VCorpus(VectorSource(text))

meta(corpus.docs, tag = "author", type = "local") <- source # meta 데이터 삽입

lapply(corpus.docs,meta) # corpus 확인

tidy(corpus.docs) # corpus를 tibble형태로 변환
tidy(corpus.docs) %>% 
  unnest_tokens(output = word, input = text) %>% select(source=author, word)


### 문재인 박근혜 비교

library(dplyr)
# 문재인 대통령 연설문 불러오기
raw_moon <- readLines("data/speech_moon.txt", encoding = "UTF-8")
moon <- raw_moon %>%
  as_tibble() %>%
  mutate(president = "moon") # 결합 이후 구분을 위해 파생변수 추가

# 박근혜 대통령 연설문 불러오기
raw_park <- readLines("data/speech_park.txt", encoding = "UTF-8")
park <- raw_park %>%
  as_tibble() %>%
  mutate(president = "park")

# 데이터 결합
bind_speeches <- bind_rows(moon, park) %>%
  select(president, value)

bind_speeches %>% count(president)

head(bind_speeches)
tail(bind_speeches)

# 기본적인 전처리
library(stringr)
speeches <- bind_speeches %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "), # 모든 한글이 아닌것을 공백처리
         value = str_squish(value)) # 좌/우 공백 제거
speeches

# 토큰화
library(tidytext)
library(KoNLP)
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun) # 명사 추출 (token = extractNoun)
speeches

frequency <- speeches %>%
  count(president, word) %>% # 연설문 및 단어별 빈도
  filter(str_count(word) > 1) # 두 글자 이상 추출
head(frequency)


# dplyr::slice_max() : 값이 큰 상위 n개의 행을 추출해 내림차순 정렬
top10 <- frequency %>%
  group_by(president) %>% # president별로 분리
  arrange(desc(n)) %>% # 상위 10개 추출
  head(10)
top10

top10 <- frequency %>%
  group_by(president) %>% # president별로 분리
  arrange(desc(n)) %>% # 상위 10개 추출
  head(10) %>% filter(president == "park")


# 위와 다르게 대통령별로 10개씩 순차적으로 추출

  frequency %>%
  group_by(president) %>% 
  slice_max(n, n= 10)
top10

print(top10, )

  frequency %>%
  group_by(president) %>% 
  slice_max(n, n= 10, with_ties = F) # 동점이 있을 때 n에 입력한 만큼만 행 추출

library(ggplot2)

ggplot(top10, aes(x = reorder(word,n),
                  y = n,
                  fill = president))+
  geom_col()+
  coord_flip()+
  facet_wrap(~president, scales = "free_y") # 범주별 분리, y축도 분리

# tidytext :: scale_x_reordered() : 각 단어 뒤의 범주 항목 제거

ggplot(top10, aes(x = reorder_within(word,n,president),
                  y=n,
                  fill = president))+
  geom_col()+
  coord_flip()+
  facet_wrap(~president, scales = "free_y")+
  scale_x_reordered()+
  labs(x=NULL)+
  theme(text = element_text(family = "nanumgothic")) # 폰트

## KoNLP 설치

install.packages("multilinguer")
library(multilinguer)
install_jdk()

install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch")) 
library(KoNLP)

a = "저는 동아대학교 경영정보학과 3학년 이동현입니다. 데이터 분석에 관심이 많고, 많은 것을 배워보고 싶습니다."
extractNoun(a)


