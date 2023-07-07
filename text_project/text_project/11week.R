library(dplyr)

raw_moon <- readLines("data/speech_moon.txt", encoding = "UTF-8")
moon <- raw_moon %>%
  as_tibble() %>%
  mutate(president = "moon")

raw_park <- readLines("data/speech_park.txt", encoding = "UTF-8")
park <- raw_park %>%
  as_tibble() %>%
  mutate(president = "park")

bind_speeches <- bind_rows(moon,park) %>% 
  select(president, value)

bind_speeches %>% count(president)

# 전처리
library(stringr)
speeches <- bind_speeches %>% 
  mutate(value = str_replace_all(value, "^[가-힣]"," "),
         value = str_squish(value))

speeches

# 토큰화
library(tidytext)
library(KoNLP)
speeches <- speeches %>% 
  unnest_tokens(input = value, 
                output = word, 
                token = extractNoun)

speeches

frequency <- speeches %>% 
  count(president,word) %>% # 연설문 및 단어별 빈도
  filter(str_count(word)>1) # 두 글자 이상 추출

head(frequency)
frequency

# dplyr :: slice_max() : 값이 큰 상위 n개의 행을 추출해 내림차순 정렬
top10 <- frequency %>% 
  group_by(president) %>% 
  arrange(desc(n)) %>% 
  head(10)

top10

top10 <- frequency %>% 
  group_by(president) %>% 
  slice_max(n, n=10, with_ties = F) # 동률인 것을 무시
top10

library(ggplot2)
ggplot(top10, aes(x=reorder(word,n),
                  y=n,
                  fill = president))+
  geom_col()+
  coord_flip()+
  facet_wrap(~ president, scales = "free_y") # 대통령별 분리


# tidytext :: scale_x_reordered() : 각 단어 뒤의 범주 항목 제거

ggplot(top10, aes(x=reorder_within(word,n, president),
                  y=n,
                  fill = president))+
  geom_col()+
  coord_flip()+
  facet_wrap(~ president, scales = "free_y")+
  scale_x_reordered()+
  labs(x=NULL)
  # theme(text = element_text(family = "nanumgothic")) # 폰트

# 11-1 odds ratio

df_long <- frequency %>% 
  group_by(president) %>% 
  slice_max(n,n=10) %>% 
  filter(word %in% c("국민","우리","정치","행복"))

df_long

# pivoting(행/렬 전환과 유사)
install.packages("tidyr")
library(tidyr)

df_wide <- df_long %>% 
  pivot_wider(names_from = president,
              values_from = n)

df_long
df_wide

frequency_wide <- frequency %>% 
  pivot_wider(names_from = president, # 컬럼명
              values_from = n,        # 값
              values_fill = list(n=0))# 결측치 대체
frequency_wide

# odds ratio 계산
frequency_wide <- frequency_wide %>% 
  mutate(ratio_moon = ((moon)/sum(moon)), # moon에서 단어의 비중
         ratio_park = ((park)/sum(park))) # park에서 단어의 비중 

frequency_wide

# 단어 비중 비교를 위해서 각 행에 1을 더함(0을 제거하기 위함)

frequency_wide <- frequency_wide %>% 
  mutate(ratio_moon = ((moon+1)/sum(moon+1)), # moon에서 단어의 비중
         ratio_park = ((park+1)/sum(park+1))) # park에서 단어의 비중 

frequency_wide

# "moon"에서 상대적인 비중 클수록 1보다 큰 값
# "park"에서 상대적인 비중 클수록 1보다 작은 값

frequency_wide <- frequency_wide %>% 
  mutate(odds_ratio = ratio_moon/ratio_park)

frequency_wide

frequency_wide %>% 
  arrange(-odds_ratio)

frequency_wide %>% 
  arrange(odds_ratio)

# 상대적으로 중요한 단어 추출
top10 <- frequency_wide %>% 
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)

top10

# 대통령을 구분하는 파생변수 생성
top10 <- top10 %>% 
  mutate(president = ifelse(odds_ratio > 1, "moon", "park"),
         n = ifelse(odds_ratio > 1, moon, park))

top10 <- top10 %>% 
  group_by(president) %>% 
  slice_max(n, n=10, with_ties = F)

top10

# 그래프 별로 축 별도 설정

ggplot(top10, aes(x=reorder_within(word,n, president),
                  y=n,
                  fill = president))+
  geom_col()+
  coord_flip()+
  facet_wrap(~ president, scales = "free_y")+
  scale_x_reordered()+
  labs(x=NULL)

# 로그 오즈비

frequency_wide <- frequency_wide %>% 
  mutate(log_odds_ratio = log(odds_ratio))

frequency_wide %>% 
  arrange(-log_odds_ratio)

frequency_wide %>% 
  arrange(log_odds_ratio)

top10 <- frequency_wide %>% 
  group_by(president = ifelse(log_odds_ratio > 0, "moon", "park")) %>% 
  slice_max(abs(log_odds_ratio), n =10,with_ties = F)
top10

top10 %>% 
  arrange(-log_odds_ratio) %>% 
  select(word, log_odds_ratio, president)

# 서로 다른 방향으로 막대 그래프 그리기
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = president))+
  geom_col()+
  coord_flip()+
  labs(x=NULL)

### 11-2 DTM

text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetar$ian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")


# 텍스트 전처리
library(tm)
corpus.docs <- VCorpus(VectorSource(text))
corpus.docs <- tm_map(corpus.docs, content_transformer(tolower)) # 소문자 변환
corpus.docs <- tm_map(corpus.docs, removeWords, stopwords('english')) # 불용어 제거

myRemove <- content_transformer(function(x, pattern) 
  {return(gsub(pattern, "", x))}) # 특정 패턴을 가지는 문자열 제거를 위한 함수생성
corpus.docs <- tm_map(corpus.docs, myRemove, "(f|ht)tp\\S+\\s*") # 함수를 이용한 URL 삭제
corpus.docs <- tm_map(corpus.docs, removePunctuation) # 문장부호 삭제
corpus.docs <- tm_map(corpus.docs, removeNumbers) # 숫자 삭제
corpus.docs <- tm_map(corpus.docs, stripWhitespace) # 여백 삭제
corpus.docs <- tm_map(corpus.docs, content_transformer(trimws)) # 텍스트 앞뒤의 공백 삭제
corpus.docs <- tm_map(corpus.docs, stemDocument) # 어간 추출
corpus.docs <- tm_map(corpus.docs, content_transformer(gsub),
                      pattern = 'economist', replacement = 'economi') # 동의어 처리

# corpus 형식의 DTM 생성(문서-용어 행렬 변환)

?DocumentTermMatrix

corpus.dtm <- DocumentTermMatrix(corpus.docs,
                   control = list(wordLengths = c(2,Inf)))
corpus.dtm

nTerms(corpus.dtm) # 전체 단어의 개수
nDocs(corpus.dtm) # 문서의 개수 확인
Terms(corpus.dtm) # 단어 자체 확인

Docs(corpus.dtm) # 문서의 이름 확인(초기 값은 숫자)
row.names(corpus.dtm) <- c("BBC","CNN","FOX")

inspect(corpus.dtm) # dtm matrix 확인
inspect(corpus.dtm[1:3,10:19]) # 인덱싱을 통해 접근

# tidytext 형식의 DTM 생성

source <- c("BBC","CNN","FOX")
library(tidytext)
library(dplyr)
library(SnowballC)

text.df <- tibble(source = source, text = text)
text.df

text.df$text <- gsub("(f|ht)tp\\S+\\s*","",text.df$text) # url 삭제
text.df$text <- gsub("\\d+","",text.df$text) # 숫자 삭제
tidy.docs <- text.df %>% 
  unnest_tokens(output = word, input = text) %>% # 토큰화
  anti_join(stop_words, by = "word") %>% # 불용어 제거
  mutate(word = wordStem(word)) # 어간 추출

tidy.docs$word <- gsub("\\s+","",tidy.docs$word) # 공백 제거
tidy.docs$word <- gsub("economist", "economi", tidy.docs$word) # 동의어 처리

tidy.docs %>% count(source,word) # source, word를 중심으로 수량 확인
tidy.docs

tidy.dtm <- tidy.docs %>% count(source,word) %>% 
  cast_dtm(document = source, term = word, value = n) # 행, 열, 셀 값

Terms(tidy.dtm) # 단어 자체 확인
Docs(tidy.dtm)
inspect(tidy.dtm[1:2,3:5])
inspect(tidy.dtm)








