##################### corpus #####################

text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetar$ian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")
rm(list=ls())
install.packages("tm")
library(tm)

data(crude)
crude

lapply(crude, meta)

crude[[1]] # 리스트 인덱싱과 유사(15개의 메타데이터 요소, 527개의 문자)
crude[[1]]$content
crude[[1]]$meta

## tm 패키지 안에 있는 VCorpus()
VCorpus() # 하나의 코퍼스로 제작 가능(메타정보, 컨텐츠)
getSources() # 데이터 구조에 따른 인식 함수 종류 확인

VectorSource(text) # 벡터에서 corpus source 인식
corpus.docs <- VCorpus(VectorSource(text))
class(corpus.docs)

corpus.docs

## content에 접근

inspect(corpus.docs[1]) # 메타데이터 개수, 컨텐츠 개수 확인 가능
inspect(corpus.docs[[1]]) # 대괄호 하나를 쓰면 기본적인 문서에 대한 개괄적 내용, 두 개를 쓰면 구체적인 문자열도 확인 가능

as.character(corpus.docs[[1]]) # 문자만 추출(컨텐츠 추출)

lapply(corpus.docs, as.character) # list 형태로 데이터에서 문자열 반복해서 추출

str(corpus.docs)

corpus.docs[[1]]$content
lapply(corpus.docs, content) # corpus안에서 content만 뽑아서 list 형태로 리턴
as.vector(unlist(lapply(corpus.docs, content))) # 리스트 형태를 제거하고 벡터로 리턴

paste(as.vector(unlist(lapply(corpus.docs, content))), collapse = " ") # 하나의 문장으로 연결

### 메타 데이터에 접근

corpus.docs[1]$meta
corpus.docs[[1]]$meta

meta(corpus.docs[[1]], tag = "author") # 대괄호 2개를 쓴 위와 동일하지만, tag인자를 통해 가져올 파라미터 지정 가능
meta(corpus.docs[[1]], tag = "id")

meta(corpus.docs[[1]], tag = "author", type = "local") <- "Dong-A" # 비어있던 author에 값 삽입

# 기존 메타 데이터에 삽입, 여러 개를 동시에 삽입 할때는 corpus 자체에 삽입
cor.author <- c("Dong-A","Lee","Park")
meta(corpus.docs, tag = "author", type = "local") <- cor.author 

lapply(corpus.docs, meta) # 1,2,3번째 메타 데이터를 모두 가져옴
lapply(corpus.docs, meta, tag = "author") # 1,2,3번째의 author 데이터만 가져옴

# 9-2

lapply(corpus.docs, meta)
lapply(corpus.docs, content)

category <- c("health","lifestyle","business")
meta(corpus.docs, tag = "category", type = "local") <- category # 새로운 메타 데이터에 새로운 값 삽입

meta(corpus.docs, tag = "origin", type = "local") <- NULL # 메타 데이터 삭제

# tm_filter를 활용해 corpus에서 임의의 함수를 지정하고, content안에 weight or diet가 포함된 문자열의 개수를 확인
# any함수를 사용해 논리형의 개수 파악 후 개수 결과만 출력

corpus.docs.filter <- tm_filter(corpus.docs, FUN = function(x)
  any(grep("weight|diet", content(x)))) 

?grep

corpus.docs.filter

# lapply 함수를 사용해 실제 값 확인
lapply(corpus.docs.filter,content)

# corpus의 메타데이터의 tag에 일치하는 요소가 있는지 T/F로 리턴
index <- meta(corpus.docs, "author") == "Dong-A" | meta(corpus.docs, "author") == "Lee"
index

lapply(corpus.docs, meta)

meta(corpus.docs, tag = "author", type = "local") <- c("Lee","Kim","Park")

lapply(corpus.docs[index], content) # index가 True인 content만 리턴

# corpus를 text 파일 형태로 저장
writeCorpus(corpus.docs)

# txt 저장 확인
list.files(pattern = "\\.txt")

# 텍스트 정제
getTransformations() # tm 패키지에서 제공하는 옵션

?tm_map()

toupper()
tolower()

content_transformer() # tm_map안에서 base 패키지를 써야할 때 사용
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs, content_transformer(tolower)) # 전부 소문자로 변경

# 불용어 stopwords (사용하지 않는 데이터 정제)

stopwords("english") # tm 패키지 안에서 정의된 불용어 확인

# removeWords 뒤에 불용어를 지정해야 함
corpus.docs <- tm_map(corpus.docs, removeWords, stopwords("english"))

# 함수(x,pattern), gsub 함수를 사용해 패턴을 공백으로 대체하는 함수 제작
myremoves <- content_transformer(function(x, pattern)
{return(gsub(pattern, "", x))})

# tm_map(corpus, 함수, pattern 지정) 이때 pattern은 URL 정규 표현식 사용해 URL 제거
corpus.docs <- tm_map(corpus.docs, myremoves, "(f|ht)tp\\S+\\s*")
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs, removePunctuation) # 문장 부호(괄호, 점)등 제거
corpus.docs <- tm_map(corpus.docs, removeNumbers) # 숫자 제거
corpus.docs <- tm_map(corpus.docs, stripWhitespace) # 공백 제거(맨 앞의 공백은 TM패키지에서 제거 불가)
corpus.docs <- tm_map(corpus.docs, content_transformer(trimws)) # 문장 앞, 뒤 공백 제거
lapply(corpus.docs, content)

# 어근 처리

corpus.docs <- tm_map(corpus.docs, stemDocument) # 추출되며 기존의 형태가 변경(과거형, 복수형 등 삭제)
lapply(corpus.docs, content)

# 유의어, 동의어를 하나의 단어로 사용(gsub 함수 사용, 뒤에 gsub의 인자 삽입)
corpus.docs <- tm_map(corpus.docs, content_transformer(gsub), pattern = "economist", 
                      replacement = "economi")
lapply(corpus.docs, content)

############## tidytext, 토큰화 #############
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


################# 문재인 박근혜 비교 ######################

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

################# DTM ##################

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


################ 미국 대통령 연설 ###############

install.packages("quanteda")
library(quanteda)

summary(data_corpus_inaugural) # 미국 역대 대통령 연설문 데이터

class(data_corpus_inaugural)

library(tidytext)
library(tibble)
library(dplyr)

tidy(data_corpus_inaugural) # 하나의 문서를 하나의 행으로 제작(tibble 형태)

# 연임한 대통령의 연설문을 하나로 결합

us.address <- tidy(data_corpus_inaugural) %>%
  filter(Year > 1990) %>% 
  group_by(President, FirstName) %>% 
  arrange(Year) %>% 
  summarise_all(list(~trimws(paste(., collapse = " ")))) %>% # 연임한 대통령을 하나로 합쳐주는 것.
  ungroup()

us.address

library(tm) # corpus 변환을 위해 사용
?DataframeSource() # 사용하기 위해 doc_id, text 컬럼 필요

us.address <- us.address %>% 
  select(text, everything()) %>% # 순서 변경, text를 제일 앞으로
  add_column(doc_id = 1:nrow(.), .before = 1) # doc_id를 만들고 제일 앞으로

us.address # df이기 때문에 dfsource사용

address.corpus <- VCorpus(DataframeSource(us.address)) # 전처리한 데이터를 corpus화
address.corpus
lapply(address.corpus, meta)

# 전처리

address.corpus <- tm_map(address.corpus, content_transformer(tolower)) #소문자 변환

address.corpus[1]$content # 메타 데이터, 컨텐츠 출력
address.corpus[[1]]$content # 컨텐츠 원본 출력

sort(stopwords("english")) # 정의된 불용어 정렬 출력



address.corpus[[1]]$content

address.corpus <- tm_map(address.corpus, removePunctuation) # 구두점 제거
lapply(address.corpus[1], content)

address.corpus <- tm_map(address.corpus, removeNumbers) # 숫자 제거
address.corpus <- tm_map(address.corpus, stripWhitespace) # 공백 제거

address.corpus <- tm_map(address.corpus, content_transformer(trimws)) # 앞뒤 공백 제거

address.corpus <- tm_map(address.corpus, content_transformer(gsub),
                         pattern = "america|american|americans",
                         replacement = "america") # 유의어 처리

mystopwords <- c(stopwords("english"), c("must","will","can","bless","america"))
address.corpus <- tm_map(address.corpus, removeWords, mystopwords) # 불용어 제거

############## DTM ###########

address.dtm <- DocumentTermMatrix(address.corpus) # corpus를 dtm 형태로 변환

inspect(address.dtm) # tf 확인
# non/-sparse entries : 0이 아닌 것 / 0인 것 개수
# sparsity :
# maximal term length : 제일 긴 단어 
# tf : 단어 출연 빈도 수

as.matrix(address.dtm)
termfreq <- colSums(as.matrix(address.dtm)) # 개별 단어의 빈도 확인
termfreq

length(termfreq) # dtm의 제공 정보와 동일

termfreq[head(order(termfreq, decreasing = T))] # 상위 6개 단어 추출

termfreq[head(order(termfreq, decreasing = T),10)] # 상위 10개 단어 추출
termfreq[tail(order(termfreq, decreasing = T),10)] # 하위 10개 단어 추출

findFreqTerms(address.dtm, lowfreq = 40) # 최소 40번 이상 등장한 단어 추출

findFreqTerms(address.dtm, lowfreq = 40, highfreq = 80) # 40~80번 사이 등장한 단어 추출

library(ggplot2)

class(termfreq)

termfreq.df <- data.frame(word = names(termfreq), frequency = termfreq) # df 제작
head(termfreq.df)

ggplot(subset(termfreq.df, frequency >= 40), 
       aes(reorder(word,frequency), frequency,fill=word))+
  geom_col(color = "dimgray", width = 0.5, show.legend = F)+
  geom_text(aes(label = frequency), size = 3, color = "black", hjust = 1)+
  labs(x=NULL, y = "빈도", title = "연설문")+
  coord_flip()

inspect(address.dtm) # dtm 구조 확인
row.names(address.dtm) <- c("clinton","Bush","Obama","Trump","Biden") # dtm 열 이름 변경
Docs(address.dtm) # 열 이름 확인

# 워드 클라우드

set.seed(1234)

library(wordcloud)
library(RColorBrewer)
head(termfreq)
wordcloud(words = names(termfreq), freq = termfreq,
          scale = c(3,0.3), min.freq = 10,
          rot.per = 0.1, random.order = FALSE,
          colors = brewer.pal(6,'Dark2'))


address.tf <- tidy(address.dtm)
address.tf

# 대통령 별 단어 사용 빈도 추출
address.tf <- address.tf %>% 
  mutate(document = factor(document, levels = c("clinton","Bush","Obama","Trump","Biden"))) %>% 
  arrange(desc(count)) %>% # 정렬
  group_by(document) %>% 
  top_n(n=10, wt = count) %>%  # 상위 10개 추출
  ungroup()

address.tf

ggplot(address.tf,
       aes(reorder_within(term,count,document),count,fill=document))+
  geom_col(show.legend = F)+
  facet_wrap(~document, ncol = 3, scales = "free") + # 대통령 별 그래프 분리
  scale_x_reordered()+
  labs(x=NULL, y = "term frequency count")+
  coord_flip()


# facet_wrap으로 그룹화한 경우 정렬을 위해 reorder_within 사용 
# reorder_within을 사용한 경우 scale_x_reordered()도 사용

############# TF-IDF ###############
# 점수가 높은 단어일수록 다른 문서에는 많지 않고 해당 문서에서 자주 등장하는 단어
# idf : df가 클수록 작아지고, 반대일 경우 커짐
# idf가 클수록 드물게 사용되는 단어, 작을수록 흔하게 사용되는 단어

address.dtm2 <- DocumentTermMatrix(address.corpus, 
                                   control = list(weighting = weightTfIdf)) # DTM을 tf-idf로 제작

inspect(address.dtm2)

row.names(address.dtm2) <- c("clinton","Bush","Obama","Trump","Biden")

tidy(address.dtm2)

address.tfidf <- tidy(address.dtm2) %>% 
  mutate(tf_idf = count, count = NULL)

address.tfidf

# 위와 동일

address.tfidf <- address.tfidf %>% 
  mutate(document = factor(document, levels = c("clinton","Bush","Obama","Trump","Biden"))) %>% 
  arrange(desc(tf_idf)) %>% # 정렬
  group_by(document) %>% 
  top_n(n=10, wt = tf_idf) %>%  # 상위 10개 추출
  ungroup()

address.tfidf

ggplot(address.tfidf,
       aes(reorder_within(term,tf_idf,document),tf_idf,fill=document))+
  geom_col(show.legend = F)+
  facet_wrap(~document, ncol = 3, scales = "free") + # 대통령 별 그래프 분리
  scale_x_reordered()+
  labs(x=NULL, y = "tf_idf count")+
  coord_flip()

############# tidytext ################

library(quanteda)
library(tidytext)
library(tibble)
library(dplyr)

us.address <- tidy(data_corpus_inaugural) %>%
  filter(Year > 2000) %>% 
  group_by(President, FirstName) %>% 
  summarise_all(list(~trimws(paste(., collapse = " ")))) %>% 
  arrange(Year) %>% 
  ungroup()

# tidy-text 형식으로 진행
us.address

# 단어 기준 토큰화
address.words <- us.address %>% 
  unnest_tokens(word, text)
address.words

# 전처리
address.words <- address.words %>% 
  anti_join(stop_words, by = 'word') %>% # 불용어 제거
  filter(!grepl(pattern = "\\d+", word)) %>% # 숫자 제거
  mutate(word = gsub(pattern = "'", replacement = "", word)) %>% # 공백 제거
  mutate(word = gsub(pattern = "america|americas|american|americans",
                     replacement = "america",word)) %>% # 동의어 처리
  count(President, word, sort = T, name = 'count') %>%  # 단어 빈도 수 계산
  ungroup()

address.words

# 단어 빈도 시각화

address.words %>% 
  filter(!(word == "america")) %>% # 뺄 단어 사용
  group_by(word) %>% 
  summarise(count=sum(count)) %>% 
  arrange(desc(count)) %>% 
  top_n(n=10, wt=count) %>% 
  ggplot(aes(reorder(word,count),count)) +
  geom_col(color = 'dimgray', fill = 'salmon', width = 0.6, show.legend = F)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=count), size = 3.5, color = "black", vjust = 0)+
  labs(x=NULL, y = "Term Frequency(count)")+
  coord_flip()

# tf-idf 계산

address.words <- address.words %>% 
  bind_tf_idf(term = word, document = President, n = count)

address.words

# tf-idf 순위 확인

par(mfrow=(c(1,2)))

address.words %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(document = factor(President, levels = c("clinton","Bush","Obama","Trump","Biden"))) %>% 
  group_by(President) %>% 
  top_n(7, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(reorder_within(word,tf_idf, President), tf_idf, fill = President))+
  geom_col(show.legend = F)+
  facet_wrap(~President, ncol = 2, scales = "free")+
  scale_x_reordered()+
  labs(x=NULL, y = "Term frequency-Inverse Document Frequency")+
  coord_flip()

# tf-idf 상위 7개 단어 확인

address.words %>% 
  arrange(desc(tf)) %>% 
  mutate(document = factor(President, levels = c("clinton","Bush","Obama","Trump","Biden"))) %>% 
  group_by(President) %>% 
  top_n(7, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(reorder_within(word,tf_idf, President), tf_idf, fill = President))+
  geom_col(show.legend = F)+
  facet_wrap(~President, ncol = 2, scales = "free")+
  scale_x_reordered()+
  labs(x=NULL, y = "Term frequency(proportion)")+
  coord_flip()

################### 감성분석 ####################

library(tidytext)
library(textdata)

## 감성사전 
get_sentiments(lexicon = 'bing')
unique(get_sentiments(lexicon = 'bing')$sentiment)

get_sentiments(lexicon = 'afinn')
unique(get_sentiments('afinn')$value)
summary((get_sentiments('afinn')$value))

get_sentiments(lexicon = 'nrc')
unique(get_sentiments(lexicon = 'nrc')$sentiment)

get_sentiments(lexicon = 'loughran')
unique(get_sentiments(lexicon = 'loughran')$sentiment)


install.packages("purrr")
install.packages("lubridate")
install.packages("reshape2")
install.packages("readr")
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(lubridate) 

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00438/Health-News-Tweets.zip"
local.copy <- tempfile()
download.file(url, local.copy, mode = "wb")
Sys.setlocale("LC_TIME", "English")

map(unzip(zipfile = local.copy,
          files = c("Health-Tweets/bbchealth.txt", 
                    "Health-Tweets/cnnhealth.txt", 
                    "Health-Tweets/foxnewshealth.txt", 
                    "Health-Tweets/NBChealth.txt")), 
    read_delim, delim = "|", quote ="",
    col_types = list(col_character(), col_character(), col_character()), 
    col_names = c("id","datetime","tweet"))

health.twitter <- 
  map(unzip(zipfile = local.copy,
            files = c("Health-Tweets/bbchealth.txt",
                      "Health-Tweets/cnnhealth.txt",
                      "Health-Tweets/foxnewshealth.txt",
                      "Health-Tweets/NBChealth.txt")),
      read_delim, delim = "|", quote = "", 
      col_types = list(col_character(), col_character(),  col_character()),
      col_names = c("id","datetime","tweet")) %>% 
  map2(c("bbc", "cnn", "foxnews", "nbc"), #map2 함수를 활용하여 이름 붙이기
       ~cbind(.x, source=.y)) %>% 
  reduce(bind_rows) %>% 
  as_tibble()  %>% 
  mutate(datetime=ymd_hms(strtime(datetime,
                                  "%a %b %d %H:%M:%S +0000 %Y")))
unlink(local.copy)
Sys.setlocale()

# 파일이 다운로드 되지 않는 경우 csv 파일 불러오기
health.twitter <- read.csv("data/health.twitter.csv") %>% as.tibble()
health.twitter

# 신문사 종류 확인      
health.twitter %>% count(source)

### tidytext 형식으로 단어 추출(긍정/부정의 출현 빈도 확인)

## 전처리 (반복적인 수행이 필요함)
library(stringr)
health.words <- health.twitter %>% 
  select(-id, -X) %>% 
  mutate(tweet = str_replace_all(tweet, pattern = "(f|ht)tp\\S+s*", replacement = "")) %>% 
  mutate(tweet = str_replace_all(tweet, pattern = "\\d+", replacement = "")) %>% 
  mutate(tweet = str_replace_all(tweet, pattern = "\\bRT", replacement = "")) %>% #리트윗 삭제(\\b)
  mutate(tweet = str_replace_all(tweet, pattern = "@\\S+", replacement = "")) %>% 
  mutate(tweet = str_replace_all(tweet, pattern = "&amp", replacement = "")) %>% 
  unnest_tokens(word, tweet) # 토큰화
health.words

## 감성어휘사전과 결합
health.words %>% 
  inner_join(get_sentiments("bing"), by = "word") # 감성어휘 사전에 없는 단어는 사라짐 

# 단어 빈도 확인
health.words %>% 
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(word, sentiment, sort = TRUE)

# 빈도 수 상위 긍정/부정 단어 각각 10개씩 확인
health.words %>% 
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup()



## 시각화
library(ggplot2)
library(scales)


# 긍정과 부정 빈도를 구분하여 시각화하기 위해 nsign 열 생성
health.sentiment<- health.words %>% 
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(nsign = ifelse(sentiment == "negative", -n, n)) # sentiment가 negative이면 출현 빈도를 -n으로 표시
health.sentiment


# 막대그래프
ggplot(health.sentiment,
       aes(x=reorder(word, nsign), y=nsign,
           fill = factor(sentiment,
                         levels = c("positive", "negative")))) + 
  geom_col(color = "lightslategray", width = 0.8)+
  geom_text(aes(label=n), size= 3, color="black",
            hjust=ifelse(health.sentiment$nsign <0, 1.1, -0.1))+
  scale_fill_manual(values = c("cornflowerblue", "tomato")) +
  scale_y_continuous(breaks = pretty(health.sentiment$nsign),
                     labels = abs(pretty(health.sentiment$nsign))) +
  labs(x=NULL, y = 'count') + 
  theme(legend.position = "bottom", legend.title = element_blank()) +
  coord_flip()

#고빈도 단어 제거하기(의학용어와 관련된 단어이기 때문에)  
health.sentiment<- health.words %>% 
  inner_join(get_sentiments("bing"), by = "word") %>% 
  filter(!(word == "patient"|word == "cancer"|word == "virus")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(nsign = ifelse(sentiment == "negative", -n, n))

health.sentiment

# 막대그래프 (단어의 출현빈도를 활용)
ggplot(health.sentiment,
       aes(x=reorder(word, n), y=n,
           fill = factor(sentiment,
                         levels = c("positive", "negative")))) + 
  geom_col(color = "lightslategray", width = 0.8, show.legend = FALSE)+
  geom_text(aes(label=n), size= 3, color="black",
            hjust= 1.2)+
  scale_fill_manual(values = c("lightsteelblue", "lightsalmon")) +
  facet_wrap(~factor(sentiment,
                     levels = c("positive", "negative")),
             ncol = 2, scales = 'free') +
  labs(x=NULL, y = 'Count') + 
  coord_flip()

# wordcloud(긍정/부정 단어 나타내기)
library(wordcloud)
library(reshape2)

set.seed(123)
health.words %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  filter(!(word == "patient"|word == "cancer"|word == "virus")) %>%
  count(word, sentiment, sort = TRUE) %>% 
  ungroup() %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("tomato","cornflowerblue"),
                   title.size = 2,
                   title.colors = c("red","blue"),
                   title.bg.colors = "wheat",
                   scale = c(4, 0.3), max.words = 200) #범주를 나타내는 단어 크기 

# 시각화를 통해 뉴스 서비스 별 긍부정 단어 확인

# 뉴스 별 빈도 상위 단어 추출
health.sentiment <- health.words %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  filter(!(word == "patient"|word == "cancer"|word == "virus")) %>%
  count(word, sentiment, source, sort = TRUE) %>%
  group_by(source, sentiment) %>% 
  top_n(10, n) %>% 
  ungroup()

health.sentiment


# 서비스별 긍부정 단어 막대그래프
ggplot(health.sentiment,
       aes(reorder_within(x=word, by =n, within = source),
           y=n, fill=source))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ factor(source,
                      labels = c("BBC", "CNN", "Fox News", "NBC"))+ sentiment,
             ncol=2, scale = "free")+
  scale_x_reordered()+
  labs(x=NULL, y="Count") +
  coord_flip()

# wordcloud(긍정/부정 단어 나타내기)
library(wordcloud)
library(reshape2)

set.seed(123)
health.words %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  filter(!(word == "patient"|word == "cancer"|word == "virus")) %>%
  count(word, sentiment, sort = TRUE) %>% 
  ungroup() %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("tomato","cornflowerblue"),
                   title.size = 2,
                   title.colors = c("red","blue"),
                   title.bg.colors = "wheat",
                   scale = c(4, 0.3), max.words = 200) #범주를 나타내는 단어 크기 


###################### 트위터 이낙연 이재명 비교 ##################

library(dplyr)
library(readr)
# 2020년 8월 13일~21일 이낙연 이재명 경기도지사를 언급한 트위터 데이터 
# https://www.tidytextmining.com/tfidf.html

bind_tweet <- bind_rows( # row_bind로 2개의 데이터를 하나로 결합
  read_csv("data/tweet_nak.csv") %>% mutate(candidate = "이낙연"),
  read_csv("data/tweet_jae.csv") %>% mutate(candidate = "이재명")) # 구분자 변수 생성
glimpse(bind_tweet) # 데이터 구성 확인

# 총 13,928개의 트윗으로 구성된 데이터, 두 후보를 함께 언급한 트윗이 있을테니, 중복된 행 존재

bind_tweet

# 트위터 전처리
install.packages("lubridate")
install.packages("textclean")
install.packages("dplyr")
library(lubridate)
library(textclean)
library(stringr)
library(tibble)
library(readr)
library(dplyr)
set.seed(1234)


tweet <- bind_tweet %>%
  mutate(text = replace_tag(str_to_lower(text)), # id 태그 제거
         text = str_squish(replace_html(text)), # html 특수 문자 제거
         date = date(created_at)) %>% # 날짜 변수 생성
  filter(!str_detect(text, "https://")) %>% # 광고 트윗 제거
  group_by(candidate) %>% # 중복 글 제거
  distinct(text, .keep_all = T) %>%
  group_by(candidate, date, screen_name) %>%  
  ungroup()

# slice_sample(n=5) # 사용자별 하루 최대 5개 추출

glimpse(tweet)
dim(tweet)

# 날짜, 후보별 빈도
frequency_date <- tweet %>%
  count(date, candidate)
frequency_date

# 선 그래프
library(ggplot2)
ggplot(frequency_date, aes(x = date, y = n, col = candidate)) +
  geom_line()

# SNS 이슈 알아보기
# 유독 두 후보의 언급량이 많은 8월 14일에 무슨 일이 있었는지 알아보기

library(tidytext)
library(KoNLP)
word_tweet_raw <- tweet %>%
  unnest_tokens(input = text,
                output = word,
                token = "words",
                drop = F)


frequency14 <- word_tweet_raw %>%
  mutate(category = ifelse(date == "2020-08-14", "target", "etc")) %>%
  filter(str_count(word) >= 2) %>%
  count(category, word, sort = T)

frequency14

# Wide form으로 변환(행/열 전환)
library(tidyr)
wide14 <- frequency14 %>%
  pivot_wider(names_from = category,
              values_from = n,
              values_fill = list(n = 0))


# 로그 오즈비 변수 추가
wide14 <- wide14 %>%
  mutate(log_odds_ratio = log(((target + 1) / (sum(target + 1))) /
                                ((etc + 1) / (sum(etc + 1)))))
# log_odds_ratio 높은 순 출력
wide14 %>%
  arrange(-log_odds_ratio) %>%
  head(20)

#트윗 원문 살펴보기
tweet %>%
  filter(date == "2020-08-14" & str_detect(text, "조사")) %>%
  head(10) %>%
  pull(text)

# 8월 18일과 19일에 이낙연 의원의 언급량이 크게 상승함

frequency_nak1819 <- word_tweet_raw %>%
  mutate(category = ifelse(date >= "2020-08-18" &
                             date <= "2020-08-19", "target", "etc")) %>%
  filter(candidate == "이낙연" & str_count(word) >= 2) %>%
  count(category, word, sort = T)

# Wide form으로 변환
wide_nak1819 <- frequency_nak1819 %>%
  pivot_wider(names_from = category,
              values_from = n,
              values_fill = list(n = 0))

# 로그 오즈비 변수 추가
wide_nak1819 <- wide_nak1819 %>%
  mutate(log_odds_ratio = log(((target + 1) / (sum(target + 1))) /
                                ((etc + 1) / (sum(etc + 1)))))
# log_odds_ratio 높은 순 출력
wide_nak1819 %>%
  arrange(-log_odds_ratio) %>%
  head(20)

# 트윗 내용 확인
tweet %>%
  filter(date >= "2020-08-18" & date <= "2020-08-19" &
           candidate == "이낙연" & str_detect(text, "다행입니다")) %>%
  head(10) %>%
  pull(text)


# 감정 단어 살펴보기
# 감정 사전 불러오기
dic <- read_csv("data/knu_sentiment_lexicon.csv")
summary(dic)

# 감정 점수 부여, 감정 극성 분류
word_tweet <- word_tweet_raw %>%
  left_join(dic, by = "word") %>% # 감정 점수 부여
  mutate(polarity = ifelse(is.na(polarity), 0, polarity), # NA를 0으로 변환
         sentiment = ifelse(polarity == 2, "긍정", # 감정 범주 분류
                            ifelse(polarity == -2, "부정", "중립")))

# 자주 언급한 단어 추출
top10_word <- word_tweet %>%
  # 불용어 제거
  filter(!(candidate == "이낙연" & str_detect(word, "이낙연")) & # 이낙연이 이낙연을 언급한 것 제거
           !(candidate == "이재명" & str_detect(word, "이재명"))) %>%
  filter(str_count(word) >= 2) %>%
  count(candidate, sentiment, word) %>%
  group_by(candidate, sentiment) %>%
  slice_max(n, n = 10, with_ties = F)
top10_word

table(top10_word$sentiment)

# 그래프 그리기
ggplot(top10_word, aes(x = reorder_within(word, n, candidate),
                       y = n,
                       fill = sentiment)) +
  geom_col() +
  coord_flip() +
  facet_wrap(candidate ~ sentiment, # 후보, 감정 범주별 그래프 생성
             scales = "free") +
  scale_x_reordered()


# 감정 경향 살펴보기
# 트윗 감정 점수 구하기
sentiment_tweet <- word_tweet %>%
  group_by(candidate, status_id) %>% # 후보자, id별 점수
  summarise(score = sum(polarity)) %>%
  ungroup()

sentiment_tweet

# 트윗 원문에 감정 점수 결합
tweet <- tweet %>%
  left_join(sentiment_tweet, by = c("candidate", "status_id"))

# 감정 점수 히스토그램
hist(tweet$score)
tweet

# 확률 밀도 구하기기
ggplot(tweet, aes(x = score, fill = candidate)) +
  geom_density(adjust = 2, alpha = 0.3)


#트윗 감정 추이 선 그래프 만들기

tweet <- tweet %>%
  mutate(sentiment = ifelse(score >= 1, "긍정",
                            ifelse(score <= -1, "부정", "중립")))

# 후보, 감정별 빈도 및 비율
frequency_sentiment <- tweet %>%
  group_by(candidate) %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n))
frequency_sentiment


#날짜별 감성 변화
sentiment_candidate <- tweet %>%
  count(date, candidate, sentiment)
sentiment_candidate

## 트윗 감정 추이 선 그래프
ggplot(sentiment_candidate, aes(x = date, y = n, col = sentiment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ candidate, nrow = 2, scales = "free_x")


################# 윤석열 예측 ####################

library(dplyr)
library(stringr)
library(ggplot2)
library(tm)
library(SnowballC)
library(quanteda)
library(tibble)
library(textdata)
library(textclean)
library(purrr)
library(readr)
library(lubridate)
library(scales)

############ 윤석열 연설문 ################

raw_yoon = readLines("data/speech_yoon.txt", encoding = "UTF-8")
raw_yoon

yoon <- raw_yoon %>% str_replace_all("[^가-힣]", " ")
yoon

yoon <- str_squish(yoon)
yoon

yoon <- as_tibble(yoon)
yoon

library(tidytext)
library(KoNLP)

yoon_space <- yoon %>% 
  unnest_tokens(input = value, output = word, token = "words")

yoon_space1 <- yoon %>% 
  unnest_tokens(input = value, output = word, token = extractNoun)

yoon_space

yoon_space1

yoon_space <- yoon_space %>% count(word, sort = T) %>% 
  filter(str_count(word)>1)

yoon_space1 <- yoon_space1 %>% count(word, sort = T) %>% 
  filter(str_count(word)>1)

top_20_yoon <- yoon_space %>% head(20)
top_20_yoon1 <- yoon_space1 %>% head(20)

top_20_yoon
top_20_yoon1


a <- raw_yoon %>% str_replace_all("[^가-힣]", " ") %>% 
  str_squish() %>% 
  as_tibble() %>% 
  unnest_tokens(input = value, output = word, token = extractNoun) %>% 
  count(word, sort = T) %>% 
  filter(str_count(word)>1) %>% 
  head(20)


ggplot(a, aes(x=reorder(word,n),n))+
  geom_col()+
  coord_flip()

library(ggwordcloud)

ggplot(a, aes(label = word, size = n))+
  geom_text_wordcloud(seed = 1234)+
  scale_radius(limits = c(3,NA), # 최소, 최대 단어 빈도
               range = c(3,30))+  # 최소, 최대 글자 크기
  theme_minimal()

