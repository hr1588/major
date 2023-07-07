
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

# DTM

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

# TF-IDF : 점수가 높은 단어일수록 다른 문서에는 많지 않고 해당 문서에서 자주 등장하는 단어
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

## tidytext

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

