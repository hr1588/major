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
  summarise_all(list(~trimws(paste(., collapse = " ")))) %>% 
  arrange(Year) %>% 
  ungroup()

us.address

library(tm)
?DataframeSource() # 사용하기 위해 doc_id, text 컬럼 필요

us.address <- us.address %>% 
  select(text, everything()) %>% # 순서 변경, text를 제일 앞으로
  add_column(doc_id = 1:nrow(.), .before = 1) # doc_id를 만들고 제일 앞으로

us.address

address.corpus <- VCorpus(DataframeSource(us.address)) # 전처리한 데이터를 corpus화
address.corpus
lapply(address.corpus, meta)

# 전처리

address.corpus <- tm_map(address.corpus, content_transformer(tolower)) #소문자 변환

address.corpus[1]$content # 메타 데이터, 컨텐츠 출력
address.corpus[[1]]$content # 컨텐츠 원본 출력

sort(stopwords("english")) # 정의된 불용어 정렬 출력

mystopwords <- c(stopwords("english"), c("must","will","can"))

address.corpus <- tm_map(address.corpus, removeWords, mystopwords) # 불용어 제거
address.corpus[[1]]$content

address.corpus <- tm_map(address.corpus, removePunctuation) # 구두점 제거
lapply(address.corpus[1], content)

address.corpus <- tm_map(address.corpus, removeNumbers) # 숫자 제거
address.corpus <- tm_map(address.corpus, stripWhitespace) # 공백 제거

address.corpus <- tm_map(address.corpus, content_transformer(trimws)) # 앞뒤 공백 제거

address.corpus <- tm_map(address.corpus, content_transformer(gsub),
                         pattern = "america|american|americans",
                         replacement = "america") # 유의어 처리

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

row.names(address.dtm) <- c("clinton","Bush","Obama","Trump","Biden") # dtm 열 이름 변경
Docs(address.dtm)

address.tf <- tidy(address.dtm)

# 대통령 별 단어 사용 빈도 추출
address.tf <- address.tf %>% 
  mutate(document = factor(document, levels = c("clinton","Bush","Obama","Trump","Biden"))) %>% 
  arrange(desc(count)) %>% # 정렬
  group_by(document) %>% 
  top_n(n=10, wt = count) %>% 
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

# TF-IDF
?DocumentTermMatrix



