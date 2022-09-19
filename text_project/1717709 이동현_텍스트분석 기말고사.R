# 1717709 이동현 텍스트분석 기말고사

library(tm)
library(stringr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(KoNLP)
library(tibble)
library(RColorBrewer)
library(lubridate)
library(textclean)
library(readr)

bts <- read_csv("data/news_comment_BTS.csv")
dic <- read_csv("data/knu_sentiment_lexicon.csv")

# 문제 1.
dim(bts)
table(is.na(bts))

# 문제 2
news_comment <- bts %>%
  mutate(id = row_number(),
         reply = str_squish(replace_html(reply)))

news <- news_comment %>%
  select(id, reply)

news

news <- news %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F)
news

# 문제 3
news_polarity <- news %>%
  left_join(dic, by = "word") %>% # 감정 점수 부여
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  arrange(-polarity)

score_comment <- news_polarity %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

# 문제 4

news_polarity <- score_comment %>%
  mutate(sentiment = ifelse(score >= 1, "긍정",
                            ifelse(score <= -1, "부정", "중립")))

news_polarity %>% count(sentiment)
table(news_polarity$sentiment)

# 문제 5

library(tidyr)

comment <-news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F)

frequency_word <- news_polarity %>%
  count(sentiment, word, sort = T)

frequency_word

wide <- frequency_word %>%
  filter(sentiment != "중립") %>% 
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))

wide <- wide %>%
  mutate(log_odds_ratio = log(((긍정 + 1) / (sum(긍정 + 1))) /
                ((부정 + 1) / (sum(부정 + 1)))))

wide

wide %>%
  filter(rank(log_odds_ratio) <= 10 | rank(-log_odds_ratio) <= 10) %>%  arrange(-log_odds_ratio)

log((189+1)/sum(wide$중립+1))

# 문제 7
get_sentiments(lexicon = 'bing')

?str_detect
