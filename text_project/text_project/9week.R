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






