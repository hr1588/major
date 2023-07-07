rm(list=ls())
string <- c("data analysis is useful",
            "business analytics is helpful",
            "visullization of data is interesting for data scientists")
string

# 벡터의 위치 확인
grep("data",string) 
grep("data",string, value=T) 

string[grep("data",string)]

grepl("data",string) # grep과 동일한데 T/F로 결과 출력

# 위치, pattern의 길이, 타입, byte 사용여부를 순서대로 출력, 동일한 데이터의 첫번째 값만 리턴
regexpr(pattern = "data", text = string) 

# regexpr과 동일한데 결과값을 list로 return, 모든 패턴을 리턴
gregexpr("data", string)

# regmatches() 특정 문구 추출할 때 사용

regmatches(x=string,m=regexpr(pattern = "data", text = string)) # 1, 3번째 벡터의 첫번쨰 data 출력
regmatches(x=string,m=gregexpr(pattern = "data", text = string)) # 리스트 형태로 1,2,3번째의 data 출력

# 특정 문구를 제외하고 나머지 추출
regmatches(string, regexpr("data",string), invert = T)
regmatches(string, gregexpr("data",string), invert = T)

# sub()는 regexpr처럼 첫 번째만 적용, gsub()는 gregexpr처럼 데이터가 포함된 모든 벡터에 적용

?sub
sub(pattern = "data", replacement = "text", x= string)
gsub(pattern = "data", replacement = "text", x= string)

# strsplit()

strsplit(string, " ")
unlist(strsplit(string, " ")) # 리스트 형태로 리턴된 strsplit를 하나의 벡터로 결합
unique(unlist(strsplit(string, " "))) # 중복된 단어를 한번만 추출

# 지정된 단어를 공백으로 치환하고 출력
gsub(pattern = "is|of|for", replacement = "", x= unique(unlist(strsplit(string, " "))))

# 잘못 입력한 단어(오타 등)을 공백 혹은 다른 문자로 치환
sub(pattern = "date", replacement = "data", x= unique(unlist(strsplit(string, " "))))
unique(sub(pattern = "date", replacement = "data", x= unique(unlist(strsplit(string, " ")))))

### stringr()
install.packages("stringr")
library(stringr)

string

#grepl()과 동일한 기능
str_detect(string, pattern = "data")
?str_detect

str_detect(string, "DATA") # DATA가 아닌 data가 있어서 전부 FALSE 출력
str_detect(string, fixed("DATA", ignore_case = T)) # data에 대해서는 대소문자 구분 X

str_detect(c("aby","acy","a.y"),"a.y") # 전부 TRUE
str_detect(c("aby","acy","a.y"),fixed("a.y")) # fixed를 사용해 .(메타문자)가 아닌 문자 그대로 인식
str_detect(c("aby","acy","a.y"),"a\\.y") # fixed와 동일한 결과 출력

# 위치 검출
str_locate(string, "data")
str_locate_all(string, "data") # list형태로 출력

# 문자열 추출, regmatches()와 동일
str_extract(string,"data")
str_extract_all(string,"data") # list형태로 출력
str_extract_all(string,"data", simplify = T) # 행렬형태로 출력

unlist(str_extract_all(string,"data"))

#### 실습
sentences5 <- sentences[1:5]
sentences5

str_extract(sentences5,"(a|the|A|The) (\\w+)") # 정규 표현식과 시퀀스 활용
str_extract_all(sentences5,"(a|the|A|The) (\\w+)") # 패턴과 결합된 단어만 출력

str_match(sentences5,"(a|the|A|The) (\\w+)") # 패턴과 결합된 단어와 그 단어를 분할한 결과 출력
str_match_all(sentences5, "(a|the|A|The) (\\w+)")

# 문자 치환

str_replace(string, "data", "text")
str_replace_all(string, "data", "text")

# 문자 분할

str_split(string," ")
str_split(sentences5," ")
unlist(str_split(sentences5," "))
unique(unlist(str_split(sentences5," ")))

str_split(sentences5," ", n=5) # 한 문장을 정해진 개수로만 분할, 즉 뒤의 단어는 하나로 결합되어 출력
str_split(sentences5," ", n=5, simplify = T) # 리스트 형태가 아닌 행렬 형태로 return

### base 패키지에 없는 추가 기능

str_length(string) # 각 문자열의 길이
str_count(string,"data") # 지정된 패턴이 사용된 개수
str_count(string, "\\w+") # 각 문자열에서 사용한 단어 개수, 시퀀스 활용
string

mon <- 1:12
str_pad(mon, width = 2, side = "left", pad = "0") # 데이터셋에 임의의 값을 너비와 방향을 정해 삽입

string <- c("data analysis is useful",
            "business analytics is helpful",
            "visullization of data is interesting for data scientists")

string_pad <- str_pad(string, width = max(str_length(string)),side = "both",pad = " ")
str_trim(string_pad, side = "both") # 공백을 삭제


