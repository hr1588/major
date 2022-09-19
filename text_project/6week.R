rm(list=ls())
txt <- "Data Analytics is useful. Data Analytics is also interesting"
words <- c("at","bat","cat","chaenomeloes","chase","cheep","check","cheese","chick","hat")
words2 <- c("12 Dec","OK","http://","<TITLE>Time?</TITLE>","12345","Home")

# 텍스트의 치환
sub()
gsub()

?sub()
sub(pattern = "Data", replacement = "Business", x=txt)  # sub는 첫 번째 문자열에 대해서만 적용
gsub(pattern = "Data", replacement = "Business", x=txt) # gsub를 쓰면 모두 변경
gsub(pattern = "Data", replacement = "", x=txt) # gsub를 활용해 필요없는 문자를 공백으로 대체

text2 <- c("product.csv","order.csv","customer.csv")
gsub(pattern = ".csv", replacement = "", text2) 

# 정규 표현식

words <- c("at","bat","cat","chaenomeloes","chase","cheep","check","cheese","chick","hat","ca-t","chasesed","hated")

grep("che", words, value = T) # words에서 che를 포함하는 단어 추출
grep("a",words,value = T)
grep("at",words,value = T)

grep("[ch]", words, value = T) # c 혹은 h를 포함한 문자 벡터 추출(둘 중에 하나만 있어도 추출)

grep("[at]", words, value = T)
grep("che|at",words,value = T) # che 혹은 at가 포함된 문자 벡터 추출

grep("che|at",words,value = T)
grep("ch(e|i)ck",words,value = T) # ch로 시작하고 ck로 끝나는데 중간이 e 혹은 i인 벡터 추출

# 수량자 ? * +

grep("chas?e",words,value = T) # ? 바로 앞의 문자(s)가 cha와 e사이에 0 혹은 1회 반복
grep("chas*e",words,value = T) # s가 0회이상 반복
grep("chas+e",words,value = T) # s가 1회이상 반복

grep("ch+e",words,value = T)

grep("ch(a*|e*)se",words,value = T) # ch와 se사이에 a 또는 e가 0회이상 반복

# 메타문자 ^(시작) $(끝)

grep("^c",words,value = T) # c로 시작하는 문자 벡터 추출
grep("t$",words,value = T) # t로 끝나는 문자 벡터 추출

grep("^c.t$",words,value = T) # c로 시작하고 t로 끝나는 문자 벡터 추출(. = 모든 문자열)
grep("^c.*t$",words,value = T) # c로 시작하고 t로 끝나는 문자 중 문자열이 0회이상 반복되는 벡터 추출

grep("^[ch]?at",words,value = T) # 문자열 시작에 c 혹은 h가 없거나 한번 있고 뒤에 at로 끝나는 벡터 추출(at가 문장 끝은 아님)

# 문자 클래스

words2 <- c("12 Dec","OK","http://","<TITLE>Time?</TITLE>","12345","Hi there")

grep("[[:alnum:]]",words2,value = T) # 알파벳과 숫자를 포함한 벡터 추출
grep("[[:alpha:]]",words2,value = T) # 알파벳을 포함한 벡터 추출
grep("[[:digit:]]",words2,value = T) # 숫자를 포함한 벡터 추출
grep("[[:punct:]]",words2,value = T) # 특수문자를 포함한 벡터 추출
grep("[[:space:]]",words2,value = T) # 공백 문자를 포함한 벡터 추출

# 문자 클래스 시퀀스

grep("\\w+",words2,value = T) # \\w+ => 모든 문자열 출력
grep("\\s+",words2,value = T) # 공백 포함
grep("\\d+",words2,value = T) # 숫자 포함
grep("\\D+",words2,value = T) # 숫자를 제외한 나머지




