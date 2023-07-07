# subset

subset(mtcars, subset = (mpg>30),select = mpg)
subset(mtcars, subset = (cyl == 4 & am == 0), select = c(mpg,hp,wt))
subset(mtcars, subset = (mpg < mean(mpg)), select = mpg)

# tibble

library(tibble)

v1 <- c("a001","a002","a003")
v2 <- c("Mouse","Keyboard","USB")
v3 <- c(30000,35000,40000)

product = tibble(id=v1,name=v2,price=v3)
str(product)

# apply

x = matrix(1:20,4,5)

apply(X=x, MARGIN = 1, FUN = max) # margin = 1 (행), 각각의 행에서 fun(최대값) 출력
apply(X=x, MARGIN = 2, FUN = max) # margin = 2 (열), 각각의 열에서 fun(최대값) 출력
apply(X=x, MARGIN = 2, FUN = mean) # 열에 대한 평균
apply(X=x, MARGIN = 2, FUN = min)  # 열에 대한 최솟값
apply(X=x, MARGIN = 2, FUN = sum)  # 열에 대한 합

rm(list=ls())

###

x <- "We have a dream"
class(x)

nchar(x) # number of character
length(x) # 전체 벡터의 개수

y <- c("We","have","a","dream")
y
nchar(y) # 개별 벡터의 길이
length(y) # y의 원소 개수
length(y[4]) # 1개
nchar(y[4]) # 5개

letters

sort(letters, decreasing = T) # 역순으로 정렬

tolower(fox.says) # 전부 소문자
toupper(fox.says) # 전부 대문자

fox.says <- "It is only with the HEART that one can See Rightly"

# text 분할, 결합(strsplit, 사용시 list타입으로 리턴)

fox.said <- "What is essential is invisible to the eye"
strsplit(fox.said, split = " ") # 공백을 중심으로 문자열 구분
mode(strsplit(fox.said, split = " ")) # 리스트 타입 리턴

strsplit(fox.said, split = "") # 공백을 포함한 모든 문자를 하나씩 구분

fox.said.words <- unlist(strsplit(fox.said, split = " "))
mode(fox.said.words) # 문자열 타입 리턴

fox.said.words[3]
fox.said.words[1]

unlist(strsplit(fox.said, split = " "))[3]

strsplit(fox.said, split = " ")[1][3] # 리스트 인덱싱, 위와 동일한 결과 출력

p1 <- "What would I do without your smart mouth?"
p2 <- "Drawing me in, and you kicking me out"  
p3 <- "You've got my head spinning, no kidding, I can't pin you down"

allofme <- c(p1,p2,p3)
allofme
strsplit(allofme, split = " ")

strsplit(allofme, split = " ")[[3]]
strsplit(allofme, split = " ")[3]
typeof(strsplit(allofme, split = " ")[[3]]) # character
typeof(strsplit(allofme, split = " ")[3]) # list
strsplit(allofme, split = " ")[[3]][[5]]
strsplit(allofme, split = " ")[[1]][[7]]

###

bear <- "Cause ALL OF ME Love all of you"
strsplit(bear, split = " ")

unlist(strsplit(bear,split = " "))
bear.words <- strsplit(bear, split = " ")[[1]]
unique(bear.words) # unique 함수는 대/소문자를 구분
unique(tolower(bear.words)) # 모두 소문자로 바꿔서 중복된 값 of 삭제, toupper()도 사용 가능
unique(toupper(bear.words))

paste("Everyone","wants","to","fly") # 각각의 벡터를 하나로 결합(공백 O)
paste("Everyone","wants","to","fly", sep = "-") # 공백자리에 구분자 - 삽입
paste0("Everyone","wants","to","fly") # 공백 없이 결합

paste(bear.words, collapse = " ")
paste(pi, sqrt(pi))

paste("25 degrees Celsius is", 25*1.8+32, "degree Fahernheit") # 식은 연산해서 결과 출력

heroes <- c("Batman","Captin America","Hulk")
colores <- c("Black","Blue","Green")

paste(heroes, colores) # 각각 자리에 맞는 벡터끼리 결합

paste("Type", 1:10) # 자동으로 1:10 앞에 Type 부착, 10개의 벡터 출력
paste(heroes,"wants","to","fly") # 각각의 heroes 벡터 뒤에 공백 포함 문자열 출력

paste(bear.words, collapse = " ") # 공백을 기준으로 벡터 결합

## 2차시

paste(month.abb, 1:12) # 달에 1부터 12까지 순서대로 부착
paste(month.abb, 1:12, sep = "-")
paste(month.abb, 1:12, sep = "-", collapse = "_")
paste(month.abb, 1:12, sep = "-", collapse = " ") # 하나의 벡터로 출력

# outer(곱연산 함수)

outer(1:3, 1:3)
outer(c(1,2,3), c(1,2,3))

countries <- c("KOR","US","EU")
stat = c("GDP","Pop","Area")

outer(countries,stat, FUN = paste, sep = "-") # FUN = 붙여주는 인자, 없이 실행하면 오류 발생

# 고객 주문 결과를 보여주는 시나리오

customer <- "Lee"
buysize <- 10
deliveryday <- 2

paste("hello", customer, ", your order of", buysize,"product(s) will be delivered within",deliveryday)

# sprintf()

sprintf("hello %s, your order of %s, product(s) will be delivered within %s",
       customer, buysize, deliveryday) # %s 자리에 입력된 변수가 순서대로 출력

customer <- c("Ryu","kim","Choi")
buysize <- c(10,8,9)
deliveryday <- c(2,3,7.5)

sprintf("hello %s, your order of %s, product(s) will be delivered within %.2f",
        customer, buysize, deliveryday) # 3개의 벡터가 각각 연산되어 출력

# %.1f => 소수점 한 자리 까지 출력 (%.2f => 두 자리까지 출력)
?sprintf

# substr() 텍스트에서 특정 부분만을 출력
substr("Text Analytics", start = 1, stop = 4) # 1-4자리 출력
substr("Text Analytics", start = 4, stop = 1) # "" 출력
substr("Text Analytics", start = 6, stop = 14) 

substring("Text Analytics", 6) # 특정 시작 위치부터 끝까지 출력

class <- c("Data analyics", "Data visualization", "Data science introduction")
substr(class, 1,4)
substring(class,6)

# 서로 길이가 다른 벡터에서 문자열 추출

countries <- c("Korea, KR","United States, US", "China, CN")
substring(countries, nchar(countries)-1) # nchar(), 길이 함수를 활용
substr(countries, nchar(countries)-1,nchar(countries))
nchar(countries)

# grep()

head(islands)
landnames <- names(islands)
landnames

# 이름 중 new라는 문자를 포함한 원소의 인덱싱 값 추출
index <- grep(pattern = "New", x= landnames)
landnames[index]
landnames[30:34]
index

# value = T를 쓰면 인덱스 값이 아니라 그대로 값을 추출
grep(pattern = "New", x= landnames, value = T)






