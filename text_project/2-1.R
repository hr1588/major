a<-read.csv('product.csv', header=T, sep=',', encoding='latin1', na.strings="-")
View(a)

#file.choose()를 사용해 파일 열기 창 이용 가능

a<-read.csv(file.choose(), header=T, sep=',', encoding='latin1', na.strings="-")
View(a)

a <- read.csv(file.choose())

write.table(a, 'save_2.csv', row.names=F, quote=F, sep=',')
# 복사를 할 때 오류가 발생하면 새로 치거나 따옴표, 쉼표 확인
# write.table을 이용하면 csv 혹은 txt 파일로 저장되어 복사됨
write.table(a,'textmining.csv')

# read_csv() = 콤마로 구분된 csv 형식
# read_delim() = 임의의 구분자 형식(공백)
# read_fwf() = 고정 길이를 갖는 형식
# read_table() = 하나 이상의 빈칸으로 구성된 테이블 형식
# read_lines(), read_file() = 비정형의 텍스트 파일 읽기

# pander 패키지(바로 파일 오픈)
install.packages("pander")
library(pander)

openFileInOS("product.csv")
openFileInOS("product.txt")
openFileInOS("product-missing.txt") # 결측치가 . 으로 표시

# readr 패키지(R에서 파일 오픈)
install.packages("readr")
library(readr)

read_csv(file = "product.csv")
read_csv(file = "product-with-no-header.csv",
         col_names = c("id","name","price")) # 열에 이름 부착

read.csv(file="product-missing.txt",
         na=".")

# product-missing.csv 제작
a = read.table("product-missing.txt")
write.table(a,"product-missing.csv",row.names=F, quote=F, sep=',')

read.csv(file="product-missing.csv",
         na=".")

# delim(공백 같은 구분자)
read_delim(file = "product.txt", delim = " ")
read_delim(file = "product-with-no-header.csv",
           delim = ",",
           col_names = c("id","name","price"))

a <- read.delim(file.choose())
b <- read.delim(file.choose(), sep = ";")

# fwf(각각 데이터의 열이 고정된 데이터)

fwf_empty()
fwf_positions() # 시작과 끝 위치를 사용
fwf_widths()
fwf_cols() # 유사한 작업 가능

# fwf_empty를 사용해 공백의 위치를 확인하고 값을 col_positions에 대입

read_fwf(file="product-fwf.txt",
         col_positions = fwf_empty(file="product-fwf.txt",
                                   col_names = c("id","name","price")))

# fwf_width를 이용해 데이터의 열 개수를 입력해 구분, col_names는 생략 가능
read_fwf(file="product-fwf.txt",
         col_positions = fwf_widths(widths = c(5,10,8),
                                    col_names = c("id","name","price")))

# read_table(공백이 있을 때 보편적으로 사용)

read_table(file="product-fwf.txt",
           c("id","name","price"))

read_table(file = "product.txt",
           c("id","name","price"))

# read_lines(비정형 데이터, 라인(행)을 읽음, 행 단위로 데이터를 가져옴)

read_lines(file="won-dollar.txt", skip = 1,n_max=2) 
# skip => 해당 숫자 다음 행부터 표시, n_max => 최대 가져올 개수

# read_file(하나의 인자로 가져옴)
read_file(file = "won-dollar.txt")

# write_csv(csv로 저장)

a = Orange
View(a)
write_csv(x=Orange, file="orange.csv")
read_csv(file="orange.csv")

# write_delim(txt 파일 저장, 구분문자 지정)

write_delim(x=Orange, file = "orange.txt",delim = ";")
read_delim(file="orange.txt",delim = ";")

# 숫자만 뽑아내고 싶을 때 사용
parse_number("$100") # 100
class(parse_number("$100") )
parse_number("10%") # 10
parse_number("Salary per year: $30,000")


