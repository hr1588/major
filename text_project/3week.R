product <- list(id="a001",name="mouse",price=30000)
product

product$price = 40000
product[[3]] = 40000 #3번째 원소 변경

product[["price"]] = 50000
product

product[3] = 70000
product

product[[3]] = c(30000,40000) # 3번쨰에 원소 2개 할당(대괄호 2개는 직접 원소 접근)

product[3] = list(c(50000,60000)) # [] 대괄호 하나는 리스트에 접근

names = c("Mon","tue","wed","thur","fri","sat","sun")
values = c(842,729,786,844,851,750,702)

traffic.death = list()
traffic.death

traffic.death[names] = values

traffic.death < 750
traffic.death[traffic.death < 750] = NULL # []안의 조건식을 null처리
traffic.death


### data.frame

df1 = data.frame(sex = "female", month = 1, weight = 3.5)
df2 = data.frame(sex = "male", month = 3, weight = 4.8)
df3 = data.frame(sex = "male", month = 4, weight = 5.3)
df4 = data.frame(sex = "female", month = 9, weight = 7.5)
df5 = data.frame(sex = "female", month = 7, weight = 8.3)

lst = list(df1,df2,df3,df4,df5)
lst

str(lst)
lst[1]
lst[[1]]

rbind(lst[[1]],lst[[2]]) # row가 합쳐진 데이터 프레임

a = do.call(rbind, lst) # (방법, 변수)
a

?do.call

lst1 = list(sex = "female", month = 1, weight = 3.5)
lst2 = list(sex = "male", month = 3, weight = 4.8)
lst3 = list(sex = "male", month = 4, weight = 5.3)
lst4 = list(sex = "female", month = 9, weight = 7.5)
lst5 = list(sex = "female", month = 7, weight = 8.3)

lst = list(lst1,lst2,lst3,lst4,lst5)
lst

as.data.frame(lst[[1]])

b=lapply(lst, as.data.frame) # 앞에 리스트 데이터셋 지정, 두번째에 실행할 함수 작성
b
mode(b)

c = do.call(as.data.frame,lst) # 오류 발생

do.call(rbind,b) 

### 

?state
state.abb
state.area
state.name
state.region

us.state = data.frame(state.abb, state.name, state.region,
                      state.area, stringsAsFactors = F)


str(us.state)

## list indexing

us.state[[2]]
str(us.state[[2]])

us.state[2]
us.state[c(2,4)]


# matrix indexing [행, 열]

us.state[,2]
us.state[1,2]

us.state[ ,2, drop = F] # drop = F를 설정하면 data.frame형태로 가져옴 = us.state[2]
us.state[,c(2,4)]

us.state[["state.name"]] #열의 이름을 활용한 indexing
us.state$state.name
us.state[,"state.name"]

us.state[c("state.name","state.area")] # list형태 indexing
us.state[ ,c("state.name","state.area")] # matrix형태 indexing


# contition based indexing

state.x77
str(state.x77)

states = data.frame(state.x77)
states
str(states)

row.names(states) # 행의 이름을 가져오는 함수

states$Name = row.names(states)
head(states)

row.names(states) = NULL # 행의 이름을 따로 지정했기 때문에 앞의 행 이름 삭제

### 조건을 만족하는 데이터 추출

rich.states = states[states$Income > 5000, c("Name","Income")]
str(rich.states)
head(rich.states)
rm(rich.state)

large.states = states[states$Area > 100000, c("Name","Area")]
head(large.states)

?merge
merge(rich.states, large.states, all = F) # all = F를 쓰면 원소의 교집합이 추출(inner join)
merge(rich.states, large.states, all = T) # all = T를 쓰면 원소의 합집합이 추출

merge(rich.states, large.states, all.x=T) # left join
merge(rich.states, large.states, all.x=F) # all = F와 동일

merge(rich.states, large.states, all.y=T) # right join
merge(rich.states, large.states, all.x=F) # all = F와 동일

merge(rich.states, large.states, all.x=T, key = "Name") # key값을 기준으로 결합


