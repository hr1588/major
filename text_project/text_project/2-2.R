# list indexing

#[[]] 원소를 직접적으로 가져옴
#[] 하나의 리스트를 묶음으로 가져옴

product = list(c("a001","a002","a003"),c("mouse","keyboard","headphone"),c(30000,50000,20000))
product

product[[3]]
product[[2]]
product[[1]]

product[3]
product[2]
product[1]

product[[3]][[1]]

class(product[[3]]) # 실제 데이터 타입 반영
class(product[3]) # list 리턴

product[[3]]*0.95

product[c(1,2)]

product[c(FALSE,TRUE,TRUE)] # = [c(2,3)]
product[-1] # -를 사용해 특정 원소 제외
product[-2]

product1 <- list(id="A002",name="mouse",price=30000)

product1

product1[["name"]] 
product1$name

product1[c("name","price")]
product1[[c("name","price")]] # 필드명을 [[]] 안에 넣을시 에러 발생

product1[c(2,3)]

product[["lee"]] # NULL 값 리턴

product[["holder"]] # Null
product$holder # Null
product[[4]] # 에러 발생

product[c(4,2,5)] # 존재하는 값(2)만 가져오고, 없는 값은 NULL로 리턴

# list는 중첩이 가능하다.(if문 처럼)

lst = list(one = 1, two = 2, three = list(alpha=3.1,beta=3.2))
lst

# three 안의 alpha 값 추출 = lst[[3]][[1]] = lst$three$alpha = lst[["three]]$alpha
lst[["three"]][["alpha"]]

# list 내부에서 특정 값을 추출하기 위해서는 name을 활용하거나, $와 name을 활용하거나,
# [[]]을 활용해 추출해볼 수 있다.



         