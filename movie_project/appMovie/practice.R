t <- which(titles == "범죄도시2") 
s = predict[t]
length(s[s==1])

s1 = table(s)
s1

str(s1)
class(s1)
names(s1) <- c("부정","중립","긍정")
pct = round(s1/sum(s1))*100
pct
s1[1,]
s1
