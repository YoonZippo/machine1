library(dplyr)
library(magrittr)
mid<- 75+ 88+ 91+ 68+ 82
fin<- 60+ 87+ 55+ 47+ 92

mid/5
fin/5

(75+ 88+ 82)/3
(60+ 87+ 55)/3


a<-(75*0.5)+(60*0.3)
b<-88*0.5+87*0.3
c<-91*0.5+55*0.3
d<-68*0.5+ 47*0.3
e<-82*0.5+ 92*0.3

print(b)
print(e)
print(c)
print(a)
print(d)%>%print(a)



b %>% print()
