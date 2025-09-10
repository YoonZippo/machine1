library(BSDA)
library(ggplot2)
library(magrittr)


a=c(4,8,6,4,5,3,7,5,4,6,8,9,10,2,4,6,7,8,10,4)
sd(a)
mean(a)


A=c(160,180,180,160)
A=A*0.6
B=c(170,190,160,150)
B=B*0.4
(A+B)/2

box=read.csv("/Users/kimzippo/Downloads/machine1/boxplot_score.csv")
df <- data.frame(group = c(rep( box$Score_A, 1)),  values = c(1))
  
df <- data.frame(box$Score_A)
df2 <- data.frame(box$Score_A)
  
ggplot(df, aes(x =df, y=df2)) +
  geom_boxplot(fill = c("lightblue","green"), outlier.color = "red") +
  labs(title = "Boxplot Example") +
  xlab("Group") +
  ylab("Values")
ggplot(df, aes(x =box$Score_B)) +
  geom_boxplot(fill = c("green"), outlier.color = "red") +
  labs(title = "Boxplot Example") +
  xlab("Group") +
  ylab("Values")
boxplot(df2)
boxplot(df)
  

q1 <- quantile(box$Score_A,0.75)
q2 <- quantile(box$Score_A,0.25)


(88-90)/(sqrt((100^2)/4))
(92-90)/sqrt((100^2)/4)
(1-0.5160)*2

표본푱균 -모집단/sqrt(vywnsvusck^2/vyqhs )

(18-20)/sqrt((10^2)/49)
(20-20)/sqrt(10^2/49)
(1-0.5)-(1-0.9192)

med=read.csv("/Users/kimzippo/Downloads/machine1/Medicine.csv")
med

t.test(med$A_group, med$B_group)
sd1=sd(med$A_group)
sd2=sd(med$B_group)
z.test(med$A_group, med$B_group, sigma.x=sd1, sigma.y=sd2, conf.level = 0.9)














