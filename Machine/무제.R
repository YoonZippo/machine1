library(dplyr)
library(magrittr)
library(nnet)

# 1.(1)
rm(list=ls()); gc()
earthquake <- read.csv("earthquake.csv", header = T, stringsAsFactors = F)
earthquake$latitude <- (earthquake$latitude %>% 
                          strsplit(" ") %>% 
                          unlist)[1:(2*nrow(earthquake)) %% 2 == 1] %>% as.numeric
earthquake$longitude <- (earthquake$longitude %>% 
                           strsplit(" ") %>% 
                           unlist)[1:(2*nrow(earthquake)) %% 2 == 1] %>% as.numeric
KNU.latitude <- 37.27   # 강원대학교 삼척캠퍼스 위도
earthquake %>% 
  filter(latitude < KNU.latitude) %>% 
  filter(size == max(size)) %>% 
  select(time, location)

# 1.(2)
earthquake$North.Korea <- ifelse(substr(earthquake$location,1,2)=="북한","O","X")
earthquake %>% 
  filter(North.Korea == "O") %>% 
  filter(latitude == min(latitude)) %>% 
  select(size)

# 1.(3)
earthquake$Kangwon <- ifelse(substr(earthquake$location,1,2)=="강원","O","X")
earthquake$month <- substr(earthquake$time,6,7) %>% as.factor
earthquake %>% 
  filter(Kangwon == "O") %>% 
  select(month) %>%
  table %>% 
  which.max

# 1.(4)
earthquake$Sea.Area <- ifelse(substr(earthquake$location,
                                     nchar(earthquake$location)-1,
                                     nchar(earthquake$location)) =="해역","O","X")
earthquake$Jeju <- ifelse(substr(earthquake$location,1,2)=="제주","O","X")
earthquake %>% 
  filter(Sea.Area == "O" & Jeju == "O") %>% 
  filter(size <= 2.5) %>% 
  summarise(count=n())

# 1.(5)
earthquake$Incheon <- ifelse(substr(earthquake$location,1,2)=="인천","O","X")
earthquake %>% 
  filter(Incheon == "O") %$% 
  size %>% 
  boxplot(main="인천 지진 규모 상자그림")

############################################################

# 2번
rm(list=ls()); gc()
library(pracma)
library(rootSolve)
f <- function(theta) tan(theta)-6/tan(theta)-1
theta <- uniroot(f, c(4, 3/2*pi))$root
sec(theta) + csc(theta)

############################################################

# 3. 가위바위보 게임
rm(list=ls()); gc()
rsp <- c("가위", "바위", "보")
result <- data.frame(rep(rsp,each=3), rep(rsp,times=3))
names(result) <- c("human","computer")
result$outcome <- c("무","패","승","승","무","패","패","승","무")
result # 가위, 바위, 보 규칙표

readrsp <- function() { 
  n <- readline(prompt="가위, 바위, 보, (게임을 끝낼려면 종료를 입력) : ") 
  if(n == "종료") return("게임 종료")
  if(!(n %in% c("가위", "바위", "보"))) {
    cat("똑바로 입력하세요. ㅡㅡ; \n")
    return(readrsp())
  } else return(n)
}

rsp.start <- function() {
  rsp.series <- NULL
  cat("가위 바위 보 대결 시작!\n")
  repeat {
    h <- readrsp()
    if(h=="게임 종료") {
      cat(h,"\n")
      rsp.series %>% table %>% print
      break
    }
    com <- sample(c("가위", "바위", "보"), 1)
    a <- result %>% 
      filter(human == h & computer == com) %>% 
      select(3) %>% 
      unlist
    rsp.series <- append(rsp.series,a)
    cat("컴퓨터는",com,"를 냈습니다. ",a)
  }
}
rsp.start()        # 가위, 바위, 보 게임 시작

############################################################

# 4번
rm(list=ls()); gc()
ob <- 0
for(i1 in 1:4)
  for(i2 in 1:4)
    for(i3 in 1:4)
      for(i4 in 1:4)
        for(i5 in 1:4) {
          i <- c(i1, i2, i3, i4, i5)
          if(all(i >= sqrt(1:5)) && length(unique(i)) == 3) {
            print(i)
            ob <- ob + 1
          }
        }
ob

############################################################

# 5번
rm(list=ls()); gc()
load("P5.Rdata")
# (1)번
plot(P5, col="orange", pch = 18,
     xlab="x (엑스)", ylab="y (와이)",
     main = "Scatter plot (산점도)",
     cex.axis = 1.5)

# (2)번
x1 <- P5$x
x2 <- sin(x1)
x3 <- log(x1)
y <- P5$y

lreg <- lm(y ~ x1 + x2 + x3)
coeff <- lreg$coefficients %>% unname

print(coeff)
beta0 <- coeff[1]
beta1 <- coeff[2]
beta2 <- coeff[3]
beta3 <- coeff[4]

f <- function(x1) beta0 + beta1*x1 + beta2*sin(x1) + beta3*log(x1)
curve(f, min(P5$x), max(P5$x), 
      col="blue", lty = 4, lwd = 3,
      add = T)

# (3)번
cat("답 : x = 23일 때, y의 예측값은", f(23), "이다.\n")

############################################################

# 6번 option 1
rm(list=ls()); gc()
df <- data.frame(b=sample(-10:10, 3000, replace = T),
                 c=sample(-10:10, 3000, replace = T))
df %<>% cbind.data.frame(a=1,.) 
df %<>% filter(b^2-4*a*c >= 0)

root_eq <- function(abc) {
  x1 <- (-abc[2] + sqrt(abc[2]^2-4*abc[1]*abc[3]))/(2*abc[1])
  x2 <- (-abc[2] - sqrt(abc[2]^2-4*abc[1]*abc[3]))/(2*abc[1])
  return(c(x1,x2))
}

df <- df %>% 
  as.matrix %>% 
  apply(1, root_eq) %>%
  t %>% 
  as.data.frame %>% 
  cbind.data.frame(df,.)
names(df)[4:5] <- c("x1","x2")

nn <- nnet(df[,1:3], df[,4:5],
           size=20, maxit=10000,
           linout=T, decay = 0.00001)

predict(nn, data.frame(a=1,b=3,c=2)) %>% round(0)  # x^2 + 3x + 2 = 0 의 근 예측
predict(nn, data.frame(a=1,b=-5,c=6)) %>% round(0)    # x^2 - 5x + 6 = 0 의 근 예측

############################################################

# 6번 option 2
rm(list=ls()); gc()

# 자연수 n보다 작거나 같은 소수의 갯수를 구하는 함수
count_prime.number <- function(y) {
  prime.number <- NULL
  x <- 2:y
  repeat {
    prime.number <- append(prime.number,x[1])
    x <- x[x %% x[1] != 0]
    if(length(x)==0) break
  }
  return(prime.number %>% length)
}

# 데이터 만들기
df <- data.frame(x=2:3004)
df$y <- df$x %>% as.matrix %>% apply(1,count_prime.number)

# 신경망 모델 및 예측
nn <- nnet(df$x, df$y, size=20, maxit=3000,
           linout=T, decay = 0.001)
pred <- function(x) {
  predict(nn, data.frame(x=x)) %>%
    round(0) %>% 
    as.vector %>% 
    return
}
cat("답 : 3005보다 작거나 같은 소수는", pred(3005), "개로 예측된다.\n")