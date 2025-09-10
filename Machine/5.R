library(dplyr)

#첫번째 모집단에서 샘플 64개의 평균이 4, 분산 16, 
#두번째 모집단에서 샘플 75개의 평균이 12, 분산 48 일 때, 
#두개의 샘플간의 평균의 차이가 -6보다 작을 확률을 구해라

Z=(4-12)/sqrt((16/64)+(48/75))

#평균이 10이고 분산이 25인 첫 번째 모집단에서 50개의 표본이 있고, 
#평균이 20이고 분산이 45인 두 번째 모집단에서 60개의 표본이 있다고 가정할 때, 
#두 표본 간의 평균 차이가 나는지를 판단해라

Z=(10-20)/sqrt((25/50)+(45/60))
Z


group_a <- c(85, 88, 90, 92, 91, 87, 89, 86, 84, 83)
group_b <- c(78, 82, 80, 85, 84, 87, 83, 81, 80, 79)

t.test(group_a, group_b, alternative = "two.sided")
t.test(group_a, group_b, alternative = "less")
t.test(group_a, group_b, alternative = "greater")

library(BSDA)
group_a <- c(85, 88, 90, 92, 91, 87, 89, 86, 84, 83, 85, 88, 90, 92, 91, 87, 89, 86, 84, 83, 85, 88, 90, 92, 91, 87, 89, 86, 84, 83, 85, 88, 90, 92, 91, 87, 89, 86, 84, 83)
group_b <- c(78, 82, 80, 85, 84, 87, 83, 81, 80, 79, 78, 82, 80, 85, 84, 87, 83, 81, 80, 79, 78, 82, 80, 85, 84, 87, 83, 81, 80, 79, 78, 82, 80, 85, 84, 87, 83, 81, 80, 79)

sd_a=sd(group_a)
sd_b=sd(group_b)

result <- z.test(x=group_a, y=group_b, sigma.x=sd_a,sigma.y=sd_b, alternative="two.sided")
t_test_result <- t.test(group_a, group_b, alternative = "two.sided")






 z.test(x=group_a, y=group_b, sigma.x=sd_a,sigma.y=sd_b, alternative="two.sided")





