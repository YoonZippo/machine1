data <- read.csv("/Users/kimzippo/Desktop/KNU/3rdYear/Machine/ANOVA/class_scores.csv", stringsAsFactors = TRUE)
#anova 분석
anova_result <- aov(Score ~ Class, data=data)
#분석결과 출력 (P밸류: Pr(>F))
summary(anova_result)
#사후검증 출력
library(multcomp)

tukey_result <- glht(anova_result, linfct = mcp(Class="Tukey"))
summary(tukey_result)

#분산분석
grow <- read.csv("/Users/kimzippo/Desktop/KNU/3rdYear/Machine/ANOVA/growth.csv", stringsAsFactors = TRUE) 

#이원분산분석( 독립변수간 상호작용X)
anova_result <- aov(growth ~ fertilizer + water, data = grow)
summary(anova_result)

#이원분산분석( 독립변수간 상호작용O)
anova_result <- aov(growth ~ fertilizer * water, data=grow)
summary(anova_result)

#사후검정
library(car) #등분산검정 (F테스트는 두개의 집단에서 등분산검정
              #리벤테스트는 두개 이상의 집단에서 등분산검정)
library(rstatix) #games_howell_test(집단의 요소가 개수차이가 클떄)

#사후검정(fertilizer)
#p-value<0.05일 때 games-howell 검증사용가능(등분산검정을 통해 등분산이 아닐 경우)
leveneTest(growth~fertilizer, data= grow)
tukey_result <- glht(anova_result, linfct = mcp(fertilizer = "Tukey"))
games_howell_test(grow, growth~fertilizer)
#1 growth A      B          1.81    0.257      3.36 0.02
# A와 B 위치를 바꾸어 B-A로 계산해야함

#사후검정(water)
leveneTest(growth ~ water, data=grow)
tukey_result <- glht(anova_result, linfct = mcp(water = "Tukey"))
games_howell_test(grow, growth~water)

# 사후검정(fertilizer*water)
# 교호작용을 하나의 그룹 변수로 통합
grow$group <- interaction(grow$fertilizer, grow$water)
group_model <- aov(growth~group, data=grow)

leveneTest(growth ~ group, data=grow)
tukey_result <- glht(group_model, linfct=mcp(group="Tukey"))
summary(tukey_result)

games_howell_test(grow, growth~group)

#------------------------
cafe <- read.csv("/Users/kimzippo/Desktop/KNU/3rdYear/Machine/ANOVA/cafe.csv", stringsAsFactors = TRUE) 
cafe

leveneTest(Satisfaction~CoffeeType, data= cafe)
games_howell_test(cafe, Satisfaction~CoffeeType)

leveneTest(Satisfaction~CafeMood, data= cafe)
games_howell_test(cafe, Satisfaction~CafeMood)

leveneTest(Satisfaction~SeatLocation, data= cafe)
games_howell_test(cafe, Satisfaction~SeatLocation)

cafe$group <- interaction(cafe$CoffeeType, cafe$CafeMood, cafe$SeatLocation)
group_model <- aov(Satisfaction~group, data=cafe)
tukey_result <- glht(group_model, linfct=mcp(group="Tukey"))
summary(tukey_result)
games_howell_test(cafe, Satisfaction~group)
