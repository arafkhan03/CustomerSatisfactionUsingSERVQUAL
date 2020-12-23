# Package Installation
library(psych)
library(xlsx)
library(dplyr)
library(plyr)
library(skimr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(car)



# Read File
df <- read.xlsx(file = "/Users/araf03/Desktop/SERVQUAL/Data/SERVQUAL-Raw-Data.xlsx",
                sheetIndex = 1)



# Keep Required Data
str(df)
df <- df[, 1:50]



# Functions to Set Numeric
Agreement <- function(x) {as.numeric(factor(x,
                  # specifify the levels in the correct order:
                  levels=c("Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree")))
        }

Importance <- function(x){as.numeric(factor(x,
                                            # specifify the levels in the correct order:
                                            levels=c("Extremely Unimportant", "Unimportant", "Neutral", "Important", "Extremely Important")))
        }




# Running the Functions Separately (Expectation & Perception)
df1 <- data.frame(apply(df[, 29:50], MARGIN = 2, FUN = Agreement))
df2 <- data.frame(apply(df[, 7:28], MARGIN = 2, FUN = Importance))



# Combine and Make the Final Data
data <- cbind(df[,1:6], df2, df1)



# Rename the Columns
## Initial Ones
names(data)[4] <- "monthly_income"
names(data)[5] <- "Profession"
names(data)[6] <- "monthly_visit"
## Expectations
names(data)[7] <- "E1_product_availability"
names(data)[8] <- "E2_employee_sincerity"
names(data)[9] <- "E3_accurate_record"
names(data)[10] <- "E4_employee_efficiency"
names(data)[11] <- "E5_service_time"
names(data)[12] <- "E6_service_promptness"
names(data)[13] <- "E7_assistance"
names(data)[14] <- "E8_helping_attitude"
names(data)[15] <- "E9_accuracy_timeliness"
names(data)[16] <- "E10_emplyee_confidence"
names(data)[17] <- "E11_employee_knowledge"
names(data)[18] <- "E12_safe_transaction"
names(data)[19] <- "E13_employee_behaviour"
names(data)[20] <- "E14_promotional_strategies"
names(data)[21] <- "E15_employee_attention"
names(data)[22] <- "E16_understanding_needs"
names(data)[23] <- "E17_operating_hours"
names(data)[24] <- "E18_best_interest"
names(data)[25] <- "E19_equipment"
names(data)[26] <- "E20_product_organization"
names(data)[27] <- "E21_decor_appeal"
names(data)[28] <- "E22_employee_presentation"
## Perceptions
names(data)[29] <- "P1_product_availability"
names(data)[30] <- "P2_employee_sincerity"
names(data)[31] <- "P3_accurate_record"
names(data)[32] <- "P4_employee_efficiency"
names(data)[33] <- "P5_service_time"
names(data)[34] <- "P6_service_promptness"
names(data)[35] <- "P7_assistance"
names(data)[36] <- "P8_helping_attitude"
names(data)[37] <- "P9_accuracy_timeliness"
names(data)[38] <- "P10_emplyee_confidence"
names(data)[39] <- "P11_employee_knowledge"
names(data)[40] <- "P12_safe_transaction"
names(data)[41] <- "P13_employee_behaviour"
names(data)[42] <- "P14_promotional_strategies"
names(data)[43] <- "P15_employee_attention"
names(data)[44] <- "P16_understanding_needs"
names(data)[45] <- "P17_operating_hours"
names(data)[46] <- "P18_best_interest"
names(data)[47] <- "P19_equipment"
names(data)[48] <- "P20_product_organization"
names(data)[49] <- "P21_decor_appeal"
names(data)[50] <- "P22_employee_presentation"




# Cronbach's Alpha
## EXPECTATION
### SA-1. Reliability (Expectation) Q:1-5
alpha(data[, 7:11])[1]
### SA-2. Responsiveness (Expectation) Q:6-9
alpha(data[, 12:15])[1]
### SA-3. Assurance (Expectation) Q:10-13
alpha(data[, 16:19])[1]
### SA-4. Empathy (Expectation) Q:14-18
alpha(data[, 20:24])[1]
### SA-5. Tangibility (Expectation) Q:19-22
alpha(data[, 25:28])[1]
## PERCEPTION
### SA-1. Reliability (Perception) Q:1-5
alpha(data[, 29:33])[1]
### SA-2. Responsiveness (Perception) Q:6-9
alpha(data[, 34:37])[1]
### SA-3. Assurance (Perception) Q:10-13
alpha(data[, 38:41])[1]
### SA-4. Empathy (Perception) Q:14-18
alpha(data[, 42:46])[1]
### SA-5. Tangibility (Perception) Q:19-22
alpha(data[, 47:50])[1]




# Respondents Profile
## Age
age_count_id <- data %>%
        dplyr::group_by(Age, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Age = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)
age_count_id$Percentage <- as.numeric(format(age_count_id$Count_by_Age/
                                                           sum(age_count_id$Count_by_Age), 
                                                   scientific = FALSE, digits = 2))
### Assign & Sort by Index
age_count_id$Index <- c(2, 3, 4, 1)
age_count_id <-  arrange(age_count_id, age_count_id$Index)
### Plot Number of Ids by Age
ylim_age <- c(0, 1.1*max(age_count_id$Count_by_Age))
xx_age <- barplot(age_count_id$Count_by_Age, xaxt = 'n', xlab = '', width = 0.85,
                       ylim = ylim_age, main = "Number of Respondents by Age Group", 
                       ylab = "Frequency")
text(x = xx_age, y = age_count_id$Count_by_Age, 
     label = age_count_id$Percentage, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_age, labels=age_count_id$Age, tick=FALSE, las=2, line=-0.5, cex.axis=0.8)

## Gender
gender_count_id <- data %>%
        dplyr::group_by(Gender, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Gender = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)
gender_count_id$Percentage <- as.numeric(format(gender_count_id$Count_by_Gender/
                                                     sum(gender_count_id$Count_by_Gender), 
                                             scientific = FALSE, digits = 2))
## Profession
prof_count_id <- data %>%
        dplyr::group_by(Profession, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Prof = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)
prof_count_id$Percentage <- as.numeric(format(prof_count_id$Count_by_Prof/
                                                      sum(prof_count_id$Count_by_Prof), 
                                              scientific = FALSE, digits = 2))
### Plot Number of Ids by Prof
par(mar = c(8,4,4,2))
ylim_prof <- c(0, 1.1*max(prof_count_id$Count_by_Prof))
xx_prof <- barplot(prof_count_id$Count_by_Prof, xaxt = 'n', xlab = '', width = 0.85,
                   ylim = ylim_prof, main = "Number of Respondents by Profession", 
                   ylab = "Frequency")
text(x = xx_prof, y = prof_count_id$Count_by_Prof, 
     label = prof_count_id$Percentage, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_prof, labels=prof_count_id$Profession, tick=FALSE, las=2, line=-0.5, cex.axis=0.8)

## Income
income_count_id <- data %>%
        dplyr::group_by(monthly_income, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Income = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)
income_count_id$Percentage <- as.numeric(format(income_count_id$Count_by_Income/
                                                        sum(income_count_id$Count_by_Income), 
                                                scientific = FALSE, digits = 2))
### Assign & Sort by Index
income_count_id$Index <- c(2, 3, 4, 5, 1)
income_count_id <-  arrange(income_count_id, income_count_id$Index)
### Plot Number of Ids by Income
par(mar = c(6,4,4,2))
ylim_inc <- c(0, 1.1*max(income_count_id$Count_by_Income))
xx_inc <- barplot(income_count_id$Count_by_Income, xaxt = 'n', xlab = '', width = 0.85,
                  ylim = ylim_inc, main = "Number of Respondents by Monthly Income/Allowance in BDT", 
                  ylab = "Frequency",
                  cex.main = 0.9)
text(x = xx_inc, y = income_count_id$Count_by_Income, 
     label = income_count_id$Percentage, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_inc, labels=income_count_id$monthly_income, tick=FALSE, las=2, line=-0.5, cex.axis=0.8)

## Usage
use_count_id <- data %>%
        dplyr::group_by(monthly_visit, .drop = TRUE) %>%
        dplyr::summarize(Count_by_Use = n()) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)
use_count_id$Percentage <- as.numeric(format(use_count_id$Count_by_Use/
                                                        sum(use_count_id$Count_by_Use), 
                                                scientific = FALSE, digits = 2))
### Plot Number of Ids by Monthly Visits
par(mar = c(5,4,4,2))
ylim_use <- c(0, 1.1*max(use_count_id$Count_by_Use))
xx_use <- barplot(use_count_id$Count_by_Use, xaxt = 'n', xlab = '', width = 0.85,
                  ylim = ylim_use, main = "Number of Respondents by Average Number of Monthly Visits to \n Superstores with Purchase Intention", 
                  ylab = "Frequency",
                  cex.main = 0.9)
text(x = xx_use, y = use_count_id$Count_by_Use, 
     label = use_count_id$Percentage, pos = 3, cex = 0.8, col = "black")
axis(1, at=xx_use, labels=use_count_id$monthly_visit, tick=FALSE, las=2, line=-0.5, cex.axis=0.8)



# Data Pasre [Unnecessary]
## EXPECTATION
### SA-1. Reliability (Expectation) Q:1-5
E1_reliability <- c(data[, 7], data[, 8], data[, 9], data[, 10], data[, 11])
### SA-2. Responsiveness (Expectation) Q:6-9
E2_responsiveness <- c(data[, 12], data[, 13], data[, 14], data[, 15])
### SA-3. Assurance (Expectation) Q:10-13
E3_assurance <- c(data[, 16], data[, 17], data[, 18], data[, 19])
### SA-4. Empathy (Expectation) Q:14-18
E4_empathy <- c(data[, 20], data[, 21], data[, 22], data[, 23], data[, 24])
### SA-5. Tangibility (Expectation) Q:19-22
E5_tangibility <- c(data[, 25], data[, 26], data[, 27], data[, 28])
## PERCEPTION
### SA-1. Reliability (Perception) Q:1-5
P1_reliability <- c(data[, 29], data[, 30], data[, 31], data[, 32], data[, 33])
### SA-2. Responsiveness (Perception) Q:6-9
P2_responsiveness <- c(data[, 34], data[, 35], data[, 36], data[, 37])
### SA-3. Assurance (Perception) Q:10-13
P3_assurance <- c(data[, 38], data[, 39], data[, 40], data[, 41])
### SA-4. Empathy (Perception) Q:14-18
P4_empathy <- c(data[, 42], data[, 43], data[, 44], data[, 45], data[, 46])
### SA-5. Tangibility (Perception) Q:19-22
P5_tangibility <- c(data[, 47], data[, 48], data[, 49], data[, 50])

Perception <- data.frame(P1_reliability, E1_reliability)




# Gap Score
mean(P1_reliability) - mean(E1_reliability)
mean(P2_responsiveness) - mean(E2_responsiveness)
mean(P3_assurance) - mean(E3_assurance)
mean(P4_empathy) - mean(E4_empathy)
mean(P5_tangibility) - mean(E5_tangibility)

# Levene Variance Test
leveneTest(P1_reliability, E1_reliability)
leveneTest(P2_responsiveness, E2_responsiveness)
leveneTest(P3_assurance, E3_assurance)
leveneTest(P4_empathy, E4_empathy)
leveneTest(P5_tangibility, E5_tangibility)

# Two Sample T Test
t.test(P1_reliability, E1_reliability, mu=0, alt="two.sided", conf=0.95, 
       var.eq = T, paired = F)
t.test(P2_responsiveness, E2_responsiveness, mu=0, alt="two.sided", conf=0.95, 
       var.eq = F, paired = F)
t.test(P3_assurance, E3_assurance, mu=0, alt="two.sided", conf=0.95, 
       var.eq = T, paired = F)
t.test(P4_empathy, E4_empathy, mu=0, alt="two.sided", conf=0.95, 
       var.eq = T, paired = F)
t.test(P5_tangibility, E5_tangibility, mu=0, alt="two.sided", conf=0.95, 
       var.eq = T, paired = F)


# Individual Gap Score
data$P_Reliability_Mean <- rowMeans(data[, 29:33])
data$E_Reliability_Mean <- rowMeans(data[, 7:11])
data$Gap_Score_Reliability <- data$P_Reliability_Mean - data$E_Reliability_Mean

data$P_Responsiveness_Mean <- rowMeans(data[, 34:37])
data$E_Responsiveness_Mean <- rowMeans(data[, 12:15])
data$Gap_Score_Responsiveness <- data$P_Responsiveness_Mean - data$E_Responsiveness_Mean

data$P_Assurance_Mean <- rowMeans(data[, 38:41])
data$E_Assurance_Mean <- rowMeans(data[, 16:19])
data$Gap_Score_Assurance <- data$P_Assurance_Mean - data$E_Assurance_Mean

data$P_Empathy_Mean <- rowMeans(data[, 42:46])
data$E_Empathy_Mean <- rowMeans(data[, 20:24])
data$Gap_Score_Empathy <- data$P_Empathy_Mean - data$E_Empathy_Mean

data$P_Tangibility_Mean <- rowMeans(data[, 47:50])
data$E_Tangibility_Mean <- rowMeans(data[, 25:28])
data$Gap_Score_Tangibility <- data$P_Tangibility_Mean - data$E_Tangibility_Mean


# Gap Score by Gender
par(mfrow = c(3,2))
boxplot(data$Gap_Score_Reliability~data$Gender, main = "Reliability Gap Score",
        xlab = "Gender", ylab = "Average Score")
boxplot(data$Gap_Score_Responsiveness~data$Gender, main = "Responsiveness Gap Score",
        xlab = "Gender", ylab = "Average Score")
boxplot(data$Gap_Score_Assurance~data$Gender, main = "Assurance Gap Score",
        xlab = "Gender", ylab = "Average Score")
boxplot(data$Gap_Score_Empathy~data$Gender, main = "Empathy Gap Score",
        xlab = "Gender", ylab = "Average Score")
boxplot(data$Gap_Score_Tangibility~data$Gender, main = "Tangibility Gap Score",
        xlab = "Gender", ylab = "Average Score")

## Levene Variance Test
leveneTest(data$Gap_Score_Reliability[data$Gender=='Male'], 
           data$Gap_Score_Reliability[data$Gender=='Female'])
leveneTest(data$Gap_Score_Responsiveness[data$Gender=='Male'], 
           data$Gap_Score_Responsiveness[data$Gender=='Female'])
leveneTest(data$Gap_Score_Assurance[data$Gender=='Male'], 
           data$Gap_Score_Assurance[data$Gender=='Female'])
leveneTest(data$Gap_Score_Empathy[data$Gender=='Male'], 
           data$Gap_Score_Empathy[data$Gender=='Female'])
leveneTest(data$Gap_Score_Tangibility[data$Gender=='Male'], 
           data$Gap_Score_Tangibility[data$Gender=='Female'])

## Two Sample T Test
t.test(data$Gap_Score_Reliability[data$Gender=='Male'], 
       data$Gap_Score_Reliability[data$Gender=='Female'], mu=0, alt="two.sided", conf=0.95, 
       var.eq = T, paired = F)
t.test(data$Gap_Score_Responsiveness[data$Gender=='Male'], 
       data$Gap_Score_Responsiveness[data$Gender=='Female'], mu=0, alt="two.sided", conf=0.95, 
       var.eq = T, paired = F)
t.test(data$Gap_Score_Assurance[data$Gender=='Male'], 
       data$Gap_Score_Assurance[data$Gender=='Female'], mu=0, alt="two.sided", conf=0.95, 
       var.eq = T, paired = F)
t.test(data$Gap_Score_Empathy[data$Gender=='Male'], 
       data$Gap_Score_Empathy[data$Gender=='Female'], mu=0, alt="two.sided", conf=0.95, 
       var.eq = T, paired = F)
t.test(data$Gap_Score_Tangibility[data$Gender=='Male'], 
       data$Gap_Score_Tangibility[data$Gender=='Female'], mu=0, alt="two.sided", conf=0.95, 
       var.eq = T, paired = F)

## ANOVA for Gender [EXTRA]
summary(aov(Gap_Score_Reliability~Gender, data = data))
summary(aov(Gap_Score_Responsiveness~Gender, data = data))
summary(aov(Gap_Score_Assurance~Gender, data = data))
summary(aov(Gap_Score_Empathy~Gender, data = data))
summary(aov(Gap_Score_Tangibility~Gender, data = data))




# Gap Score by Age Group
par(mfrow = c(3,2))
boxplot(data$Gap_Score_Reliability~data$Age, main = "Reliability Gap Score",
        xlab = "Age", ylab = "Average Score")
boxplot(data$Gap_Score_Responsiveness~data$Age, main = "Responsiveness Gap Score",
        xlab = "Age", ylab = "Average Score")
boxplot(data$Gap_Score_Assurance~data$Age, main = "Assurance Gap Score",
        xlab = "Age", ylab = "Average Score")
boxplot(data$Gap_Score_Empathy~data$Age, main = "Empathy Gap Score",
        xlab = "Age", ylab = "Average Score")
boxplot(data$Gap_Score_Tangibility~data$Age, main = "Tangibility Gap Score",
        xlab = "Age", ylab = "Average Score")

summary(aov(Gap_Score_Reliability~Age, data = data))
summary(aov(Gap_Score_Responsiveness~Age, data = data))
summary(aov(Gap_Score_Assurance~Age, data = data))
summary(aov(Gap_Score_Empathy~Age, data = data))
summary(aov(Gap_Score_Tangibility~Age, data = data))




# Gap Score by Income
par(mfrow = c(3,2))
boxplot(data$Gap_Score_Reliability~data$monthly_income, main = "Reliability Gap Score",
        xlab = "Monthly Income/Allowance in BDT", ylab = "Average Score")
boxplot(data$Gap_Score_Responsiveness~data$monthly_income, main = "Responsiveness Gap Score",
        xlab = "Monthly Income/Allowance in BDT", ylab = "Average Score")
boxplot(data$Gap_Score_Assurance~data$monthly_income, main = "Assurance Gap Score",
        xlab = "Monthly Income/Allowance in BDT", ylab = "Average Score")
boxplot(data$Gap_Score_Empathy~data$monthly_income, main = "Empathy Gap Score",
        xlab = "Monthly Income/Allowance in BDT", ylab = "Average Score")
boxplot(data$Gap_Score_Tangibility~data$monthly_income, main = "Tangibility Gap Score",
        xlab = "Monthly Income/Allowance in BDT", ylab = "Average Score")

summary(aov(Gap_Score_Reliability~monthly_income, data = data))
summary(aov(Gap_Score_Responsiveness~monthly_income, data = data))
summary(aov(Gap_Score_Assurance~monthly_income, data = data))
summary(aov(Gap_Score_Empathy~monthly_income, data = data))
summary(aov(Gap_Score_Tangibility~monthly_income, data = data))




# Gap Score by Profession
par(mfrow = c(3,2))
boxplot(data$Gap_Score_Reliability~data$Profession, main = "Reliability Gap Score",
        xlab = "Profession", ylab = "Average Score", cex.axis = 0.5)
boxplot(data$Gap_Score_Responsiveness~data$Profession, main = "Responsiveness Gap Score",
        xlab = "Profession", ylab = "Average Score", cex.axis = 0.5)
boxplot(data$Gap_Score_Assurance~data$Profession, main = "Assurance Gap Score",
        xlab = "Profession", ylab = "Average Score", cex.axis = 0.5)
boxplot(data$Gap_Score_Empathy~data$Profession, main = "Empathy Gap Score",
        xlab = "Profession", ylab = "Average Score", cex.axis = 0.5)
boxplot(data$Gap_Score_Tangibility~data$Profession, main = "Tangibility Gap Score",
        xlab = "Profession", ylab = "Average Score", cex.axis = 0.5)

summary(aov(Gap_Score_Reliability~Profession, data = data))
summary(aov(Gap_Score_Responsiveness~Profession, data = data))
summary(aov(Gap_Score_Assurance~Profession, data = data))
summary(aov(Gap_Score_Empathy~Profession, data = data))
summary(aov(Gap_Score_Tangibility~Profession, data = data))





# Gap Score by Usage
par(mfrow = c(3,2))
boxplot(data$Gap_Score_Reliability~data$monthly_visit, main = "Reliability Gap Score",
        xlab = "Monthly Visit", ylab = "Average Score")
boxplot(data$Gap_Score_Responsiveness~data$monthly_visit, main = "Responsiveness Gap Score",
        xlab = "Monthly Visit", ylab = "Average Score")
boxplot(data$Gap_Score_Assurance~data$monthly_visit, main = "Assurance Gap Score",
        xlab = "Monthly Visit", ylab = "Average Score")
boxplot(data$Gap_Score_Empathy~data$monthly_visit, main = "Empathy Gap Score",
        xlab = "Monthly Visit", ylab = "Average Score")
boxplot(data$Gap_Score_Tangibility~data$monthly_visit, main = "Tangibility Gap Score",
        xlab = "Monthly Visit", ylab = "Average Score")

summary(aov(Gap_Score_Reliability~monthly_visit, data = data))
summary(aov(Gap_Score_Responsiveness~monthly_visit, data = data))
summary(aov(Gap_Score_Assurance~monthly_visit, data = data))
summary(aov(Gap_Score_Empathy~monthly_visit, data = data))
summary(aov(Gap_Score_Tangibility~monthly_visit, data = data))









save.image("SERVQUAL.RData")









# [Others]

data1$Exp_Relaibility_Sum <- rowSums(data[, 7:11])
data1$Exp_Relaibility_Mean <- rowMeans(data[, 7:11])

boxplot(data1$Exp_Relaibility_Mean ~ data1$Age)
summary(aov(Exp_Relaibility_Mean ~ Age, data = data1))

a <- data1 %>%
        dplyr::group_by(Age, .drop = TRUE) %>%
        dplyr::summarize(Sum = sum(Exp_Relaibility_Sum), Count = n(Age)) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

a$mean <- (a$Sum/a$Count)/5

anova(a$mean)
boxplot(a$mean ~ a$Age)

summary(aov(mean ~ Age, data = a))


par(mfrow = c(1,1))
boxplot(data$E1_product_availability ~ data$Age)

leveneTest(P1_reliability ~ E1_reliability)


mean(P1_reliability)

boxplot(P1_reliability, E1_reliability)

mean(data[, 29:33])
mean(data$E1_product_availability)

data %>% 
        group_by(data[, 29:33]) %>%
        summarise_all("mean")

a <- as.matrix(data[, 29:33])



aframe

avector <- as.vector(data['P1_product_availability'])
class(avector)

avector <- data[['E3_accurate_record':'P1_product_availability']]
class(avector)

avector <- data[,24:33]
class(avector)

mean(avector)

data[[29:33]]

colMeans(data[,c("E3_accurate_record", "P1_product_availability")], na.rm=TRUE)

ER <- append(data[, 29], data[, 30])

data.frame(a = c(data[,"a"], df[,"b"]))

df <- data[, 1:7]

df <- rbind

mean(ER)

colMeans(data[, 29:33])

a <- as.list(data)

a <- data.frame(c(data[, 1:5], data[, 17]))


a <- data %>%
        dplyr::select(Age, E1_product_availability, E2_employee_sincerity, E3_accurate_record,
                      E4_employee_efficiency, E5_service_time) %>%
        dplyr::group_by(Age, .drop = TRUE) %>%
        dplyr::summarize(mean(E1_reliability)) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

e1 <- data[, 1:7]
e2 <- data[, c(1:6, 8)]
e3 <- data[, c(1:6, 9)]
e4 <- data[, c(1:6, 10)]
e5 <- data[, c(1:6, 11)]

names(e1)[7] <- "Expectation_Reliability"
names(e2)[7] <- "Expectation_Reliability"
names(e3)[7] <- "Expectation_Reliability"
names(e4)[7] <- "Expectation_Reliability"
names(e5)[7] <- "Expectation_Reliability"

df_expectation_reliability <- rbind(e1, e2, e3, e4, e5)


e6 <- data[, c(1:6, 12)]
e7 <- data[, c(1:6, 13)]
e8 <- data[, c(1:6, 14)]
e9 <- data[, c(1:6, 15)]

names(e6)[7] <- "Expectation_Responsiveness"
names(e7)[7] <- "Expectation_Responsiveness"
names(e8)[7] <- "Expectation_Responsiveness"
names(e9)[7] <- "Expectation_Responsiveness"

df_expectation_responsiveness <- rbind(e6, e7, e8, e9)


e10 <- data[, c(1:6, 16)]
e11 <- data[, c(1:6, 17)]
e12 <- data[, c(1:6, 18)]
e13 <- data[, c(1:6, 19)]

names(e10)[7] <- "Expectation_Assurance"
names(e11)[7] <- "Expectation_Assurance"
names(e12)[7] <- "Expectation_Assurance"
names(e13)[7] <- "Expectation_Assurance"

df_expectation_assurance <- rbind(e10, e11, e12, e13)


e14 <- data[, c(1:6, 20)]
e15 <- data[, c(1:6, 21)]
e16 <- data[, c(1:6, 22)]
e17 <- data[, c(1:6, 23)]
e18 <- data[, c(1:6, 24)]

names(e14)[7] <- "Expectation_Empathy"
names(e15)[7] <- "Expectation_Empathy"
names(e16)[7] <- "Expectation_Empathy"
names(e17)[7] <- "Expectation_Empathy"
names(e18)[7] <- "Expectation_Empathy"

df_expectation_empathy <- rbind(e14, e15, e16, e17, e18)


e19 <- data[, c(1:6, 25)]
e20 <- data[, c(1:6, 26)]
e21 <- data[, c(1:6, 27)]
e22 <- data[, c(1:6, 28)]

names(e19)[7] <- "Expectation_Tangibility"
names(e20)[7] <- "Expectation_Tangibility"
names(e21)[7] <- "Expectation_Tangibility"
names(e22)[7] <- "Expectation_Tangibility"

df_expectation_tangibility <- rbind(e19, e20, e21, e22)


Expectation_Reliability_Gender <- df_expectation_reliability %>%
        dplyr::group_by(Gender, .drop = TRUE) %>%
        dplyr::summarize(mean(Expectation_Reliability)) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

Expectation_Responsiveness_Gender <- df_expectation_responsiveness %>%
        dplyr::group_by(Gender, .drop = TRUE) %>%
        dplyr::summarize(mean(Expectation_Responsiveness)) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

Expectation_Assurance_Gender <- df_expectation_assurance %>%
        dplyr::group_by(Gender, .drop = TRUE) %>%
        dplyr::summarize(mean(Expectation_Assurance)) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

Expectation_Empathy_Gender <- df_expectation_empathy %>%
        dplyr::group_by(Gender, .drop = TRUE) %>%
        dplyr::summarize(mean(Expectation_Empathy)) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

Expectation_Tangibility_Gender <- df_expectation_tangibility %>%
        dplyr::group_by(Gender, .drop = TRUE) %>%
        dplyr::summarize(mean(Expectation_Tangibility)) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)

Expectation_Gender <- cbind(Expectation_Reliability_Gender, Expectation_Responsiveness_Gender,
                            Expectation_Assurance_Gender, Expectation_Empathy_Gender,
                            Expectation_Tangibility_Gender)
Expectation_Gender <- Expectation_Gender[, c(1:2,4,6,8,10)]



Expectation_Reliability_Age <- df_expectation_reliability %>%
        dplyr::group_by(Age, .drop = TRUE) %>%
        dplyr::summarize(mean(Expectation_Reliability)) %>%
        dplyr::ungroup() %>%
        remove_missing(na.rm = FALSE)



rowsum(data[, 20:24])



colnames(data)


