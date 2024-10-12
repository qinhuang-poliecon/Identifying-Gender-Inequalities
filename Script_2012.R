#### Pathways to Political Participation 2012 Dataset
#### Date: 30/04/2023

rm(list = ls())

### Loading Libraries 

library(tidyverse)
library(haven)
library(dplyr)
library(QCA)
library(SetMethods)



### Loading the Dataset 

load("Data/2012.rda")
data_original <- da35012.0001


### Data Cleaning

data <- data_original %>%
  select(QI1, QVII1, QVII2, QVII3, QVII4, QX17, QX23, QVI17, QGENDER
  )


#### Calibration ####

## Calibrate gender

table(data$QGENDER) 
data$QGENDER <- as.character(data$QGENDER)
unique(data$QGENDER)
data$Gender <- NA 
data$Gender[data$QGENDER == "(1) Male"] <- 1
data$Gender[data$QGENDER == "(2) Female"] <- 0
table(data$Gender)



## Calibrate election vote

table(data$QI1) ## Voted in 2012
data$QI1 <- as.character(data$QI1)
unique(data$QI1)
data$Vote <- NA 
data$Vote[data$QI1 == "(2) No "] <- 0 
data$Vote[data$QI1 == "(1) Yes "] <- 1 
table(data$Vote)


table(data$Gender == 1, data$Vote)

## college education

table(data$QX17) 
data$QX17 <- as.character(data$QX17)
unique(data$QX17)
data$College <- NA 
data$College[data$QX17 == "(1) None, or grade 1 to 8 "] <- 0 
data$College[data$QX17 == "(2) High school incomplete "] <- 0 
data$College[data$QX17 == "(3) High school graduate "] <- 0 
data$College[data$QX17 == "(4) Technical or Trade "] <- 0 
data$College[data$QX17 == "(5) Some College or University "] <- 1 
data$College[data$QX17 == "(6) College or University graduate "] <- 1 
data$College[data$QX17 == "(7) Some post-graduate "] <- 1 
data$College[data$QX17 == "(8) Post Graduate or Professional Degree "] <- 1 
table(data$College)


## family background

table(data$QX23) 
data$QX23 <- as.character(data$QX23)
unique(data$QX23)
data$FB <- NA 
data$FB[data$QX23 == "(1) A Few (0-10) "] <- 0 
data$FB[data$QX23 == "(2) Enough to fill one shelf (11-25) "] <- 0 
data$FB[data$QX23 == "(3) Enough to fill one bookcase (26-100) "] <- 0 
data$FB[data$QX23 == "(4) Enough to fill several bookcases (more than 100) "] <- 1
table(data$FB)


## socialization with friends

table(data$QVII1) 
data$QVII1 <- as.character(data$QVII1)
unique(data$QVII1)
data$SocFr1 <- NA
data$SocFr1[data$QVII1 == "(1) Never "] <- 0 
data$SocFr1[data$QVII1 == "(2) Once a month"] <- 0 
data$SocFr1[data$QVII1 == "(3) A few times a month "] <- 0 
data$SocFr1[data$QVII1 == "(4) A few times a week "] <- 1 
data$SocFr1[data$QVII1 == "(5) Daily"] <- 1
table(data$SocFr1)


## socialization with friends 2

table(data$QVII2) 
data$QVII2 <- as.character(data$QVII2)
unique(data$QVII2)
data$SocFr2 <- NA
data$SocFr2[data$QVII2 == "(1) None"] <- 0 
data$SocFr2[data$QVII2 == "(2) Hardly any"] <- 0 
data$SocFr2[data$QVII2 == "(3) Less than half"] <- 0 
data$SocFr2[data$QVII2 == "(4) About half"] <- 0 
data$SocFr2[data$QVII2 == "(5) More than half"] <- 0 
data$SocFr2[data$QVII2 == "(6) Most"] <- 0
data$SocFr2[data$QVII2 == "(7) Almost all"] <- 1 

table(data$SocFr2)

## socialization with family

table(data$QVII3) 
data$QVII3 <- as.character(data$QVII3)
unique(data$QVII3)
data$SocFa1 <- NA
data$SocFa1[data$QVII3 == "(1) Never "] <- 0 
data$SocFa1[data$QVII3 == "(2) Once a month"] <- 0 
data$SocFa1[data$QVII3 == "(3) A few times a month "] <- 0 
data$SocFa1[data$QVII3 == "(4) A few times a week "] <- 1 
data$SocFa1[data$QVII3 == "(5) Daily"] <- 1

table(data$SocFa1)


## socialization with family2


table(data$QVII4) 
data$QVII4 <- as.character(data$QVII4)
unique(data$QVII4)
data$SocFa2 <- NA
data$SocFa2[data$QVII4 == "(2) No"] <- 0 
data$SocFa2[data$QVII4 == "(1) Yes"] <- 1 
table(data$SocFa2)


## high school activism

table(data$QVI17) 
data$QVI17 <- as.character(data$QVI17)
unique(data$QVI17)
data$HighAc <- NA
data$HighAc[data$QVI17 == "(1) Yes, groups at school only "] <- 0
data$HighAc[data$QVI17 == "(2) Yes, groups  outside of school only "] <- 0
data$HighAc[data$QVI17 == "(3) Yes, groups both at and outside of school "] <- 1
data$HighAc[data$QVI17 == "(4) No groups "] <- 0
table(data$HighAc)


## Saving calibration


data_male <- data %>%
  filter(Gender == 1) %>%
  select(Vote, College, FB, SocFa1, SocFa2, SocFr1, SocFr2, HighAc) 
  
data_female <- data %>%
  filter(Gender == 0) %>%
  select(Vote, College, FB, SocFa1, SocFa2, SocFr1, SocFr2, HighAc) 


### Table 6: Male Model ####


data_male <- data %>%
  filter(Gender == 1) %>%
  select(Vote, College, FB, SocFr2, HighAc) %>%
  remove_missing()

## Necessity Analysis

QCAfit(data_male$College, 
       data_male$Vote,
       cond.lab = "College Attendance") 

QCAfit(data_male$FB, 
       data_male$Vote,
       cond.lab = "Family Background")

QCAfit(data_male$SocFr2, 
       data_male$Vote,
       cond.lab = "Socialization with Friends")

QCAfit(data_male$HighAc, 
       data_male$Vote,
       cond.lab = "School Activism")

QCAfit(1-data_male$College, 
       data_male$Vote,
       cond.lab = "~College Attendance") 

QCAfit(1-data_male$FB, 
       data_male$Vote,
       cond.lab = "~Family Background")

QCAfit(1-data_male$SocFr2, 
       data_male$Vote,
       cond.lab = "~Socialization with Friends")

QCAfit(1-data_male$HighAc, 
       data_male$Vote,
       cond.lab = "~School Activism")



## Sufficiency Analysis


Male80_tt <- truthTable(data = data_male, 
                           outcome='Vote', 
                           conditions = c("FB", "College", "SocFr2", "HighAc"), 
                           incl.cut = 0.80, #consistency cut
                           pri.cut = 0.51, #PRI cut
                           n.cut = 10, #cases in a row before is classified as logical remainder (rows without enough empirical evidence)
                           
                           complete = TRUE) #shows all rows in the TT

Male80_Solution <- minimize(input =  Male80_tt, 
                    use.tilde = TRUE,
                    details = TRUE)


x1 <- Male80_tt$tt %>%
  select(-cases, -PRI)

write.csv(x1, file = "12Male80.csv", quote = F)


### Table 6: Female Model ####


data_female <- data %>%
  filter(Gender == 0) %>%
  select(Vote, College, FB, SocFr2, HighAc) %>%
  remove_missing()

## Necessity Analysis

QCAfit(data_female$College, 
       data_female$Vote,
       cond.lab = "College Attendance") 

QCAfit(data_female$FB, 
       data_female$Vote,
       cond.lab = "Family Background")

QCAfit(data_female$SocFr2, 
       data_female$Vote,
       cond.lab = "Socialization with Friends")

QCAfit(data_female$HighAc, 
       data_female$Vote,
       cond.lab = "School Activism")

QCAfit(1-data_female$College, 
       data_female$Vote,
       cond.lab = "~College Attendance") 

QCAfit(1-data_female$FB, 
       data_female$Vote,
       cond.lab = "~Family Background")

QCAfit(1-data_female$SocFr2, 
       data_female$Vote,
       cond.lab = "~Socialization with Friends")

QCAfit(1-data_female$HighAc, 
       data_female$Vote,
       cond.lab = "~School Activism")



## Sufficiency Analysis


FeMale80_tt <- truthTable(data = data_female, 
                        outcome='Vote', 
                        conditions = c("FB", "College", "SocFr2", "HighAc"), 
                        incl.cut = 0.80, #consistency cut
                        pri.cut = 0.51, #PRI cut
                        n.cut = 10, #cases in a row before is classified as logical remainder (rows without enough empirical evidence)
                        
                        complete = TRUE) #shows all rows in the TT

FeMale80_Solution <- minimize(input =  FeMale80_tt, 
                            use.tilde = TRUE,
                            details = TRUE)


x1 <- FeMale80_tt$tt %>%
  select(-cases, -PRI)

write.csv(x1, file = "12FeMale80.csv", quote = F)


### Table 5: Rregression Analysis ####

data_reg <- data_original %>%
  select(QGENDER, QI1, QIII1, QIII20, QV1, QV2, QIV4,
         QVI1, QVI2, QVI3, QVI4, QVI6, QVI17, QVI22, 
         QVI23, QVI24, QVI25, QVI29, QVI_30, QVI_31, QVI_32,
         QVII1, QVII2, QVII3, QVII4, QVII5, QVII6, QVII7,
         QIX, QIX2, QIX3, QIX4, QIX5, QIX6, QIX7, QIX8, QIX9,
         QX2, QX7,QX8,QX9,QX10,QX11,QX12,QX13,QX14,QX16,QX17,
         QX18, QX20, QX21, QX22, QX23
  ) %>%
  mutate_all(~replace(., . == 98 | . == 99, NA))

miss_var_summary(data_reg)

## subsamples

table(data_reg$QGENDER)

data_reg_male <- data_reg %>%
  filter(QGENDER == "(1) Male")

data_reg_female <- data_reg %>%
  filter(QGENDER == "(2) Female")




## Impute missing values
#imputed_male <- knnImputation(data_reg_male, k = 5) 
#imputed_female <- knnImputation(data_reg_female, k = 5) 



## Logistic regression

table(data_reg$QGENDER) #gender binary
table(data_reg$QI1) # vote binary
table(data_reg$QX17) # education ordinal
table(data_reg$QX23) # family bg ordinal 
table(data_reg$QVII1) # friend 1 ordinal
table(data_reg$QVII2) # friend 2 ordinal
table(data_reg$QVII3) # family 1 ordinal
table(data_reg$QVII4) # family 2 ordinal
table(data_reg$QVI25) # HSA ordinal


# control variables
table(data_reg$QX22) # race categorical
table(data_reg$QX20) # immigrant family
table(data_reg$QX11) # religion categorical
table(data_reg$QIV4) # political knowledge



male1<- glm(as.factor(QI1) ~ as.numeric(QX17) + as.numeric(QX23) + as.numeric(QVII1) + as.numeric(QVII2) + as.numeric(QVII3)   + as.numeric(QVI25), data = data_reg_male, family = binomial)

summary(male1)


male2<- glm(as.factor(QI1) ~ as.numeric(QX17) + as.numeric(QX23) + as.numeric(QVII1) + as.numeric(QVII2) + as.numeric(QVII3)   + as.numeric(QVI25) + as.factor(QX22) + as.numeric(QX20) + as.factor(QX11), data = data_reg_male, family = binomial)

summary(male2)


female1<- glm(as.factor(QI1) ~ as.numeric(QX17) + as.numeric(QX23) + as.numeric(QVII1) + as.numeric(QVII2) + as.numeric(QVII3)  + as.numeric(QVI25), data = data_reg_female, family = binomial)

summary(female1)

female2<- glm(as.factor(QI1) ~ as.numeric(QX17) + as.numeric(QX23) + as.numeric(QVII1) + as.numeric(QVII2) + as.numeric(QVII3)  + as.numeric(QVI25) + as.factor(QX22) + as.numeric(QX20) + as.factor(QX11), data = data_reg_female, family = binomial)

summary(female2)


