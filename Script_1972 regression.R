rm(list = ls())



### Loading Libraries

library(tidyverse)
library(haven)
library(dplyr)
library(DMwR2)
library(stats)


### Data Cleaning ####

## Merge two datasets

data_p <- read_dta("Data/PPanel.dta")
data_s <- read_dta("Data/SPanel.dta")
data_pk <-  select(data_p, "V5", "V41","V43", "V44","V51","V57","V58","V59","V60","V136","V168","V181","V185","V195","V242","V243","V244", "V258","V392", "V408","V530")
data_pk <- rename(data_pk, P_V41 = V41,  P_V43 = V43, P_V44 = V44, P_V51 = V51, P_V57 = V57, P_V58 = V58, P_V59 = V59, P_V60 = V60,  P_V136 = V136,  P_V168 = V168,  P_V181 = V181,  P_V185 = V185,  P_V195 = V195,  P_V242 = V242,  P_V243 = V243,  P_V244 = V244,  P_V258 = V258,  P_V392 = V392,  P_V408 = V408,  P_V530 = V530)
data_m <- merge(data_s, data_pk, by = "V5") ## Now we have the dataset


## Select variables
data_ml <-  select(data_m, "P_V41","P_V43", "P_V44", "P_V51", "P_V57", "P_V58", "P_V59","P_V60","P_V136","P_V168","P_V181","P_V185","P_V195","P_V242","P_V243","P_V244", "P_V258","P_V392", "P_V408","P_V530","V22", "V23","V24", "V32","V33","V34","V35","V36","V37","V41","V42","V43","V44", "V45","V46", "V58","V59","V60", "V63","V65", "V67","V69","V71","V73","V77","V78","V99","V101","V103","V104", "V112","V115", "V116","V125","V141", "V147", "V149","V150", "V151","V152","V153","V154","V155","V156","V158","V159","V179","V180", "V186","V188", "V189","V190","V191", "V192","V193", "V194","V195","V196","V200","V201","V202","V204","V207","V208","V209","V231","V232", "V364", "V380","V502", "V648")



## Remove variables with too many missings

data_fl <-  select(data_ml,-"V23", -"V24",-"V33", -"V34",-"V43", -"V67",-"V71",-"V101", -"V103",-"V104",-"V186", -"V200") 
data_fl <- as.data.frame(lapply(data_fl, as.character))


## Recode 46

data_fl <- data_fl %>% 
  mutate(V32 = plyr::mapvalues(V32, from = c("5"), to = c("0"))) %>%
  mutate(V35 = plyr::mapvalues(V35, from = c("5"), to = c("0"))) %>%
  mutate(V36 = plyr::mapvalues(V36, from = c("4", "3", "2", "1"), to = c("1","2","3", "4"))) %>%
  mutate(V37 = plyr::mapvalues(V37, from = c("5"), to = c("0"))) %>%
  mutate(V41 = plyr::mapvalues(V41, from = c("4", "3", "2", "1","5"), to = c("1","1","1", "1","0"))) %>%
  mutate(V42 = plyr::mapvalues(V42, from = c("2", "3"), to = c("0","0"))) %>%
  mutate(V44 = plyr::mapvalues(V44, from = c("5"), to = c("0"))) %>%
  mutate(V45 = plyr::mapvalues(V45, from = c("5"), to = c("0"))) %>%
  mutate(V46 = plyr::mapvalues(V46, from = c("5"), to = c("0"))) 

## Recode 78

data_fl <- data_fl %>% 
  mutate(V60 = plyr::mapvalues(V60, from = c("5"), to = c("0"))) %>%
  mutate(V63 = plyr::mapvalues(V63, from = c("5","4", "3", "2", "1","7"), to = c("0","1","2","3", "4","1"))) %>%
  mutate(V65 = plyr::mapvalues(V65, from = c("5","4", "3", "2", "1","7"), to = c("0","1","2","3", "4","1"))) %>%
  mutate(V69 = plyr::mapvalues(V69, from = c("5","4", "3", "2", "1","7"), to = c("0","1","2","3", "4","1"))) %>%
  mutate(V77 = plyr::mapvalues(V77, from = c("5","4", "3", "2", "1","7"), to = c("0","1","2","3", "4","1"))) %>%
  mutate(V78 = plyr::mapvalues(V78, from = c("5","4", "3", "2", "1","7"), to = c("0","1","2","3", "4","1"))) 
  

## Recode 155

data_fl <- data_fl %>% 
  mutate(V99 = plyr::mapvalues(V99, from = c("2", "3", "4","5","6", "7", "8"), to = c("0","0","0","0","0","0", "0"))) %>% # 1 is not divorce
  mutate(V112 = plyr::mapvalues(V112, from = c("0", "3", "2", "1"), to = c("9","1","2","3"))) %>%
  mutate(V115 = plyr::mapvalues(V115, from = c("0", "3", "2", "1"), to = c("9","1","2","3"))) %>%
  mutate(V116 = plyr::mapvalues(V116, from = c("0", "3", "2", "1"), to = c("9","1","2","3"))) %>%
  mutate(V141= plyr::mapvalues(V141, from = c("4", "3", "2", "1"), to = c("1","2","3", "4"))) %>%
  mutate(V147= plyr::mapvalues(V147, from = c("5", "3", "1"), to = c("1","2", "3"))) %>%
  mutate(V149= plyr::mapvalues(V149, from = c("5", "3", "1"), to = c("1","2", "3"))) %>%
  mutate(V150= plyr::mapvalues(V150, from = c("5", "3", "1"), to = c("1","2", "3"))) %>%
  mutate(V151= plyr::mapvalues(V151, from = c("5", "3", "1"), to = c("1","2", "3"))) %>%
  mutate(V152= plyr::mapvalues(V152, from = c("5", "3", "1"), to = c("1","2", "3"))) %>%
  mutate(V153= plyr::mapvalues(V153, from = c("5", "3", "1"), to = c("1","2", "3"))) %>%
  mutate(V154= plyr::mapvalues(V154, from = c("5", "3", "1"), to = c("1","2", "3"))) %>%
  mutate(V155= plyr::mapvalues(V155, from = c("5", "3", "1"), to = c("1","2", "3")))
  
## Recode 159

data_fl <- data_fl %>% 
  mutate(V156 = plyr::mapvalues(V156, from = c("6", "1", "2", "3","4","5","7","8"), to = c("1","0","0","0","0","0","0","0"))) %>%
  mutate(V158 = ifelse(V158=="9", "1", "0")) %>%
  mutate(V159 = ifelse(V159=="1", "1", "0"))
  
## Recode 196

data_fl <- data_fl %>%   
  mutate(V179 = plyr::mapvalues(V179, from = c("5","4", "3", "2", "1"), to = c("1","2","3", "4","5"))) %>%
  mutate(V180= plyr::mapvalues(V180, from = c("97", "98"), to = c("1","1"))) %>%
  mutate(V189= plyr::mapvalues(V189, from = c("5", "3", "2", "1"), to = c("0","1","2", "3"))) %>%
  mutate(V190= plyr::mapvalues(V190, from = c("5"), to = c("8"))) %>%
  mutate(V191= plyr::mapvalues(V191, from = c("5"), to = c("8"))) %>%
  mutate(V192= plyr::mapvalues(V192, from = c("5"), to = c("8"))) %>%
  mutate(V193= plyr::mapvalues(V193, from = c("5"), to = c("8"))) %>%
  mutate(V194= plyr::mapvalues(V194, from = c("5"), to = c("8"))) %>%
  mutate(V195= plyr::mapvalues(V195, from = c("5"), to = c("8"))) %>%
  mutate(V196= plyr::mapvalues(V196, from = c("5"), to = c("8"))) 
  

## Recode 380

data_fl <- data_fl %>% 
  mutate(V201= plyr::mapvalues(V201, from = c("4", "3", "2", "1"), to = c("0","1","2", "3"))) %>%
  mutate(V204= plyr::mapvalues(V204, from = c("0"), to = c("98"))) %>%
  mutate(V207= plyr::mapvalues(V207, from = c("90"), to = c("98"))) %>%
  mutate(V208= plyr::mapvalues(V208, from = c("90"), to = c("98"))) %>%
  mutate(V364 = plyr::mapvalues(V364, from = c("1", "2", "3","4","5","6","7"), to = c("7","6","5","4","3","2","1"))) %>%
  mutate(V380 = plyr::mapvalues(V380, from = c("1", "2", "3","4","5","6","7","0"), to = c("7","6","5","4","3","2","1","4")))


## Recode the response variable
  
data_fl <- data_fl %>% 
  mutate(vote= plyr::mapvalues(V502, from = c("0", "1", "5"), to = c("8","1","0")))


## Recode 380

data_fl <- data_fl %>% 
  mutate(V648= plyr::mapvalues(V648, from = c("5"), to = c("0"))) 

## Recode parents

data_fl <- data_fl %>% 
  mutate(P_V60= plyr::mapvalues(P_V60, from = c("5", "3", "1"), to = c("1","2", "3"))) %>%
  mutate(P_V136= plyr::mapvalues(P_V136, from = c("4", "3", "2", "1"), to = c("1","2","3", "4"))) %>%
  mutate(P_V168= plyr::mapvalues(P_V168, from = c("7", "3", "2", "1"), to = c("1","1","2", "3"))) %>%
  mutate(P_V181 = plyr::mapvalues(P_V181, from = c("5"), to = c("0"))) %>%
  mutate(P_V185 = plyr::mapvalues(P_V185, from = c("5"), to = c("0"))) %>%
  mutate(P_V195= plyr::mapvalues(P_V195, from = c("0"), to = c("98"))) %>%
  mutate(P_V242= plyr::mapvalues(P_V242, from = c("4", "3", "2", "1"), to = c("1","2","3", "4"))) %>%
  mutate(P_V243= plyr::mapvalues(P_V243, from = c("4", "3", "2", "1"), to = c("1","2","3", "4"))) %>%
  mutate(P_V258= plyr::mapvalues(P_V258, from = c("80", "88"), to = c("98","98"))) %>%
  mutate(P_V392 = plyr::mapvalues(P_V392, from = c("1", "2", "3","4","5","6","7"), to = c("7","6","5","4","3","2","1"))) %>%
  mutate(P_V408 = plyr::mapvalues(P_V408, from = c("1", "2", "3","4","5","6","7"), to = c("7","6","5","4","3","2","1")))


## Recode others

data_fl <- data_fl %>% 
  mutate(P_V181 = plyr::mapvalues(P_V181, from = c("0"), to = c("9"))) %>%
  mutate(V59 = plyr::mapvalues(V59, from = c("3"), to = c("9"))) 



  
## missing value
cols_select <- c("V180","V188","V204","V207","V208","P_V195", "P_V258")
cols_rest <- setdiff(colnames(data_fl), cols_select)
data_fl_p1 <- data_fl[,cols_select]
data_fl_p2 <- data_fl[,cols_rest]
data_fl_p1[data_fl_p1=="98" | data_fl_p1=="99"] <- NA
data_fl_p2[data_fl_p2=="8" | data_fl_p2=="9"] <- NA
data_final <- bind_cols(data_fl_p1, data_fl_p2)


## subsamples

table(data_final$V231)

data_male <- data_final %>%
  filter(V231 == 1)

data_female <- data_final %>%
  filter(V231 == 2)


### Table 1: regression analysis ####


## Impute missing values

imputed_data <- knnImputation(data_final, k = 5) 
imputed_data <- as.data.frame(imputed_data)


imputed_male <- knnImputation(data_male, k = 5) 
imputed_female <- knnImputation(data_female, k = 5) 


## Logistic regression

table(imputed_data$V231) #gender binary
table(imputed_data$V648) # college binary
table(imputed_data$V204) # SES numeric
table(imputed_data$V77)  # friends ordinal
table(imputed_data$V151) # personality ordinal
table(imputed_data$V116) # mother ordinal
table(imputed_data$V193) # high school activism
table(imputed_data$V59)  # political efficacy

# controls
table(imputed_data$V201) # religious
table(imputed_data$V232) # race, categorical
table(imputed_data$V99) # divorce
table(imputed_data$V209) # n of siblings
table(imputed_data$V158) # political knowledge
table(imputed_data$V179) # GPA

## the whole dataset

whole1 <- glm(as.factor(vote) ~ as.factor(V231)  + as.numeric(V77)*as.factor(V231) + as.numeric(V116)*as.factor(V231) + as.numeric(V59)*as.factor(V231) + as.numeric(V151)*as.factor(V231)+ as.numeric(V204)*as.factor(V231) + as.factor(V648)*as.factor(V231), data = imputed_data, family = binomial)



summary(whole1)

## only male


male1<- glm(as.factor(vote) ~ as.numeric(V77)  + as.numeric(V116) + as.numeric(V151)+ as.numeric(V59), data = imputed_male, family = binomial)

summary(male1)



male2 <- glm(as.factor(vote) ~ as.numeric(V77) + as.numeric(V116) + as.numeric(V151) + as.factor(V648) + as.numeric(V204) + as.numeric(V59), data = imputed_male, family = binomial)

summary(male2)



male3 <- glm(as.factor(vote) ~ as.numeric(V77)  + as.numeric(V116) + as.numeric(V151) + as.factor(V648) + as.numeric(V204) + as.numeric(V59)+ as.numeric(V201) + as.factor(V232)+ as.factor(V99) + as.numeric(V209) + as.numeric(V158) + as.numeric(V179), data = imputed_male, family = binomial)

summary(male3)



## only female

female1 <- glm(as.factor(vote) ~ as.numeric(V77) + as.numeric(V116) + as.numeric(V151) + as.numeric(V59), data = imputed_female, family = binomial)

summary(female1) 



female2 <- glm(as.factor(vote) ~ as.numeric(V77) + as.numeric(V116) + as.numeric(V151) + as.numeric(V59) + as.factor(V648) + as.numeric(V204), data = imputed_female, family = binomial)

summary(female2) ## 77, 116, 193 remains


female3 <- glm(as.factor(vote) ~ as.numeric(V77) + as.numeric(V116) + as.numeric(V151) + as.numeric(V59) + as.factor(V648) + as.numeric(V204) + as.numeric(V201) + as.factor(V232)+ as.factor(V99) + as.numeric(V209) + as.numeric(V158) + as.numeric(V179), data = imputed_female, family = binomial)

summary(female3)

