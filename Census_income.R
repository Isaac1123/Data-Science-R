library(readr)
census_income <- read_csv("C:/Users/yclee/OneDrive/Desktop/Academic/Data Science R/census_income.csv")
View(census_income)

census_income$...15 <-as.character(census_income$...15)
census_income$...15 <- gsub("<=50K", "0",census_income$...15)
census_income$...15 <- gsub(">50K", "1", census_income$...15)
census_income$...15 <- as.numeric(census_income$...15)

normalize <-function(var){
  my_norm <- (var-min(var))/(max(var)-min(var))
  return(my_norm)
}

census_income$age_norm <- normalize(var=census_income$age)
census_income$education_num_norm <- normalize(var=census_income$education_num)
census_income$capital_gain_norm <- normalize(var=census_income$capital_gain)


#split the data

training_idx <- sample(1:nrow(census_income), size=0.8*nrow(census_income))
census_income_train <- census_income[training_idx,]
census_income_test <- census_income[-training_idx,]

#unit 

my_logit <- glm(...15 ~ age+education_num+capital_gain, 
                data= census_income_train, family= "binomial")
summary(my_logit)


#unitless

my_logit1 <- glm(...15 ~ age_norm+education_num_norm+capital_gain_norm, 
                data= census_income_train, family= "binomial")
summary(my_logit)