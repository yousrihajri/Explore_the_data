#Libraries
library(Hmisc)
library(funModeling)
library(tidyverse)

#Working Directory
setwd (dir = dir)

#Getting data
x <- heart_disease
attributes(x)

#EDA UNIVARIATE
status <- df_status(x) ###DF for all variables
description <- describe(x); description ###List for all variables
freq <- freq(x, path_out="feq_plot.jpg") ###for categorical (factor)
profiling <- profiling_num(x) ###for numerical (integer, numeric)
plot <- plot_num(x); plot ###Hist plot for numerical variables


#EDA BIVARIATE _ correlation

###Calculating R
cor(select(x, age, resting_blood_pressure, serum_cholestoral, max_heart_rate, oldpeak))
###Calculating R2
cor(select(x, age, resting_blood_pressure, serum_cholestoral, max_heart_rate, oldpeak)^2)
library(corrplot)
library(minerva) # contains MIC statistic Maximal Information-based nonparametric exploration
correlation_table(x, str_target = "has_heart_disease")
mic <- mine(select(x, age, resting_blood_pressure, serum_cholestoral, max_heart_rate, oldpeak))
###MIC value goes from 0 to 1. Being 0 implies no correlation and 1 highest correlation
###The interpretation is the same as the R-squared.
corrplot(cor(select(x, age, resting_blood_pressure, serum_cholestoral, max_heart_rate, oldpeak)^2))
corrplot(mic$MIC, method = "pie", diag=T, addCoef.col = "red", addCoefasPercent = T)
###Discretizing x to get all numeric
library(infotheo)
y <- discretize(x) %>% mutinformation()
diag(y)=0
corrplot(y, method = "color", type = "lower",
         number.cex = 0.6, addCoef.col = "black",
         tl.col = "red", tl.srt = 90, tl.cex = 0.9,
         diag = FALSE, is.corr = F)

#Converting categorical variables into numerical
library(caret) 
dmy = dummyVars(" ~ .", data = heart_disease) 
x2 = data.frame(predict(dmy, newdata = heart_disease))
heatmap(as.matrix(scale(x2)))

#Discretizing numerical variables
##The most standard binning criteria are: . Equal range . Equal frequency . Custom bins
###Equal range
x3 <- x
x3$age <- cut_interval(x3$age, 5)
describe(x3$age)
histogram(x3$age)

###Equal frequency
x3 <- x
x3$age <- equal_freq(x3$age, n_bins=5)
describe(x3$age)
histogram(x3$age)

###Custom bins
x3 <- x
x3$age <- cut(x3$age, breaks=c(20,30,40,50,60,70))
describe(x3$age)
histogram(x3$age)

###New data arrival
new_data_x3_age <- c(x3$age, 80,82,85)
describe(new_data_x3_age) #predictive model will fail

###High cardinality, creating factor "other"
data <- funModeling::data_country
freq(data, "country")
data$country <- ifelse(data$country %in% freq(data, "country", plot=F)[1:10,]$country, 
                       data$country,
                       "other")
freq(data, "country")
country_profiling <- categ_analysis(data, input = "country", target = "has_flu")

###High cardinality automatic grouping with care about the target variable

country_groups <- auto_grouping(data = data_country, input = "country",
                               target = "has_flu", n_groups = 9, seed = 999) 

head(country_groups$df_equivalence) ###If Argentine and Chile are in group_1, 
###then they are the same, and this is how the model will see it

