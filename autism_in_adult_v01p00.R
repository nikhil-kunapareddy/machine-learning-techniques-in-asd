# Importing necessary libraries.
library(ggplot2)
library(dplyr)
library(tidyverse)
library(modelr)
library(caret)
library(Metrics)
library(stats)
library(MASS)
library(DHARMa)
library(reshape2)
library(corrplot) 
library(vcd)
library(PRROC)

# Directory path.
setwd("/Users/nikhilkunapareddy/Documents/Masters/DS5110/5_Project/Dataset/")
# Loading the .csv file into a dataframe.
autism_in_adult <- read.csv('csv_result-Autism-Adult-Data.csv')
# Omitting NA values in the dataframe.
autism_in_adult <- na.omit(autism_in_adult)
view(autism_in_adult)
# Mutating the dataframe such that the survey choice and diagnosis result is included. 
autism_in_adult <- autism_in_adult %>%  
  dplyr::select(2:11, 22)
column_header <- c("Sensitivity_to_hearing", "Broad_overview", "Ability_to_track_multiple_coversations", "Multitasking_ability", 
                   "Conversational_ability", "Socializing_ability", "Empathy", "Enjoying_pretrending_games", "Understanding_others",
                   "Issues_in_finding_new_friends", "Diagnosis")
names(autism_in_adult) <- column_header
# Changing Yes and No entries to 0 and 1.
autism_in_adult$Diagnosis <- ifelse(autism_in_adult$Diagnosis == "YES", 1, 0)
# Converting the datatypes of the dataframe.
autism_in_adult[, 1:11] <- lapply(autism_in_adult[, 1:11], as.factor)
# view(autism_in_adult)
# str(autism_in_adult)
# Long table
autism_in_adult_long <- autism_in_adult %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "factor")
# Plot with ggplot2
ggplot(autism_in_adult_long, aes(x = factor)) +
  geom_bar() +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = "Factor", y = "Count") +
  theme_minimal()
# Creating test and train data.
set.seed(1)  #For reproducibility
trainIndex_three <- createDataPartition(autism_in_adult$Diagnosis, p = 0.5, 
                                  list = FALSE, times = 1)
train_data_three <- autism_in_adult[trainIndex, ]
test_data_three <- autism_in_adult[-trainIndex, ]

chi_square_result <- chisq.test(train_data_three$Diagnosis, train_data_three$Sensitivity_to_hearing)  
print(chi_square_result)
#X-squared = 18.008, df = 1, p-value = 2.2e-05
chi_square_result <- chisq.test(train_data_three$Diagnosis, train_data_three$Broad_overview)  
print(chi_square_result)  
#X-squared = 12.082, df = 1, p-value = 0.0005092
chi_square_result <- chisq.test(train_data_three$Diagnosis, train_data_three$Ability_to_track_multiple_coversations)  
print(chi_square_result)
#X-squared = 16.436, df = 1, p-value = 5.033e-05
chi_square_result <- chisq.test(train_data_three$Diagnosis, train_data_three$Multitasking_ability)  
print(chi_square_result) 
#X-squared = 30.899, df = 1, p-value = 2.718e-08
chi_square_result <- chisq.test(train_data_three$Diagnosis, train_data_three$Conversational_ability)  
print(chi_square_result)
#X-squared = 37.612, df = 1, p-value = 8.633e-10
chi_square_result <- chisq.test(train_data_three$Diagnosis, train_data_three$Socializing_ability)  
print(chi_square_result)  
#X-squared = 58.428, df = 1, p-value = 2.108e-14
chi_square_result <- chisq.test(train_data_three$Diagnosis, train_data_three$Empathy)  
print(chi_square_result)
#X-squared = 25.078, df = 1, p-value = 5.505e-07
chi_square_result <- chisq.test(train_data_three$Diagnosis, train_data_three$Enjoying_pretrending_games)  
print(chi_square_result) 
#X-squared = 12.767, df = 1, p-value = 0.0003529
chi_square_result <- chisq.test(train_data_three$Diagnosis, train_data_three$Understanding_others)  
print(chi_square_result)
#X-squared = 47.289, df = 1, p-value = 6.124e-12
chi_square_result <- chisq.test(train_data_three$Diagnosis, train_data_three$Issues_in_finding_new_friends)  
print(chi_square_result) 
#X-squared = 14.852, df = 1, p-value = 0.0001163
# Building a regression model.
regression_model_three <- glm(Diagnosis ~ Socializing_ability + Understanding_others, 
                              data = train_data_three, family = 'binomial')
summary(regression_model_three)
regression_model_three_predictions <- predict(regression_model_three, type = "response")
predictions_three <- c(regression_model_three_predictions)
true_values_three <- c(train_data_three$Diagnosis)
# Creating a ROC curve
sens <- function(c, p, ref, positive = levels(ref)[2])
{
  mean((p > c)[ref == positive], na.rm=TRUE)
}
sens(0.5, predictions_three, train_data_three$Diagnosis)
spec <- function(c, p, ref, negative = levels(ref)[1])
{
  mean((p < c)[ref == negative], na.rm=TRUE)
}
spec(0.5, predictions_three, train_data_three$Diagnosis)
roc <- tibble(p=seq(from=0, to=1, by=0.01)) %>%
  mutate(sensitivity = map_dbl(p, sens, p=predictions_three, ref=train_data_three$Diagnosis),
         specificity = map_dbl(p, spec, p=predictions_three, ref=train_data_three$Diagnosis),
         TPR=sensitivity,
         FPR=1 - specificity)
#view(roc)
ggplot(roc, aes(x=FPR, y=TPR)) + 
  geom_path(color="red", size=1) +
  geom_vline(xintercept=0, color="green", linetype="dotdash") +
  geom_hline(yintercept=1, color="green", linetype="dotdash") +
  geom_abline(intercept=0, slope=1, color="blue", linetype="dotted") +
  labs(title = "Precision-Recall Curve of 'Autism in Adults' Model",
       x="False positive rate (1 - specificity)",
       y="True positive rate (sensitivity)") +
  theme_minimal()