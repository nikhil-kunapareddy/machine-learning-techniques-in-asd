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
autism_in_sdolescent <- read.csv('Autism-Adolescent-Data.csv')
# Omitting NA values in the dataframe.
autism_in_sdolescent <- na.omit(autism_in_sdolescent)
# Mutating the dataframe such that the survey choice and diagnosis result is included. 
autism_in_sdolescent <- autism_in_sdolescent %>%  
  dplyr::select(2:11, 22)
column_header <- c("Sensitivity_to_hearing", "Broad_overview", "Ability_to_track_multiple_coversations", "Multitasking_ability", 
                   "Conversational_ability", "Socializing_ability", "Empathy", "Enjoying_pretrending_games", "Understanding_others",
                   "Issues_in_finding_new_friends", "Diagnosis")
names(autism_in_sdolescent) <- column_header
# Changing Yes and No entries to 0 and 1.
autism_in_sdolescent$Diagnosis <- ifelse(autism_in_sdolescent$Diagnosis == "YES", 1, 0)
# Converting the datatypes of the dataframe.
autism_in_sdolescent[, 1:11] <- lapply(autism_in_sdolescent[, 1:11], as.factor)
# view(autism_in_sdolescent)
# str(autism_in_sdolescent)
# Long table
autism_in_sdolescent_long <- autism_in_sdolescent %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "factor")
# Plot with ggplot2
ggplot(autism_in_sdolescent_long, aes(x = factor)) +
  geom_bar() +
  facet_wrap(~ variable, scales = "free_x") +
  labs(x = "Factor", y = "Count") +
  theme_minimal()
# Creating test and train data.
set.seed(1)  #For reproducibility
trainIndex_two <- createDataPartition(autism_in_sdolescent$Diagnosis, p = 0.5, 
                                      list = FALSE, times = 1)
train_data_two <- autism_in_sdolescent[trainIndex_two, ]
test_data_two <- autism_in_sdolescent[-trainIndex_two, ]
# Chi-squared test
chi_square_result <- chisq.test(train_data_two$Diagnosis, train_data_two$Sensitivity_to_hearing)  
print(chi_square_result)
#X-squared = 1.5474, df = 1, p-value = 0.2135
chi_square_result <- chisq.test(train_data_two$Diagnosis, train_data_two$Broad_overview)  
print(chi_square_result)  
#X-squared = 1.2853, df = 1, p-value = 0.2569
chi_square_result <- chisq.test(train_data_two$Diagnosis, train_data_two$Ability_to_track_multiple_coversations)  
print(chi_square_result)
#X-squared = 12.23, df = 1, p-value = 0.0004703
chi_square_result <- chisq.test(train_data_two$Diagnosis, train_data_two$Multitasking_ability)  
print(chi_square_result) 
#X-squared = 16.669, df = 1, p-value = 4.451e-05
chi_square_result <- chisq.test(train_data_two$Diagnosis, train_data_two$Conversational_ability)  
print(chi_square_result)
#X-squared = 6.3163, df = 1, p-value = 0.01196
chi_square_result <- chisq.test(train_data_two$Diagnosis, train_data_two$Socializing_ability)  
print(chi_square_result)  
#X-squared = 8.225, df = 1, p-value = 0.004132
chi_square_result <- chisq.test(train_data_two$Diagnosis, train_data_two$Empathy)  
print(chi_square_result)
#X-squared = 7.9277, df = 1, p-value = 0.004868
chi_square_result <- chisq.test(train_data_two$Diagnosis, train_data_two$Enjoying_pretrending_games)  
print(chi_square_result) 
#X-squared = 8.4769, df = 1, p-value = 0.003597
chi_square_result <- chisq.test(train_data_two$Diagnosis, train_data_two$Understanding_others)  
print(chi_square_result)
#X-squared = 10.14, df = 1, p-value = 0.001451
chi_square_result <- chisq.test(train_data_two$Diagnosis, train_data_two$Issues_in_finding_new_friends)  
print(chi_square_result) 
#X-squared = 14.202, df = 1, p-value = 0.0001642
# Creating regression model.
regression_model_two <- glm(Diagnosis ~ train_data_two$Multitasking_ability, 
                            data = train_data_two, family = 'binomial')
summary(regression_model_two)
regression_model_two_predictions <- predict(regression_model_two, type = "response")
predictions_two <- c(regression_model_two_predictions)
true_values_two <- c(train_data_two$Diagnosis)
# Plotting ROC curve.
sens <- function(c, p, ref, positive = levels(ref)[2])
{
  mean((p > c)[ref == positive], na.rm=TRUE)
}
sens(0.5, predictions_two, train_data_two$Diagnosis)
spec <- function(c, p, ref, negative = levels(ref)[1])
{
  mean((p < c)[ref == negative], na.rm=TRUE)
}
spec(0.5, predictions_two, train_data_two$Diagnosis)
roc <- tibble(p=seq(from=0, to=1, by=0.01)) %>%
  mutate(sensitivity = map_dbl(p, sens, p=predictions_two, ref=train_data_two$Diagnosis),
         specificity = map_dbl(p, spec, p=predictions_two, ref=train_data_two$Diagnosis),
         TPR=sensitivity,
         FPR=1 - specificity)
#view(roc)
ggplot(roc, aes(x=FPR, y=TPR)) + 
  geom_path(color="red", size=1) +
  geom_vline(xintercept=0, color="green", linetype="dotdash") +
  geom_hline(yintercept=1, color="green", linetype="dotdash") +
  geom_abline(intercept=0, slope=1, color="blue", linetype="dotted") +
  labs(title = "Precision-Recall Curve of 'Autism in Adoloscent' Model",
       x="False positive rate (1 - specificity)",
       y="True positive rate (sensitivity)") +
  theme_minimal()