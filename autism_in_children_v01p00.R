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
library(rpart)
library(rpart.plot)
library(wrapr)

# Setting the directory path.
setwd("/Users/nikhilkunapareddy/Documents/Masters/DS5110/5_Project/Dataset/autistic+spectrum+disorder+screening+data+for+children")
# Loading the .csv file into a dataframe.
autism_in_children <- read.csv('Autism-Child-Data.csv')
# view(autism_in_children)
# str(autism_in_children)
# Omitting NA values in the dataframe.
autism_in_children <- na.omit(autism_in_children)
# view(autism_in_children)
# Mutating the dataframe such that the survey choice and diagnosis result are included. 
autism_in_children <- autism_in_children %>%  
  dplyr::select(1:15, 21)
# Assigning new column names to the dataframe.
column_header <- c("Sensitivity_to_hearing", "Broad_overview", "Ability_to_track_multiple_coversations", "Multitasking_ability", 
                   "Conversational_ability", "Socializing_ability", "Empathy", "Enjoying_pretrending_games", "Understanding_others",
                   "Issues_in_finding_new_friends", "Age", "Gender", "Ethnicity", "Jaundice", "Autism_in_family", "Diagnosis")
names(autism_in_children) <- column_header
view_autism_in_children <- autism_in_children
# Changing Yes and No entries to 0 and 1.
autism_in_children$Diagnosis <- ifelse(autism_in_children$Diagnosis == "YES", 1, 0)
# Converting the datatypes of the dataframe.
autism_in_children[, 1:16] <- lapply(autism_in_children[, 1:16], as.factor)
#autism_in_children[, 11] <- factor(autism_in_children[, 11])
# Converting the datatypes of the dataframe.
# autism_in_children[, 11] <- as.integer(autism_in_children[, 11])
# view(autism_in_children)
# str(autism_in_children)
# Creating a long table for data visualization.
autism_in_children_long <- autism_in_children %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "factor")
# Plot with ggplot2
ggplot(autism_in_children_long, aes(x = factor)) +
  geom_bar() +
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = 'A brief overview of the survey responses in "Autism in Children" dataset', 
  x = "Predictor Variable", y = "Count") +
  theme_minimal()
# Creating test and train data.
set.seed(1)  #For reproducibility
trainIndex <- createDataPartition(autism_in_children$Diagnosis, p = 0.5, 
                                  list = FALSE, times = 1)
train_data_one <- autism_in_children[trainIndex, ]
test_data_one <- autism_in_children[-trainIndex, ]
view(train_data_one)
# Evaluating using chi-square test.
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Gender)  
print(chi_square_result)
# X-squared = 0.0061295, df = 1, p-value = 0.9376
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Ethnicity)  
print(chi_square_result)
# X-squared = 22.351, df = 10, p-value = 0.01341
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Jaundice)  
print(chi_square_result)
# X-squared = 0, df = 1, p-value = 1
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Autism_in_family)  
print(chi_square_result)
# X-squared = 1.7463e-05, df = 1, p-value = 0.9967
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Sensitivity_to_hearing)  
print(chi_square_result)
#X-squared = 27.924, df = 1, p-value = 1.262e-07
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Broad_overview)  
print(chi_square_result)  
#X-squared = 9.3791, df = 1, p-value = 0.002195
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Ability_to_track_multiple_coversations)  
print(chi_square_result)
#X-squared = 22.135, df = 1, p-value = 2.541e-06
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Multitasking_ability)  
print(chi_square_result) 
#X-squared = 52.945, df = 1, p-value = 3.43e-13
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Conversational_ability)  
print(chi_square_result)
#X-squared = 12.184, df = 1, p-value = 0.0004821
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Socializing_ability)  
print(chi_square_result)  
#X-squared = 14.14, df = 1, p-value = 0.0001697
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Empathy)  
print(chi_square_result)
#X-squared = 16.609, df = 1, p-value = 4.594e-05
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Enjoying_pretrending_games)  
print(chi_square_result) 
#X-squared = 20.754, df = 1, p-value = 5.223e-06
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Understanding_others)  
print(chi_square_result)
#X-squared = 36.41, df = 1, p-value = 1.598e-09
chi_square_result <- chisq.test(train_data_one$Diagnosis, train_data_one$Issues_in_finding_new_friends)  
print(chi_square_result) 
#X-squared = 19.97, df = 1, p-value = 7.866e-06
# Creating a regression model and storing predictions.
regression_model_one <- glm(Diagnosis ~ Multitasking_ability, 
                              data = train_data_one, family = 'binomial')
summary(regression_model_one)
regression_model_one_predictions <- predict(regression_model_one, type = "response")
predictions_one <- c(regression_model_one_predictions)
true_values_one <- c(train_data_one$Diagnosis)
# Plotting ROC curve.
sens <- function(c, p, ref, positive = levels(ref)[2])
{
  mean((p > c)[ref == positive], na.rm=TRUE)
}
sens(0.5, predictions_one, train_data_one$Diagnosis)
spec <- function(c, p, ref, negative = levels(ref)[1])
{
  mean((p < c)[ref == negative], na.rm=TRUE)
}
spec(0.5, predictions_one, train_data_one$Diagnosis)
roc <- tibble(p=seq(from=0, to=1, by=0.01)) %>%
  mutate(sensitivity = map_dbl(p, sens, p=predictions_one, ref=train_data_one$Diagnosis),
         specificity = map_dbl(p, spec, p=predictions_one, ref=train_data_one$Diagnosis),
         TPR=sensitivity,
         FPR=1 - specificity)
# view(roc)
ggplot(roc, aes(x=FPR, y=TPR)) + 
  geom_path(color="red", size=1) +
  geom_vline(xintercept=0, color="green", linetype="dotdash") +
  geom_hline(yintercept=1, color="green", linetype="dotdash") +
  geom_abline(intercept=0, slope=1, color="blue", linetype="dotted") +
  labs(title = "Precision-Recall Curve of 'Autism in Children' Model",
    x="False positive rate (1 - specificity)",
    y="True positive rate (sensitivity)") +
  theme_minimal()


view_aic <- view_autism_in_children
view_aic <- view_aic %>%
  filter_all(all_vars(. != "?"))
view_aic <- autism_in_children %>%  
  dplyr::select(12:16)
view_aic[, 1:5] <- lapply(view_aic[, 1:5], as.factor)

ggplot(view_aic, aes(x = Ethnicity), fill = Diagnosis) +
  geom_bar() +
  labs(title = "Distribution of respondants across various ethnicities", x = "Ethnicity", y = "Frequency")

view_aic_long <- view_aic %>%
  gather(key = "Variable", value = "Values", Jaundice, Autism_in_family) %>%
  mutate(Category = ifelse(Variable %in% c("Jaundice", "Autism_in_family"), NA, Diagnosis))

custom_labels <- c('Autism_in_family'="Presence of Autism in Family", 'Jaundice'="Effected with Jaundice during birth")
fill_labels <- c("Negative", "Positive")

ggplot(view_aic_long, aes(x = Values, fill = Diagnosis)) +
  geom_bar() +
  facet_wrap(~ Variable, scales = "free", labeller = as_labeller(custom_labels)) +
  labs(title = "Distribution of Autism Diagnosis Results Across Various Parameters", x = "Categories", y = "Count of responses", fill = "Diagnosis Result") +
  scale_fill_manual(values = c("NO" = "lightgreen", "YES" = "darkgreen"), labels = fill_labels)

children <- autism_in_children
children_long <- children %>%
  gather(key = "Variable", value = "Values", Sensitivity_to_hearing, Broad_overview, 
         Ability_to_track_multiple_coversations, Multitasking_ability, Conversational_ability,
         Socializing_ability, Empathy, Enjoying_pretrending_games, Understanding_others, 
         Issues_in_finding_new_friends) %>%
  mutate(Category = ifelse(Variable %in% c("Sensitivity_to_hearing", "Broad_overview", 
                                           "Ability_to_track_multiple_coversations",
                                           "Multitasking_ability", "Conversational_ability",
                                           "Socializing_ability", "Empathy", "Enjoying_pretrending_games", 
                                           "Understanding_others", "Issues_in_finding_new_friends"), NA, Diagnosis))

custom_labels_1 <- c('Sensitivity_to_hearing'="Hearing Sensitivity", 
                   'Broad_overview'="Broad Overview",
                   'Ability_to_track_multiple_coversations'="Tracking multiple conversations", 
                   'Multitasking_ability'="Multitasking ability",
                   'Conversational_ability'="Conversational ability", 
                   'Socializing_ability'="Socializing ability",
                   'Empathy'="Empathy", 
                   'Enjoying_pretrending_games'="Enjoying pretending",
                   'Understanding_others'="Understanding others", 
                   'Issues_in_finding_new_friends'="Issues with finding new friends")
ggplot(children_long, aes(x = Values, fill = Diagnosis)) +
  geom_bar() +
  facet_wrap(~ Variable, scales = "free", labeller = as_labeller(custom_labels_1)) +
  labs(title = "Overview of Survey Responses", x = "Variables", y = "Count of responses", fill = "Diagnosis Result") +
  scale_fill_manual(values = c("NO" = "lightgreen", "YES" = "darkgreen"), labels = fill_labels)