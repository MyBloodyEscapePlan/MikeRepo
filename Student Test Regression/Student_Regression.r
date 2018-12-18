install.packages("rpart")
install.packages("rpart.plot")
install.packages("sigr")
install.packages("randomForest")

library("randomForest")
library("readr")
library("ggplot2")
library("dplyr")
library("sigr")
library("rpart")
library("rpart.plot")

performance_df = read.csv("StudentsPerformance.csv")

head(performance_df)

#Reads in a dummified version of the original dataset
#The test scores are also standardized
encoded_df = read.csv("Encoded Performance.csv")


#Student math score has a linear relationship with both Reading and Writing.
#Therefore, I can infer overall test performance by math score alone.
#This makes my analysis much simpler

ggplot(encoded_df, aes(Math, Reading)) +
  geom_point()

ggplot(encoded_df, aes(Math, Writing)) +
  geom_point()

cor(encoded_df["Math"], encoded_df["Reading"])
cor(encoded_df["Math"], encoded_df["Writing"])

#Before modeling, I want to see how math z-score varies with other variables
ggplot(performance_df, aes(race.ethnicity, math.score)) +
  geom_boxplot()

#Add more boxplots
#Boxplot comparing student math scores by parents' levels of education
ggplot(performance_df, aes(parental.level.of.education, math.score)) +
  geom_boxplot()

#Put t-tests here
#Compares math scores between Standard and Reduced Lunch Students
standard_lunch_students <- performance_df %>%
  filter(lunch == "standard")

reduced_lunch_students <- performance_df %>%
  filter(lunch == "free/reduced")

#Significant difference in math scores for student lunch
t.test(standard_lunch_students$math.score, reduced_lunch_students$math.score)

#Sereis of t-tests comparing math scores by race
group_A_performance <- performance_df %>%
  filter(race.ethnicity == "group A")

group_C_performance <- performance_df %>%
  filter(race.ethnicity == "group C")

group_D_performance <- performance_df %>%
  filter(race.ethnicity == "group D")

group_E_performance <- performance_df %>%
  filter(race.ethnicity == "group E")

#Group A vs Group C: no sig
t.test(group_A_performance$math.score, group_C_performance$math.score)

#Group A vs Group E: sig
t.test(group_A_performance$math.score, group_E_performance$math.score)

#Group C vs Group E: sig
t.test(group_C_performance$math.score, group_E_performance$math.score)

#Group D vs Group E: sig
t.test(group_D_performance$math.score, group_E_performance$math.score)

#Group D vs Group C: sig
t.test(group_D_performance$math.score, group_C_performance$math.score)

#Group D vs Group A: sig
t.test(group_D_performance$math.score, group_A_performance$math.score)

#Look at gender
male_perf <- performance_df %>%
  filter(gender == "male")

female_perf <- performance_df %>%
  filter(gender == "female")

#t-test shows a significant difference in math performance
#between genders
t.test(male_perf$math.score, female_perf$math.score)

#Look at test prep
test_prep_completed <- performance_df %>%
  filter(test.preparation.course == "completed")

no_test_prep <- performance_df %>%
  filter(test.preparation.course == "none")

#t-test shows significant difference
t.test(test_prep_completed$math.score, no_test_prep$math.score)


#Randomly shuffles rows in dataset
new_perf_df <- encoded_df[sample(nrow(encoded_df)),]

#Split data into train, validation, and test
set.seed(1)
assignment <- sample(1:3, size = nrow(new_perf_df), prob = c(.7, .15, .15), replace = TRUE)
train_df <- new_perf_df[assignment == 1,]
val_df <- new_perf_df[assignment == 2,]
test_df <- new_perf_df[assignment == 3,]


#Decided to use a tree-based model since all of the predictors are categorical

#Since all of the predictors had significant t-tests, all of them are included in the formula
fmla <- Math ~ Gender + Group + Parent.Education + Lunch + Test.Prep

tree_model <- rpart(fmla, data = train_df, method = "anova")

#Plot of tree model
rpart.plot(tree_model, yesno = 2, type = 0, extra = 0)

#Calculates RMSE given the model and dataset
get_rmse <- function(model, df) {
  pred <- predict(model, df)
  return(sqrt(mean((df$Math - pred)^2)))
}

get_rmse(tree_model, val_df)


plotcp(tree_model)
print(tree_model$cptable)

tree_model_opt <- prune(tree = tree_model, cp = .010)
rpart.plot(tree_model_opt, yesno = 2, type = 0, extra = 0)

#The optimization of cp doesn't change the error
get_rmse(tree_model_opt, val_df)

#Train a series of models to find the best one
minsplit <- seq(1, 20, 2)
maxdepth <- seq(5, 30, 10)

hyper_grid <- expand.grid(minsplit = minsplit, maxdepth = maxdepth)
nrow(hyper_grid)

models <- list()

for(i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  models[[i]] <- rpart(fmla, train_df, method = "anova", minsplit = minsplit, maxdepth = maxdepth)
}
#Find model with lowest rsme
rsme_values <- c()

for(i in 1:length(models)) {
  rsme_values[i] <- get_rmse(models[[i]], val_df)
} 
min(rsme_values)
best_model <- models[[which.min(rsme_values)]]
best_model$control

#Gets RSME for optimal model
get_rmse(best_model, test_df)
plotcp(best_model)
rpart.plot(best_model, yesno = 2, type = 0, extra = 0)

#Since a single tree has too much variance,
#I want to use an averaged ensemble of trees (Random Forest)

#Creates sequences of hyperparameters for the random forest models
mtry = seq(1, 5, 1) 
sampsize = nrow(train_df)*c(.75, 1)
nodesize = seq(3, 8, 1)

hyper_grid <- expand.grid(mtry = mtry, sampsize = sampsize, nodesize = nodesize)
nrow(hyper_grid)


models <- list()
rsme_val <- c()

#Creates a model for each hyper-grid row and stores RSME in vector
for(i in 1:nrow(hyper_grid)) {
  model <- randomForest(formula = fmla, data = train_df, mtry = hyper_grid$mtry[i], nodesize = hyper_grid$nodesize[i], sampsize = hyper_grid$sampsize[i])
  
  rsme_val[i] <- get_rmse(model, val_df)
}
#Index where lowest RSME is
opt_i <- which.min(rsme_val)
min(rsme_val)
print(hyper_grid[opt_i,])

mtry_best = 2
sampsize_best = 522
nodesize_best = 7

#RF model using best hyperparameters 
best_rf_model <- randomForest(fmla, train_df, mtry = mtry_best, nodesize = nodesize_best, sampsize = sampsize_best)

#Even with a Random Forest model, the error is still quite high
get_rmse(best_rf_model, test_df)

