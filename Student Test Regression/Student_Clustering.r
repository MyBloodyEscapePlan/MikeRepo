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

ggplot(performance_df, aes(race.ethnicity, math.score)) +
  geom_boxplot()

encoded_df = read.csv("Encoded Performance.csv")

ggplot(encoded_df, aes(as.character(Group), Math)) +
  geom_boxplot()


ggplot(encoded_df, aes(Math, Reading, color = Gender)) +
  geom_point()

cor(encoded_df["Math"], encoded_df["Reading"])

ggplot(encoded_df, aes(Math, Writing, color = Gender)) +
  geom_point()

cor(encoded_df["Math"], encoded_df["Writing"])

cor(encoded_df["Reading"], encoded_df["Writing"])

male_performance <- performance_df %>%
  filter(gender == "male")

ggplot(male_performance, aes(math.score, color = race.ethnicity)) +
  geom_histogram()

standard_male_perf <- male_performance %>%
  filter(lunch == "standard")

reduced_male_perf <- male_performance %>%
  filter(lunch == "free/reduced")

ggplot(reduced_male_perf, aes(math.score, color = race.ethnicity)) +
  geom_histogram()

#Compares math score means of standard and reduced lunch males
compare_male_by_lunch <- male_performance %>%
  group_by(lunch) %>%
  summarize(
    count = n(),
    mean(math.score)
  )

standard_male_math <- standard_male_perf$math.score

reduced_male_math <- reduced_male_perf$math.score

#t-test for standard and reduced lunch male math scores
t.test(standard_male_math, reduced_male_math)

#Distribution of math scores for reduced lunch males
ggplot(standard_male_perf, aes(math.score, color = race.ethnicity)) +
  geom_histogram()

#Performance data for the female students
female_performance <- performance_df %>%
  filter(gender == "female")
  
#Distribution of math score for overall female population
ggplot(female_performance, aes(math.score, color = race.ethnicity)) +
  geom_histogram()

compare_female_by_lunch <- female_performance %>%
  group_by(lunch) %>%
  summarize(
    mean(math.score)
  )
  
standard_lunch_students <- performance_df %>%
  filter(lunch == "standard")

reduced_lunch_students <- performance_df %>%
  filter(lunch == "free/reduced")

#Significant differene in math scores
t.test(standard_lunch_students$math.score, reduced_lunch_students$math.score)

reduced_completed_count = 0

for(test in reduced_lunch_students$test.preparation.course) {
  if(test == "completed") {
    
    reduced_completed_count = reduced_completed_count + 1
    
  }
}

standard_completed_count = 0

for(test in standard_lunch_students$test.preparation.course) {
  if(test == "completed") {
    
    standard_completed_count = standard_completed_count + 1
    
  }
}

#Proportion of reduced lunch kids who completed test preparation
proportion_reduced_completed_prep = reduced_completed_count/length(reduced_lunch_students$test.preparation.course)

#Proportion of standard lunch kids who completed test preparation
proportion_standard_completed_prep = standard_completed_count/length(standard_lunch_students$test.preparation.course)

#Proportions are about the same. Test preparation does not account for the difference 
#between the two lunch groups

#Groups reduced lunch students by race and compares mean math scores
reduced_lunch_by_race <- reduced_lunch_students %>%
  group_by(race.ethnicity) %>%
  summarize(
    count = n(),
    mean(math.score)
  )

standard_lunch_by_race <- standard_lunch_students %>%
  group_by(race.ethnicity) %>%
  summarize(
    count = n(),
    mean(math.score)
  )

#Displays count of race in reduced lunch populace and proportion completed test prep
ggplot(reduced_lunch_students, aes(race.ethnicity, fill = test.preparation.course)) +
  geom_bar()

#Displays count of race in standard lunch populace and proportion completed test prep
ggplot(standard_lunch_students, aes(race.ethnicity, fill = test.preparation.course)) +
  geom_bar()

#Boxplot comparing student math scores by parents' levels of education
ggplot(performance_df, aes(parental.level.of.education, math.score)) +
  geom_boxplot()

#Dataset of college educated parents
high_edu_parents <- performance_df %>%
  filter(parental.level.of.education %in% c("bachelor's degree", "master's degree", "associate's degree")) %>%
  mutate(Average.math.score = mean(math.score))

#Dataset of standard educated parents
stand_edu_parents <- performance_df %>%
  filter(parental.level.of.education %in% c("some college", "some high school", "high school")) %>%
  mutate(Average.math.score = mean(math.score))

#There is a statistically-significant difference between math scores of students 
#with higher educated parents than students without
t.test(high_edu_parents$math.score, stand_edu_parents$math.score)

#Next block counts the number of students with higher-educated parents
#in both lunch groups
high.edu.in.reduced.count = 0
high.edu.in.standard.count = 0
for(p in reduced_lunch_students$parental.level.of.education) {
  if(p == "bachelor's degree" | p == "master's degree" | p == "associate's degree") {
    high.edu.in.reduced.count = high.edu.in.reduced.count + 1
  }
}

for(p in standard_lunch_students$parental.level.of.education) {
  if(p == "bachelor's degree" | p == "master's degree" | p == "associate's degree") {
    high.edu.in.standard.count = high.edu.in.standard.count + 1
  }
}

#Proportion of higher-educated parents in both lunch groups are about the same(~40%)
#Parental education does not account for the difference in the lunch groups' math scores
prop.high.edu.in.reduced = high.edu.in.reduced.count/length(reduced_lunch_students$parental.level.of.education)
prop.high.edu.in.standard = high.edu.in.standard.count/length(standard_lunch_students$parental.level.of.education)

#Shows distribution of math scores, colored by race
ggplot(performance_df, aes(math.score, fill = race.ethnicity)) +
  geom_histogram()

demographic.count.overall <- performance_df %>%
  group_by(race.ethnicity) %>%
  summarize(
    count = n(),
    mean(math.score)
  )

demographic.count.standard <- standard_lunch_students %>%
  group_by(race.ethnicity) %>%
  summarize(
    count = n(),
    mean(math.score)
  )

demographic.count.reduced <- reduced_lunch_students %>%
  group_by(race.ethnicity) %>%
  summarize(
    count = n(),
    mean(math.score)
  )

ggplot(performance_df, aes(race.ethnicity, math.score)) +
  geom_boxplot()

#Removes any rows with a math score of 0
i = 0
for(score in performance_df$math.score){
  i = i + 1
  if(as.integer(score) < 25){
    performance_df_1 <- performance_df[-i,]
  }
}

ggplot(performance_df_1, aes(race.ethnicity, math.score)) +
  geom_boxplot()

#Sereis of t-tests comparing math scores by race
group_A_performance <- performance_df %>%
  filter(race.ethnicity == "group A")


group_C_performance <- performance_df %>%
  filter(race.ethnicity == "group C")

group_D_performance <- performance_df %>%
  filter(race.ethnicity == "group D")

group_E_performance <- performance_df %>%
  filter(race.ethnicity == "group E")

#Group A vs Group C
t.test(group_A_performance$math.score, group_C_performance$math.score)

#Group A vs Group E
t.test(group_A_performance$math.score, group_E_performance$math.score)

#Group C vs Group E
t.test(group_C_performance$math.score, group_E_performance$math.score)

#Group D vs Group E
t.test(group_D_performance$math.score, group_E_performance$math.score)

#Group D vs Group C
t.test(group_D_performance$math.score, group_C_performance$math.score)

#Group D vs Group A
t.test(group_D_performance$math.score, group_A_performance$math.score)

#Randomly shuffles rows in dataset
new_perf_df <- encoded_df[sample(nrow(encoded_df)),]

#Split data into train, validation, and test
set.seed(1)
assignment <- sample(1:3, size = nrow(new_perf_df), prob = c(.7, .15, .15), replace = TRUE)
train_df <- new_perf_df[assignment == 1,]
val_df <- new_perf_df[assignment == 2,]
test_df <- new_perf_df[assignment == 3,]

#Fit linear regression models
#model_1 <- lm(Math ~ Group + Lunch + Parent.Education, data = train_df)
#wrapFTest(model_1)
#model_2 <- lm(Math ~ Group + Lunch, data = train_df)
#wrapFTest(model_2)

#R-squared for both models is considerably low. Maybe the data is non-linear

#Use tree-based regression model
fmla_1 <- Math ~ Group + Lunch + Parent.Education
fmla_2 <- Math ~ Group + Lunch
tree_model_1 <- rpart(fmla_1, data = train_df, method = "anova")

#Tree plot of first model
rpart.plot(tree_model_1, yesno = 2, type = 0, extra = 0)

tree_model_2 <- rpart(fmla_2, data = train_df, method = "anova")
rpart.plot(tree_model_2, yesno = 2, type = 0, extra = 0)

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

#Model 3 includes gender and test prep
fmla_3 <- Math ~ Group + Lunch + Gender + Test.Prep
tree_model_3 <- rpart(fmla_3, data = train_df, method = "anova")
rpart.plot(tree_model_3, yesno = 2, type = 0, extra = 0)

#Calculates RMSE given the model and dataset
get_rmse <- function(model, df) {
  pred <- predict(model, df)
  return(sqrt(mean((df$Math - pred)^2)))
}

get_rmse(tree_model_1, train_df)
#get_rmse(tree_model_2, test_df)
get_rmse(tree_model_3, train_df)

#Model 3 is the best but needs pruning
plotcp(tree_model_3)
print(tree_model_3$cptable)

tree_model_3_opt <- prune(tree = tree_model_3, cp = .013)
rpart.plot(tree_model_3_opt, yesno = 2, type = 0, extra = 0)

get_rmse(tree_model_3_opt, val_df)

fmla_4 <- Math ~ Group + Test.Prep + Lunch + Gender + Parent.Education
fmla_5 <- Math ~ Lunch + Test.Prep

#Train a series of models to find the best one
minsplit <- seq(1, 20, 2)
maxdepth <- seq(5, 30, 10)

hyper_grid <- expand.grid(minsplit = minsplit, maxdepth = maxdepth)
nrow(hyper_grid)

models <- list()

for(i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  models[[i]] <- rpart(fmla_2, train_df, method = "anova", minsplit = minsplit, maxdepth = maxdepth)
}
#Find model with lowest rsme
rsme_values <- c()

for(i in 1:length(models)) {
  rsme_values[i] <- get_rmse(models[[i]], val_df)
} 
min(rsme_values)
best_model <- models[[which.min(rsme_values)]]
best_model$control

#Optimal formula is fmla_2 and lowest RMSE is 0.86
get_rmse(best_model, test_df)
plotcp(best_model)
rpart.plot(best_model, yesno = 2, type = 0, extra = 0)

#Since a single tree has too much variance,
#I want to use an averaged ensemble of trees

#First tune for optimum number of features randomly chosen
#at each split

feature_select <- c("Gender", "Group", "Parent.Education", "Lunch", "Test.Prep")
set.seed(123)
res <- tuneRF(x = subset(train_df, select = feature_select), y = train_df$Math, ntreeTry = 500)
print(res)
mtry_opt = 2

nrow(train_df)
mtry = seq(1, 3, 1) 
sampsize = nrow(train_df)*c(.75, 1)
nodesize = seq(3, 8, 1)

hyper_grid <- expand.grid(mtry = mtry, sampsize = sampsize, nodesize = nodesize)
nrow(hyper_grid)

oob_err <- c()
models <- list()
rsme_val <- c()

#Creates a model for each hyper-grid row and stores RSME in vector
for(i in 1:nrow(hyper_grid)) {
  model <- randomForest(formula = fmla_4, data = train_df, mtry = hyper_grid$mtry[i], nodesize = hyper_grid$nodesize[i], sampsize = hyper_grid$sampsize[i])
  
  rsme_val[i] <- get_rmse(model, val_df)
}
#Index where lowest RSME is
opt_i <- which.min(rsme_val)
min(rsme_val)
print(hyper_grid[opt_i,])

mtry_best = 1
sampsize_best = 696
nodesize_best = 5

#RF model using best hyperparameters 
best_rf_model <- randomForest(fmla_4, train_df, mtry = mtry_best, nodesize = nodesize_best, sampsize = sampsize_best)

#Random Forest shows a stark improvement over single regression tree
get_rmse(best_rf_model, test_df)
