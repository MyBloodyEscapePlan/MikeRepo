install.packages("randomForest")
library("randomForest")

#Since a single tree has too much variance,
#I want to use an averaged ensemble of trees

#First tune for optimum number of features randomly chosen
#at each split

