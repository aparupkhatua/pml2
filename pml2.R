# This is an R Markdown document desribing the project Step 1 : Reading the data files in data frames
setwd("F:/PML")
pml.training <- read.csv("pml-training.csv")
pml.testing <- read.csv("pml-testing.csv")
# Step 2 : There are features with lot of 'NA' , mising value and empty srting. As a thumbrule we have used a threshhold of 50% or 10000 attributes

trngsml <- data.frame(pml.training[1])
for (i in 2:ncol(pml.training)) {
  
  if ((sum(is.na(pml.training[i])) < 10000) & (sum(pml.training[i] == "") < 
                                                 10000) & (sum(pml.training[i] == "NA") < 10000)) {
    
    trngsml <- cbind(trngsml, pml.training[i])
    
  }
}
# In Step 2 , we select 59 fetaures , from a total of 160

# Step 3: Use Random Forest

library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
trngrf <- train(classe ~ ., trngsml, method = "rf", importance = TRUE, ntree = 50)
## Loading required package: randomForest
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
## Loading required package: class
confusionMatrix(trngrf)
## Bootstrapped (25 reps) Confusion Matrix 
## 
## (entries are percentages of table totals)
##  
##           Reference
## Prediction    A    B    C    D    E
##          A 28.4  0.0  0.0  0.0  0.0
##          B  0.0 19.5  0.0  0.0  0.0
##          C  0.0  0.0 17.4  0.0  0.0
##          D  0.0  0.0  0.0 16.3  0.0
##          E  0.0  0.0  0.0  0.0 18.4
# As randomforest is an ensemble model , we don't have a need for cross validation, the in sample error is zero , the out of sample error is lower bounded by zero.

# Step 4: Predict on testing with the model

p <- predict(trngrf, pml.testing)
p <- as.character(p)
# Step 5: Creating the Files for submisson

pml_write_files <- function(x) {
n = length(x)
for (i in 1:n) {
filename = paste0("problem_id_", i, ".txt")
write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
col.names = FALSE)
}
}
# setwd("H:/Tech/Practical Machine Learning/Week 3")
pml_write_files(p)
p <- as.character(p)
