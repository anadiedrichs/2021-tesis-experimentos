library(here)
source(here("utils","dataset.R"))
library(ranger)
library(dplyr)
library(pROC)

porc_train = 0.68

#' Junín mejor modelo, cutoff
#' 
#' 
dataset <- get_dataset_for_classification_all("junin")
#' filtro Tunuyan quien más ayuda
dataset <- dataset %>% select(starts_with("junin") | starts_with("tunuyan")| starts_with("tmin"))

set.seed(963)
until <- round(nrow(dataset)*porc_train)
train_set = dataset[1:until-1, ] 
test_set = dataset[until:nrow(dataset), ]

# random forest
rf.fit <- ranger(tmin ~ ., data = train_set,probability = TRUE)
pred.rf <- predict(rf.fit,test_set)

r <- roc(test_set$tmin,pred.rf$predictions[,1])

plot(r,print.auc=TRUE,print.thres=TRUE,grid(0.1,0.1))
#' PR curve 