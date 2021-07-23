library(dplyr) # for data manipulation
library(caret) # for model-building
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations
library(ggplot2)
library(rpart)
set.seed(2969)

imbal_train <- twoClassSim(5000,
                           intercept = -25,
                           linearVars = 20,
                           noiseVars = 10)

imbal_test  <- twoClassSim(5000,
                           intercept = -25,
                           linearVars = 20,
                           noiseVars = 10)

#Distribition des classes
ggplot(data.frame(imbal_train$Class), aes(x=imbal_train$Class)) +geom_bar()
table(imbal_train$Class)
prop.table(table(imbal_train$Class))
#Nbre exact
n<-sum(imbal_train$Class=="Class1")
m<-5000-n
#--------------------------------------------#
#Entraînement du modèle des arbres de décision sur des données originelles
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE)

# Construction d'un modèle d'apprentissage suivant le modèle des arbres de décision 
#---------------------------------------------------------------------#
set.seed(5627)

# orig_fit <- train(Class ~ .,data = imbal_train,method = "gbm",verbose = FALSE,metric = "ROC",trControl = ctrl)

orig_fit <- train(Class ~., data = imbal_train, method = "rpart",trControl=ctrl,
                   tuneLength = 10, 
                   parms=list(split='information'),
                   metric = "ROC")

#Traditional Method

# model_control=rpart.control(xval=10,cp=0.00001)
# orig_fit = rpart(Class~.,control=model_control,data=imbal_train)
#---------------------------------------------------------------------#

# Build custom AUC function to extract AUC
# from the caret model object

test_roc <- function(model, data) {
  
  roc(data$Class,
      predict(model, data, type = "prob")[, "Class2"])
  
}

# orig_fit %>%
# test_roc(data = imbal_test) %>%
# auc()

dtree_fit %>%
  test_roc(data = imbal_test) %>%
  auc()

#-------------------------------------------------------------------#
#Apprentissage sur des données équilibrées

# Use the same seed to ensure same cross-validation splits

ctrl$seeds <- orig_fit$control$seeds

# Build down-sampled model

ctrl$sampling <- "down"

down_fit <- train(Class ~., data = imbal_train, method = "rpart",trControl=ctrl,
                   tuneLength = 10, 
                   parms=list(split='information'),
                   metric = "ROC")

# down_fit <- train(Class ~ .,
#                   data = imbal_train,
#                   method = "gbm",
#                   verbose = FALSE,
#                   metric = "ROC",
#                   trControl = ctrl)

# Build up-sampled model

ctrl$sampling <- "up"

up_fit <- train(Class ~., data = imbal_train, method = "rpart",trControl=ctrl,
                  tuneLength = 10, 
                  parms=list(split='information'),
                  metric = "ROC")



# up_fit <- train(Class ~ .,
#                 data = imbal_train,
#                 method = "gbm",
#                 verbose = FALSE,
#                 metric = "ROC",
#                 trControl = ctrl)

# Build smote model

ctrl$sampling <- "smote"


smote_fit <- train(Class ~., data = imbal_train, method = "rpart",trControl=ctrl,
                  tuneLength = 10, 
                  parms=list(split='information'),
                  metric = "ROC")

# smote_fit <- train(Class ~ .,
#                    data = imbal_train,
#                    method = "gbm",
#                    verbose = FALSE,
#                    metric = "ROC",
#                    trControl = ctrl)

#-----------------------------------------------------------#
#Test des modèles sur des données déséquilibrées

model_list <- list(original = orig_fit,
                   Undersamling = down_fit,
                   Oversampling = up_fit,
                   SMOTE = smote_fit)

model_list_roc <- model_list %>%
  map(test_roc, data = imbal_test)

model_list_roc %>%
  map(auc)

#--------------------------------------------------------------#
#Plot de la fonction ROC et comparaison des modèles des données déséquilibrées

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 5 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)